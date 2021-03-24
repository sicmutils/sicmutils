;;
;; Copyright © 2021 Sam Ritchie.
;; This work is based on the Scmutils system of MIT/GNU Scheme:
;; Copyright © 2002 Massachusetts Institute of Technology
;;
;; This is free software;  you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or (at
;; your option) any later version.
;;
;; This software is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this code; if not, see <http://www.gnu.org/licenses/>.
;;

(ns sicmutils.calculus.metric
  (:require [sicmutils.calculus.basis :as b]
            [sicmutils.calculus.coordinate :as coord]
            [sicmutils.calculus.derivative :refer [D]]
            [sicmutils.calculus.form-field :as ff]
            [sicmutils.calculus.vector-field :as vf]
            [sicmutils.calculus.manifold :as m]
            [sicmutils.function :as f]
            [sicmutils.generic :as g]
            [sicmutils.matrix :as matrix]
            [sicmutils.structure :as s :refer [up down]]
            [sicmutils.value :as v]))

;; ## Metrics

;; A metric is a function that takes two vector fields and produces a function
;; on the manifold.

(defn embedding-map->metric-components [n xi->rectangular]
  (let [h (D xi->rectangular)]
    (if (= n 1)
      (down (down (g/dot-product h h)))
      (s/generate
       n ::s/down
       (fn [i]
         (s/generate
          n ::s/down
          (fn [j]
            (g/dot-product (nth h i)
                           (nth h j)))))))))

(defn coordinate-system->metric-components [coordsys]
  (let [n (:dimension (m/manifold coordsys))
        ;; assumes internal rectangular representation
        xi->x (f/compose m/manifold-point-representation
                         (m/point coordsys))]
    (embedding-map->metric-components n xi->x)))

(defn coordinate-system->metric [coordinate-system]
  (let [basis (b/coordinate-system->basis coordinate-system)
        oneform-basis (b/basis->oneform-basis basis)
        ->components (coordinate-system->metric-components
                      coordinate-system)
        Chi (m/chart coordinate-system)]
    (letfn [(the-metric [v1 v2]
              (fn [m]
                (let [gcoeffs (->components (Chi m))]
                  (g/* (g/* gcoeffs ((oneform-basis v1) m))
                       ((oneform-basis v2) m)))))]
      (with-meta the-metric
        {:arguments [::vf/vector-field
                     ::vf/vector-field]}))))

(defn coordinate-system->inverse-metric [coordinate-system]
  (let [basis (b/coordinate-system->basis coordinate-system)
        vector-basis (b/basis->vector-basis basis)
        ->components
        (g// 1 (coordinate-system->metric-components coordinate-system))
        Chi (m/chart coordinate-system)]
    (letfn [(the-inverse-metric [w1 w2]
              (fn [m]
                (let [gcoeffs (->components (Chi m))]
                  (g/* (g/* gcoeffs
                            (s/mapr (fn [e] ((w1 e) m))
                                    vector-basis))
                       (s/mapr (fn [e] ((w2 e) m))
                               vector-basis)))))]
      (with-meta the-inverse-metric
        {:arguments [::ff/one-form-field
                     ::ff/one-form-field]}))))

;; Symbolic metrics are often useful for testing.

(defn make-metric [name coordinate-system]
  (fn gij [i j]
    (if (<= i j)
      (m/literal-manifold-function
       (symbol (str name "_" i j))
       coordinate-system)
      (gij j i))))

(defn literal-metric [name coordinate-system]
  ;; Flat coordinate systems here only.
  (let [basis (b/coordinate-system->basis coordinate-system)
        oneform-basis (b/basis->oneform-basis basis)
        gij (make-metric name coordinate-system)
        n (g/dimension oneform-basis)
        gcoeffs (s/generate
                 n ::s/down
                 (fn [i]
                   (s/generate
                    n ::s/down
                    (fn [j]
                      (gij i j)))))]
    (letfn [(the-metric [v1 v2]
              (g/* (g/* gcoeffs (oneform-basis v1))
                   (oneform-basis v2)))]
      (with-meta the-metric
        {:arguments [::vf/vector-field
                     ::vf/vector-field]}))))

(defn components->metric [components basis]
  (let [oneform-basis (b/basis->oneform-basis basis)]
    (fn the-metric [v1 v2]
      (g/* (oneform-basis v1)
           (g/* components (oneform-basis v2))))))

(defn metric->components [metric basis]
  (let [vector-basis (b/basis->vector-basis basis)]
    (s/mapr (fn [e_i]
              (s/mapr (fn [e_j]
                        (metric e_i e_j))
                      vector-basis))
            vector-basis)))

;;; Given a metric and a basis, to compute the inverse metric

(defn metric->inverse-components [metric basis]
  (fn [m]
    (let [g_ij ((metric->components metric basis) m)
          oneform-basis (b/basis->oneform-basis basis)
          g-ij (matrix/s:inverse
                (s/typical-object oneform-basis)
                g_ij
                (s/typical-object oneform-basis))]
      g-ij)))

(defn invert [metric basis]
  (letfn [(the-inverse-metric [w1 w2]
            (let [vector-basis (b/basis->vector-basis basis)
                  g-ij (metric->inverse-components metric basis)]
              (g/* (g/* g-ij (s/mapr w1 vector-basis))
                   (s/mapr w2 vector-basis))))]
    (with-meta the-inverse-metric
      {:arguments [::ff/oneform-field
                   ::ff/oneform-field]})))

;; Over a map...

(defn metric-over-map [mu:N->M g-on-M]
  (letfn [(vector-field-over-map->vector-field [V-over-mu n]
            ;; This helper has no clear meaning.
            (vf/procedure->vector-field
             (fn [f]
               (fn [m]
                 ;;(assert (= m (mu:N->M n)))
                 ((V-over-mu f) n)))
             `(~'vector-field-over-map->vector-field
               ~(v/freeze V-over-mu))))
          (the-metric [v1 v2]
            (fn [n]
              ((g-on-M
                (vector-field-over-map->vector-field v1 n)
                (vector-field-over-map->vector-field v2 n))
               (mu:N->M n))))]
    (with-meta the-metric
      {:arguments [::vf/vector-field
                   ::vf/vector-field]})))

;; Raising and lowering indices...
;;
;; To make a vector field into a one-form field
;;  ie a (1,0) tensor into a (0,1) tensor

(defn lower [metric]
  (fn [u]
    (letfn [(omega [v]
              (metric v u))]
      (ff/procedure->oneform-field
       omega
       `(~'lower
         ~(v/freeze u)
         ~(v/freeze metric))))))

(def vector-field->oneform-field lower)
(def drop1 lower)

;; To make a one-form field  into a vector field
;;  ie a (0,1) tensor into a (1,0) tensor

(defn raise [metric basis]
  (let [gi (invert metric basis)]
    (fn [omega]
      (let [v (b/contract
               (fn [e_i e-i]
                 (g/* (gi omega e-i) e_i))
               basis)]
        (vf/procedure->vector-field
         v
         `(~'raise
           ~(v/freeze omega)
           ~(v/freeze metric)))))))

(def oneform-field->vector-field raise)
(def raise1 raise)

;; For making a (2,0) tensor into a (0,2) tensor

(defn drop2 [metric-tensor basis]
  (fn [tensor]
    (letfn [(omega [v1 v2]
              (b/contract
               (fn [e1 w1]
                 (b/contract
                  (fn [e2 w2]
                    (g/* (metric-tensor v1 e1)
                         (tensor w1 w2)
                         (metric-tensor e2 v2)))
                  basis))
               basis))]
      (with-meta omega
        {:arguments [::vf/vector-field
                     ::vf/vector-field]}))))

;; For making a (0,2) tensor into a (2,0) tensor

(defn raise2 [metric-tensor basis]
  (let [gi (invert metric-tensor basis)]
    (fn [tensor02]
      (letfn[(v2 [omega1 omega2]
               (b/contract
                (fn [e1 w1]
                  (b/contract
                   (fn [e2 w2]
                     (g/* (gi omega1 w1)
                          (tensor02 e1 e2)
                          (gi w2 omega2)))
                   basis))
                basis))]
        (with-meta v2
          {:arguments [::ff/oneform-field
                       ::ff/oneform-field]})))))

;; To compute the trace of a (0,2) tensor

(defn trace2down [metric-tensor basis]
  (let [inverse-metric-tensor (invert metric-tensor basis)]
    (fn [tensor02]
      (let [f (b/contract
               (fn [e1 w1]
                 (b/contract
                  (fn [e2 w2]
                    (g/* (inverse-metric-tensor w1 w2)
                         (tensor02 e1 e2)))
                  basis))
               basis)]
        (with-meta f
          {:arguments [::v/function]})))))

;; To compute the trace of a (2,0) tensor

(defn trace2up [metric-tensor basis]
  (fn [tensor20]
    (let [f (b/contract
             (fn [e1 w1]
               (b/contract
                (fn [e2 w2]
                  (g/* (metric-tensor e1 e2)
                       (tensor20 w1 w2)))
                basis))
             basis)]
      (with-meta f
        {:arguments [::v/function]}))))


;; Unfortunately raise is very expensive because the matrix is
;; inverted for each manifold point.

(defn sharpen [metric basis m]
  (let [g-ij ((metric->inverse-components metric basis) m)
	      vector-basis (b/basis->vector-basis basis)
	      oneform-basis (b/basis->oneform-basis basis)]
    (fn sharp [oneform-field]
      (let [oneform-coeffs
	          (s/mapr (fn [ei] ((oneform-field ei) m))
		                vector-basis)
            vector-coeffs (g/* g-ij oneform-coeffs)]
	      (s/sumr g/* vector-coeffs vector-basis)))))

;;; Useful metrics

(def S2-metric
  (let [[theta phi] (coord/coordinate-functions m/S2-spherical)
        [dtheta dphi] (ff/coordinate-system->oneform-basis m/S2-spherical)
        the-metric (fn [v1 v2]
                     (g/+ (g/* (dtheta v1) (dtheta v2))
	                        (g/* (g/expt (g/sin theta) 2)
	                             (dphi v1) (dphi v2))))]
    (with-meta the-metric
      {:arguments [::vf/vector-field
                   ::vf/vector-field]})))
