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
            [sicmutils.calculus.form-field :as ff]
            [sicmutils.calculus.vector-field :as vf]
            [sicmutils.calculus.manifold :as m]
            [sicmutils.generic :as g]
            [sicmutils.matrix :as matrix]
            [sicmutils.structure :as s]))

;; ## Metrics

;; A metric is a function that takes two vector fields and produces a function
;; on the manifold.

(comment
  (define (coordinate-system->metric-components coordsys)
    (let* ((n (coordsys 'dimension))
           (xi->x ;assumes internal rectangular representation
            (compose manifold-point-representation
                     (point coordsys))))
      (embedding-map->metric-components n xi->x)))

  (define (embedding-map->metric-components n xi->rectangular)
    (let ((h (D xi->rectangular)))
      (if (= n 1)
        (down (down (dot-product h h)))
        (s:generate n 'down
                    (lambda (i)
                            (s:generate n 'down
                                        (lambda (j)
                                                (dot-product (ref h i)
                                                             (ref h j))))))))))

(comment
  (define (coordinate-system->metric coordinate-system)
    (let* ((basis (coordinate-system->basis coordinate-system))
           (oneform-basis (basis->oneform-basis basis))
           (->components
            (coordinate-system->metric-components coordinate-system))
           (Chi (chart coordinate-system)))
      (define ((the-metric v1 v2) m)
        (let ((gcoeffs (->components (Chi m))))
          (* (* gcoeffs ((oneform-basis v1) m))
             ((oneform-basis v2) m))))
      (declare-argument-types! the-metric
                               (list vector-field? vector-field?))
      the-metric)))


(comment
  (define (coordinate-system->inverse-metric coordinate-system)
    (let* ((basis (coordinate-system->basis coordinate-system))
           (vector-basis (basis->vector-basis basis))
           (->components
            (/ 1
               (coordinate-system->metric-components coordinate-system)))
           (Chi (chart coordinate-system)))
      (define ((the-inverse-metric w1 w2) m)
        (let ((gcoeffs (->components (Chi m))))
          (* (* gcoeffs
                (s/mapr (lambda (e) ((w1 e) m))
                        vector-basis))
             (s/mapr (lambda (e) ((w2 e) m))
                     vector-basis))))
      (with-meta the-inverse-metric
        {:arguments [::ff/one-form-field
                     ::ff/one-form-field]}))))

;; Symbolic metrics are often useful for testing.

(comment
  (define (make-metric name coordinate-system)
    (define (gij i j)
      (if (<= i j)
        (literal-manifold-function
         (string->symbol
          (string-append (symbol->string name)
                         "_"
                         (number->string i)
                         (number->string j)))
         coordinate-system)
        (gij j i)))
    gij)

  (define (literal-metric name coordinate-system)
    ;; Flat coordinate systems here only.
    (let ((basis (coordinate-system->basis coordinate-system)))
      (let ((oneform-basis (basis->oneform-basis basis))
            (gij (make-metric name coordinate-system)))
        (let ((n (s:dimension oneform-basis)))
          (let ((gcoeffs
                 (s:generate n 'down
                             (lambda (i)
                                     (s:generate n 'down
                                                 (lambda (j)
                                                         (gij i j)))))))
            (define (the-metric v1 v2)
              (* (* gcoeffs (oneform-basis v1))
                 (oneform-basis v2)))
            (declare-argument-types! the-metric
                                     (list vector-field? vector-field?))
            the-metric))))))

(comment
  (define (components->metric components basis)
    (let ((oneform-basis (basis->oneform-basis basis)))
      (define (the-metric v1 v2)
        (* (oneform-basis v1) (* components (oneform-basis v2))))
      the-metric)))

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

(comment
  (define (metric-over-map mu:N->M g-on-M)
    (define (vector-field-over-map->vector-field V-over-mu n)
      ;; This helper has no clear meaning.
      (procedure->vector-field
       (lambda (f)
               (lambda (m)
                       ;;(assert (= m (mu:N->M n)))
                       ((V-over-mu f) n)))
       `(vector-field-over-map->vector-field
         ,(diffop-name V-over-mu))))
    (define (the-metric v1 v2)
      (lambda (n)
              ((g-on-M
                (vector-field-over-map->vector-field v1 n)
                (vector-field-over-map->vector-field v2 n))
               (mu:N->M n))))
    (declare-argument-types! the-metric
                             (list vector-field? vector-field?))
    the-metric))

(comment
  ;;; Raising and lowering indices...

;;; To make a vector field into a one-form field
;;;  ie a (1,0) tensor into a (0,1) tensor

  (define ((lower metric) u)
    (define (omega v)
      (metric v u))
    (procedure->oneform-field omega
                              `(lower ,(diffop-name u)
                                       ,(diffop-name metric))))

  (define vector-field->oneform-field lower)
  (define drop1 lower)


;;; To make a one-form field  into a vector field
;;;  ie a (0,1) tensor into a (1,0) tensor

  (define (raise metric basis)
    (let ((gi (metric:invert metric basis)))
      (lambda (omega)
              (define v
                (contract (lambda (e_i e~i)
                                  (* (gi omega e~i) e_i))
                          basis))
              (procedure->vector-field v
                                       `(raise ,(diffop-name omega)
                                                ,(diffop-name metric))))))

  (define oneform-field->vector-field raise)
  (define raise1 raise)

;;; For making a (2,0) tensor into a (0,2) tensor

  (define ((drop2 metric-tensor basis) tensor)
    (define (omega v1 v2)
      (contract
       (lambda (e1 w1)
               (contract
                (lambda (e2 w2)
                        (* (metric-tensor v1 e1)
                           (tensor w1 w2)
                           (metric-tensor e2 v2)))
                basis))
       basis))
    (declare-argument-types! omega (list vector-field? vector-field?))
    omega)


;;; For making a (0,2) tensor into a (2,0) tensor

  (define (raise2 metric-tensor basis)
    (let ((gi (metric:invert metric-tensor basis)))
      (lambda (tensor02)
              (define (v2 omega1 omega2)
                (contract
                 (lambda (e1 w1)
                         (contract
                          (lambda (e2 w2)
                                  (* (gi omega1 w1)
                                     (tensor02 e1 e2)
                                     (gi w2 omega2)))
                          basis))
                 basis))
              (declare-argument-types! v2 (list oneform-field? oneform-field?))
              v2)))


;;; To compute the trace of a (0,2) tensor

  (define (trace2down metric-tensor basis)
    (let ((inverse-metric-tensor
           (metric:invert metric-tensor basis)))
      (lambda (tensor02)
              (define f
                (contract
                 (lambda (e1 w1)
                         (contract
                          (lambda (e2 w2)
                                  (* (inverse-metric-tensor w1 w2)
                                     (tensor02 e1 e2)))
                          basis))
                 basis))
              (declare-argument-types! f (list function?))
              f)))


;;; To compute the trace of a (2,0) tensor

  (define ((trace2up metric-tensor basis) tensor20)
    (define f
      (contract
       (lambda (e1 w1)
               (contract
                (lambda (e2 w2)
                        (* (metric-tensor e1 e2)
                           (tensor20 w1 w2)))
                basis))
       basis))
    (declare-argument-types! f (list function?))
    f))

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
