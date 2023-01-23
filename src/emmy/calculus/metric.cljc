#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.calculus.metric
  (:require [emmy.calculus.basis :as b]
            [emmy.calculus.coordinate :as coord]
            [emmy.calculus.derivative :refer [D]]
            [emmy.calculus.form-field :as ff]
            [emmy.calculus.indexed :as ci]
            [emmy.calculus.manifold :as m]
            [emmy.calculus.vector-field :as vf]
            [emmy.function :as f]
            [emmy.generic :as g]
            [emmy.matrix :as matrix]
            [emmy.structure :as s :refer [down]]
            [emmy.value :as v]))

;; ## Metrics

;; A metric is a function that takes two vector fields and produces a function
;; on the manifold.

(defn embedding-map->metric-components
  [n xi->rectangular]
  (let [h   (D xi->rectangular)
        ref (fn [f k]
              (f/compose #(get % k) f))]
    (if (= n 1)
      (down (down (g/dot-product h h)))
      (s/generate
       n ::s/down
       (fn [i]
         (s/generate
          n ::s/down
          (fn [j]
            (g/dot-product (ref h i)
                           (ref h j)))))))))

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
      (ci/with-argument-types
        the-metric
        [::vf/vector-field
         ::vf/vector-field]))))

(defn coordinate-system->inverse-metric [coordinate-system]
  (let [basis (b/coordinate-system->basis coordinate-system)
        vector-basis (b/basis->vector-basis basis)
        ->components
        (g// 1 (coordinate-system->metric-components
                coordinate-system))
        Chi (m/chart coordinate-system)]
    (letfn [(the-inverse-metric [w1 w2]
              (fn [m]
                (let [gcoeffs (->components (Chi m))]
                  (g/* (g/* gcoeffs
                            (s/mapr (fn [e] ((w1 e) m))
                                    vector-basis))
                       (s/mapr (fn [e] ((w2 e) m))
                               vector-basis)))))]
      (ci/with-argument-types
        the-inverse-metric
        [::ff/oneform-field
         ::ff/oneform-field]))))

;; Symbolic metrics are often useful for testing.

(defn- make-metric [name coordinate-system]
  (fn gij [i j]
    (if (<= i j)
      (m/literal-manifold-function
       (symbol (str name "_" i j))
       coordinate-system)
      (gij j i))))

(defn literal-metric
  "Flat coordinate systems here only."
  [name coordinate-system]
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
      (ci/with-argument-types
        the-metric
        [::vf/vector-field
         ::vf/vector-field]))))

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

(defn metric->inverse-components
  "Given a metric and a basis, computes the inverse metric."
  [metric basis]
  (fn the-coeffs [m]
    (let [g_ij ((metric->components metric basis) m)
          oneform-basis (b/basis->oneform-basis basis)
          typical (s/typical-object oneform-basis)]
      (matrix/s:inverse typical g_ij typical))))

(defn invert [metric basis]
  (letfn [(the-inverse-metric [w1 w2]
            (let [vector-basis (b/basis->vector-basis basis)
                  g-ij (metric->inverse-components metric basis)]
              (g/* (g/* g-ij (s/mapr w1 vector-basis))
                   (s/mapr w2 vector-basis))))]
    (ci/with-argument-types
      the-inverse-metric
      [::ff/oneform-field
       ::ff/oneform-field])))

;; Over a map...

(defn metric-over-map [mu:N->M g-on-M]
  (letfn [(make-fake-vector-field [V-over-mu n]
            (vf/procedure->vector-field
             (fn [f]
               (fn [_]
                 ((V-over-mu f) n)))
             `(~'make-fake-vector-field
               ~(v/freeze V-over-mu))))
          (the-metric [v1 v2]
            (fn [n]
              ((g-on-M
                (make-fake-vector-field v1 n)
                (make-fake-vector-field v2 n))
               (mu:N->M n))))]
    (ci/with-argument-types
      the-metric
      [::vf/vector-field
       ::vf/vector-field])))

;; ### Raising and lowering indices

(defn lower
  "To make a vector field into a one-form field, ie, a (1,0) tensor into a (0,1)
  tensor."
  [metric]
  (fn [u]
    (letfn [(omega [v]
              (metric v u))]
      (ff/procedure->oneform-field
       omega
       `(~'lower
         ~(v/freeze u)
         ~(v/freeze metric))))))

(def ^{:doc "Alias for [[lower]]."}
  vector-field->oneform-field
  lower)

(def ^{:doc "Alias for [[lower]]."}
  drop1
  lower)

(defn raise
  "To make a one-form field into a vector field, ie, a (0,1) tensor into a (1,0)
  tensor."
  [metric basis]
  (let [gi (invert metric basis)]
    (fn [omega]
      (let [v (b/contract
               (fn [vf-i ff-i]
                 (g/* (gi omega ff-i) vf-i))
               basis)]
        (vf/procedure->vector-field
         v
         `(~'raise
           ~(v/freeze omega)
           ~(v/freeze metric)))))))

(def ^{:doc "Alias for [[raise]]."}
  oneform-field->vector-field
  raise)

(def ^{:doc "Alias for [[raise]]."}
  raise1
  raise)

(defn drop2
  "For making a (2,0) tensor into a (0,2) tensor."
  [metric-tensor basis]
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
      (ci/with-argument-types
        omega
        [::vf/vector-field
         ::vf/vector-field]))))

(defn raise2
  "For making a (0,2) tensor into a (2,0) tensor."
  [metric-tensor basis]
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
        (ci/with-argument-types
          v2
          [::ff/oneform-field
           ::ff/oneform-field])))))

(defn trace2down
  "Computes the trace of a (0,2) tensor."
  [metric-tensor basis]
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
        (ci/with-argument-types
          f
          [::v/function])))))

(defn trace2up
  "Computes the trace of a (2,0) tensor"
  [metric-tensor basis]
  (fn [tensor20]
    (let [f (b/contract
             (fn [e1 w1]
               (b/contract
                (fn [e2 w2]
                  (g/* (metric-tensor e1 e2)
                       (tensor20 w1 w2)))
                basis))
             basis)]
      (ci/with-argument-types
        f
        [::v/function]))))


;; Unfortunately raise is very expensive because the matrix is
;; inverted for each manifold point.

(defn sharpen [metric basis m]
  (let [g-ij ((metric->inverse-components metric basis) m)
	      vector-basis (b/basis->vector-basis basis)]
    (fn sharp [oneform-field]
      (let [oneform-coeffs
	          (s/mapr (fn [ei] ((oneform-field ei) m))
		                vector-basis)
            vector-coeffs (g/* g-ij oneform-coeffs)]
	      (s/sumr g/* vector-coeffs vector-basis)))))

;; ## Useful metrics

(def S2-metric
  (let [[theta] (coord/coordinate-functions m/S2-spherical)
        [dtheta dphi] (ff/coordinate-system->oneform-basis m/S2-spherical)]
    (-> (fn the-metric [v1 v2]
          (g/+ (g/* (dtheta v1) (dtheta v2))
	             (g/* (g/expt (g/sin theta) 2)
	                  (dphi v1) (dphi v2))))
        (ci/with-argument-types
          [::vf/vector-field
           ::vf/vector-field]))))
