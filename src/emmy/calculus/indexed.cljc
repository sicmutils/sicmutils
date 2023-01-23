#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.calculus.indexed
  "This namespace implements minimal support for indexed objects and typed
  functions."
  (:refer-clojure :exclude [+ - * /])
  (:require [emmy.calculus.basis :as b]
            [emmy.calculus.form-field :as ff]
            [emmy.calculus.manifold :as m]
            [emmy.calculus.vector-field :as vf]
            [emmy.generic :as g :refer [+ *]]
            [emmy.operator :as o]
            [emmy.structure :as s]
            [emmy.util :as u]
            [emmy.util.aggregate :as ua]
            [emmy.util.permute :as permute]
            [emmy.value :as v]))

;; ## Minimal support for Indexed Objects
;;
;; Indices here are the components of tensors relative to a basis.

(defn- meta-k
  "Takes a function or operator `f` and a metadata (or context) key `k` and
  attempts to fetch it from the metadata (or context). Returns `default` if `k`
  has no entry."
  ([f k]
   (meta-k f k nil))
  ([f k default]
   (if (o/operator? f)
     (k (o/context f) default)
     (k (meta f) default))))

(defn- with-kvs
  "Returns a copy of `f` with the `k`, `v` pair added to its metadata (if a
  function) or context (if an operator)."
  [f & kvs]
  (if (o/operator? f)
    (o/with-context f (apply assoc (o/context f) kvs))
    (apply vary-meta f assoc kvs)))

(defn argument-types
  "Given an operator or function `f`, returns its registered vector of argument
  types, or `[]` if none exist.

  argument types are, for example,

  ```clojure
  [::ff/oneform-field ::vf/vector-field ::vf/vector-field]
  ```

  for a `Christoffel-2`, which takes one oneform field and two vector fields."
  [f]
  (meta-k f :arguments []))

(defn ^:no-doc has-argument-types?
  "Returns true if `f` has any argument types registered, false otherwise."
  [f]
  (boolean
   (seq (argument-types f))))

(defn with-argument-types
  "Given some operator or function `f`, returns a copy of `f` with the supplied
  argument types `types` registered in its metadata (if a function) or
  context (if an operator).

  Retrieve these types with [[argument-types]]."
  [f types]
  (let [args (into [] types)]
    (with-kvs f
      :arguments args
      :arity [:exactly (count args)])))

(defn index-types
  "Given an operator or function `f`, returns its registered vector of index
  types, or `[]` if none exist.

  index types are, for example,

  ```clojure
  ['up 'down 'down]
  ```

  for a `Christoffel-2`, which takes one oneform field and two vector fields."
  [f]
  (meta-k f :index-types []))

(defn ^:no-doc has-index-types?
  "Returns true if `f` has any index types registered, false otherwise."
  [f]
  (boolean
   (seq (index-types f))))

(defn with-index-types
  "Given some operator or function `f`, returns a copy of `f` with the supplied
  index types `types` registered in its metadata (if a function) or
  context (if an operator).

  Retrieve these types with [[index-types]]."
  [f types]
  (let [v (into [] types)]
    (with-kvs f
      :index-types v
      :arity [:exactly (count v)])))

;; An argument-typed function of type (n . m) takes n oneform fields and m
;; vector-fields, in that order, and produces a function on a manifold point.
;;
;; An indexed function of type (n . m) takes n+m indices and gives a function on
;; a manifold point.
;;
;; For each argument-typed function and basis, there is an indexed function that
;; gives the function resulting from applying the argument-typed function to the
;; basis elements specified by the corresponding indices.

(defn- valid-arg-types?
  "Returns true if `ts` is a well-formed-enough sequence of argument types to use
  for generating an indexed function via [[typed->indexed]], false otherwise.

  Validates that:

  - The sequence of types `ts` is not empty
  - every entry in `ts` derives from `::vf/vector-field` or `::ff/oneform-field`
  - form fields come before vector fields."
  [ts]
  (letfn [(one-ff? [t]
            (isa? t ::ff/oneform-field))
          (vf? [t]
            (isa? t ::vf/vector-field))]
    (and (seq ts)
         (every? (some-fn one-ff? vf?) ts)
         (every? vf? (drop-while one-ff? ts)))))

(defn typed->indexed [f basis]
  (let [arg-types (argument-types f)]
    (assert (valid-arg-types? arg-types)
            (str "Invalid arg types: " arg-types))
    (let [vector-basis (b/basis->vector-basis basis)
          oneform-basis (b/basis->oneform-basis basis)
          idx-types (map (fn [t]
                           (if (isa? t ::vf/vector-field)
                             'down
                             'up))
                         arg-types)]
      (-> (fn indexed [indices]
            (assert (= (count indices)
                       (count arg-types))
                    (str "Indices count doesn't match expected argument types."
                         "  Indices:" indices
                         ", arg-types: " arg-types))
            (let [args (mapv (fn [t idx]
                               (if (isa? t ::vf/vector-field)
                                 (get vector-basis idx)
                                 (get oneform-basis idx)))
                             arg-types
                             indices)]
              (apply f args)))
          (with-index-types idx-types)))))

(defn- valid-index-types?
  "Returns true if `ts` is a well-formed-enough sequence of index types to use for
  generating a typed function via [[indexed->typed]], false otherwise.

  Validates that:

  - The sequence of types `ts` is not empty
  - every entry in `ts` is either the symbol `'up` or `'down`
  - all `'up` entries (corresponding to oneform fields) come before `'down`
    entries (corresponding to vector fields)"
  [ts]
  (boolean
   (and (seq ts)
        (every? #{'up 'down} ts)
        (every? #{'down} (drop-while #{'up} ts)))))

(defn- validate-typed-args!
  "Returns true if:

  - every argument in `args` has a corresponding index type in `index-types`
  - every `'up` in `index-types` is aligned with a [[form-field/oneform-field?]]
    argument in `args`
  - every `'down` in `index-types` is aligned with a [[vector-field/vector-field?]]
    argument in `args`

  false otherwise.

  `index-types` is assumed to have passed a [[valid-index-types?]] check."
  [index-types args]
  (assert (= (count index-types)
             (count args))
          (str "The number "
               (count index-types) " of index-types doesn't match the number "
               (count args) " of arguments."))
  (assert (every? true?
                  (map (fn [index-type arg]
                         (or (and (= index-type 'up)
                                  (ff/oneform-field? arg))
                             (and (= index-type 'down)
                                  (vf/vector-field? arg))))
                       index-types
                       args))
          (str "Args do not match index-types 'up must be paired with oneform-fields and 'down with vector fields."
               " Args:" (pr-str args)
               ", indices: " (pr-str index-types))))

(defn indexed->typed [indexed basis]
  (let [index-types (index-types indexed)]
    (assert (valid-index-types? index-types)
            (str "Invalid index types: " index-types))
    (let [vector-basis  (b/basis->vector-basis basis)
          oneform-basis (b/basis->oneform-basis basis)
          n              (b/basis->dimension basis)
          arg-types (mapv {'up ::ff/oneform-field
                           'down ::vf/vector-field}
                          index-types)]
      (-> (fn typed [& args]
            (validate-typed-args! index-types args)
            (let [n-seq  (reverse (range n))
                  combos (permute/cartesian-product
                          (for [x args]
                            (map (fn [i arg]
                                   [i (if (vf/vector-field? arg)
                                        ((get oneform-basis i) arg)
                                        (arg (get vector-basis i)))])
                                 n-seq
                                 (repeat x))))]
              (ua/generic-sum
               (for [combo combos
                     :let [indices      (map first combo)
                           product-args (map peek combo)]]
                 (apply *
                        (indexed indices)
                        (reverse product-args))))))
          (with-argument-types arg-types)))))

(defn outer-product [T1 T2]
  (let [i1 (index-types T1)
        i2 (index-types T2)]
    (assert (seq i1) "No index types registered for T1!")
    (assert (seq i2) "No index types registered for T2!")
    (let [{nu1 'up nd1 'down} (frequencies i1)
          {nu2 'up nd2 'down} (frequencies i2)
          nup (+ (or nu1 0) (or nu2 0))
          ndp (+ (or nd1 0) (or nd2 0))
          np  (+ nup ndp)
          n1  (+ nup nd1)]
      (letfn [(product [args]
                (assert (= (count args) np)
                        (str "Wrong number of args to outer-product: "
                             (count args)
                             ", expected: " np))
                (let [argv (into [] args)]
                  (* (T1 (into (subvec argv 0 nu1)
                               (subvec argv nup n1)))
                     (T2 (into (subvec argv nu1 nup)
                               (subvec argv n1 np))))))]
        (with-index-types product
          (concat (repeat nup 'up)
                  (repeat ndp 'down)))))))

(letfn [(insertv [coll i v]
          (let [l (subvec coll 0 i)
                r (subvec coll i)]
            (apply conj l v r)))]

  (defn contract [T u d n]
    (let [i-types (index-types T)]
      (assert (seq i-types) "No index types registered for T!")
      (let [{nu 'up nd 'down} (frequencies i-types)]
        (assert (and (<= 0 u) (< u nu)
                     (<= 0 d) (< d nd))
                (str "Contraction indices " u ", " d "  not in the correct range. "
                     "Each must be >= 0 and < the respective number of "
                     "'up and 'down instances in the index types registered with T. "
                     "These were " nu " and " nd "."))
        (let [nuc (dec nu)
              ndc (dec nd)]
          (-> (fn contraction [args]
                (let [argv (into [] args)]
                  (ua/generic-sum
                   (fn [i]
                     (T (concat
                         (insertv (subvec argv 0 nuc) u i)
                         (insertv (subvec argv nuc) d i))))
                   0 n)))
              (with-index-types
                (concat (repeat nuc 'up)
                        (repeat ndc 'down)))))))))

(defn typed->structure [T basis]
  (let [vector-basis  (b/basis->vector-basis basis)
        oneform-basis (b/basis->oneform-basis basis)]
    (letfn [(lp [arg-types argv]
              (if (empty? arg-types)
                (apply T argv)
                (let [[t & ts] arg-types]
                  (s/mapr (fn [e]
                            (lp ts (conj argv e)))
                          (cond (isa? t ::vf/vector-field)
                                vector-basis

                                (isa? t ::ff/oneform-field)
                                oneform-basis

                                :else (u/illegal
                                       (str "Invalid argument type: " (pr-str t)
                                            ". Every arg must be a vector field or oneform field.")))))))]
      (lp (argument-types T) []))))

(defn structure->typed [coeff-functions basis]
  (let [vector-basis  (b/basis->vector-basis basis)
        oneform-basis (b/basis->oneform-basis basis)
        arg-types     (loop [cf  coeff-functions
                             acc []]
                        (if-not (s/structure? cf)
                          acc
                          (let [shape (s/opposite-orientation
                                       (s/orientation cf))
                                t (case shape
                                    ::s/up ::vf/vector-field
                                    ::s/down ::ff/oneform-field)]
                            (recur (nth cf 0)
                                   (conj acc t)))))]
    (-> (fn indexed-fn [& args]
          (assert (= (count args)
                     (count arg-types))
                  (str "The number of args " (count args)
                       " did not match the expected arity " (count arg-types) ". "
                       "Please supply args corresponding to the expected types " arg-types "."))
          (assert (every? true? (map (fn [arg arg-type]
                                       (isa? (v/kind arg) arg-type))
                                     args arg-types))
                  (str "Invalid arguments: " args ". "
                       "Please supply args corresponding to the expected types " arg-types "."))
          (letfn [(lp [args arg-types]
                    (if (empty? args)
                      m/one-manifold-function
                      (let [arg (first args)
                            arg-type (first arg-types)]
                        (cond (isa? arg-type ::vf/vector-field)
                              (s/mapr (fn [etilde]
                                        (* (etilde arg)
                                           (lp (rest args)
                                               (rest arg-types))))
                                      oneform-basis)

                              (isa? arg-type ::ff/oneform-field)
                              (s/mapr (fn [e]
                                        (* (arg e)
                                           (lp (rest args)
                                               (rest arg-types))))
                                      vector-basis)))))]
            (* (lp args arg-types)
               coeff-functions)))
        (with-argument-types arg-types))))
