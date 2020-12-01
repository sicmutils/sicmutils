;
; Copyright © 2017 Colin Smith.
; This work is based on the Scmutils system of MIT/GNU Scheme:
; Copyright © 2002 Massachusetts Institute of Technology
;
; This is free software;  you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3 of the License, or (at
; your option) any later version.
;
; This software is distributed in the hope that it will be useful, but
; WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
; General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this code; if not, see <http://www.gnu.org/licenses/>.
;

(ns sicmutils.function
  (:require [sicmutils.abstract.number :as an]
            [sicmutils.expression :as x]
            [sicmutils.generic :as g]
            [sicmutils.matrix :as m]
            [sicmutils.numsymb :as ns]
            [sicmutils.polynomial]
            [sicmutils.structure :as s]
            [sicmutils.util :as u]
            [sicmutils.value :as v]
            [sicmutils.calculus.derivative :as d])
  #?(:clj
     (:import [clojure.lang IFn])))

(declare literal-apply)

(defn ^:private sicm-set->exemplar
  "Convert a SICM-style set (e.g., Real or (UP Real Real)) to
  an exemplar (an instance of the relevant type)."
  [s]
  (cond
    (= s 'Real) 0

    (sequential? s)
    (let [[constructor & args] s]
      (case constructor
        X     (mapv sicm-set->exemplar args)
        UP    (apply s/up (map sicm-set->exemplar args))
        DOWN  (apply s/down (map sicm-set->exemplar args))
        UP*   (apply s/up (repeat (second args) (sicm-set->exemplar (first args))))
        DOWN* (apply s/down (repeat (second args) (sicm-set->exemplar (first args))))
        X*    (into [] (repeat (second args) (sicm-set->exemplar (first args))))))))

(defn sicm-signature->domain-range
  "Convert a SICM-style literal function signature (e.g.,
  '(-> Real (X Real Real)) ) to our 'exemplar' format."
  [[arrow domain range]]
  (when-not (and (= '-> arrow) domain range)
    (u/illegal (str "A SICM signature is of the form '(-> domain range), got: " arrow domain range)))
  [(let [d (sicm-set->exemplar domain)]
     (if (vector? d) d [d]))
   (sicm-set->exemplar range)])

(defrecord Function [name arity domain range]
  Object
  (toString [_] (str name) )
  v/Value
  (nullity? [_] false)
  (unity? [_] false)
  (zero-like [_] (fn [& _] (v/zero-like range)))
  (numerical? [_] false)
  (freeze [_] (v/freeze name))
  (kind [_] ::function)

  #?@(:clj
      [IFn
       (invoke [this x] (literal-apply this [x]))
       (invoke [this x y] (literal-apply this [x y]))
       (invoke [this x y z] (literal-apply this [x y z]))
       (invoke [this w x y z] (literal-apply this [w x y z]))
       (applyTo [this xs] (literal-apply this xs))]

      :cljs
      [IFn
       (-invoke [this a]
                (literal-apply this [a]))
       (-invoke [this a b]
                (literal-apply this [a b]))
       (-invoke [this a b c]
                (literal-apply this [a b c]))
       (-invoke [this a b c d]
                (literal-apply this [a b c d]))
       (-invoke [this a b c d e]
                (literal-apply this [a b c d e]))
       (-invoke [this a b c d e f]
                (literal-apply this [a b c d e f]))
       (-invoke [this a b c d e f g]
                (literal-apply this [a b c d e f g]))
       (-invoke [this a b c d e f g h]
                (literal-apply this [a b c d e f g h]))
       (-invoke [this a b c d e f g h i]
                (literal-apply this [a b c d e f g h i]))
       (-invoke [this a b c d e f g h i j]
                (literal-apply this [a b c d e f g h i j]))
       (-invoke [this a b c d e f g h i j k]
                (literal-apply this [a b c d e f g h i j k]))
       (-invoke [this a b c d e f g h i j k l]
                (literal-apply this [a b c d e f g h i j k l]))
       (-invoke [this a b c d e f g h i j k l m]
                (literal-apply this [a b c d e f g h i j k l m]))
       (-invoke [this a b c d e f g h i j k l m n]
                (literal-apply this [a b c d e f g h i j k l m n]))
       (-invoke [this a b c d e f g h i j k l m n o]
                (literal-apply this [a b c d e f g h i j k l m n o]))
       (-invoke [this a b c d e f g h i j k l m n o p]
                (literal-apply this [a b c d e f g h i j k l m n o p]))
       (-invoke [this a b c d e f g h i j k l m n o p q]
                (literal-apply this [a b c d e f g h i j k l m n o p q]))
       (-invoke [this a b c d e f g h i j k l m n o p q r]
                (literal-apply this [a b c d e f g h i j k l m n o p q r]))
       (-invoke [this a b c d e f g h i j k l m n o p q r s]
                (literal-apply this [a b c d e f g h i j k l m n o p q r s]))
       (-invoke [this a b c d e f g h i j k l m n o p q r s t]
                (literal-apply this [a b c d e f g h i j k l m n o p q r s t]))
       (-invoke [this a b c d e f g h i j k l m n o p q r s t rest]
                (literal-apply this (concat [a b c d e f g h i j k l m n o p q r s t]  rest)))]))

(defn literal-function
  ([f] (->Function f [:exactly 1] [0] 0))
  ([f signature]
   (let [[domain range] (sicm-signature->domain-range signature)]
     (literal-function f domain range)))
  ([f domain range]
   (cond (number? range)
         (let [arity (if (vector? domain)
                       (count domain)
                       1)]
           (->Function f [:exactly arity]
                       (if (vector? domain) domain [domain])
                       range))

         (s/structure? range)
         (let [n           (count range)
               orientation (s/orientation range)
               template    (s/literal f n orientation)]
           (s/mapr #(literal-function %1 domain %2)
                   template
                   range))
         :else
         (u/illegal (str "WTF range" domain)))))

(def ^:private derivative-symbol 'D)

;; --------------------
;; Algebra of functions
;;

(defn ^:private unary-operation
  "For a unary operator (like sqrt), returns a function of one function
  which when called will apply the operation to the result of the
  original function (so that ((unary-operation sqrt) f) x) will return
  (sqrt (f x))."
  [operator]
  (with-meta (partial comp operator) {:arity [:exactly 1]}))

(defn ^:private binary-operation
  "For a given binary operator (like +), returns a function of two
  functions which will produce the pointwise operation of the results
  of applying the two functions to the input. That
  is, (binary-operation +) applied to f and g will produce a function
  which computes (+ (f x) (g x)) given x as input."
  [operator]
  (let [h (fn [f g]
            (let [f-numeric (v/numerical? f)
                  g-numeric (v/numerical? g)
                  f-arity (if f-numeric (v/arity g) (v/arity f))
                  g-arity (if g-numeric f-arity (v/arity g))
                  arity (v/joint-arity [f-arity g-arity])
                  f1 (if f-numeric (with-meta
                                     (constantly f)
                                     {:arity arity
                                      :from :binop}) f)
                  g1 (if g-numeric (with-meta
                                     (constantly g)
                                     {:arity arity
                                      :from :binop}) g)]
              (let [h (condp = arity
                        [:exactly 0]
                        #(operator (f1) (g1))
                        [:exactly 1]
                        #(operator (f1 %) (g1 %))
                        [:exactly 2]
                        #(operator (f1 %1 %2) (g1 %1 %2))
                        [:exactly 3]
                        #(operator (f1 %1 %2 %3) (g1 %1 %2 %3))
                        [:exactly 4]
                        #(operator (f1 %1 %2 %3 %4) (g1 %1 %2 %3 %4))
                        [:exactly 5]
                        #(operator (f1 %1 %2 %3 %4 %5) (g1 %1 %2 %3 %4 %5))
                        [:exactly 6]
                        #(operator (f1 %1 %2 %3 %4 %5 %6) (g1 %1 %2 %3 %4 %5 %6))
                        [:exactly 7]
                        #(operator (f1 %1 %2 %3 %4 %5 %6 %7) (g1 %1 %2 %3 %4 %5 %6 %7))
                        [:exactly 8]
                        #(operator (f1 %1 %2 %3 %4 %5 %6 %7 %8) (g1 %1 %2 %3 %4 %5 %6 %7 %8))
                        [:exactly 9]
                        #(operator (f1 %1 %2 %3 %4 %5 %6 %7 %8 %9) (g1 %1 %2 %3 %4 %5 %6 %7 %8 %9))
                        [:exactly 10]
                        #(operator (f1 %1 %2 %3 %4 %5 %6 %7 %8 %9 %10) (g1 %1 %2 %3 %4 %5 %6 %7 %8 %9 %10))
                        [:at-least 0]
                        #(operator (apply f1 %&) (apply g1 %&))
                        (u/illegal (str  "unsupported arity for function arithmetic " arity)))]
                (with-meta h {:arity f-arity :from :function-binop}))))]
    (with-meta h {:arity [:exactly 2]})))

(defn- make-binary-operation
  "Given a generic and binary function operation,
  define the multimethods necessary to introduce this operation
  to function arguments."
  ([generic-op]
   (make-binary-operation generic-op generic-op))
  ([generic-op binary-op]
   (let [binop (binary-operation binary-op)]
     (doseq [signature [[::function ::function]
                        [::function ::cofunction]
                        [::cofunction ::function]]]
       (defmethod generic-op signature [a b] (binop a b))))))

(defn- make-unary-operation
  [generic-op]
  (let [unary-op (unary-operation generic-op)]
    (defmethod generic-op [::function] [a] (unary-op a))))

(make-binary-operation g/add g/+)
(make-binary-operation g/sub g/-)
(make-binary-operation g/mul g/*)
(make-binary-operation g/div g/divide)
(make-binary-operation g/expt)

(make-unary-operation g/negate)
(make-unary-operation g/invert)
(make-unary-operation g/sqrt)
(make-unary-operation g/sin)
(make-unary-operation g/cos)
(make-unary-operation g/tan)
(make-unary-operation g/asin)
(make-unary-operation g/acos)

(make-unary-operation g/atan)
(make-binary-operation g/atan)

(make-unary-operation g/sinh)
(make-unary-operation g/cosh)
(make-unary-operation g/tanh)
(make-unary-operation g/square)
(make-unary-operation g/cube)
(make-unary-operation g/exp)
(make-unary-operation g/log)

(comment
  "This comment comes from scmutils, function.scm, in the definition of
  `transpose-defining-relation`:

  T is a linear transformation T:V -> W
  the transpose of T, T^t:W* -> V*
  Forall a in V, g in W*,  g:W -> R
  (T^t(g))(a) = g(T(a)).")

(defmethod g/transpose [::function] [f]
  (fn [g]
    (fn [a]
      (g (f a)))))

(make-binary-operation g/cross-product)
(make-binary-operation g/gcd)

;; Complex Operations

(make-unary-operation g/real-part)
(make-unary-operation g/imag-part)
(make-unary-operation g/magnitude)
(make-unary-operation g/angle)
(make-unary-operation g/conjugate)

(defmethod g/simplify [Function] [a] (g/simplify (:name a)))
(derive ::x/numeric ::cofunction)
(derive ::v/number ::cofunction)
(derive ::s/structure ::cofunction)
(derive ::m/matrix ::cofunction)

;; Clojure functions, returns by v/primitive-kind.
(derive ::v/function ::function)
(derive ::function :sicmutils.series/coseries)
;; ------------------------------------
;; Differentiation of literal functions
;;

(defn symbolic-derivative?
  [expr]
  (and (sequential? expr)
       ;; XXX GJS uses 'derivative here; should we? doesn't he just
       ;; have to change it back to D when printing?
       (= (first expr) derivative-symbol)))

(defn iterated-symbolic-derivative?
  [expr]
  (and (sequential? expr)
       (sequential? (first expr))
       (ns/expt? (first expr))
       (= (second (first expr)) derivative-symbol)))

(defn symbolic-increase-derivative [expr]
  (cond (symbolic-derivative? expr)
        (list (ns/expt derivative-symbol 2) (fnext expr))
        (iterated-symbolic-derivative? expr)
        (list (ns/expt derivative-symbol
                       (+ (first (nnext (first expr)))
                          1))
              (fnext expr))
        :else
        (list derivative-symbol expr)))

(defn ^:private make-partials
  [f v]
  ;; GJS calls this function (the loop below) "fd"; we have no idea
  ;; what that stands for or what
  ;; is being attempted here
  (letfn [(fd [indices vv]
            (cond (s/structure? vv)
                  (s/same vv (map-indexed (fn [i element]
                                            (fd (conj indices i) element))
                                          vv))
                  (or (v/numerical? vv)
                      (x/abstract? vv))
                  (let [fexp (if (= (:arity f) [:exactly 1])  ; univariate
                               (if (= (first indices) 0)
                                 (if (= (count indices) 1)
                                   (symbolic-increase-derivative (:name f))
                                   `((~'partial ~@(next indices)) ~(:name f)))
                                 (u/illegal "wrong indices"))
                               `((~'partial ~@indices) ~(:name f)))]
                    (->Function fexp (:arity f) (:domain f) (:range f)))
                  :else
                  (u/illegal (str "make-partials WTF " vv))))]
    (fd [] v)))


(defn ^:private literal-derivative
  [f xs]
  (let [v (m/seq-> xs)
        maxtag (->> v flatten d/max-order-tag)
        ve (->> v (s/mapr #(d/without-tag maxtag %)) seq)
        dv (->> v (s/mapr #(d/with-tag maxtag %)))]
    (d/canonicalize-differential
     (d/dx+dy (apply f ve)
              (reduce d/dx+dy (map (fn [partialx dx]
                                     (d/dx*dy (apply partialx ve) dx))
                                   (flatten (make-partials f v))
                                   (flatten dv)))))))

(defn ^:private check-argument-type
  "Check that the argument provided at index i has the same type as
  the exemplar expected."
  [f provided expected indexes]
  (cond (number? expected)
        (when-not (v/numerical? provided)
          (u/illegal (str "expected numerical quantity in argument " indexes
                          " of function call " f
                          " but got " provided)))
        (s/structure? expected)
        (do (when-not (and (or (s/structure? provided) (sequential? provided))
                           (= (s/orientation provided) (s/orientation expected))
                           (= (count provided) (count expected)))
              (u/illegal (str "expected structure matching " expected
                              " but got " provided )))
            (doseq [[provided expected sub-index] (map list provided expected (range))]
              (check-argument-type f provided expected (conj indexes sub-index))))
        (keyword? expected) ;; a keyword has to match the argument's kind
        (when-not (= (v/kind provided) expected)
          (u/illegal (str "expected argument of type " expected " but got " (v/kind provided)
                          " in call to function " f)))

        :else (u/illegal (str "unexpected argument example. got " provided " want " expected))))

(defn ^:private literal-apply
  [f xs]
  (check-argument-type f xs (:domain f) [0])
  (if (some d/differential? xs)
    (literal-derivative f xs)
    (an/literal-number `(~(:name f) ~@(map v/freeze xs)))))

;;; Utilities

(defn compose
  "Compose is like Clojure's standard comp, but for this system we
  like to know the arity of our functions, so that we can calculate
  their derivatives with structure, etc. The arity of a composition is
  simply the arity of its rightmost (that is, first to be applied)
  function term."
  [& fns]
  (let [a (v/arity (last fns))]
    (with-meta (apply comp fns) {:arity a})))

(defmacro with-literal-functions
  [litfns & body]
  `(let ~(vec (interleave
               (map (fn [s]
                      (if (symbol? s) s (first s)))
                    litfns)
               (map (fn [s]
                      (cond (symbol? s)
                            `(literal-function (quote ~s))
                            (and (sequential? s)
                                 (= (count s) 3))
                            `(literal-function (quote ~(first s))
                                               ~(second s)
                                               ~(nth s 2))
                            :else (u/illegal (str "unknown literal function type" s))))
                    litfns)))
     ~@body))
