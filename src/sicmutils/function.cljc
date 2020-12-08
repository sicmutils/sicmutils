
;; Copyright © 2017 Colin Smith.
;; This work is based on the Scmutils system of MIT/GNU Scheme:
;; Copyright © 2002 Massachusetts Institute of Technology

;; This is free software;  you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or (at
;; your option) any later version.

;; This software is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this code; if not, see <http://www.gnu.org/licenses/>.
;;

(ns sicmutils.function
  (:require [sicmutils.generic :as g]
            [sicmutils.util :as u]
            [sicmutils.value :as v]))

;; ## Function Algebra
;;
;; this namespace extends the sicmutils generic operations to Clojure functions
;; and multimethods. (Of course, this includes the generic operations
;; themselves!)

;; ### Utilities

(defn compose
  "Compose is like Clojure's standard comp, but for this system we
  like to know the arity of our functions, so that we can calculate
  their derivatives with structure, etc. The arity of a composition is
  simply the arity of its rightmost (that is, first to be applied)
  function term."
  [& fns]
  (let [a (v/arity (last fns))]
    (with-meta (apply comp fns) {:arity a})))

;; ## Generic Implementations
;;
;; A `::confunction` is a type that we know how to combine with a function in a
;; binary operation.

(derive ::v/scalar ::cofunction)

(defn- unary-operation
  "For a unary operator (like sqrt), returns a function of one function which when
  called will apply the operation to the result of the original function (so
  that ((unary-operation sqrt) f) x) will return
  (sqrt (f x))."
  [operator]
  (-> (partial comp operator)
      (with-meta {:arity [:exactly 1]})))

(defn- binary-operation
  "For a given binary operator (like +), returns a function of two functions which
  will produce the pointwise operation of the results of applying the two
  functions to the input. That is, (binary-operation +) applied to f and g will
  produce a function which computes (+ (f x) (g x)) given x as input."
  [operator]
  (let [h (fn [f g]
            (let [f-numeric (v/numerical? f)
                  g-numeric (v/numerical? g)
                  f-arity   (if f-numeric (v/arity g) (v/arity f))
                  g-arity   (if g-numeric f-arity     (v/arity g))
                  arity     (v/joint-arity [f-arity g-arity])
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

(defn- defunary
  [generic-op]
  (let [unary-op (unary-operation generic-op)]
    (defmethod generic-op [::v/function] [a] (unary-op a))))

(defn- defbinary
  "Given a generic and binary function operation,
  define the multimethods necessary to introduce this operation
  to function arguments."
  ([generic-op]
   (defbinary generic-op generic-op))
  ([generic-op binary-op]
   (let [binop (binary-operation binary-op)]
     (doseq [signature [[::v/function ::v/function]
                        [::v/function ::cofunction]
                        [::cofunction ::v/function]]]
       (defmethod generic-op signature [a b] (binop a b))))))

(defbinary g/add g/+)
(defbinary g/sub g/-)
(defbinary g/mul g/*)
(defbinary g/div g/divide)
(defbinary g/expt)

(defunary g/negate)
(defunary g/invert)
(defunary g/sqrt)
(defunary g/sin)
(defunary g/cos)
(defunary g/tan)
(defunary g/asin)
(defunary g/acos)

(defunary g/atan)
(defbinary g/atan)

(defunary g/sinh)
(defunary g/cosh)
(defunary g/tanh)

(defunary g/square)
(defunary g/cube)

(defunary g/exp)
(defunary g/log)

(comment
  "This comment expands on a comment from scmutils, function.scm, in the
  definition of `transpose-defining-relation`:

  $T$ is a linear transformation

  $$T : V -> W$$

  the transpose of $T$ is

  $$T^t : (W -> R) -> (V -> R)$$

  \\forall a \\in V, g \\in (W -> R),

  T^t : g \\to g \\circ T

  ie:

  (T^t(g))(a) = g(T(a))")
(defmethod g/transpose [::v/function] [f]
  (fn [g]
    (fn [a]
      (g (f a)))))

(defunary g/determinant)
(defunary g/trace)
(defbinary g/cross-product)
(defbinary g/gcd)
(defbinary g/lcm)

;; Complex Operations

(defunary g/real-part)
(defunary g/imag-part)
(defunary g/magnitude)
(defunary g/angle)
(defunary g/conjugate)
