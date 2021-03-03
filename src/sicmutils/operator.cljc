;;
;; Copyright © 2017 Colin Smith.
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

(ns sicmutils.operator
  (:refer-clojure :rename {identity core-identity
                           name core-name}
                  #?@(:cljs [:exclude [get identity name]]))
  (:require [sicmutils.differential :as d]
            [sicmutils.function :as f]
            [sicmutils.generic :as g]
            [sicmutils.series :as series]
            [sicmutils.util :as u]
            [sicmutils.value :as v])
  #?(:clj
     (:import (clojure.lang IFn ILookup IObj))))

(declare op:get)

(deftype Operator [o arity name context m]
  v/Value
  (zero? [_] false)
  (one? [_] false)
  (identity? [_] false)
  (zero-like [_] (Operator. v/zero-like arity 'zero context m))
  (one-like [_] (Operator. core-identity arity 'identity context m))
  (identity-like [_] (Operator. core-identity arity 'identity context m))
  (freeze [_] (v/freeze name))
  (kind [_] (:subtype context))

  f/IArity
  (arity [_] arity)

  d/IPerturbed
  (perturbed? [_] false)
  (replace-tag [_ old new]
    (Operator. (d/replace-tag o old new) arity name context m))
  (extract-tangent [_ tag]
    (Operator. (d/extract-tangent o tag) arity name context m))

  #?@(:clj
      [ILookup
       (valAt [this k] (op:get this k))
       (valAt [this k not-found]
              (u/illegal "Operators don't support the not-found arity of get!"))])

  Object
  (toString [_] (let [n (v/freeze name)]
                  (str (if (seqable? n) (seq n) n))))

  #?@(:clj
      [IObj
       (meta [_] m)
       (withMeta [_ meta] (Operator. o arity name context meta))

       IFn
       (invoke [_ f] (o f))
       (invoke [_ f g] (o f g))
       (invoke [_ f g h] (o f g h))
       (invoke [_ f g h i] (o f g h i))
       (applyTo [_ fns] (apply o fns))]

      :cljs
      [IMeta
       (-meta [_] m)

       IWithMeta
       (-with-meta [_ meta] (Operator. o arity name context meta))

       ILookup
       (-lookup [this k] (op:get this k))
       (-lookup [this k not-found]
                (u/illegal "Operators don't support the not-found arity of get!"))

       IFn
       (-invoke [_ a] (o a))
       (-invoke [_ a b] (o a b))
       (-invoke [_ a b c] (o a b c))
       (-invoke [_ a b c d] (o a b c d))
       (-invoke [_ a b c d e] (o a b c d e))
       (-invoke [_ a b c d e f] (o a b c d e f))
       (-invoke [_ a b c d e f g] (o a b c d e f g))
       (-invoke [_ a b c d e f g h] (o a b c d e f g h))
       (-invoke [_ a b c d e f g h i] (o a b c d e f g h i))
       (-invoke [_ a b c d e f g h i j] (o a b c d e f g h i j))
       (-invoke [_ a b c d e f g h i j k] (o a b c d e f g h i j k))
       (-invoke [_ a b c d e f g h i j k l] (o a b c d e f g h i j k l))
       (-invoke [_ a b c d e f g h i j k l m]
                (o a b c d e f g h i j k l m))
       (-invoke [_ a b c d e f g h i j k l m n]
                (o a b c d e f g h i j k l m n))
       (-invoke [_ a b c d e f g h i j k l m n o-arg]
                (o a b c d e f g h i j k l m n o-arg))
       (-invoke [_ a b c d e f g h i j k l m n o-arg p]
                (o a b c d e f g h i j k l m n o-arg p))
       (-invoke [_ a b c d e f g h i j k l m n o-arg p q]
                (o a b c d e f g h i j k l m n o-arg p q))
       (-invoke [_ a b c d e f g h i j k l m n o-arg p q r]
                (o a b c d e f g h i j k l m n o-arg p q r))
       (-invoke [_ a b c d e f g h i j k l m n o-arg p q r s]
                (o a b c d e f g h i j k l m n o-arg p q r s))
       (-invoke [_ a b c d e f g h i j k l m n o-arg p q r s t]
                (o a b c d e f g h i j k l m n o-arg p q r s t))
       (-invoke [_ a b c d e f g h i j k l m n o-arg p q r s t rest]
                (apply o a b c d e f g h i j k l m n o-arg p q r s t rest))]))

(do (ns-unmap 'sicmutils.operator '->Operator)
    (defn ->Operator
      "Positional factory function for [[Operator]].

  The final argument `m` defaults to nil if not supplied."
      ([o arity name context]
       (Operator. o arity name context nil))
      ([o arity name context m]
       (Operator. o arity name context m))))

#?(:cljs
   (extend-type Operator
     IPrintWithWriter
     (-pr-writer [x writer _]
       (write-all writer (.toString x)))))

#?(:clj
   (defmethod print-method Operator [^Operator op ^java.io.Writer w]
     (.write w (.toString op))))

(defn operator?
  "Returns true if the supplied `x` is an instance of [[Operator]], false
  otherwise."
  [x]
  (instance? Operator x))

(defn procedure
  "Returns the backing procedure of the supplied [[Operator]]. Errors if a
  non-[[Operator]] is supplied."
  [op]
  (if (operator? op)
    (.-o ^Operator op)
    (u/illegal (str "non-operator supplied: " op))))

(defn arity
  "Returns the arity of the supplied [[Operator]]. Errors if a non-[[Operator]] is
  supplied."
  [op]
  (if (operator? op)
    (.-arity ^Operator op)
    (u/illegal (str "non-operator supplied: " op))))

(defn name
  "Returns the stored name of the supplied [[Operator]]. Errors if a
  non-[[Operator]] is supplied."
  [op]
  (if (operator? op)
    (.-name ^Operator op)
    (u/illegal (str "non-operator supplied: " op))))

(defn context
  "Returns the context field of the supplied [[Operator]]. Errors if a
  non-[[Operator]] is supplied."
  [op]
  (if (operator? op)
    (.-context ^Operator op)
    (u/illegal (str "non-operator supplied: " op))))

(defn make-operator
  "Returns an [[Operator]] wrapping the supplied procedure `f` with the name
  `name`.

  Optionally accepts a `context` map that will be stored inside the
  returned [[Operator]]."
  ([f name]
   (make-operator f name {}))
  ([f name context]
   (->Operator f
               (:arity context (f/arity f))
               name
               (into {:subtype ::operator} context)
               nil)))

(defn- op:get
  "Returns an [[Operator]] that composes a lookup of the form `#(get % k)` with
  the wrapped procedure of the [[Operator]] `o`."
  [o k]
  (make-operator
   (f/get (procedure o) k)
   `(~'compose (~'component ~k)
     ~(name o))))

(def ^{:doc "Identity operator. Returns its argument unchanged."}
  identity
  (make-operator core-identity 'identity))

(defn- joint-context
  "Merges type context maps of the two operators. Where the maps have keys in
  common, they must agree; disjoint keys become part of the new joint context."
  [o p]
  {:pre [(operator? o)
         (operator? p)]}
  (reduce (fn [joint-ctx [k v]]
            (if-let [cv (k joint-ctx)]
              (if (= cv v)
                joint-ctx
                (u/illegal (str "incompatible operator context: " (context o) (context p))))
              (assoc joint-ctx k v)))
          (context o)
          (context p)))

(defn- combine-f-op
  "Returns a new operator generated by combining a non-operator `f` on the left
  with an operator `o` on the right, using the binary operation `op`.

  `sym` is used to generate a proper symbolic name for the new operator.

  The combination occurs by coercing `f` to an operator that composes with its
  argument before combining with `operator`. As an example, the following two
  expressions are equivalent:

  (+ <f> <operator>)

  (+ (make-operator (fn [g] (comp <f> g)) <name>)
     <operator>)

  If `f` isn't already a function it's coerced to a function via `(constantly
  <f>)`."
  [op sym f o]
  (let [h (f/coerce-to-fn f [:exactly 1])]
    (->Operator (fn [g] (op (f/compose h g) (o g)))
                (arity o)
	              `(~sym ~(v/freeze f) ~(name o))
                (context o))))

(defn- combine-op-f
  "Returns a new operator generated by combining an operator `o` on the left with
  a non-operator `f` on the right, using the binary operation `op`.

  `sym` is used to generate a proper symbolic name for the new operator.

  The combination occurs by coercing `f` to an operator that composes with its
  argument before combining with `operator`. As an example, the following two
  expressions are equivalent:

  (+ <operator> <f>)

  (+ <operator>
     (make-operator (fn [g] (comp <f> g)) <name>))

  If `f` isn't already a function it's coerced to a function via `(constantly
  <f>)`."
  [op sym o f]
  (let [h (f/coerce-to-fn f [:exactly 1])]
    (->Operator (fn [g] (op (o g) (f/compose h g)))
                (arity o)
	              `(~sym ~(name o) ~(v/freeze f))
                (context o))))

(defn- negate
  "Returns a new operator that composes [[g/negate]] with its own wrapped
  operation. Equivalent to:

  (g/* (make-operator g/negate 'negate) o)"
  [o]
  (->Operator (fn [& fs]
                (g/negate (apply o fs)))
              (arity o)
              (list '- (name o))
	            (context o)))

(defn- o:-
  "Subtract one operator from another. Produces an operator which computes the
  difference of applying the supplied operators."
  [o p]
  (->Operator #(g/sub (apply o %&) (apply p %&))
              (f/joint-arity [(arity o) (arity p)])
              `(~'- ~(name o) ~(name p))
              (joint-context o p)))

(defn- f-o [f o] (combine-f-op g/sub '- f o))
(defn- o-f [o f] (combine-op-f g/sub '- o f))

(defn- o:+
  "Add two operators. Produces an operator which adds the result of applying the
  given operators."
  [o p]
  (->Operator #(g/add (apply o %&) (apply p %&))
              (f/joint-arity [(f/arity o) (f/arity p)])
              `(~'+ ~(name o) ~(name p))
              (joint-context o p)))

(defn- f+o [f o] (combine-f-op g/add '+ f o))
(defn- o+f [o f] (combine-op-f g/add '+ o f))

(defn- o:*
  "Multiplication of operators is defined as their composition."
  [o p]
  (->Operator (f/compose o p)
              (arity p)
              `(~'* ~(name o) ~(name p))
              ;; TODO this seems fishy... why not unite them?
              (context p)))

(defn- f*o
  "Multiply an operator by a non-operator on the left. The non-operator acts on
  its argument by multiplication."
  [f o]
  (->Operator (fn [& gs]
                (g/mul f (apply o gs)))
              (arity o)
              `(~'* ~(v/freeze f) ~(name o))
              (context o)))

(defn- o*f
  "Multiply an operator by a non-operator on the right. The non-operator acts on
  its argument by multiplication."
  [o f]
  (->Operator (fn [& gs]
                (apply o (map (fn [g] (g/mul f g)) gs)))
              (arity o)
              `(~'* ~(name o) ~(v/freeze f))
              (context o)))

(defn- o-div-n
  "Returns a new operator that multiplies the output of `o` by the inverse of
  `n`."
  [o n]
  (->Operator (fn [& gs]
                (g/mul (g/invert n) (apply o gs)))
              (arity o)
	            `(~'/ ~(name o) ~n)
              (context o)))

(defn commutator [o p]
  (g/- (g/* o p) (g/* p o)))

(defn anticommutator [o p]
  (g/+ (g/* o p) (g/* p o)))

(defn exp
  "Returns an operator represented by a Taylor series expansion of $e^x$, applied
  to `op`. This expanded series of operators is itself an operator that applies
  each element to its argument.

  Put another way: `(exp g)` to an operator g means forming the power series

  ```
  I + g + 1/2 g^2 + ... + 1/n! g^n
  ```

  where (as elsewhere) exponentiating the operator means `n`-fold composition."
  [op]
  (assert (= (arity op) [:exactly 1]) "sicmutils.operator/exp")
  (->Operator (series/exp-series op)
              [:exactly 1]
              `(~'exp ~(name op))
              (context op)))

(defn expn
  "Similar to `exp`, but takes an optional argument `n` that defines an order for
  each term of the taylor series expansion."
  ([op] (exp op))
  ([op n]
   (assert (= (arity op) [:exactly 1]) "sicmutils.operator/expn")
   (->Operator (-> (series/exp-series op)
                   (series/inflate n))
               [:exactly 1]
               `(~'exp ~(name op))
               (context op))))

(derive ::v/scalar ::co-operator)
(derive ::v/function ::co-operator)

(doseq [[op f sym] [[g/exp series/exp-series 'exp]
                    [g/cos series/cos-series 'cos]
                    [g/sin series/sin-series 'sin]
                    [g/tan series/tan-series 'tan]
                    [g/sec series/sec-series 'sec]
                    [g/acos series/acos-series 'acos]
                    [g/asin series/asin-series 'asin]
                    [g/atan series/atan-series 'atan]
                    [g/cosh series/cosh-series 'cosh]
                    [g/sinh series/sinh-series 'sinh]
                    [g/tanh series/tanh-series 'tanh]
                    [g/asinh series/asinh-series 'asinh]
                    [g/atanh series/atanh-series 'atanh]]]
  (let [assert-str (str "g/" sym " :sicmutils.operator/operator")]
    (defmethod op [::operator] [g]
      (assert (= (arity g) [:exactly 1]) assert-str)
      (->Operator (f g)
                  [:exactly 1]
                  `(~sym ~(name g))
                  (context g)))))

(defmethod g/add [::operator ::operator] [o p] (o:+ o p))
(defmethod g/add [::operator ::co-operator] [o f] (o+f o f))
(defmethod g/add [::co-operator ::operator] [f o] (f+o f o))

(defmethod g/negate [::operator] [o] (negate o))

(defmethod g/sub [::operator ::operator] [o p] (o:- o p))
(defmethod g/sub [::operator ::co-operator] [o f] (o-f o f))
(defmethod g/sub [::co-operator ::operator] [f o] (f-o f o))

(defmethod g/mul [::operator ::operator] [o p] (o:* o p))
(defmethod g/mul [::operator ::co-operator] [o f] (o*f o f))
(defmethod g/mul [::co-operator ::operator] [f o] (f*o f o))

(defmethod g/square [::operator] [o] (o:* o o))
(defmethod g/cube [::operator] [o] (o:* o (o:* o o)))
(defmethod g/expt [::operator ::v/native-integral] [o n]
  {:pre [(not (g/negative? n))]}
  (reduce o:* identity (repeat n o)))

(defmethod g/div [::operator ::v/scalar] [o n] (o-div-n o n))
(defmethod g/solve-linear-right [::operator ::v/scalar] [o n] (o-div-n o n))
(defmethod g/solve-linear [::v/scalar ::operator] [n o] (o-div-n o n))
