#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.operator
  (:refer-clojure :exclude [get identity name])
  (:require [clojure.core :as core]
            [pattern.rule :refer [rule-simplifier]]
            [emmy.differential :as d]
            [emmy.function :as f]
            [emmy.generic :as g]
            [emmy.series :as series]
            [emmy.simplify.rules :as rules]
            [emmy.util :as u]
            [emmy.value :as v])
  #?(:clj
     (:import (clojure.lang IFn ILookup IObj))))

(def ^{:private true
       :doc "Simplifier that acts on associative products and sums, and collects
  products into exponents. Operator multiplication is NOT associative, so only
  adjacent products are collected."}
  simplify-operator-name
  (rule-simplifier
   (rules/associative '+ '*)
   rules/exponent-contract
   (rules/unary-elimination '+ '*)))

(declare op:get)

(deftype Operator [o arity name context m]
  v/Value
  (zero? [this]
    (if-let [z-fn (:zero? context)]
      (z-fn this)
      (= o v/zero-like)))

  ;; NOTE: `one?` is the multiplicative identity; by default, we return false
  ;; because the system doesn't currently check if the types match for
  ;; multiplicative identity. So `(* o:identity 5)` would return 5, which is
  ;; incorrect. (We should get back a new operator that carries the scale-by-5
  ;; along until the final function resolves.)
  (one? [this]
    (if-let [one-fn (:one? context)]
      (one-fn this)
      false))

  (identity? [this]
    (if-let [id-fn (:identity? context)]
      (id-fn this)
      (= o core/identity)))

  (zero-like [this]
    (if-let [z-fn (:zero-like context)]
      (z-fn this)
      (Operator. v/zero-like arity 'zero context m)))

  (one-like [this]
    (if-let [one-fn (:one-like context)]
      (one-fn this)
      (Operator. core/identity arity 'identity context m)))

  (identity-like [this]
    (if-let [id-fn (:identity-like context)]
      (id-fn this)
      (Operator. core/identity arity 'identity context m)))

  (freeze [_]
    (simplify-operator-name
     (v/freeze name)))

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
       (valAt [_ _ _]
              (u/illegal "Operators don't support the not-found arity of get!"))])

  Object
  (toString [o]
    (let [n (v/freeze o)]
      (str (if (seqable? n)
             (seq n)
             n))))

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
       (-lookup [_ _ _]
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

(defn ^:no-doc with-context
  "Returns a copy of the supplied operator with `ctx` substituted for its
  context."
  [op ctx]
  (if (operator? op)
    (let [op ^Operator op]
      (->Operator (.-o op) (.-arity op) (.-name op)
                  ctx
                  (.-m op)))
    (u/illegal (str "non-operator supplied: " op))))

(defn make-operator
  "Returns an [[Operator]] wrapping the supplied procedure `f` with the symbolic
  name `name`. (`name` defaults to `'???`.)

  Optionally accepts a `context` map that will be stored inside the
  returned [[Operator]]."
  ([f]
   (make-operator f '??? {}))
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
  (make-operator core/identity 'identity))

(defn- joint-context
  "Merges type context maps of the two operators. Where the maps have keys in
  common, they must agree; disjoint keys become part of the new joint context.

  The exception is the :subtype key; if the values aren't
  equal, [[joint-context]] chooses the parent if one derives from the other, or
  throws if not."
  [o p]
  {:pre [(operator? o)
         (operator? p)]}
  (reduce-kv (fn [joint-ctx k v]
               (if-let [cv (k joint-ctx)]
                 (cond (= v cv)  joint-ctx

                       (and (= k :subtype) (isa? cv v))
                       (assoc joint-ctx k v)

                       (and (= k :subtype) (isa? v cv))
                       joint-ctx

                       :else
                       (u/illegal
                        (str "incompatible operator context: "
                             (context o) (context p)
                             " at key: " k)))
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
                (context o)
                nil)))

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
                (context o)
                nil)))

(defn- negate
  "Returns a new operator that composes [[g/negate]] with its own wrapped
  operation. Equivalent to:

  (g/* (make-operator g/negate 'negate) o)"
  [o]
  (->Operator (fn [& fs]
                (g/negate (apply o fs)))
              (arity o)
              (list '- (name o))
	            (context o)
              (meta o)))

(defn- o:-
  "Subtract one operator from another. Produces an operator which computes the
  difference of applying the supplied operators."
  [o p]
  (let [ctx (joint-context o p)]
    (if (v/zero? p)
      (with-context o ctx)
      (->Operator (fn [& xs]
                    (g/sub (apply o xs)
                           (apply p xs)))
                  (f/joint-arity [(arity o) (arity p)])
                  `(~'- ~(name o) ~(name p))
                  ctx
                  nil))))

(defn- f-o [f o] (combine-f-op g/sub '- f o))
(defn- o-f [o f] (combine-op-f g/sub '- o f))

(defn- o:+
  "Add two operators. Produces an operator which adds the result of applying the
  given operators."
  [o p]
  (let [ctx (joint-context o p)]
    (cond (v/zero? o) (with-context p ctx)
          (v/zero? p) (with-context o ctx)
          :else
          (->Operator (fn [& xs]
                        (g/add (apply o xs)
                               (apply p xs)))
                      (f/joint-arity [(f/arity o) (f/arity p)])
                      `(~'+ ~(name o) ~(name p))
                      ctx
                      nil))))

(defn- f+o [f o] (combine-f-op g/add '+ f o))
(defn- o+f [o f] (combine-op-f g/add '+ o f))

(defn- o:*
  "Multiplication of operators is defined as their composition."
  ([] identity)
  ([o] o)
  ([o p]
   (let [ctx (joint-context o p)]
     (cond (v/identity? o) (with-context p ctx)
           (v/identity? p) (with-context o ctx)
           (v/zero? o)     (with-context o ctx)
           :else
           (->Operator (f/compose o p)
                       (arity p)
                       `(~'* ~(name o) ~(name p))
                       ctx
                       nil)))))

(defn- f*o
  "Multiply an operator by a non-operator on the left. The non-operator acts on
  its argument by multiplication."
  [f o]
  (->Operator (fn [& gs]
                (g/mul f (apply o gs)))
              (arity o)
              `(~'* ~(v/freeze f) ~(name o))
              (context o)
              (meta o)))

(defn- o*f
  "Multiply an operator by a non-operator on the right. The non-operator acts on
  its argument by multiplication."
  [o f]
  (->Operator (fn [& gs]
                (apply o (map (fn [g] (g/mul f g)) gs)))
              (arity o)
              `(~'* ~(name o) ~(v/freeze f))
              (context o)
              (meta o)))

(defn- o-div-n
  "Returns a new operator that multiplies the output of `o` by the inverse of
  `n`."
  [o n]
  (->Operator (fn [& gs]
                (g/mul (g/invert n) (apply o gs)))
              (arity o)
	            `(~'/ ~(name o) ~n)
              (context o)
              (meta o)))

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
  (assert (= (arity op) [:exactly 1]) "emmy.operator/exp")
  (->Operator (series/exp-series op)
              [:exactly 1]
              `(~'exp ~(name op))
              (context op)
              (meta op)))

(defn expn
  "Similar to `exp`, but takes an optional argument `n` that defines an order for
  each term of the taylor series expansion."
  ([op] (exp op))
  ([op n]
   (assert (= (arity op) [:exactly 1]) "emmy.operator/expn")
   (->Operator (-> (series/exp-series op)
                   (series/inflate n))
               [:exactly 1]
               `(~'exp ~(name op))
               (context op)
               (meta op))))

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
                    [g/acot series/acot-series 'acot]
                    [g/cosh series/cosh-series 'cosh]
                    [g/sinh series/sinh-series 'sinh]
                    [g/tanh series/tanh-series 'tanh]
                    [g/asinh series/asinh-series 'asinh]
                    [g/atanh series/atanh-series 'atanh]]]
  (let [assert-str (str "g/" sym " :emmy.operator/operator")]
    (defmethod op [::operator] [g]
      (assert (= (arity g) [:exactly 1]) assert-str)
      (->Operator (f g)
                  [:exactly 1]
                  `(~sym ~(name g))
                  (context g)
                  nil))))

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
  (reduce o:* (repeat n o)))

(defmethod g/div [::operator ::v/scalar] [o n] (o-div-n o n))
(defmethod g/solve-linear-right [::operator ::v/scalar] [o n] (o-div-n o n))
(defmethod g/solve-linear [::v/scalar ::operator] [n o] (o-div-n o n))
