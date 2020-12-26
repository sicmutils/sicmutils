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

(ns sicmutils.operator
  (:refer-clojure :rename {get core-get
                           get-in core-get-in
                           identity core-identity}
                  #?@(:cljs [:exclude [get get-in identity]]))
  (:require [sicmutils.differential :as d]
            [sicmutils.function :as f]
            [sicmutils.generic :as g]
            [sicmutils.series :as series]
            [sicmutils.util :as u]
            [sicmutils.value :as v])
  #?(:clj
     (:import [clojure.lang IFn])))

(defrecord Operator [o arity name context]
  f/IArity
  (arity [_] arity)

  v/Value
  (zero? [_] false)
  (one? [_] false)
  (identity? [_] false)
  (zero-like [_] (Operator. v/zero-like arity 'zero context))
  (one-like [_] (Operator. core-identity arity 'identity context))
  (identity-like [_] (Operator. core-identity arity 'identity context))
  (freeze [_] (v/freeze name))
  (kind [_] (:subtype context))

  Object
  (toString [_] (let [n (v/freeze name)]
                  (str (if (seqable? n) (seq n) n))))

  #?@(:clj
      [IFn
       (invoke [_ f] (o f))
       (invoke [_ f g] (o f g))
       (invoke [_ f g h] (o f g h))
       (invoke [_ f g h i] (o f g h i))
       (applyTo [_ fns] (apply o fns))]

      :cljs
      [IFn
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
   (defmethod print-method Operator [^Operator s ^java.io.Writer w]
     (.write w (.toString s))))

(defn make-operator
  [o name & {:as context}]
  (->Operator o
              (or (:arity context) [:exactly 1])
              name
              (into {:subtype ::operator} context)))

(defn operator?
  [x]
  (instance? Operator x))

(defn get
  "Returns the value mapped to key, not-found or nil if key not present.

  TODO note special-cased for operators
  TODO move this and get-in into the deftype!!!"
  ([o k]
   (if (operator? o)
     (make-operator
      (f/get (:o o) k)
      `(~'compose (~'fn [~'x] (~'get ~'x ~k))
        ~(:name o)))
     (core-get o k)))
  ([o k not-found]
   (if (operator? o)
     (make-operator
      (f/get (:o o) k not-found)
      `(~'compose (~'fn [~'x] (~'get ~'x ~k ~not-found))
        ~(:name o)))
     (core-get o k not-found))))

(defn get-in
  "Returns the value in a nested associative structure, where ks is a sequence of
  keys. Returns nil if the key is not present, or the not-found value if
  supplied.

  TODO note special-cased for operators.
  TODO move this and get into the deftype!!!"
  ([o ks]
   (if (operator? o)
     (make-operator
      (f/get-in (:o o) ks)
      `(~'compose (~'fn [~'x] (~'get-in ~'x ~ks))
        ~(:name o)))
     (core-get-in o ks)))
  ([o ks not-found]
   (if (operator? o)
     (make-operator
      (f/get-in (:o o) ks not-found)
      `(~'compose (~'fn [~'x] (~'get-in ~'x ~ks ~not-found))
        ~(:name o)))
     (core-get-in o ks not-found))))

(def identity
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
                (u/illegal (str "incompatible operator context: " (:context o) (:context p))))
              (assoc joint-ctx k v)))
          (:context o)
          (:context p)))

(defn- op-o-f [op sym o f]
  (let [h (f/coerce-to-fn f [:exactly 1])]
    (->Operator (fn [g] (op (o g) (f/compose h g)))
                (:arity o)
	              `(~sym ~(:name o) ~(v/freeze f))
                (:context o))))

(defn- op-f-o [op sym f o]
  (let [h (f/coerce-to-fn f [:exactly 1])]
    (->Operator (fn [g] (op (f/compose h g) (o g)))
                (:arity o)
	              `(~sym ~(v/freeze f) ~(:name o))
                (:context o))))

(defn negate [o]
  (->Operator (fn [& fs]
                (g/negate (apply o fs)))
              (:arity o)
              (list '- (:name o))
	            (:context o)))

(defn- o:-
  "Subtract one operator from another. Produces an operator which computes the
  difference of applying the supplied operators."
  [o p]
  (->Operator #(g/- (apply o %&) (apply p %&))
              (f/joint-arity [(:arity o) (:arity p)])
              `(~'- ~(:name o) ~(:name p))
              (joint-context o p)))

(defn- o-f [o f] (op-o-f g/- '- o f))
(defn- f-o [o f] (op-o-f g/- '- f o))

(defn- o:+
  "Add two operators. Produces an operator which adds the result of applying the
  given operators."
  [o p]
  (->Operator #(g/+ (apply o %&) (apply p %&))
              (f/joint-arity [(f/arity o) (f/arity p)])
              `(~'+ ~(:name o) ~(:name p))
              (joint-context o p)))

(defn- o+f [o f] (op-o-f g/+ '+ o f))
(defn- f+o [o f] (op-o-f g/+ '+ f o))

(defn- o:*
  "Multiplication of operators is defined as their composition"
  [o p]
  (->Operator (with-meta (comp o p) {:arity (:arity p)})
              (:arity p)
              `(~'* ~(:name o) ~(:name p))
              ;; TODO this seems fishy... why not unite them?
              (:context p)))

(defn- f*o
  "Multiply an operator by a non-operator on the left. The non-operator acts on
  its argument by multiplication."
  [f o]
  (->Operator (fn [& gs]
                (g/* f (apply o gs)))
              (:arity o)
              `(~'* ~(v/freeze f) ~(:name o))
              (:context o)))

(defn- o*f
  "Multiply an operator by a non-operator on the right. The non-operator acts on
  its argument by multiplication."
  [o f]
  (->Operator (fn [& gs]
                (apply o (map (fn [g] (g/* f g)) gs)))
              (:arity o)
              `(~'* ~(:name o) ~(v/freeze f))
              (:context o)))

(defn o-div-n [o n]
  (->Operator (fn [& gs]
                (g/* (g// n) (apply o gs)))
              (:arity o)
	            `(~'/ ~(:name o) ~n)
              (:context o)))

(defn commutator [o p]
  (g/- (g/* o p) (g/* p o)))

(defn anticommutator [o p]
  (g/+ (g/* o p) (g/* p o)))

(defn exp
  "Returns an operator represented by a Taylor series expansion of $e^x$, applied
  to `op`. This expanded series of operators is itself an operator that applies
  each element to its argument.

  Put another way: `(exp g)` to an operator g means forming the power series

  I + g + 1/2 g^2 + ... + 1/n! g^n

  where (as elsewhere) exponentiating the operator means n-fold composition."
  [op]
  (assert (= (:arity op) [:exactly 1]) "sicmutils.operator/exp")
  (->Operator (series/exp-series op)
              [:exactly 1]
              `(~'exp ~(:name op))
              (:context op)))

(defn expn
  "Similar to `exp`, but takes an optional argument `n` that defines an order for
  each term of the taylor series expansion."
  ([op] (exp op))
  ([op n]
   (assert (= (:arity op) [:exactly 1]) "sicmutils.operator/expn")
   (->Operator (-> (series/exp-series op)
                   (series/inflate n))
               [:exactly 1]
               `(~'exp ~(:name op))
               (:context op))))

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
      (assert (= (:arity g) [:exactly 1]) assert-str)
      (->Operator (f g)
                  [:exactly 1]
                  `(~sym ~(:name g))
                  (:context g)))))

(defmethod g/add [::operator ::operator] [o p] (o:+ o p))
(defmethod g/add [::operator ::co-operator] [o f] (o+f o f))
(defmethod g/add [::co-operator ::operator] [f o] (f+o f o))

(defmethod g/negate [::operator] [o] (negate o))

(defmethod g/sub [::operator ::operator] [o p] (o:- o p))
(defmethod g/sub [::operator ::co-operator] [o f] (o-f o f))
(defmethod g/sub [::co-operator ::operator] [f o] (f-o f o))

(defmethod g/mul [::operator ::operator] [o p] (o:* o p))
(defmethod g/mul [::operator ::co-operator] [o n] (o*f o n))
(defmethod g/mul [::co-operator ::operator] [n o] (f*o n o))

(defmethod g/expt [::operator ::v/native-integral] [o n]
  {:pre [(not (g/negative? n))]}
  (reduce o:* identity (repeat n o)))

(defmethod g/div [::operator ::scalar] [o n] (o-div-n o n))

(defmethod g/square [::operator] [o] (o:* o o))

(defmethod g/simplify [::operator] [o] (:name o))
