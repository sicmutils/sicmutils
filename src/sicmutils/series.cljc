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

(ns sicmutils.series
  (:refer-clojure :exclude [identity])
  (:require [sicmutils.expression :as x]
            [sicmutils.generic :as g]
            [sicmutils.numbers]
            [sicmutils.util :as u]
            [sicmutils.value :as v])
  #?(:clj
     (:import (clojure.lang AFn IFn Seqable ISeq Sequential))))

;; # Power Series
;;
;; Following Power Serious here!
;;
;; TODO check for tests: https://github.com/pdonis/powerseries/blob/master/powerseries.py
;;
;; We would prefer to just use native Clojure lazy sequences to represent series
;; objects. But, they must be invokable as functions, so we must wrap them in a
;; deftype.
;;
;; ## Sequence Operations
;;
;; First, let's make a function that can get us a series.

(defn- ->series
  "Form the infinite sequence starting with the supplied values. The
  remainder of the series will be filled with the zero-value
  corresponding to the first of the given values."
  [xs]
  (lazy-cat xs (repeat (v/zero-like (first xs)))))

#_
(= [1 2 3 4 0 0 0 0 0 0]
   (take 10 (->series [1 2 3 4])))

;; Now, start to define operations. We'll prefix these by `seq` since right now
;; they operate on sequences, not on the series type we'll later define.

(defn- seq:negate [xs]
  (map g/negate xs))

#_
(= [-1 -2 -3 -4 0 0 0]
   (take 7 (seq:negate (->series [1 2 3 4]))))

;; ### Addition and Subtraction
;;
;; The core idea that we'll keep using is that by popping off head from tail, we
;; can rearrange and get a recursion relation. Here's addition:
;;
;; $$F+G=\left(f+x F_{1}\right)+\left(g+x G_{1}\right)=(f+g)+x\left(F_{1}+G_{1}\right)$$

(defn- seq:+ [f g]
  (map g/+ f g))

#_
(= [0 2 4 6 8]
   (take 5 (seq:+ (range) (range))))

;; If we want to add a constant, we simply add it to the first element of the
;; series and pop it back on. We'll supply two arities here in case `+` on the
;; constant isn't commutative.

(defn- seq+c [f c]
  (lazy-seq
   (cons (g/+ (first f) c) (rest f))))

(defn- c+seq [c f]
  (lazy-seq
   (cons (g/+ c (first f)) (rest f))))

#_
(let [series (->series [1 2 3 4])]
  (= [11 2 3 4 0 0]
     (take 6 (seq+c series 10))
     (take 6 (c+seq 10 series))))

;; Subtraction is easy too... but for reasons I don't understand, we get a
;; failure later when we use `g/-`, so let me do the negate trick here:

(defn- seq:- [f g]
  (seq:+ f (seq:negate g)))

#_
(= [0 0 0 0 0]
   (take 5 (seq:- (range) (range))))

;; Then constant subtraction is /not/ commutative:

(defn- seq-c [f c]
  (lazy-seq
   (cons (g/- (first f) c) (rest f))))

(defn- c-seq [c f]
  (lazy-seq
   (cons (g/- c (first f)) (seq:negate (rest f)))))

#_
(= [-10 1 2 3 4]
   (take 5 (seq-c (range) 10)))

#_
(= [10 -1 -2 -3 -4]
   (take 5 (c-seq 10 (range))))

;; ### Multiplication
;;
;; First, and go back to Doug for a description of why... but let's first
;; address multiplication by a scalar.

(defn- seq*c [f c] (map #(g/mul % c) f))
(defn- c*seq [c f] (map #(g/mul c %) f))

;; Armed with these, examine the general multiplication formula, obtained by
;; expanding out the head:tail representations of each sequence and multiplying
;; them out:
;;
;; $$F \times G=\left(f+x F_{1}\right) \times\left(g+x G_{1}\right)=f g+x\left(f G_{1}+F_{1} \times G\right)$$
;;
;; This is also called the "Cauchy Product" of the two sequences:
;; https://en.wikipedia.org/wiki/Cauchy_product

(defn- seq:* [f g]
  (letfn [(step [f]
            (lazy-seq
             (let [f*g  (g/mul (first f) (first g))
                   f*G1 (c*seq (first f) (rest g))
                   F1*G (step (rest f))]
               (cons f*g (seq:+ f*G1 F1*G)))))]
    (step f)))

#_
(= [0 4 11 20 30 40 50 60 70 80]
   (take 10 (seq:* (range) (->series [4 3 2 1]))))

;; ### Division
;;
;; This is trickier. The quotient $Q$ of $F$ and $G$ should satisfy:
;;
;; $$F = Q \times G$$
;;
;; See Doug for the derivation on what happens next.

(defn seq:div [f g]
  (lazy-seq
   (let [f0 (first f) fs (rest f)
         g0 (first g) gs (rest g)]
     (if (zero? g0)
       (if (zero? f0)
         (seq:div fs gs)
         (u/arithmetic-ex "ERROR: denominator has a zero constant term"))
       (let [q (g/div f0 g0)]
         (cons q (-> (seq:- fs (c*seq q gs))
                     (seq:div g))))))))

;; ### Reciprocal
;;
;; Power series reciprocal comes in here:
;; https://swtch.com/~rsc/thread/squint.pdf Talk about what is going on after
;; absorbing that a bit more.

(defn seq:invert [f]
  (lazy-seq
   (let [finv    (g/invert (first f))
         F1*Finv (seq:* (rest f) (seq:invert f))
         tail    (c*seq finv (seq:negate F1*Finv))]
     (cons finv tail))))

;; BOOM, this works.
#_
(let [x 3]
  (= [1 0 0 0 0]
     (take 5 (seq:* (iterate inc x) (seq:invert (iterate inc x))))))

;; ### Functional Composition
;;
;; TODO, describe what is going on on page 6:

(defn seq:compose [f g]
  (letfn [(step [f]
            (lazy-seq
             ;; TODO Annoyingly this assert seems to have to happen in here...
             (assert (zero? (first g)))
             (let [[f0 & fs] f
                   gs (rest g)
                   tail (seq:* gs (step fs))]
               (cons f0 tail))))]
    (step f)))

#_
(= [1 1 3 8 21 55 144 377 987 2584]
   (take 10 (seq:compose (iterate (fn [x] x) 1)
                         (cons 0 (iterate inc 1)))))

;; ### Functional Reversion
;;
;; Find the functional inverse of a power series.

(defn seq:revert [f]
  {:pre [(zero? (first f))]}
  (letfn [(step [g]
            (lazy-seq
             (let [G1   (rest g)
                   R    (step g)]
               (cons 0 (seq:invert
                        (seq:compose G1 R))))))]
    (step f)))

#_
(= [0 1 -2 5 -14]
   (take 5 (seq:revert (cons 0 (iterate inc 1)))))

#_
(let [f (cons 0 (iterate inc 1))]
  (= [0 1 0 0 0]
     (take 5 (seq:compose f (seq:revert f)))))

;; ### Differentiation and Integration
;;
;; Page 7:

(defn- seq:deriv [f]
  (map g/* (rest f) (iterate inc 1)))

;; This represents... the definite integral, so we don't need a constant term.

(defn- seq:integral
  ([s] (seq:integral s 0))
  ([s constant-term]
   (cons constant-term
         (map g/div s (iterate inc 1)))))

;; ## Exponentiation

(defn seq:expt [s e]
  (letfn [(expt [base pow]
            (loop [n pow
                   y (->series [1])
                   z base]
              (let [t (even? n)
                    n (quot n 2)]
                (cond
                  t (recur n y (seq:* z z))
                  (zero? n) (seq:* z y)
                  :else (recur n (seq:* z y) (seq:* z z))))))]
    (cond (pos? e)  (expt s e)
          (zero? e) (->series [1])
          :else (seq:invert (expt s (g/negate e))))))

;; ### Square Roots
;;
;; This is a series that, multiplied by itself, get you the original thing. This
;; is from page 8.

(defn seq:sqrt
  "Implementation only works in a few special cases. Note them, talk about page
  8!"
  [[f1 & [f2 & fs] :as f]]
  (letfn [(step [g]
            (lazy-seq
             (-> (seq:div
                  (seq:deriv g)
                  (c*seq 2 (step g)))
                 (seq:integral 1))))]
    (cond (and (v/nullity? f1)
               (v/nullity? f2))
          (cons f1 (step fs))

          (v/unity? f1) (step f)

          :else (u/illegal "Sequence must start with [0, 0] or 1."))))

;; ## Function Examples from Doug

;; On page 6, he has this:

(= [1 0 -6 0 12 0 -8 0 0 0]
   (take 10 (seq:expt (->series [1 0 -2]) 3)))

;; power series that sums to 1/(1-x) in its region of convergence.
#_
(take 10 (seq:div (->series [1])
                  (->series [1 -1])))

#_
(= (range 1 11)
   (take 10 (seq:div (->series [1])
                     (-> (->series [1 -1])
                         (seq:expt 2)))))

;; ## Various Taylor Series

(def expx
  (lazy-seq
   (seq:integral expx 1)))

#_
(= [
    1
    1
    #sicm/ratio 1/2
    #sicm/ratio 1/6
    #sicm/ratio 1/24
    #sicm/ratio 1/120
    #sicm/ratio 1/720
    #sicm/ratio 1/5040
    #sicm/ratio 1/40320
    #sicm/ratio 1/362880
    ]
   (take 10 expx2))

(declare cosx)
(def sinx (lazy-seq (seq:integral cosx)))
(def cosx (lazy-seq (c-seq 1 (seq:integral sinx))))
(def tanx (seq:div sinx cosx))

#_
(= [0
    1
    0
    #sicm/ratio -1/6
    0
    #sicm/ratio 1/120
    0
    #sicm/ratio -1/5040
    0
    #sicm/ratio 1/362880
    ]
   (take 10 sinx))

#_
(= [1
    0
    #sicm/ratio -1/2
    0
    #sicm/ratio 1/24
    0
    #sicm/ratio -1/720
    0
    #sicm/ratio 1/40320
    0
    ]
   (take 10 cosx))

(declare sinhx)
(def coshx (lazy-seq (seq:integral sinhx 1)))
(def sinhx (lazy-seq (seq:integral coshx)))
(def tanhx (seq:div sinhx coshx))


(def atanx (seq:integral (cycle [1 0 -1 0])))

;; ## Tests

#_
(->> (seq:- sinx (seq:sqrt (c-seq 1 (seq:expt cosx 2))))
     (take 30)
     (every? zero?))

#_
(->> (seq:- (seq:div sinx cosx)
            (seq:revert
             (seq:integral
              (seq:invert (->series [1 0 1])))))
     (take 30)
     (every? zero?))

;; ## Generating Functions


;; ### Catalan numbers

(def catalan
  (lazy-cat [1] (seq:* catalan catalan)))

#_
(is (= [1 1 2 5 14 42 132 429 1430 4862]
       (take 10 catalan)))

;; ordered trees...

(declare tree' forest' list')
(def tree' (lazy-cat [0] forest'))
(def list' (lazy-cat [1] list'))
(def forest' (seq:compose list' tree'))

#_
(= [0 1 1 2 5 14 42 132 429 1430]
   (take 10 tree'))

;; The catalan numbers again!

(def fib (lazy-cat [0 1] (map + fib (rest fib))))

;; See here for the recurrence relation:
;; https://en.wikipedia.org/wiki/Binomial_coefficient#Multiplicative_formula

(defn- binomial* [n]
  (letfn [(f [acc prev n k]
            (if (zero? n)
              acc
              (let [next (/ (* prev n) k)
                    acc' (conj! acc next)]
                (recur acc' next (dec n) (inc k)))))]
    (persistent!
     (f (transient [1]) 1 n 1))))

(defn binomial
  "The coefficients of (1+x)^n"
  [n]
  (->series (binomial* n)))

;; ## Making Series
;;
;; Next, we need to wrap all this up in types.

(declare zero one value)

;; TODO make arity a multimethod, so we can support all of these overriders?
(deftype Series [arity s]
  v/Value
  (nullity? [_] (empty? s))
  (unity? [_] false)
  (zero-like [_] zero)
  (one-like [_] one)
  (numerical? [_] false)
  (freeze [_]
    (let [prefix (sequence
                  (comp (take 4) (map g/simplify))
                  s)]
      `[~'Series ~arity ~@prefix ~'...]))
  (kind [_] ::series)

  Object
  (toString [S] (str (v/freeze S)))

  #?@
  (:clj
   [IFn
    (invoke [_] (Series. arity (map #(%) s)))
    (invoke [_ a]
            (Series. arity (map #(% a) s)))
    (invoke [_ a b]
            (Series. arity (map #(% a b) s)))
    (invoke [_ a b c]
            (Series. arity (map #(% a b c) s)))
    (invoke [_ a b c d]
            (Series. arity (map #(% a b c d) s)))
    (invoke [_ a b c d e]
            (Series. arity (map #(% a b c d e) s)))
    (invoke [_ a b c d e f]
            (Series. arity (map #(% a b c d e f) s)))
    (invoke [_ a b c d e f g]
            (Series. arity (map #(% a b c d e f g) s)))
    (invoke [_ a b c d e f g h]
            (Series. arity (map #(% a b c d e f g h) s)))
    (invoke [_ a b c d e f g h i]
            (Series. arity (map #(% a b c d e f g h i) s)))
    (invoke [_ a b c d e f g h i j]
            (Series. arity (map #(% a b c d e f g h i j) s)))
    (invoke [_ a b c d e f g h i j k]
            (Series. arity (map #(% a b c d e f g h i j k) s)))
    (invoke [_ a b c d e f g h i j k l]
            (Series. arity (map #(% a b c d e f g h i j k l) s)))
    (invoke [_ a b c d e f g h i j k l m]
            (Series. arity (map #(% a b c d e f g h i j k l m) s)))
    (invoke [_ a b c d e f g h i j k l m n]
            (Series. arity (map #(% a b c d e f g h i j k l m n) s)))
    (invoke [_ a b c d e f g h i j k l m n o]
            (Series. arity (map #(% a b c d e f g h i j k l m n o) s)))
    (invoke [_ a b c d e f g h i j k l m n o p]
            (Series. arity (map #(% a b c d e f g h i j k l m n o p) s)))
    (invoke [_ a b c d e f g h i j k l m n o p q]
            (Series. arity (map #(% a b c d e f g h i j k l m n o p q) s)))
    (invoke [_ a b c d e f g h i j k l m n o p q r]
            (Series. arity (map #(% a b c d e f g h i j k l m n o p q r) s)))
    (invoke [_ a b c d e f g h i j k l m n o p q r s]
            (Series. arity (map #(% a b c d e f g h i j k l m n o p q r s) s)))
    (invoke [_ a b c d e f g h i j k l m n o p q r s t]
            (Series. arity (map #(% a b c d e f g h i j k l m n o p q r s t) s)))
    (invoke [_ a b c d e f g h i j k l m n o p q r s t rest]
            (Series. arity (map #(apply % a b c d e f g h i j k l m n o p q r s t rest) s)))
    (applyTo [s xs] (AFn/applyToHelper s xs))

    Seqable
    (seq [_] s)

    Sequential

    ISeq
    (first [_] (first s))
    (next [_] (Series. arity (next s)))
    (more [_] (Series. arity (.more ^ISeq s)))
    (cons [_ o] (Series. arity (cons o s)))]

   :cljs
   [IFn
    (-invoke [_] (Series. arity (map #(%) s)))
    (-invoke [_ a]
             (Series. arity (map #(% a) s)))
    (-invoke [_ a b]
             (Series. arity (map #(% a b) s)))
    (-invoke [_ a b c]
             (Series. arity (map #(% a b c) s)))
    (-invoke [_ a b c d]
             (Series. arity (map #(% a b c d) s)))
    (-invoke [_ a b c d e]
             (Series. arity (map #(% a b c d e) s)))
    (-invoke [_ a b c d e f]
             (Series. arity (map #(% a b c d e f) s)))
    (-invoke [_ a b c d e f g]
             (Series. arity (map #(% a b c d e f g) s)))
    (-invoke [_ a b c d e f g h]
             (Series. arity (map #(% a b c d e f g h) s)))
    (-invoke [_ a b c d e f g h i]
             (Series. arity (map #(% a b c d e f g h i) s)))
    (-invoke [_ a b c d e f g h i j]
             (Series. arity (map #(% a b c d e f g h i j) s)))
    (-invoke [_ a b c d e f g h i j k]
             (Series. arity (map #(% a b c d e f g h i j k) s)))
    (-invoke [_ a b c d e f g h i j k l]
             (Series. arity (map #(% a b c d e f g h i j k l) s)))
    (-invoke [_ a b c d e f g h i j k l m]
             (Series. arity (map #(% a b c d e f g h i j k l m) s)))
    (-invoke [_ a b c d e f g h i j k l m n]
             (Series. arity (map #(% a b c d e f g h i j k l m n) s)))
    (-invoke [_ a b c d e f g h i j k l m n o]
             (Series. arity (map #(% a b c d e f g h i j k l m n o) s)))
    (-invoke [_ a b c d e f g h i j k l m n o p]
             (Series. arity (map #(% a b c d e f g h i j k l m n o p) s)))
    (-invoke [_ a b c d e f g h i j k l m n o p q]
             (Series. arity (map #(% a b c d e f g h i j k l m n o p q) s)))
    (-invoke [_ a b c d e f g h i j k l m n o p q r]
             (Series. arity (map #(% a b c d e f g h i j k l m n o p q r) s)))
    (-invoke [_ a b c d e f g h i j k l m n o p q r s]
             (Series. arity (map #(% a b c d e f g h i j k l m n o p q r s) s)))
    (-invoke [_ a b c d e f g h i j k l m n o p q r s t]
             (Series. arity (map #(% a b c d e f g h i j k l m n o p q r s t) s)))
    (-invoke [_ a b c d e f g h i j k l m n o p q r s t rest]
             (Series. arity (map #(apply % a b c d e f g h i j k l m n o p q r s t rest) s)))

    IPrintWithWriter
    (-pr-writer [x writer _]
                (write-all writer
                           "#object[sicmutils.series.Series \""
                           (.toString x)
                           "\"]"))

    ICollection
    (conj [_ o] (Series. arity (cons o s)))

    ISeqable
    (-seq [_] s)

    ISequential

    ISeq
    (-first [_] (-first s))
    (-rest [_] (-rest s))]))

#?(:clj
   (defmethod print-method Series [^Series s ^java.io.Writer w]
     (.write w (.toString s))))

(defn series? [s] (instance? Series s))

(defn- starting-with*
  "Version that lets us specify arities"
  ([prefix]
   (starting-with* prefix v/arity:exactly-0))
  ([prefix arity]
   (->Series arity (->series prefix))))

(defn starting-with
  "Form the infinite sequence starting with the supplied values. The
  remainder of the series will be filled with the zero-value
  corresponding to the first of the given values."
  [& prefix]
  (starting-with* prefix v/arity:exactly-0))

(defn generate
  "Produce the series generated by (f i) for i in 0, 1, ..."
  [f]
  (->Series v/arity:exactly-0 (map f (range))))

(def zero (starting-with 0))
(def one (starting-with 1))
(def identity (starting-with 0 1))

(defn constant
  ([c] (constant v/arity:exactly-0))
  ([c arity] (starting-with* [c] arity)))

(defn partial-sums
  "Form the infinite sequence of partial sums of the given series"
  [^Series s]
  (->Series (.-arity s) (reductions g/+ s)))

(defn fmap
  [f ^Series s]
  (->Series (.-arity s)
            (map f s)))

(defn sum [s n]
  (transduce (take (inc n)) g/+ s))

;; ## Examples

(defn value
  "Find the value of the series S applied to the argument x.
  This assumes that S is a series of applicables. If, in fact, S is a
  series of series-valued applicables, then the result will be a sort
  of layered sum of the values.

  Concretely, suppose that S has the form

    [[A1 A2 A3...] [B1 B2 B3...] [C1 C2 C3...]...]

  Then, this series applied to x will yield the series of values
    [(A1 x) (+ (A2 x) (B1 x)) (+ (A3 x) (B2 x) (C1 x)) ...]"
  [^Series S x]
  (letfn [(collect [[s & s-tail]]
            (let [first-result (s x)]
              (if (series? first-result)
                (let [[r & r-tail] first-result]
                  (lazy-seq (cons r (seq:+ r-tail (collect s-tail)))))

                ;; note that we have already realized first-result,
                ;; so it does not need to be behind lazy-seq.
                (cons first-result (lazy-seq (collect s-tail))))))]
    (cond (= (.-arity S) v/arity:exactly-0)
          (->Series (.-arity S)
                    (collect (.-s S)))

          :else (u/unsupported
                 (str "Cannot apply series of arity " (:arity S))))))

;; ## Generic Implementations

(derive ::x/numerical-expression ::coseries)

(defmethod g/add [::series ::series] [^Series s ^Series t]
  {:pre [(= (.-arity s) (.-arity t))]}
  (->Series (.-arity s) (seq:+ (.-s s) (.-s t))))

;; TODO complete this theme!
(defmethod g/add [v/seqtype ::series] [xs ^Series s]
  (g/add (starting-with* xs (.-arity s)) s))

(defmethod g/add [::series v/seqtype] [^Series s xs]
  (g/add s (starting-with* xs (.-arity s))))

(defmethod g/add [::coseries ::series] [c ^Series s]
  (->Series (.-arity s) (c+seq c s)))

(defmethod g/add [::series ::coseries] [^Series s c]
  (->Series (.-arity s) (seq+c s c)))

(defmethod g/negate [::series] [s] (fmap g/negate s))

(defmethod g/sub [::series ::series] [^Series s ^Series t]
  {:pre [(= (.-arity s) (.-arity t))]}
  (->Series (.-arity s) (seq:- (.-s s) (.-s t))))

(defmethod g/sub [::coseries ::series] [c ^Series s]
  (->Series (.-arity s) (c-seq c s)))

(defmethod g/sub [::series ::coseries] [^Series s c]
  (->Series (.-arity s) (seq-c s c)))

(defmethod g/mul [::series ::series] [^Series s ^Series t]
  {:pre [(= (.-arity s) (.-arity t))]}
  (->Series (.-arity s) (seq:* (.-s s) (.-s t))))

(defmethod g/mul [::coseries ::series] [c ^Series s]
  (->Series (.-arity s) (c*seq c (.-s s))))

(defmethod g/mul [::series ::coseries] [^Series s c]
  (->Series (.-arity s) (seq*c (.-s s) c)))

(defmethod g/square [::series] [s] (g/mul s s))

(defmethod g/cube [::series] [s] (g/mul (g/mul s s) s))

(defmethod g/invert [::series] [^Series s]
  (->Series (.-arity s) (seq:invert (.-s s))))

(defmethod g/div [::coseries ::series] [c ^Series s]
  (c*seq c (seq:invert (.-s s))))

(defmethod g/div [::series ::coseries] [^Series s c]
  (fmap #(g// % c) s))

(defmethod g/div [::series ::series] [^Series s ^Series t]
  {:pre [(= (.-arity s) (.-arity t))]}
  (->Series (.-arity s) (seq:div (.-s s) (.-s t))))

(defmethod g/partial-derivative [::series v/seqtype] [^Series s selectors]
  (let [a (.-arity s)]
    (condp = a
      v/arity:exactly-0
      (->Series a (map #(g/partial-derivative % selectors) (.-s s)))

      v/arity:exactly-1
      (if (empty? selectors)
        (->Series (.-arity s) (seq:deriv (.-s s)))
        (u/illegal (str "Cannot yet take partial derivatives of a series: "
                        s selectors)))

      :else
      (u/illegal (str "Can't differentiate series with arity " a)))))
