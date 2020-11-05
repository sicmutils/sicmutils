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

(ns sicmutils.series
  (:refer-clojure :exclude [identity])
  (:require [sicmutils.expression :as x]
            [sicmutils.generic :as g]
            [sicmutils.numbers]
            [sicmutils.util :as u]
            [sicmutils.value :as v])
  #?(:clj
     (:import (clojure.lang AFn IFn Seqable Sequential))))

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
     (cond (and (v/nullity? f0) (v/nullity? g0))
           (seq:div fs gs)

           (v/nullity? f0)
           (cons f0 (seq:div fs g))

           (v/nullity? g0)
           (u/arithmetic-ex "ERROR: denominator has a zero constant term")

           :else (let [q (g/div f0 g0)]
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

;; ### Constant Division

(defn seq-div-c [f c]
  (c*seq c (seq:invert f)))

(defn c-div-seq [c f]
  (map #(g// % c) f))

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

;; ## Application

(defn- seq:p-value
  "Evaluates the power series, and converts it back down to a normal series."
  [f x]
  (let [one    (v/one-like x)
        powers (iterate #(g/* x %) one)]
    (map g/* f powers)))

(declare series?)

(defn- seq:value
  "Find the value of the Series S applied to the arguments xs.

  This assumes that S is a series of applicables. If, in fact, S is a
  series of series-valued applicables, then the result will be a sort
  of layered sum of the values.

  Concretely, suppose that S has the form

    [[A1 A2 A3...] [B1 B2 B3...] [C1 C2 C3...]...]

  Then, this series applied to x will yield the series of values
    [(A1 x) (+ (A2 x) (B1 x)) (+ (A3 x) (B2 x) (C1 x)) ...]"
  [f xs]
  (letfn [(collect [[f & fs]]
            (let [result (apply f xs)]
              (if (series? result)
                (lazy-seq
                 (let [[r & r-tail] result]
                   (cons r (seq:+ r-tail (collect fs)))))

                ;; note that we have already realized first-result,
                ;; so it does not need to be behind lazy-seq.
                (cons result (lazy-seq (collect fs))))))]
    (collect f)))

;; ## Various Taylor Series

(def expx
  (lazy-seq
   (seq:integral expx 1)))

(def log1-x
  (seq:integral (repeat -1)))

;; https://en.wikipedia.org/wiki/Mercator_series
(def log1+x
  (seq:integral (cycle [1 -1])))

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
   (take 10 expx))

(declare cosx)
(def sinx (lazy-seq (seq:integral cosx)))
(def cosx (lazy-seq (c-seq 1 (seq:integral sinx))))
(def tanx (seq:div sinx cosx))
(def secx (seq:invert cosx))

(def asinx (seq:revert sinx))
(def acosx (c-seq (g/div 'pi 2) asinx))
(def atanx (seq:integral (cycle [1 0 -1 0])))
(def acotx (c-seq (g/div 'pi 2) atanx))
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
(def asinhx (seq:revert sinhx))
(def atanhx (seq:revert tanhx))

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
(= [1 1 2 5 14 42 132 429 1430 4862]
   (take 10 catalan))

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

;; ## Type Wrappers
;;
;; Next, we need to wrap all this up in types. We'll need two:
;;
;; - PowerSeries
;; - Series
;;
;; Both similar, except they apply differently.

(declare s-zero s-one)

(deftype Series [xs]
  v/Value
  (nullity? [_] false)
  (unity? [_] false)
  (zero-like [_] s-zero)
  (one-like [_] s-one)
  (numerical? [_] false)
  (freeze [_]
    (let [prefix (sequence
                  (comp (take 4) (map g/simplify))
                  xs)]
      `[~'Series ~@prefix ~'...]))
  (kind [_] ::series)

  Object
  (toString [S] (str (v/freeze S)))

  #?@
  (:clj
   [Seqable
    (seq [_] xs)

    IFn
    (invoke [_]
            (Series. (seq:value xs [])))
    (invoke [_ a]
            (Series. (seq:value xs [a])))
    (invoke [_ a b]
            (Series. (seq:value xs [a b])))
    (invoke [_ a b c]
            (Series. (seq:value xs [a b c])))
    (invoke [_ a b c d]
            (Series. (seq:value xs [a b c d])))
    (invoke [_ a b c d e]
            (Series. (seq:value xs [a b c d e])))
    (invoke [_ a b c d e f]
            (Series. (seq:value xs [a b c d e f])))
    (invoke [_ a b c d e f g]
            (Series. (seq:value xs [a b c d e f g])))
    (invoke [_ a b c d e f g h]
            (Series. (seq:value xs [a b c d e f g h])))
    (invoke [_ a b c d e f g h i]
            (Series. (seq:value xs [a b c d e f g h i])))
    (invoke [_ a b c d e f g h i j]
            (Series. (seq:value xs [a b c d e f g h i j])))
    (invoke [_ a b c d e f g h i j k]
            (Series. (seq:value xs [a b c d e f g h i j k])))
    (invoke [_ a b c d e f g h i j k l]
            (Series. (seq:value xs [a b c d e f g h i j k l])))
    (invoke [_ a b c d e f g h i j k l m]
            (Series. (seq:value xs [a b c d e f g h i j k l m])))
    (invoke [_ a b c d e f g h i j k l m n]
            (Series. (seq:value xs [a b c d e f g h i j k l m n])))
    (invoke [_ a b c d e f g h i j k l m n o]
            (Series. (seq:value xs [a b c d e f g h i j k l m n o])))
    (invoke [_ a b c d e f g h i j k l m n o p]
            (Series. (seq:value xs [a b c d e f g h i j k l m n o p])))
    (invoke [_ a b c d e f g h i j k l m n o p q]
            (Series. (seq:value xs [a b c d e f g h i j k l m n o p q])))
    (invoke [_ a b c d e f g h i j k l m n o p q r]
            (Series. (seq:value xs [a b c d e f g h i j k l m n o p q r])))
    (invoke [_ a b c d e f g h i j k l m n o p q r s]
            (Series. (seq:value xs [a b c d e f g h i j k l m n o p q r s])))
    (invoke [_ a b c d e f g h i j k l m n o p q r s t]
            (Series. (seq:value xs [a b c d e f g h i j k l m n o p q r s t])))
    (invoke [_ a b c d e f g h i j k l m n o p q r s t rest]
            (Series. (seq:value xs (concat [a b c d e f g h i j k l m n o p q r s t] rest))))
    (applyTo [s xs] (AFn/applyToHelper s xs))]

   :cljs
   [ISeqable
    (-seq [_] s)

    IPrintWithWriter
    (-pr-writer [x writer _]
                (write-all writer
                           "#object[sicmutils.series.Series \""
                           (.toString x)
                           "\"]"))

    IFn
    (-invoke [_]
             (Series. (seq:value xs [])))
    (-invoke [_ a]
             (Series. (seq:value xs [a])))
    (-invoke [_ a b]
             (Series. (seq:value xs [a b])))
    (-invoke [_ a b c]
             (Series. (seq:value xs [a b c])))
    (-invoke [_ a b c d]
             (Series. (seq:value xs [a b c d])))
    (-invoke [_ a b c d e]
             (Series. (seq:value xs [a b c d e])))
    (-invoke [_ a b c d e f]
             (Series. (seq:value xs [a b c d e f])))
    (-invoke [_ a b c d e f g]
             (Series. (seq:value xs [a b c d e f g])))
    (-invoke [_ a b c d e f g h]
             (Series. (seq:value xs [a b c d e f g h])))
    (-invoke [_ a b c d e f g h i]
             (Series. (seq:value xs [a b c d e f g h i])))
    (-invoke [_ a b c d e f g h i j]
             (Series. (seq:value xs [a b c d e f g h i j])))
    (-invoke [_ a b c d e f g h i j k]
             (Series. (seq:value xs [a b c d e f g h i j k])))
    (-invoke [_ a b c d e f g h i j k l]
             (Series. (seq:value xs [a b c d e f g h i j k l])))
    (-invoke [_ a b c d e f g h i j k l m]
             (Series. (seq:value xs [a b c d e f g h i j k l m])))
    (-invoke [_ a b c d e f g h i j k l m n]
             (Series. (seq:value xs [a b c d e f g h i j k l m n])))
    (-invoke [_ a b c d e f g h i j k l m n o]
             (Series. (seq:value xs [a b c d e f g h i j k l m n o])))
    (-invoke [_ a b c d e f g h i j k l m n o p]
             (Series. (seq:value xs [a b c d e f g h i j k l m n o p])))
    (-invoke [_ a b c d e f g h i j k l m n o p q]
             (Series. (seq:value xs [a b c d e f g h i j k l m n o p q])))
    (-invoke [_ a b c d e f g h i j k l m n o p q r]
             (Series. (seq:value xs [a b c d e f g h i j k l m n o p q r])))
    (-invoke [_ a b c d e f g h i j k l m n o p q r s]
             (Series. (seq:value xs [a b c d e f g h i j k l m n o p q r s])))
    (-invoke [_ a b c d e f g h i j k l m n o p q r s t]
             (Series. (seq:value xs [a b c d e f g h i j k l m n o p q r s t])))
    (-invoke [_ a b c d e f g h i j k l m n o p q r s t rest]
             (Series. (seq:value xs (concat [a b c d e f g h i j k l m n o p q r s t] rest))))]))

#?(:clj
   (defmethod print-method Series [^Series s ^java.io.Writer w]
     (.write w (str "#object[sicmutils.series.Series \""
                    (.toString s)
                    "\"]"))))

(def s-zero (Series. (->series [0])))
(def s-one (Series. (->series [1])))

;; ### Power Series

(declare zero one)

(deftype PowerSeries [xs]
  v/Value
  (nullity? [_] false)
  (unity? [_] false)
  (zero-like [_] zero)
  (one-like [_] one)
  (numerical? [_] false)
  (freeze [_]
    (let [prefix (sequence
                  (comp (take 4) (map g/simplify))
                  xs)]
      `[~'PowerSeries ~@prefix ~'...]))
  (kind [_] ::power-series)

  Object
  (toString [S] (str (v/freeze S)))

  #?@
  (:clj
   [Seqable
    (seq [_] xs)

    IFn
    (invoke [_ a] (Series. (seq:p-value xs a)))]

   :cljs
   [ISeqable
    (-seq [_] xs)

    IFn
    (-invoke [_ a] (Series. (seq:p-value xs a)))

    IPrintWithWriter
    (-pr-writer [this writer _]
                (write-all writer
                           "#object[sicmutils.series.PowerSeries \""
                           (.toString this)
                           "\"]"))]))

#?(:clj
   (defmethod print-method PowerSeries [^PowerSeries s ^java.io.Writer w]
     (.write w (str "#object[sicmutils.series.PowerSeries \""
                    (.toString s)
                    "\"]"))))

;; ## Series Methods

(defn series?
  "Test if it's a series OR a power series, either one."
  [s]
  (or (instance? Series s)
      (instance? PowerSeries s)))

(defn power-series?
  "Do we specifically have a power series? The difference is we can apply this
  thing as a function."
  [s]
  (instance? PowerSeries s))

(defn- starting-with* [prefix]
  (->Series (->series prefix)))

(defn starting-with
  "Form the infinite sequence starting with the supplied values. The
  remainder of the series will be filled with the zero-value
  corresponding to the first of the given values."
  [& prefix]
  (starting-with* prefix))

(defn generate
  "Produce the series generated by (f i) for i in 0, 1, ..."
  [f]
  (->Series (map f (range))))

(def zero (starting-with* [0]))
(def one (starting-with* [1]))
(def identity (starting-with* [0 1]))

(defn constant
  [c] (starting-with* [c]))

(defn partial-sums
  "Form the series of partial sums of the given series"
  [^Series s]
  (->Series (reductions g/+ s)))

(defn fmap
  "TODO switch between series, power series"
  [f s]
  (->Series (map f s)))

(defn sum [s n]
  (transduce (take (inc n)) g/+ s))

;; ## Generic Implementations

(derive ::x/numerical-expression ::coseries)

;; ### Series Implementations

(doseq [[ctor kind] [[->Series ::series]
                     [->PowerSeries ::power-series]]]
  (defmethod g/add [kind kind] [s t]
    (ctor (seq:+ (seq s) (seq t))))

  (defmethod g/add [::coseries kind] [c s]
    (ctor (c+seq c (seq s))))

  (defmethod g/add [kind ::coseries] [s c]
    (ctor (seq+c (seq s) c)))

  (defmethod g/negate [kind] [s]
    (ctor (seq:negate (seq s))))

  (defmethod g/sub [kind kind] [s t]
    (ctor (seq:- (seq s) (seq t))))

  (defmethod g/sub [::coseries kind] [c s]
    (ctor (c-seq c (seq s))))

  (defmethod g/sub [kind ::coseries] [s c]
    (ctor (seq-c (seq s) c)))

  (defmethod g/mul [kind kind] [s t]
    (ctor (seq:* (seq s) (seq t))))

  (defmethod g/mul [::coseries kind] [c s]
    (ctor (c*seq c (seq s))))

  (defmethod g/mul [kind ::coseries] [s c]
    (ctor (seq*c (seq s) c)))

  (defmethod g/square [kind] [s]
    (let [xs (seq s)]
      (ctor (seq:* xs xs))))

  (defmethod g/cube [kind] [s]
    (let [xs (seq s)]
      (ctor (seq:* (seq:* xs xs) xs))))

  (defmethod g/invert [kind] [s]
    (ctor (seq:invert (seq s))))

  (defmethod g/div [::coseries kind] [c s]
    (ctor (c-div-seq c (seq s))))

  (defmethod g/div [kind ::coseries] [s c]
    (ctor (seq-div-c (seq s) c)))

  (defmethod g/div [kind kind] [s t]
    (ctor (seq:div (seq s) (seq t))))

  (defmethod g/simplify [kind] [s]
    (map g/simplify (seq s))))

;; ## Derivatives

(defmethod g/partial-derivative [::series v/seqtype] [^Series s selectors]
  (->Series (map #(g/partial-derivative % selectors)
                 (.-xs s))))

(defmethod g/partial-derivative [::power-series v/seqtype] [^PowerSeries s selectors]
  (if (empty? selectors)
    (->PowerSeries (seq:deriv (.-xs s)))
    (u/illegal
     (str "Cannot yet take partial derivatives of a power series: " s selectors))))

(def exp-series (->PowerSeries expx))
(def sin-series (->PowerSeries sinx))
(def cos-series (->PowerSeries cosx))
(def sinh-series (->PowerSeries sinhx))
(def cosh-series (->PowerSeries coshx))
(def tan-series (->PowerSeries tanx))
(def atan-series (->PowerSeries atanx))
(defn binomial-series [n]
  (->PowerSeries (binomial n)))

;; TODO add more.

;; Missing:
;;
;; - function-> takes the constant term and generates a power series.
;; - ->function, turn into a power series
;; - inflate, not sure yet!

(defn inflate
  "Inflates each term by a factor of n, so good."
  [xs n]
  (if (<= n 1)
    xs
    (let [zero  (v/zero-like (first xs))
          zeros (repeat (dec n) zero)]
      (->Series
       (->> (map cons xs (repeat zeros))
            (apply concat))))))

;; - make nth work!
;; - constant series
