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
  (:require [sicmutils.generic :as g]
            [sicmutils.util :as u]
            [sicmutils.value :as v])
  #?(:clj
     (:import (clojure.lang AFn IFn
                            Seqable ISeq))))

;; # Power Series
;;
;; Following Power Series here!
;;
;; We would prefer to just use native Clojure lazy sequences to represent series
;; objects. But, they must be invokable as functions, so we must wrap them in a
;; deftype.
;;
;; ## Sequence Operations

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
  ([[x :as prefix] arity]
   (let [s (lazy-cat prefix (repeat (v/zero-like x)))]
     (->Series arity s))))

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

;; Each of these return sequences.
(defn- c*s [c s] (map #(g/* c %) s))
(defn- s*c [s c] (map #(g/* % c) s))
(defn- s+s [s t] (map g/+ s t))
(defn- s-s [s t]
  (map (fn [l r]
         ;; TODO very strange, in ch6 this leads to problems.
         (g/+ l (g/negate r)))
       s t))

(defn- s*s
  "The Cauchy product of the two sequences"
  [s t]
  (letfn [(step [s]
            (lazy-seq
             (cons (g/mul (first s)
                          (first t))
                   (s+s (c*s (first s) (rest t))
                        (step (rest s))))))]
    (step s)))

(defn s-invert
  "TODO document."
  [s]
  (lazy-seq
   (cons 1 (map g/negate
                (s*s
                 (rest s)
                 (s-invert s))))))

(defn s-div
  "TODO document."
  [s t]
  (if (zero? (first t))
    (println "ERROR: denominator has a zero constant term")
    (c*s (g/invert (first t))
         (s*s s (s-invert (s*c t (g/invert (first t))))))))

(defn s-compose
  "TODO document."
  [f g]
  (lazy-seq
   (cons (first f)
         (s*s (rest g)
              (s-compose
               (rest f)
               (cons 0 (rest g)))))))

(defn s-reverse
  "TODO documention Reversion"
  [s]
  (lazy-seq
   (cons 0
         (s-invert
          (s-compose s (s-reverse s))))))

(defn- differentiate [[_ & tail]]
  (map g/* tail (generate inc)))

(defn- integrate [s]
  (map g/div s (generate inc)))

(defn sqrt-series [s]
  (lazy-seq
   (cons 1
         (s+s (repeat 1)
              (integrate
               (s-div
                (differentiate s)
                (s*c (sqrt-series s) 2)))))))

;; TODO integration
;; TODO double check differentiation

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
  (letfn [(collect [[s0 & s-more]]
            (let [first-result (s0 x)]
              (if (series? first-result)
                (let [[r0 & r-more] first-result]
                  (lazy-seq (cons r0 (s+s r-more (collect s-more)))))

                ;; note that we have already realized first-result,
                ;; so it does not need to be behind lazy-seq.
                (cons first-result (lazy-seq (collect s-more))))))]
    (cond (= (.-arity S) v/arity:exactly-0)
          (->Series (.-arity S)
                    (collect (.-s S)))

          :else (u/unsupported
                 (str "Cannot apply series of arity " (:arity S))))))

;; ## Generic Implementations

(defmethod g/add [::series ::series] [^Series s ^Series t]
  {:pre [(= (.-arity s) (.-arity t))]}
  (->Series (.-arity s) (s+s (.-s s) (.-s t))))

(defmethod g/add [::v/default ::series] [c ^Series s]
  (->Series (.-arity s)
            (let [[h & tail] (.-s s)]
              (lazy-seq
               (cons (g/+ c h) tail)))))

(defmethod g/add [::series ::v/default] [^Series s c]
  (->Series (.-arity s)
            (let [[h & tail] (.-s s)]
              (lazy-seq
               (cons (g/+ h c) tail)))))

(defmethod g/negate [::series] [s] (fmap g/negate s))

(defmethod g/sub [::series ::series] [^Series s ^Series t]
  {:pre [(= (.-arity s) (.-arity t))]}
  (->Series (.-arity s) (s-s (.-s s) (.-s t))))

(defmethod g/sub [::v/default ::series] [c ^Series s]
  (->Series (.-arity s)
            (let [[h & tail] (.-s s)]
              (lazy-seq
               (cons (g/- c h) (map g/negate tail))))))

(defmethod g/sub [::series ::v/default] [^Series s c]
  (->Series (.-arity s)
            (let [[h & tail] (.-s s)]
              (lazy-seq
               (cons (g/- h c) tail)))))

(defmethod g/mul [::series ::series] [^Series s ^Series t]
  {:pre [(= (.-arity s) (.-arity t))]}
  (->Series (.-arity s) (s*s (.-s s) (.-s t))))

(defmethod g/mul [::v/default ::series] [c ^Series s]
  (->Series (.-arity s) (c*s c (.-s s))))

(defmethod g/mul [::series ::v/default] [^Series s c]
  (->Series (.-arity s) (s*c (.-s s) c)))

(defmethod g/square [::series] [s] (g/mul s s))

(defmethod g/cube [::series] [s] (g/mul (g/mul s s) s))

(defmethod g/invert [::series] [^Series s]
  (->Series (.-arity s) (s-invert (.-s s))))

(defmethod g/div [::v/default ::series] [c ^Series s]
  (g/mul (constant c (.-arity s))
         (g/invert s)))

(defmethod g/div [::series ::v/default] [^Series s c]
  (fmap #(g// % c) s))

(defmethod g/div [::series ::series] [^Series s ^Series t]
  {:pre [(= (.-arity s) (.-arity t))]}
  (->Series (.-arity s) (s-div (.-s s) (.-s t))))

;; TODO - exponent

(comment
  (define (series:expt s e)
    (letrec ((square (lambda (s) (mul-series s s)))
             (series:one (cons-stream :one zero-stream))
             (zuras
              (lambda (t e k)
                      (cons-stream
                       :one
                       (stream:c*s (div-coeff e k)
                                   (mul-series
                                    t
                                    (zuras t
                                           (sub-coeff e 1)
                                           (fix:+ k 1)))))))
             (iexpt
              (lambda (s e)
                      (cond ((fix:< e 0) (invert-series (iexpt s (fix:negate e))))
                            ((fix:= e 0) :one)
                            ((fix:= e 1) s)
                            ((even? e) (square
                                        (iexpt s (fix:quotient e 2))))
                            (else
                             (mul-series
                              s
                              (square
                               (iexpt s
                                      (fix:quotient (fix:- e 1) 2))))))))
             (expt
              (lambda (s e)
                      (if (exact-integer? e)
                        (iexpt s e)
                        (stream:c*s
                         (expt-coeff (head s) e)
                         (zuras (stream:s/c (tail s) (head s)) e 1))))))
            (make-series (series:arity s)
                         (expt (series->stream s) e)))))

(defmethod g/partial-derivative [::series v/seqtype] [^Series s selectors]
  (let [a (.-arity s)]
    (condp = a
      v/arity:exactly-0
      (->Series a (map #(g/partial-derivative % selectors) (.-s s)))

      v/arity:exactly-1
      (if (empty? selectors)
        (->Series (.-arity s) (differentiate (.-s s)))
        (u/illegal (str "Cannot yet take partial derivatives of a series: "
                        s selectors)))

      :else
      (u/illegal (str "Can't differentiate series with arity " a)))))

;; ## Examples
