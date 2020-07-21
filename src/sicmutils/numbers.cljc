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

(ns sicmutils.numbers
  (:refer-clojure :rename {zero? core-zero?
                           / core-div
                           + core-plus
                           - core-minus
                           * core-times}
                  #?@(:cljs [:exclude [zero? / + - *]]))
  (:require #?(:clj [clojure.math.numeric-tower :as nt])
            [sicmutils.complex :refer [complex]]
            [sicmutils.generic :as g]
            [sicmutils.value :as v])
  #?(:clj
     (:import (clojure.lang BigInt Ratio))))

;; TODO: these are also defined privately in `sicmutils.numsymb`. Where is the
;; right place to put these? Also: are we benefitting here from the numeric
;; tower implementation?
(def ^:private compute-sqrt #?(:clj nt/sqrt :cljs Math/sqrt))
(def ^:private compute-expt #?(:clj nt/expt :cljs Math/pow))
(def ^:private compute-abs #?(:clj nt/abs :cljs Math/abs))
(def ^:private numtype #?(:clj Number :cljs js/Number))
#?(:cljs (def ^:private ratio? (constantly false)))

(extend-type #?(:clj Number :cljs number)
  v/Value
  (nullity? [x] (core-zero? x))
  (unity? [x] (== 1 x))
  (zero-like [_] 0)
  (one-like [_] 1)
  (freeze [x] x)
  (exact? [x] (or (integer? x) (ratio? x)))
  (numerical? [_] true)
  (kind [x] (type x)))

(defn ^:private define-unary-operation
  [generic-operation core-operation]
  (defmethod generic-operation [numtype] [a]
    (core-operation a)))

(defn ^:private define-binary-operation
  [generic-operation core-operation]
  (defmethod generic-operation [numtype numtype] [a b]
    (core-operation a b)))

;; Generic operations defined on and between numbers.
(define-unary-operation g/tan #(Math/tan %))
(define-unary-operation g/exp #(Math/exp %))
(define-unary-operation g/negate core-minus)
(define-unary-operation g/invert core-div)
(define-unary-operation g/square #(compute-expt % 2))
(define-unary-operation g/cube #(compute-expt % 3))
(define-unary-operation g/exp #(Math/exp %))
(define-unary-operation g/abs compute-abs)
(define-unary-operation g/magnitude compute-abs)

(define-binary-operation g/add #?(:clj +' :cljs core-plus))
(define-binary-operation g/mul #?(:clj *' :cljs core-times))
(define-binary-operation g/sub #?(:clj -' :cljs core-minus))
(define-binary-operation g/div core-div)
(define-binary-operation g/expt compute-expt)

(define-unary-operation g/cos #(Math/cos %))
(define-unary-operation g/sin #(Math/sin %))
(define-unary-operation g/tan #(Math/tan %))
(define-unary-operation g/atan #(Math/atan %))
(defmethod g/atan [numtype numtype] [y x] (Math/atan2 y x))

;; Operations which allow promotion to complex numbers when their
;; arguments would otherwise result in a NaN if computed on the real
;; line

(defmethod g/asin
  [numtype]
  [a]
  (if (> (g/abs a) 1)
    (g/asin (complex a))
    (Math/asin a)))

(defmethod g/acos
  [numtype]
  [a]
  (if (> (g/abs a) 1)
    (g/acos (complex a))
    (Math/acos a)))

(defmethod g/sqrt
  [numtype]
  [a]
  (if (< a 0)
    (g/sqrt (complex a))
    (compute-sqrt a)))

(defmethod g/log
  [numtype]
  [a]
  (if (< a 0)
    (g/log (complex a))
    (Math/log a)))

;; This section defines methods that act differently between clojurescript and
;; Clojure. The clojure methods are all slightly more refined based on Java's
;; type system.

#?(:cljs
   (do
     (define-unary-operation g/negative? neg?)
     (define-binary-operation g/quotient quot)
     (define-binary-operation g/remainder rem))

   :clj
   (do
     (defn ^:private exact-integer-divide
       [a b]
       {:pre [(core-zero? (mod a b))]}
       (core-div a b))

     ;; Operations defined only on integral types
     (let [integral-types [Long BigInt BigInteger]]
       (doseq [lhs integral-types
               rhs integral-types]
         (defmethod g/remainder [lhs rhs] [a b] (mod a b))
         (defmethod g/remainder [rhs lhs] [b a] (mod b a))
         (defmethod g/quotient [lhs rhs] [a b] (quot a b))
         (defmethod g/quotient [rhs lhs] [b a] (quot b a))
         (defmethod g/exact-divide [lhs rhs] [a b] (exact-integer-divide a b))
         (defmethod g/exact-divide [rhs lhs] [b a] (exact-integer-divide b a))))

     (defmethod g/exact-divide [Ratio Ratio] [a b] (core-div a b))
     (defmethod g/exact-divide [Ratio BigInt] [a b] (core-div a b))

     (defmethod g/negative? [Long] [a] (neg? a))
     (defmethod g/negative? [BigInt] [a] (neg? a))
     (defmethod g/negative? [BigInteger] [a] (neg? a))))
