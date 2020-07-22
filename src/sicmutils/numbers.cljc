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
  "Implementations of the unoptimized generic operations for the numeric types
  available on the executing platform."
  (:refer-clojure :rename {zero? core-zero?
                           / core-div
                           + core-plus
                           - core-minus
                           * core-times}
                  #?@(:cljs [:exclude [zero? / + - *]]))
  (:require [sicmutils.complex :refer [complex]]
            [sicmutils.generic :as g]
            [sicmutils.util :as u]
            [sicmutils.value :as v])
  #?(:clj
     (:import (clojure.lang BigInt Ratio))))

#?(:cljs (def ^:private ratio? (constantly false)))



(defn ^:private define-unary-operation
  [generic-operation core-operation]
  (defmethod generic-operation [u/numtype] [a]
    (core-operation a)))

(defn ^:private define-binary-operation
  [generic-operation core-operation]
  (defmethod generic-operation [u/numtype u/numtype] [a b]
    (core-operation a b)))

(comment
  ;; As reference documentation, these are the implementations that one would
  ;; provide for the generic operations if there were no simplifications available.
  ;;
  ;; Instead, these implementations for numbers are provided by
  ;; `sicmutils.numsymb`. This allows us to apply simplifications inside each
  ;; operation as it's evaluated.
  (define-unary-operation g/sin #(Math/sin %))
  (define-unary-operation g/cos #(Math/cos %))
  (define-unary-operation g/tan #(Math/tan %)))

;; these overrides are here because these operations have no optimizations
;; associated. If you DO want to optimize for these data types, replace these
;; implementations with better functions.
(define-binary-operation g/add #?(:clj +' :cljs core-plus))
(define-binary-operation g/mul #?(:clj *' :cljs core-times))
(define-binary-operation g/sub #?(:clj -' :cljs core-minus))
(define-unary-operation g/negate core-minus)
(define-binary-operation g/div core-div)
(define-unary-operation g/invert core-div)
(define-binary-operation g/expt u/compute-expt)
(define-unary-operation g/abs u/compute-abs)
(define-unary-operation g/magnitude u/compute-abs)

;; trig operations
(define-unary-operation g/atan #(Math/atan %))
(define-binary-operation g/atan #(Math/atan2 % %2))

;; Operations which allow promotion to complex numbers when their
;; arguments would otherwise result in a NaN if computed on the real
;; line

(defmethod g/asin
  [u/numtype]
  [a]
  (if (> (g/abs a) 1)
    (g/asin (complex a))
    (Math/asin a)))

(defmethod g/acos
  [u/numtype]
  [a]
  (if (> (g/abs a) 1)
    (g/acos (complex a))
    (Math/acos a)))

(defmethod g/sqrt
  [u/numtype]
  [a]
  (cond (neg? a) (g/sqrt (complex a))
        (v/nullity? a) a
        (v/unity? a) (v/one-like a)
        :else (u/compute-sqrt a)))

;; Implementation that converts to complex when negative, and also attempts to
;; remain exact if possible.
(defmethod g/log
  [u/numtype]
  [a]
  (cond (neg? a) (g/log (complex a))
        (v/unity? a) (v/zero-like a)
        :else (Math/log a)))

(defmethod g/exp
  [u/numtype]
  [a]
  (if (v/nullity? a)
    (v/one-like a)
    (Math/exp a)))

;; This section defines methods that act differently between clojurescript and
;; Clojure. The clojure methods are all slightly more refined based on Java's
;; type system.
#?(:cljs
   (do
     ;; These are potentially ill-formed, since these don't make sense for
     ;; floats.
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
