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
            [sicmutils.value :as v]
            #?(:cljs goog.math.Integer)
            #?(:cljs goog.math.Long))
  #?(:clj
     (:import (clojure.lang BigInt Ratio))))

;; Smaller inheritance tree to enabled shared implementations between numeric
;; types that represent mathematical integers.
(derive u/inttype ::integral)
(derive u/longtype ::integral)

#?(:cljs
   (do (derive js/BigInt ::integral)
       (derive ::v/exact-number ::integral))

   :clj
   (do (derive BigInt ::integral)
       (derive BigInteger ::integral)))

(defmethod g/add [v/numtype v/numtype] [a b] (#?(:clj +' :cljs core-plus) a b))
(defmethod g/mul [v/numtype v/numtype] [a b] (#?(:clj *' :cljs core-times) a b))
(defmethod g/sub [v/numtype v/numtype] [a b] (#?(:clj -' :cljs core-minus) a b))
(defmethod g/negate [v/numtype] [a] (core-minus a))
(defmethod g/div [v/numtype v/numtype] [a b] (core-div a b))
(defmethod g/invert [v/numtype] [a] (core-div a))
(defmethod g/expt [v/numtype v/numtype] [a b] (u/compute-expt a b))
(defmethod g/abs [v/numtype] [a] (u/compute-abs a))
(defmethod g/magnitude [v/numtype] [a] (u/compute-abs a))

;; trig operations
(defmethod g/atan [v/numtype] [a] (Math/atan a))
(defmethod g/atan [v/numtype v/numtype] [a b] (Math/atan2 a b))

(comment
  ;; As reference documentation, these are the implementations that one would
  ;; provide for the generic operations if there were no simplifications available.
  ;;
  ;; Instead, these implementations for numbers are provided by
  ;; `sicmutils.numsymb`. This allows us to apply simplifications inside each
  ;; operation as it's evaluated.
  (defmethod g/sin [v/numtype] [a] (Math/sin a))
  (defmethod g/cos [v/numtype] [a] (Math/cos a))
  (defmethod g/tan [v/numtype] [a] (Math/tan a)))

;; Operations which allow promotion to complex numbers when their
;; arguments would otherwise result in a NaN if computed on the real
;; line

(defmethod g/asin
  [v/numtype]
  [a]
  (if (> (g/abs a) 1)
    (g/asin (complex a))
    (Math/asin a)))

(defmethod g/acos
  [v/numtype]
  [a]
  (if (> (g/abs a) 1)
    (g/acos (complex a))
    (Math/acos a)))

(defmethod g/sqrt
  [v/numtype]
  [a]
  (cond (neg? a) (g/sqrt (complex a))
        (v/nullity? a) a
        (v/unity? a) (v/one-like a)
        :else (u/compute-sqrt a)))

;; Implementation that converts to complex when negative, and also attempts to
;; remain exact if possible.
(defmethod g/log
  [v/numtype]
  [a]
  (cond (neg? a) (g/log (complex a))
        (v/unity? a) (v/zero-like a)
        :else (Math/log a)))

(defmethod g/exp
  [v/numtype]
  [a]
  (if (v/nullity? a)
    (v/one-like a)
    (Math/exp a)))

(defn ^:private exact-divide
  "Checked implementation of g/exact-divide general enough to use for any type
  that defines g/remainder and g/quotient."
  [a b]
  {:pre [(v/nullity? (g/remainder a b))]}
  (g/quotient a b))

(defmethod g/negative? [::integral] [a] (neg? a))
(defmethod g/exact-divide [::integral ::integral] [b a] (exact-divide b a))

;; This section defines methods that act differently between Clojurescript and
;; Clojure. The clojure methods are all slightly more refined based on Java's
;; type system.
#?(:cljs
   (do
     ;; These extend ::v/exact-number, which allows them to apply only to JS
     ;; numbers that are... exact.
     (defmethod g/negative? [::v/exact-number] [a] (neg? a))
     (defmethod g/quotient [::v/exact-number ::v/exact-number] [a b] (quot a b))
     (defmethod g/remainder [::v/exact-number ::v/exact-number] [a b] (rem a b))
     (defmethod g/modulo [::v/exact-number ::v/exact-number] [a b] (mod a b)))

   :clj
   (do
     ;; All JVM types that respond to ::integral behave correctly with Clojure's
     ;; native `quot`, `rem`, `mod`.
     (defmethod g/quotient [::integral ::integral] [b a] (quot b a))
     (defmethod g/remainder [::integral ::integral] [a b] (rem a b))
     (defmethod g/modulo [::integral ::integral] [a b] (mod a b))

     (defmethod g/exact-divide [Ratio Ratio] [a b] (core-div a b))
     (defmethod g/exact-divide [Ratio BigInt] [a b] (core-div a b))))

;; Clojurescript and Javascript have a number of numeric types available that
;; don't respond true to number? These each require their own block of method
;; implementations.
#?(:cljs
   (letfn [(goog-expt
             ;; Implementation of exponent taken from Clojure's numeric tower's
             ;; expt-int:
             ;; https://github.com/clojure/math.numeric-tower/blob/master/src/main/clojure/clojure/math/numeric_tower.clj#L72
             [base pow]
             (loop [n pow
                    y (v/one-like base)
                    z base]
               (let [t (not (.isOdd n))
                     n (.shiftRight n 1)]
                 (cond
                   t (recur n y (.multiply z z))
                   (v/nullity? n) (.multiply z y)
                   :else (recur n (.multiply z y) (.multiply z z))))))]

     ;; native BigInt type in JS.
     (defmethod g/add [js/BigInt js/BigInt] [a b] (core-plus a b))
     (defmethod g/mul [js/BigInt js/BigInt] [a b] (core-times a b))
     (defmethod g/sub [js/BigInt js/BigInt] [a b] (core-minus a b))
     (defmethod g/negate [js/BigInt] [a] (core-minus a))
     (defmethod g/expt [js/BigInt js/BigInt] [a b] (js* "~{} ** ~{}" a b))
     (defmethod g/abs [js/BigInt] [a] (if (neg? a) (core-minus a) a))
     (defmethod g/quotient [js/BigInt js/BigInt] [a b] (core-div a b))
     (defmethod g/remainder [js/BigInt js/BigInt] [a b] (js* "~{} % ~{}" a b))
     (defmethod g/magnitude [js/BigInt] [a b]
       (if (neg? a) (core-minus a) a))

     ;; Compatibility between numbers and bigint.
     (doseq [op [g/add g/mul g/sub g/expt g/remainder g/quotient]]
       (defmethod op [js/BigInt v/numtype] [a b]
         (op a (u/bigint b)))

       (defmethod op [v/numtype js/BigInt] [a b]
         (op (u/bigint a) b)))

     ;; Google Closure library's 64-bit Long and arbitrary-precision Integer
     ;; type.
     (doseq [goog-type [goog.math.Long goog.math.Integer]]
       (defmethod g/add [goog-type goog-type] [a b] (.add a b))
       (defmethod g/mul [goog-type goog-type] [a b] (.multiply a b))
       (defmethod g/sub [goog-type goog-type] [a b] (.subtract a b))
       (defmethod g/negate [goog-type] [a] (.negate a))
       (defmethod g/expt [goog-type goog-type] [a b] (goog-expt a b))
       (defmethod g/abs [goog-type] [a] (if (neg? a) (.negate a) a))
       (defmethod g/remainder [goog-type goog-type] [a b] (.modulo a b))
       (defmethod g/magnitude [goog-type] [a b] (if (neg? a) (.negate a) a))

       ;; Compatibility between basic number type and the google numeric types.
       ;; Any operation between a number and a Long or Integer will promote the
       (doseq [op [g/add g/mul g/sub g/expt g/remainder g/quotient]]
         (defmethod op [goog-type v/numtype] [a b]
           (op a (.fromNumber goog-type b)))

         (defmethod op [v/numtype goog-type] [a b]
           (op (.fromNumber goog-type a) b))))

     ;; These names are slightly different between the two types.
     (defmethod g/quotient [goog.math.Long goog.math.Long] [a b] (.div a b))
     (defmethod g/quotient [goog.math.Integer goog.math.Integer] [a b] (.divide a b))

     ;; TODO these are not defined on integral types, BUT we could get farther
     ;; if we could convert these to a BigDecimal, once we support those.
     (comment
       (defmethod g/div [js/BigInt js/BigInt] [a b] (core-div a b))
       (defmethod g/invert [js/BigInt] [a] (core-div a)))))
