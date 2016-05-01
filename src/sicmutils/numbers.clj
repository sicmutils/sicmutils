;
; Copyright (C) 2016 Colin Smith.
; This work is based on the Scmutils system of MIT/GNU Scheme.
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
  (:refer-clojure :rename {zero? core-zero? / core-div})
  (:require [sicmutils
             [generic :as g]
             [complex :refer [complex]]]
            [clojure.math.numeric-tower :as nt])
  (:import (clojure.lang BigInt Ratio)))

(defn define-unary-operation
  [generic-operation core-operation]
  (defmethod generic-operation Number [a] (core-operation a)))

(define-unary-operation g/sin #(Math/sin %))
(define-unary-operation g/cos #(Math/cos %))
(define-unary-operation g/tan #(Math/tan %))
(define-unary-operation g/exp #(Math/exp %))

;; operations which allow promotion to complex numbers when their
;; arguments would otherwise result in a NaN if computed on the real
;; line
(defmethod g/sqrt
  Number
  [a]
  (if (< a 0)
    (g/sqrt (complex a))
    (nt/sqrt a)))

(defmethod g/asin
  Number
  [a]
  (if (> (nt/abs a) 1)
    (g/asin (complex a))
    (Math/asin a)))

(defmethod g/acos
  Number
  [a]
  (if (> (nt/abs a) 1)
    (g/acos (complex a))
    (Math/acos a)))

(defmethod g/atan
  Number
  [a]
  (Math/atan a))

(defmethod g/atan
  [Number Number]
  [y x]
  (Math/atan2 y x))

(defmethod g/log
  Number
  [a]
  (if (< a 0)
    (g/log (complex a))
    (Math/log a)))

(defn ^:private exact-integer-divide
  [a b]
  {:pre [(core-zero? (mod a b))]}
  (core-div a b))

(let [integral-types [Long BigInt BigInteger]]
  (doseq [lhs integral-types
          rhs integral-types]
    (defmethod g/remainder [lhs rhs] [a b] (mod a b))
    (defmethod g/remainder [rhs lhs] [b a] (mod b a))
    (defmethod g/quotient [lhs rhs] [a b] (quot a b))
    (defmethod g/quotient [rhs lhs] [b a] (quot b a))
    (defmethod g/exact-div [lhs rhs] [a b] (exact-integer-divide a b))
    (defmethod g/exact-div [rhs lhs] [b a] (exact-integer-divide b a))))

(defmethod g/exact-div [Ratio Ratio] [a b] (core-div a b))
(defmethod g/exact-div [Ratio BigInt] [a b] (core-div a b))

(defmethod g/negative? Long [a] (neg? a))
(defmethod g/negative? BigInt [a] (neg? a))
(defmethod g/negative? BigInteger [a] (neg? a))
