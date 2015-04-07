;; Copyright (C) 2015 Colin Smith.
;; This work is based on the Scmutils system of MIT/GNU Scheme.
;;
;; This is free software;  you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or (at
;; your option) any later version.

;; This software is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this code; if not, see <http://www.gnu.org/licenses/>.

(ns math.numbers
  (:refer-clojure :rename {zero? core-zero?
                           + core-+
                           - core--
                           * core-*
                           / core-div})
  (:require [math.generic :as g]
            [math.numsymb :as ns]
            [clojure.math.numeric-tower :as nt]))

(defn- define-binary-operation
  [generic-operation core-operation]
  (defmethod generic-operation [Number Number] [a b] (core-operation a b)))

(defn define-unary-operation
  [generic-operation core-operation]
  (defmethod generic-operation Number [a] (core-operation a)))

(define-binary-operation g/add core-+)
(define-binary-operation g/sub core--)
(define-binary-operation g/mul core-*)
(define-binary-operation g/div core-div)
(define-binary-operation g/expt nt/expt)
(define-unary-operation g/negate core--)
(define-unary-operation g/invert core-div)
(define-unary-operation g/sin #(Math/sin %))
(define-unary-operation g/asin #(Math/asin %))
(define-unary-operation g/cos #(Math/cos %))
(define-unary-operation g/acos #(Math/acos %))
(define-unary-operation g/tan #(Math/tan %))
(define-unary-operation g/square #(core-* % %))
(define-unary-operation g/cube #(core-* % % %))
(define-unary-operation g/abs nt/abs)
(define-unary-operation g/sqrt nt/sqrt)
(define-unary-operation g/log #(Math/log %))
(define-unary-operation g/exp #(Math/exp %))
