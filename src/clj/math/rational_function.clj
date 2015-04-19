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

(ns math.rational-function
  (:require [math.generic :as g]
            [math.value :as v]
            [math.polynomial :as p])
  (:import [math.polynomial Polynomial]))

(defrecord RationalFunction [^long arity ^Polynomial p ^Polynomial q]
  v/Value
  (nullity? [p] false) ;; XXX
  (numerical? [_] false) ;; XXX
  (unity? [p] false)) ;; XXX

(defn make
  [p q]
  (let [arity (p/check-same-arity p q)]
    (RationalFunction. arity p q)))
