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

(ns sicmutils.value
  (:refer-clojure :rename {zero? core-zero?})
  (:import (clojure.lang Keyword Symbol)))

(defprotocol Value
  (kind [this]))

(declare primitive-kind)

(extend-type Object
  Value
  (kind [o] (primitive-kind o)))

(extend-type nil
  Value
  (kind [_] nil))

;; we record arities as a vector with an initial keyword:
;;   [:exactly m]
;;   [:between m n]
;;   [:at-least m]

#_(defn TODO-replace-me-arity
  "Return the cached or obvious arity of the object if we know it.
  Otherwise delegate to the heavy duty reflection, if we have to."
  [f]
  (or (:arity f)
      (:arity (meta f))
      (cond (symbol? f) [:exactly 0]
            ;; If f is a multifunction, then we expect that it has a multimethod
            ;; responding to the argument :arity, which returns the arity.
            (instance? MultiFn f) (f :arity)
            (fn? f) (reflect-on-arity f)
            ;; Faute de mieux, we assume the function is unary. Most math functions are.
            :else [:exactly 1])))



(defn ^:private primitive-kind
  [a]
  (cond
    (fn? a) ::function
    :else (or (:type a)
              (type a))))

(defn argument-kind
  [& args]
  (if (and (= 1 (count args))
           (keyword? (first args)))
    (first args)
    (mapv kind args)))

(def machine-epsilon
  (loop [e 1.0]
    (if (not= 1.0 (+ 1.0 (/ e 2.0)))
      (recur (/ e 2.0))
      e)))

(defn within
  "Returns a function that tests whether two values are within ε of each other."
  [^double ε]
  (fn [^double x ^double y] (< (Math/abs (- x y)) ε)))

(def twopi (* 2 Math/PI))

(defn principal-value
  [cuthigh]
  (let [cutlow (- cuthigh twopi)]
    (fn [x]
      (if (and (<= cutlow x) (< x cuthigh))
        x
        (let [y (- x (* twopi (Math/floor (/ x twopi))))]
          (if (< y cuthigh)
            y
            (- y twopi)))))))
