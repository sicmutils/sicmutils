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

(ns sicmutils.expression
  (:require [sicmutils.value :as v]))

(defrecord Expression [type expression]
  Object
  (toString [_] (str expression))
  v/Value
  (nullity? [_] false)  ;; XXX what if it's a wrapped zero? one?
  (unity? [_] false)
  (zero-like [_] 0)
  (numerical? [_] (= type ::numerical-expression))
  (exact? [_] false)
  (freeze [_] (v/freeze expression))
  (kind [_] type))

(defn literal-number
  [expression]
  (if (number? expression)
    expression
    (Expression. ::numerical-expression expression)))

(defn fmap
  "Applies f to the expression part of e and creates from that an Expression otherwise like e."
  [f e]
  (->> e :expression f (->Expression (:type e))))

(defn abstract? [^Expression x]
  ;; TODO: GJS also allows for up, down, matrix here. We do not yet have
  ;; abstract structures.
  (= (:type x) ::numerical-expression))

(defn expression-of
  [expr]
  (cond (instance? Expression expr) (:expression expr)
        (symbol? expr) expr
        :else (throw (IllegalArgumentException. (str "unknown expression type:" expr)))))

(defn variables-in
  "Return the 'variables' (e.g. symbols) found in the expression x,
  which is an unwrapped expression, as a set"
  [x]
  (if (symbol? x) #{x}
                  (->> x flatten (filter symbol?) (into #{}))))

(defn walk-expression
  "Walk the unwrapped expression x in postorder, replacing symbols found there
  with their values in the map environment, if present; the functions association
  is used for elements in function application position (first of a sequence)."
  [x variables functions]
  (let [walk (fn walk [x]
               (cond (symbol? x) (or (variables x) x)
                     (sequential? x) (apply (functions (first x)) (map walk (rest x)))
                     :else x))]
    (walk x)))
