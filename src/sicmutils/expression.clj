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

(ns sicmutils.expression
  (:require [sicmutils.value :as v]))

(defrecord Expression [type expression]
  Object
  (toString [_] (str expression))
  v/Value
  (nullity? [_] false)                                      ;; XXX what if it's a wrapped zero? one?
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
  which is an unwrapped expression."
  [x]
  (if (symbol? x) #{x}
      (->> x flatten (filter symbol?) (into #{}))))

(defn walk-expression
  "Walk the unwrapped expression x in postorder, replacing symbols found there
  with their values in the map environment, if present; an unbound
  symbol is an error. Function applications are applied."
  [environment]
  (fn walk [x]
    (cond (symbol? x) (if-let [binding (environment x)]
                        (if-not (= :sicmutils.value/function (v/kind binding)) binding x)
                        (throw (IllegalArgumentException.
                                (str "no binding for " x " found."))))
          (number? x) x
          (instance? Expression x) (walk (expression-of x))
          (sequential? x) (let [f (environment (first x))]
                            (when-not (= :sicmutils.value/function (v/kind f))
                              (throw (IllegalArgumentException.
                                      (str "no function binding for " x " found."))))
                            (apply f (map walk (rest x))))

          :else (throw (IllegalArgumentException. (str "unknown expression type " x))))))
