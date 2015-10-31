;
; Copyright (C) 2015 Colin Smith.
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

(ns net.littleredcomputer.math.expression
  (:require [net.littleredcomputer.math.value :as v]))

(defrecord Expression [type expression]
  v/Value
  (nullity? [_] false)                                      ;; XXX what if it's a wrapped zero? one?
  (unity? [_] false)
  (zero-like [_] 0)
  (numerical? [x] (= (:type x) ::number))
  (exact? [_] false)
  (compound? [_] false)
  (freeze [x] (-> x :expression v/freeze))
  (kind [_] (if (= type ::number) ::numerical-expression ::expression)))

(defn make [x]
  (Expression. ::number x))

(defn literal-number
  [expression]
  (if (number? expression)
    expression
    (Expression. ::number expression)))

(defn abstract? [^Expression x]
  ;; TODO: GJS also allows for up, down, matrix here. We do not yet have
  ;; abstract structures.
  (= (:type x) ::number))

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
  symbol is an error. Function applications are applied. If things answering
  to number? are found, they are passed through the function lift-constant."
  ([environment lift-constant]
   (fn walk [x]
     (cond (number? x) (lift-constant x)
           (symbol? x) (if-let [binding (x environment)]
                         (if-not (fn? binding) binding x)
                         (throw (IllegalArgumentException.
                                 (str "no binding for " x " found."))))
           (instance? Expression x) (walk (expression-of x))
           (sequential? x) (let [f (environment (first x))]
                             (when-not (fn? f)
                               (throw (IllegalArgumentException.
                                       (str "no function binding for " x " found."))))
                             (apply f (map walk (rest x))))

           :else (throw (IllegalArgumentException. (str "unknown expression type " x))))))
  ([environment]
   (walk-expression environment identity)))
