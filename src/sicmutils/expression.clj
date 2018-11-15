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

(ns sicmutils.expression)

;; TODO: remove if-polymorphism... tricky, though, as this prevents lots of things
;; from lowering.
(defn literal-number
  [expression]
  (if (number? expression)
    expression
    {:type ::numerical-expression
     :expression expression}))

(defn fmap
  "Applies f to the expression part of e and creates from that an Expression otherwise like e,
  except we lower to symbol or numeric type when we may.
  XXX It is not certain that this is a good idea."
  [f e]
  (let [fe (f (:expression e))]
    (cond (symbol? fe) fe
          (number? fe) fe
          :else {:type (:type e)
                 :expression fe})))

(defn expression-of
  [expr]
  (:expression expr))

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
                     (sequential? x) (apply (functions (first x)) (map walk (next x)))
                     :else x))]
    (walk x)))
