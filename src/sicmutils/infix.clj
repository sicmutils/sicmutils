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

(ns sicmutils.infix
  (:require [clojure.zip :as z]
            [clojure.string :as s]))

(defn ^:private precedence
  [op]
  (if (symbol? op)
    (condp = op
      '/ 5
      '* 5
      '+ 6
      '- 6
      1)
    1))

(defn ^:private infix-operator?
  [op]
  (or (= op '*)
      (= op '+)
      (= op '-)))

(defn ^:private higher-precedence
  [a b]
  (< (precedence a) (precedence b)))

(defn ^:private render-node [n]
  (if (z/branch? n)
    ;; then the first child is the function and the rest are the
    ;; arguments.
    (let [loc (loop [a (-> n z/next z/right)]
                (let [a' (z/replace a (render-node a))]
                  (if-let [r (z/right a')]
                    (recur r)
                    (z/up a'))))
          node (z/node loc)
          op (first node)
          args (rest node)]
      (if (infix-operator? op)
        (let [need-parens (and (z/up loc)
                               (let [upper-op (-> loc z/leftmost z/node)]
                                 (and
                                  (infix-operator? upper-op)
                                  (higher-precedence upper-op op))))
              base (s/join (if (= op '*) " " (str " " op " ")) args)]
         (if need-parens
           (str "(" base ")")
           base))
        (if (and (= op 'expt)
                 (= 2 (second args)))
          (str (first args) "²")
          (str op "(" (s/join ", " args) ")"))))

    ;; primitive case
    (z/node n)))

(defn ->infix [x]
  (-> x z/seq-zip render-node))
