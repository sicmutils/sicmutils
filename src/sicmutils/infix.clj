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

(def ^:private precedence-map
  {'D 1, :apply 2, '/ 5, '* 5, '+ 6, '- 6})

(defn ^:private precedence
  [op]
  (or (precedence-map op)
      (cond (seq? op) (recur (first op))
            (symbol? op) 2
            :else 99)))

(def ^:private infix-operators #{'* '+ '- '/})

(defn ^:private higher-precedence
  [a b]
  (< (precedence a) (precedence b)))

(defn ^:private parenthesize
  [x]
  (str "(" x ")"))

(defn ^:private parenthesize-if
  [b x]
  (if b (parenthesize x) x))

(defn ^:private make-renderer
  [options]
  (letfn [(render-node [n]
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
                    args (rest node)
                    upper-op (and (z/up loc)
                                  (-> loc z/leftmost z/node))]
                (if (infix-operators op)
                  (parenthesize-if
                   (and (infix-operators upper-op)
                        (higher-precedence upper-op op))
                   (s/join (if (= op '*) " " (str " " op " ")) args))
                  (if (and (= op 'expt)
                           (= 2 (second args)))
                    (str (first args) "²")
                    (let [r-op (render-node (z/next loc))]
                      (str (parenthesize-if (higher-precedence :apply op) r-op)
                           (parenthesize (s/join ", " args)))))))

              ;; primitive case
              (z/node n)))]
    #(-> % z/seq-zip render-node)))

(def ->infix (make-renderer {}))
