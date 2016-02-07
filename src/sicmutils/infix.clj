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
  {'∂ 1, 'D 1, :apply 2, '/ 5, '* 5, '+ 6, '- 6})

(defn ^:private precedence
  [op]
  (or (precedence-map op)
      (cond (seq? op) (precedence-map :apply)
            (symbol? op) 2
            :else 99)))

(def ^:private infix-operators #{'* '+ '- '/})

(defn ^:private higher-precedence
  [a b]
  (< (precedence a) (precedence b)))

(defn ^:private parenthesize-if
  [b x]
  (if b (str "(" x ")") x))

(defn make-renderer
  [& {:keys [juxtapose-multiply special-handlers]
      :or {special-handlers {}}}]
  (letfn [(render-node [n]
            (if (z/branch? n)
              ;; then the first child is the function and the rest are the
              ;; arguments.
              (let [fn-loc (-> n z/next)
                    arg-loc (loop [a (-> n z/next z/right)]
                              (let [a' (z/replace a (render-node a))]
                                (if-let [r (z/right a')]
                                  (recur r)
                                  (z/up a'))))
                    [op & args] (z/node arg-loc)
                    upper-op (and (z/up arg-loc)
                                  (-> arg-loc z/leftmost z/node))]
                (if (infix-operators op)
                  (parenthesize-if
                   (and (infix-operators upper-op)
                        (higher-precedence upper-op op))
                   (s/join (if (and (= op '*) juxtapose-multiply)
                             " "
                             (str " " op " "))
                           args))
                  (or (and (special-handlers op)
                           ((special-handlers op) args))
                      (str (parenthesize-if (and (z/branch? fn-loc)
                                                 (higher-precedence :apply (z/node (z/next fn-loc))))
                                            (render-node (z/next arg-loc)))
                           (parenthesize-if (or (not (higher-precedence op :apply))
                                                (> (count args) 1)
                                                (z/branch? (z/right fn-loc)))
                                            (s/join ", " args))))))

              ;; primitive case
              (z/node n)))]
    #(-> % z/seq-zip render-node)))

(def ^:private decimal-superscripts [\⁰ \¹ \² \³ \⁴ \⁵ \⁶ \⁷ \⁸ \⁹])
(def ^:private decimal-subscripts [\₀ \₁ \₂ \₃ \₄ \₅ \₆ \₇ \₈ \₉])

(defn ^:private n->script
  [n scripts]
  (apply str (map #(-> % (Character/digit 10) scripts)
                  (str n))))

(def ^:private n->subscript #(n->script % decimal-subscripts))
(def ^:private n->superscript #(n->script % decimal-superscripts))

(def ->infix
  (make-renderer
   :juxtapose-multiply true
   :special-handlers
   {'expt (fn [[x e]]
            (when (and (integer? e) ((complement neg?) e))
              (str x (n->superscript e))))
    '∂ (fn [ds]
         (when (every? #(and (integer? %) (>= % 0)) ds)
           (str "∂" (s/join "," (map n->subscript ds)))))}))
