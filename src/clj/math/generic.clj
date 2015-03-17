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

(ns math.generic
  (:refer-clojure :rename {+ core-+
                           - core--
                           / core-div
                           * core-*
                           zero? core-zero?})
  (:require [math.value :as v]
            [math.expression :as x])
  (:import [math.expression Expression]))

;;; classifiers
(defn zero?
  [x]
  (cond (number? x) (core-zero? x)
        (vector? x) (every? zero? x)
        :else (v/nullity? x)))

(defn one?
  [x]
  (or (and (number? x) (== x 1))
      (v/unity? x)))

(defn literal-number?
  [x]
  (and (instance? Expression x) (v/numerical? x)))

(defn abstract-number?
  [x]
  (or (symbol? x) (literal-number? x)))

(defn abstract-quantity?
  [x]
  (and (instance? Expression x)
       (x/abstract? x)))

(defn numerical-quantity?
  [x]
  (or (number? x)
      (abstract-number? x)
      (v/numerical? x)))

(defn scalar? [s]
  (or (numerical-quantity? s)
      (not (or (ifn? s)
               (v/compound? s)
               ))))

;; or how about something like
;; (assoc (assoc-in dtree (mapcat (fn [x] [:step x]) p))

(def empty-dtree {:steps {} :stop nil})
(defonce ^:private the-operator-table (atom {}))

(defn dtree-insert [{:keys [steps stop] :as dtree} op [predicate & predicates]]
  (if predicate
    ;; if there is another predicate, the next dtree is either the one
    ;; that governs this predicate at this stage, or a new empty one.
    (let [next-dtree (or (steps predicate) empty-dtree)]
      ;; augment the binding at this level
      (assoc dtree :steps
             (assoc steps predicate (dtree-insert next-dtree op predicates))))
    ;; no more predicates? store the current stop function.
    (do
      (if stop (prn "overwriting a binding!!" stop op dtree))
      (assoc dtree :stop op))))

(defn dtree-lookup [{:keys [steps stop]} operator [& arguments]]
  (if (some? arguments)
    ;; take a step: that means finding a predicate that matches at
    ;; this step and seeing if the subordinate dtree also matches. The
    ;; first step that matches this pair of conditions is chosen.
    (let [candidate-functions (filter identity
                                    (map (fn [[step dtree]]
                                           (and (step (first arguments))
                                                (dtree-lookup dtree operator (next arguments)))) steps))
          candidate-fnset (set candidate-functions)]
      #_(if (> (count candidate-fnset) 1)
        (prn "more than one choice for" operator arguments))
      (first candidate-functions))
    ;; otherwise we stop here.
    stop))

(defn defhandler [operator predicates f]
  (swap! the-operator-table
         (fn [operator-table]
           (let [dtree (get operator-table operator empty-dtree)]
             (assoc operator-table operator
                    (dtree-insert dtree f predicates))))))

(defn findhandler [operator arguments]
  (if-let [dtree (@the-operator-table operator)]
    (dtree-lookup dtree operator arguments)))

(defn make-operation [operator arity]
  (with-meta (fn [& args]
               (if-let [h (findhandler operator args)]
                 (apply h args)
                 (throw (IllegalArgumentException.
                         (str "no variant of " operator
                              " will work for " args "\n" (count args) "\n" (apply list (map type args)) "\n" )))))
    {:arity arity}))

(def ^:private mul (make-operation :* 2))
(def ^:private add (make-operation :+ 2))
(def ^:private sub (make-operation :- 2))
(def ^:private div (make-operation :div 2))

(def expt (make-operation :** 2))
(def negate (make-operation :negate 1))
(def invert (make-operation :invert 1))
(def sin (make-operation :sin 1))
(def cos (make-operation :cos 1))
(def tan (make-operation :tan 1))
(def square (make-operation :square 1))
(def cube (make-operation :cube 1))
(def abs (make-operation :abs 1))
(def sqrt (make-operation :sqrt 1))
(def exp (make-operation :exp 1 ))
(def log (make-operation :log 1))
(def partial-derivative (make-operation :∂ 2))
(def simplify (make-operation :simplify 1))

(defn- sort-key
  [x]
  ;; WARNING: the second term of the seq is a temporary idea
  ;; that we aren't sure we want
  (cond (symbol? x) [11 x]
        (number? x) [10 x]
        :else [(v/sort-key x) 0]))

(defn canonical-order [args]
  ;; NB: we are relying on the fact that this sort is stable, although
  ;; the Clojure documentation does not explicity guarantee this
  (sort-by sort-key args))

(defn- bin+ [a b]
  (cond (and (number? a) (number? b)) (core-+ a b)
        (zero? a) b
        (zero? b) a
        :else (add a b))
  )

(defn + [& args]
  (reduce bin+ 0 (canonical-order args)))

(defn- bin- [a b]
  (cond (and (number? a) (number? b)) (core-- a b)
        (zero? b) a
        (zero? a) (negate b)
        :else (sub a b)))

(defn - [arg & args]
  (cond (nil? arg) 0
        (nil? args) (negate arg)
        :else (bin- arg (->> args canonical-order (reduce bin+)))))

(defn- bin* [a b]
  (cond (and (number? a) (number? b)) (core-* a b)
        (and (number? a) (zero? a)) (v/zero-like b)
        (and (number? b) (zero? b)) (v/zero-like a)
        (one? a) b
        (one? b) a
        :else (mul a b)))

;;; In bin* we test for exact (numerical) zero
;;; because it is possible to produce a wrong-type
;;; zero here, as follows:
;;;
;;;               |0|             |0|
;;;       |a b c| |0|   |0|       |0|
;;;       |d e f| |0| = |0|, not  |0|
;;;
;;; We are less worried about the zero? below,
;;; because any invertible matrix is square.

(defn * [& args]
  (reduce bin* 1 (canonical-order args)))

(defn- bin-div [a b]
  (cond (and (number? a) (number? b)) (core-div a b)
        (one? b) a
        :else (div a b)))

(defn / [arg & args]
  (cond (nil? arg) 1
        (nil? args) (invert arg)
        :else (bin-div arg (->> args canonical-order (reduce bin*)))))

(def divide /)

(println "generic initialized")
