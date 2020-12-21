;;
;; Copyright © 2020 Sam Ritchie.
;; This work is based on the Scmutils system of MIT/GNU Scheme:
;; Copyright © 2002 Massachusetts Institute of Technology
;;
;; This is free software;  you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or (at
;; your option) any later version.
;;
;; This software is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this code; if not, see <http://www.gnu.org/licenses/>.
;;

(ns sicmutils.abstract.function
  (:require [sicmutils.abstract.number :as an]
            [sicmutils.expression :as x]
            [sicmutils.function :as f]
            [sicmutils.generic :as g]
            [sicmutils.matrix :as m]
            [sicmutils.numsymb :as sym]
            [sicmutils.polynomial]
            [sicmutils.structure :as s]
            [sicmutils.util :as u]
            [sicmutils.value :as v]
            [sicmutils.calculus.derivative :as d])
  #?(:clj
     (:import [clojure.lang IFn])))

;; ## Abstract Function
;;
;; This namespace declares an abstract function type, along with the support
;; structure to process the scmutils domain/range language.

(declare literal-apply)

;; This derivation allows `::function` to take advantage of all generic
;; operations installed via [[sicmutils.function]].

(derive ::function ::v/function)

(defn ^:private sicm-set->exemplar
  "Convert a SICM-style set (e.g., Real or (UP Real Real)) to
  an exemplar (an instance of the relevant type)."
  [s]
  (cond
    (= s 'Real) 0

    (sequential? s)
    (let [[constructor & args] s]
      (case constructor
        X     (mapv sicm-set->exemplar args)
        UP    (apply s/up (map sicm-set->exemplar args))
        DOWN  (apply s/down (map sicm-set->exemplar args))
        UP*   (apply s/up (repeat (second args) (sicm-set->exemplar (first args))))
        DOWN* (apply s/down (repeat (second args) (sicm-set->exemplar (first args))))
        X*    (into [] (repeat (second args) (sicm-set->exemplar (first args))))))))

(defn sicm-signature->domain-range
  "Convert a SICM-style literal function signature (e.g.,
  '(-> Real (X Real Real)) ) to our 'exemplar' format."
  [[arrow domain range]]
  (when-not (and (= '-> arrow) domain range)
    (u/illegal (str "A SICM signature is of the form '(-> domain range), got: " arrow domain range)))
  [(let [d (sicm-set->exemplar domain)]
     (if (vector? d) d [d]))
   (sicm-set->exemplar range)])

(defrecord Function [name arity domain range]
  Object
  (toString [_] (str name))
  v/Value
  (zero? [_] false)
  (one? [_] false)
  (identity? [_] false)
  (zero-like [_] (fn [& _] (v/zero-like range)))
  (one-like [_] (fn [& _] (v/one-like range)))
  (identity-like [_]
    (let [meta {:arity arity :from :identity-like}]
      (with-meta identity meta)))
  (exact? [f] (f/compose v/exact? f))
  (numerical? [_] false)
  (freeze [_] (v/freeze name))
  (kind [_] ::function)

  #?@(:clj
      [IFn
       (invoke [this x] (literal-apply this [x]))
       (invoke [this x y] (literal-apply this [x y]))
       (invoke [this x y z] (literal-apply this [x y z]))
       (invoke [this w x y z] (literal-apply this [w x y z]))
       (applyTo [this xs] (literal-apply this xs))]

      :cljs
      [IFn
       (-invoke [this a]
                (literal-apply this [a]))
       (-invoke [this a b]
                (literal-apply this [a b]))
       (-invoke [this a b c]
                (literal-apply this [a b c]))
       (-invoke [this a b c d]
                (literal-apply this [a b c d]))
       (-invoke [this a b c d e]
                (literal-apply this [a b c d e]))
       (-invoke [this a b c d e f]
                (literal-apply this [a b c d e f]))
       (-invoke [this a b c d e f g]
                (literal-apply this [a b c d e f g]))
       (-invoke [this a b c d e f g h]
                (literal-apply this [a b c d e f g h]))
       (-invoke [this a b c d e f g h i]
                (literal-apply this [a b c d e f g h i]))
       (-invoke [this a b c d e f g h i j]
                (literal-apply this [a b c d e f g h i j]))
       (-invoke [this a b c d e f g h i j k]
                (literal-apply this [a b c d e f g h i j k]))
       (-invoke [this a b c d e f g h i j k l]
                (literal-apply this [a b c d e f g h i j k l]))
       (-invoke [this a b c d e f g h i j k l m]
                (literal-apply this [a b c d e f g h i j k l m]))
       (-invoke [this a b c d e f g h i j k l m n]
                (literal-apply this [a b c d e f g h i j k l m n]))
       (-invoke [this a b c d e f g h i j k l m n o]
                (literal-apply this [a b c d e f g h i j k l m n o]))
       (-invoke [this a b c d e f g h i j k l m n o p]
                (literal-apply this [a b c d e f g h i j k l m n o p]))
       (-invoke [this a b c d e f g h i j k l m n o p q]
                (literal-apply this [a b c d e f g h i j k l m n o p q]))
       (-invoke [this a b c d e f g h i j k l m n o p q r]
                (literal-apply this [a b c d e f g h i j k l m n o p q r]))
       (-invoke [this a b c d e f g h i j k l m n o p q r s]
                (literal-apply this [a b c d e f g h i j k l m n o p q r s]))
       (-invoke [this a b c d e f g h i j k l m n o p q r s t]
                (literal-apply this [a b c d e f g h i j k l m n o p q r s t]))
       (-invoke [this a b c d e f g h i j k l m n o p q r s t rest]
                (literal-apply this (concat [a b c d e f g h i j k l m n o p q r s t]  rest)))]))

(defn literal-function
  ([f] (->Function f [:exactly 1] [0] 0))
  ([f signature]
   (let [[domain range] (sicm-signature->domain-range signature)]
     (literal-function f domain range)))
  ([f domain range]
   (cond (number? range)
         (let [arity (if (vector? domain)
                       (count domain)
                       1)]
           (->Function f [:exactly arity]
                       (if (vector? domain) domain [domain])
                       range))

         (s/structure? range)
         (let [n           (count range)
               orientation (s/orientation range)
               template    (s/literal f n orientation)]

           (s/mapr #(literal-function %1 domain %2)
                   template
                   range))
         :else
         (u/illegal (str "WTF range" range)))))

(defmacro with-literal-functions [litfns & body]
  `(let ~(vec (interleave
               (map (fn [s]
                      (if (symbol? s) s (first s)))
                    litfns)
               (map (fn [s]
                      (cond (symbol? s)
                            `(literal-function (quote ~s))
                            (and (sequential? s)
                                 (= (count s) 3))
                            `(literal-function (quote ~(first s))
                                               ~(second s)
                                               ~(nth s 2))
                            :else (u/illegal (str "unknown literal function type" s))))
                    litfns)))
     ~@body))

;; ## Differentiation of literal functions

(defn symbolic-derivative? [expr]
  (and (sequential? expr)
       ;; XXX GJS uses 'derivative here; should we? doesn't he just
       ;; have to change it back to D when printing?
       (= (first expr) d/derivative-symbol)))

(defn iterated-symbolic-derivative? [expr]
  (and (sequential? expr)
       (sequential? (first expr))
       (sym/expt? (first expr))
       (= (second (first expr)) d/derivative-symbol)))

(defn symbolic-increase-derivative [expr]
  (let [expt (sym/symbolic-operator 'expt)]
    (cond (symbolic-derivative? expr)
          (list (expt d/derivative-symbol 2) (fnext expr))
          (iterated-symbolic-derivative? expr)
          (list (expt d/derivative-symbol
                      (+ (first (nnext (first expr)))
                         1))
                (fnext expr))
          :else
          (list d/derivative-symbol expr))))

(defn- make-partials [f v]
  ;; GJS calls this function (the loop below) "fd"; we have no idea
  ;; what that stands for or what
  ;; is being attempted here
  (letfn [(fd [indices vv]
            (cond (s/structure? vv)
                  (s/same vv (map-indexed (fn [i element]
                                            (fd (conj indices i) element))
                                          vv))
                  (or (v/numerical? vv)
                      (x/abstract? vv))
                  (let [fexp (if (= (:arity f) [:exactly 1])  ; univariate
                               (if (= (first indices) 0)
                                 (if (= (count indices) 1)
                                   (symbolic-increase-derivative (:name f))
                                   `((~'partial ~@(next indices)) ~(:name f)))
                                 (u/illegal "wrong indices"))
                               `((~'partial ~@indices) ~(:name f)))]
                    (->Function fexp (:arity f) (:domain f) (:range f)))
                  :else
                  (u/illegal (str "make-partials WTF " vv))))]
    (fd [] v)))

(defn- literal-derivative [f xs]
  (let [v (m/seq-> xs)
        maxtag (->> v flatten d/max-order-tag)
        ve (->> v (s/mapr #(d/without-tag maxtag %)) seq)
        dv (->> v (s/mapr #(d/with-tag maxtag %)))]
    (d/canonicalize
     (d/d:+ (apply f ve)
            (reduce d/d:+ (map (fn [partialx dx]
                                 (d/d:* (apply partialx ve) dx))
                               (flatten (make-partials f v))
                               (flatten dv)))))))

(defn- check-argument-type
  "Check that the argument provided at index i has the same type as
  the exemplar expected."
  [f provided expected indexes]
  (cond (number? expected)
        (when-not (v/numerical? provided)
          (u/illegal (str "expected numerical quantity in argument " indexes
                          " of function call " f
                          " but got " provided)))
        (s/structure? expected)
        (do (when-not (and (or (s/structure? provided) (sequential? provided))
                           (= (s/orientation provided) (s/orientation expected))
                           (= (count provided) (count expected)))
              (u/illegal (str "expected structure matching " expected
                              " but got " provided )))
            (doseq [[provided expected sub-index] (map list provided expected (range))]
              (check-argument-type f provided expected (conj indexes sub-index))))
        (keyword? expected) ;; a keyword has to match the argument's kind
        (when-not (= (v/kind provided) expected)
          (u/illegal (str "expected argument of type " expected " but got " (v/kind provided)
                          " in call to function " f)))

        :else (u/illegal (str "unexpected argument example. got " provided " want " expected))))

(defn- literal-apply [f xs]
  (check-argument-type f xs (:domain f) [0])
  (if (some d/differential? xs)
    (literal-derivative f xs)
    (an/literal-number `(~(:name f) ~@(map v/freeze xs)))))

;; ## Specific Generics
;;
;; We can install one more method - [[sicmutils.generic/simplify]] simplifies
;; the attached name, but does not return its own function.

(defmethod g/simplify [Function] [a] (g/simplify (:name a)))
