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

(ns math.function
  (:require [math.value :as v]
            [math.expression :as x]
            [math.structure :as s]
            [math.numsymb :as ns]
            [math.calculus.derivative :as d]
            [math.generic :as g])
  (:import [math.structure Struct]
           [math.operator Operator]
           (clojure.lang IFn)))

(declare literal-apply)

(defrecord Function [expr arity domain range]
  v/Value
  (nullity? [_] false)
  (compound? [_] false)
  (unity? [_] false)
  (numerical? [_] false)
  (sort-key [_] 35)
  (freeze [f] (-> f :expr v/freeze))
  (arity [_] arity)
  (kind [_] ::function)
  IFn
  (invoke [f x] (literal-apply f [x]))
  (applyTo [f xs] (literal-apply f xs))
  )

(defn literal-function [f] (Function. f 1 [:real] :real))
(def ^:private derivative-symbol 'D)

;; --------------------
;; Algebra of functions
;;

(defn- function?
  [x]
  (and (ifn? x)
       (not (instance? Struct x))
       (not (instance? Operator x))
       (not (symbol? x))
       (not (vector? x))))

;; XXX needed?
(defn- cofunction?
  "True if f may be combined with a function."
  [f]
  (not (instance? Operator f)))

(defn- unary-operation
  "For a unary operator (like sqrt), returns a function of one function
  which when called will apply the operation to the result of the
  original function (so that ((unary-operation sqrt) f) x) will return
  (sqrt (f x))."
  [operator]
  (with-meta (partial comp operator) {:arity 1}))

(defn- binary-operation
  "For a given binary operator (like +), returns a function of two
  functions which will produce the pointwise operation of the results
  of applying the two functions to the input. That
  is, (binary-operation +) applied to f and g will produce a function
  which computes (+ (f x) (g x)) given x as input."
  [operator]
  (with-meta (fn [f g]
               (let [f-numeric (g/numerical-quantity? f)
                     g-numeric (g/numerical-quantity? g)
                     f-arity (if f-numeric (v/arity g) (v/arity f))
                     g-arity (if g-numeric f-arity (v/arity g))
                     f1 (if f-numeric (constantly f) f)
                     g1 (if g-numeric (constantly g) g)]
                 (if (not= f-arity g-arity)
                   (throw (IllegalArgumentException. "cannot combine functions of differing arity"))
                   (with-meta (cond (= f-arity 1) #(operator (f1 %) (g1 %))
                                    (= f-arity 2) #(operator (f1 %1 %2) (g1 %1 %2))
                                    (= f-arity 3) #(operator (f1 %1 %2 %3) (g1 %1 %2 %3))
                                    (= f-arity 4) #(operator (f1 %1 %2 %3 %4) (g1 %1 %2 %3 %4))
                                    :else (throw (IllegalArgumentException. "unsupported arity for function arithmetic")))
                              {:arity f-arity}))))
    {:arity 2}))

(defmacro make-binary-operations
  "Given a sequence of alternating generic and binary operations,
  define the multimethod necessary to introduce this operation
  to function arguments."
  [& generic-and-binary-ops]
  `(do ~@(map (fn [[generic-op binary-op]]
                `(let [binop# (binary-operation ~binary-op)]
                   (doseq [signature# [[::function ::function]
                                       [::function ::cofunction]
                                       [::cofunction ::function]]]
                     (defmethod ~generic-op signature# [a# b#] (binop# a# b#)))
                   ))
              (partition 2 generic-and-binary-ops))))

(defmacro make-unary-operations
  [& generic-ops]
  `(do ~@(map (fn [generic-op]
                `(let [unop# (unary-operation ~generic-op)]
                   (defmethod ~generic-op ::function [a#] (unop# a#))))
              generic-ops)))

(make-binary-operations
 g/add g/+
 g/sub g/-
 g/mul g/*
 g/div g/divide
 g/expt g/expt)

(make-unary-operations
 g/negate g/invert g/sqrt g/sin g/cos g/tan g/square g/cube g/exp g/log)
;; TODO asin acos sinh cosh ...

(defmethod g/simplify Function [a] (-> a :expr g/simplify))
(derive :math.expression/numerical-expression ::cofunction)
(derive :math.structure/structure ::cofunction)
(derive :math.value/function ::function)

;; ------------------------------------
;; Differentiation of literal functions
;;

(defn symbolic-derivative?
  [expr]
  (and (sequential? expr)
       ;; XXX GJS uses 'derivative here; should we? doesn't he just
       ;; have to change it back to D when printing?
       (= (first expr) derivative-symbol)))

(defn iterated-symbolic-derivative?
  [expr]
  (and (sequential? expr)
       (sequential? (first expr))
       (ns/expt? (first expr))
       (= (second (first expr)) derivative-symbol)))

(defn symbolic-increase-derivative [expr]
  (cond (symbolic-derivative? expr)
        (list (ns/expt derivative-symbol 2) (first (next expr)))
        (iterated-symbolic-derivative? expr)
        (list (ns/expt derivative-symbol
                       (+ (first (next (next (first expr))))
                          1))
              (first (next expr)))
        :else
        (list derivative-symbol expr)))

(defn- make-partials
  [f v]
  ;; GJS calls this function (the loop below) "fd"; we have no idea
  ;; what that stands for or what
  ;; is being attempted here
  (letfn [(fd [indices vv]
            (cond (s/structure? vv)
                  (let [^Struct s vv]
                    (s/same s (map-indexed (fn [i element]
                                             (fd (conj indices i) element)) s)))
                  (or (g/numerical-quantity? vv)
                      (g/abstract-quantity? vv))
                  (let [fexp (if (= (:arity f) 1)  ; univariate
                               (if (= (first indices) 0)
                                 (if (= (count indices) 1)
                                   (symbolic-increase-derivative (:expr f))
                                   `((g/partial-derivative ~@(next indices))))
                                 (throw (IllegalArgumentException. "wrong indices")))
                               `((g/partial-derivative ~@indices) ~(:expr f)))]
                    (Function. fexp (:arity f) (:domain f) (:range f)))
                  :else
                  (throw (IllegalArgumentException. (str "make-partials WTF " vv)))))]
    (fd [] v)
    ))

(defn- literal-derivative
  [f xs]
  (let [v (s/seq-> xs)
        maxtag (->> v flatten d/max-order-tag)
        ve (->> v (s/mapr #(d/without-tag maxtag %)) seq)
        dv (->> v (s/mapr #(d/with-tag maxtag %)))]
    (d/canonicalize-differential
     (d/dx+dy (apply f ve)
              (reduce d/dx+dy (map (fn [partialx dx]
                                     (d/dx*dy (apply partialx ve) dx))
                                   (flatten (make-partials f v))
                                   (flatten dv)))))))

(defn- literal-apply
  [f xs]
  (if (some d/differential? xs)
    (literal-derivative f xs)
    (x/literal-number (list* (:expr f) xs)))) ;; XXX cons

;;; Utilities

(defn compose
  "Compose is like Clojure's standard comp, but for this system we
  like to know the arity of our functions, so that we can calculate
  their derivatives with structure, etc. The arity of a composition is
  simply the arity of its rightmost (that is, first to be applied)
  function term."
  [& fns]
  (let [a (v/arity (last fns))]
    (with-meta (apply comp fns) {:arity a})))

(defmacro with-literal-functions
  [litfns & body]
  `(let ~(vec (interleave litfns (map (fn [s] `(literal-function (quote ~s))) litfns)))
     ~@body))


(println "function initialized")
