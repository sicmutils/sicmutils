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

(ns sicmutils.operator
  (:require [sicmutils
             [value :as v]
             [generic :as g]
             [structure :as s]])
  (:import (clojure.lang IFn)))

(defrecord Operator [o arity name]
  v/Value
  (freeze [_] name)
  (kind [_] ::operator)
  (nullity? [_] false)
  (unity? [_] false)
  (arity [_] arity)
  IFn
  (invoke [_ f] (o f))
  (invoke [_ f g] (o f g))
  (applyTo [_ fns] (apply o fns)))

(defn make-operator
  [o name]
  (Operator. o [:exactly 1] name))

(defn operator?
  [x]
  (instance? Operator x))

(def identity-operator
  (Operator. (fn [f] #(apply f %&)) [:at-least 0] 'identity))

(defn- number->operator
  [n]
  (Operator. (fn [f] (with-meta
                       #(g/* n (apply f %&))
                       {:arity (v/arity f)}))
             [:at-least 0]
             'number))

(defn ^:private function->operator
  [f]
  (Operator. (fn [g] (with-meta
                       #(g/* (apply f %&) (apply g %&))
                       {:arity (v/arity g)}))
             (v/arity f)
             'fn))

(defn- sub
  [o p]
  (Operator. (fn [f] (with-meta
                       #(g/- (apply (o f) %&) (apply (p f) %&))
                       {:arity (v/arity f)}))
             (v/joint-arity [(v/arity o) (v/arity p)])
             'sub))

(defn- add
  [o p]
  (Operator. (fn [f] (with-meta
                       #(g/+ (apply (o f) %&) (apply (p f) %&))
                       {:arity (v/arity f)}))
             (v/joint-arity [(v/arity o) (v/arity p)])
             'add))

;; multiplication of operators is treated like composition.
(defn- mul
  "Multiplication of operators is defined as their composition"
  [o p]
  (Operator. (fn [f] (with-meta
                       #(apply (o (p f)) %&)
                       {:arity (v/arity f)}))
             (v/joint-arity [(v/arity o) (v/arity p)])
             'mul))

(defmethod g/expt
  [::operator Number]
  [o n]
  {:pre [(integer? n)
         (not (neg? n))]}
  (loop [e identity-operator
         n n]
    (if (= n 0) e (recur (mul e o) (dec n)))))


(defmethod g/add [::operator ::operator] [o p] (add o p))
;; In additive operation the value 1 is considered as the identity operator
(defmethod g/add [::operator :sicmutils.expression/numerical-expression]
  [o n]
  (add o (number->operator n)))
(defmethod g/add [:sicmutils.expression/numerical-expression ::operator]
  [n o]
  (add (number->operator n) o))
(defmethod g/add
  [::operator :sicmutils.function/function]
  [o f]
  (add o (function->operator f)))
(defmethod g/add
  [:sicmutils.function/function ::operator]
  [f o]
  (add (function->operator f) o))
;; the following map operations on structures
(defmethod g/add
  [:sicmutils.structure/structure :sicmutils.expression/numerical-expression]
  [s e]
  (s/map-struct #(g/add % e) s))
(defmethod g/add
  [:sicmutils.expression/numerical-expression :sicmutils.structure/structure]
  [e s]
  (s/map-struct (partial g/add e) s))
(defmethod g/add
  [:sicmutils.structure/structure :sicmutils.calculus.derivative/differential]
  [s d]
  (s/map-struct #(g/add % d) s))
(defmethod g/add
  [:sicmutils.calculus.derivative/differential :sicmutils.structure/structure]
  [d s]
  (s/map-struct (partial g/add d) s))

(defmethod g/sub [::operator ::operator] [o p] (sub o p))
(defmethod g/sub
  [::operator :sicmutils.expression/numerical-expression]
  [o n]
  (sub o (number->operator n)))
(defmethod g/sub
  [:sicmutils.expression/numerical-expression ::operator]
  [n o]
  (sub (number->operator n) o))
(defmethod g/sub
  [::operator :sicmutils.function/function]
  [o f]
  (sub o (function->operator f)))
(defmethod g/sub
  [:sicmutils.function/function ::operator]
  [f o]
  (sub (function->operator f) o))
;; the following map operations on structures
(defmethod g/sub
  [:sicmutils.structure/structure :sicmutils.expression/numerical-expression]
  [s e]
  (s/map-struct #(g/sub % e) s) )
(defmethod g/sub
  [:sicmutils.expression/numerical-expression :sicmutils.structure/structure]
  [e s]
  (s/map-struct (partial g/sub e) s))
(defmethod g/sub
  [:sicmutils.structure/structure :sicmutils.calculus.derivative/differential]
  [s d]
  (s/map-struct #(g/sub % d) s))
(defmethod g/sub
  [:sicmutils.calculus.derivative/differential :sicmutils.structure/structure]
  [d s]
  (s/map-struct (partial g/sub d) s))

;; Multiplication of operators is defined as their application (see mul, above)
(defmethod g/mul [::operator ::operator] [o p] (mul o p))
;; When multiplied with operators, a number is treated as an operator 
;; that multiplies its input by the number.
(defmethod g/mul
  [::operator :sicmutils.function/function]
  [o f]
  (mul o (function->operator f)))
(defmethod g/mul
  [:sicmutils.function/function ::operator]
  [f o]
  (mul (function->operator f) o))
(defmethod g/mul
  [::operator :sicmutils.expression/numerical-expression]
  [o n]
  (mul o (number->operator n)))
(defmethod g/mul
  [:sicmutils.expression/numerical-expression ::operator]
  [n o]
  (mul o (number->operator n)))

(defmethod g/negate ::operator [o] (g/mul -1 o))

(defmethod g/square ::operator [o] (mul o o))

(defmethod g/simplify ::operator [o] (:name o))

(defmethod g/transpose
  ::operator
  [o]
  (Operator. (fn [f] #(g/transpose (apply (o f) %&))) 1 'transpose))

(defmethod g/cross-product
  [::operator ::operator]
  [o p]
  (fn [f]
    #(g/cross-product (apply (o f) %&) (apply (p f) %&))))


;; XXX: we need a bunch more of these, of course.
