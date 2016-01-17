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

(ns net.littleredcomputer.math.operator
  (:require [net.littleredcomputer.math
             [value :as v]
             [generic :as g]])
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
  (Operator. o 1 name))

;; XXX needed?
(defn operator?
  [x]
  (instance? Operator x))

(def ^:private identity-operator
  (Operator. (fn [f] #(apply f %&)) 1 'identity))

(defn- number->operator
  [n]
  (Operator. (fn [f] #(g/* n (apply f %&))) 1 'number))

(defn- sub
  [o p]
  (Operator. (fn [f] #(g/- (apply (o f) %&) (apply (p f) %&))) 2 'sub))

(defn- add
  [o p]
  (Operator. (fn [f] #(g/+ (apply (o f) %&) (apply (p f) %&))) 2 'add))

;; multiplication of operators is treated like composition.
(defn- mul
  [o p]
  (Operator. (fn [f] #(apply (o (p f)) %&)) 2 'mul))

(defmethod g/expt
  [::operator Number]
  [o n]
  {:pre [(integer? n)
         (not (neg? n))]}
  (loop [e identity-operator
         n n]
    (if (= n 0) e (recur (mul e o) (dec n)))))

;; When arithmetically combined with operators, a number is
;; treated as an operator that multiplies its input by the
;; number.
(defmethod g/add [::operator :net.littleredcomputer.math.expression/numerical-expression]
  [o n]
  (add o (number->operator n)))

(defmethod g/sub
  [::operator :net.littleredcomputer.math.expression/numerical-expression]
  [o n]
  (sub o (number->operator n)))

(defmethod g/mul [::operator ::operator] [o p] (mul o p))
(defmethod g/add [::operator ::operator] [o p] (add o p))
(defmethod g/square ::operator [o] (mul o o))
(defmethod g/simplify ::operator [o] (:name o))

;; XXX: we need a bunch more of these, of course.
