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

(ns math.operator
  (:require [math.value :as v]
            [math.generic :as g])
  (:import (clojure.lang IFn AFn)))

(defrecord Operator [f arity name]
  v/Value
  (freeze [o] (.name o))
  (kind [_] ::operator)
  (nullity? [_] false)
  (unity? [_] false)  ;; XXX check this
  IFn
  (invoke [operator function]
    ((:f operator) function)))

(defn make-operator
  [f name]
  (Operator. f 1 name))

;; XXX needed?
(defn operator?
  [x]
  (instance? Operator x))

(defn- expt
  [operator n]
  (if (= n 0) identity
              (fn [f] (operator ((expt operator (dec n)) f)))
              ; TODO: why can't we just write (operator (expt operator (dec n))) here?
              ))

(defn- number->operator
  [n]
  (Operator. (fn [f] #(g/* n (f %))) 1 "#"))

(defn- sub
  [o p]
  (Operator. (fn [f] #(g/- ((o f) %) ((p f) %))) 2 "sub"))

(defn- add
  [o p]
  (Operator. (fn [f] #(g/+ ((o f) %) ((p f) %))) 2 "add"))

;; multiplication of operators is treated like composition.
(defn- mul
  [o p]
  (Operator. (fn [f] #((o (p f)) %)) 2 "mul"))

(defmethod g/expt [::operator Number] [o n] (expt o n))
;; When arithmetically combined with operators, a number is
;; treated as an operator that multiplies its input by the
;; number.
(defmethod g/add [::operator Number] [o n] (add o (number->operator n)))
(defmethod g/sub [::operator Number] [o n] (sub o (number->operator n)))
(defmethod g/mul [::operator ::operator] [o p] (mul o p))
(defmethod g/simplify ::operator [o] (-> o :name g/simplify))
;; XXX: we need a bunch more of these, of course.

(println "operator initialized")
