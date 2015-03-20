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
  (:import (clojure.lang IFn)))

(defrecord Operator [f arity name]
  v/Value
  (sort-key [_] 45)
  (freeze [o] (.name o))
  (kind [_] ::operator)
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

(defmethod g/expt [::operator Number] [o n] (expt o n))

(g/defhandler :simplify [operator?] #(-> % :name g/simplify))

(println "operator initialized")
