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

(defrecord Operator [f name]
  v/Value
  (sort-key [_] 45)
  (freeze [o] (.name o))
  IFn
  (invoke [operator function]
    (let [operated-function ((:f operator) function)]
      (fn [& xs]
        (apply operated-function xs))))
  (applyTo [operator fns]
    (AFn/applyToHelper operator fns))
  )

(defn make-operator
  [f name]
  (Operator. f name))

(defn operator?
  [x]
  (instance? Operator x))

(g/defhandler :simplify [#(instance? Operator %)] #(-> % :name g/simplify))

(println "operator initialized")

