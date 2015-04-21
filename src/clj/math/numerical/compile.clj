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

(ns math.numerical.compile
  (:require [clojure.walk :refer [postwalk-replace]]
            [math.structure :as struct]
            [math.generic :as g]))

(def ^:private compiled-function-whitelist {'up `struct/up
                                            'down `struct/down
                                            'cos #(Math/cos %)
                                            'sin #(Math/sin %)
                                            'tan #(Math/tan %)
                                            '+ +
                                            '- -
                                            '* *
                                            '/ /
                                            'expt #(Math/pow %1 %2)
                                            'sqrt #(Math/sqrt %)})

(defn- construct-state-function-exp
  "Given a state model (a structure which is in the domain and range
  of the function) and its body, produce a function of the flattened
  form of the argument structure as a sequence.
  FIXME: give an example here, since nobody could figure out what's
  going on just by reading this"
  [state-model body]
  `(fn ~(-> state-model flatten vec vector)
     ~(postwalk-replace compiled-function-whitelist body)))

(defn compile-state-function
  [initial-state f]
  (let [generic-initial-state (struct/mapr (fn [_] (gensym)) initial-state)]
    (->> generic-initial-state
         f
         g/simplify
         (construct-state-function-exp generic-initial-state)
         eval)))

(defn- construct-univariate-function-exp
  [x body]
  `(fn [~x] ~(postwalk-replace compiled-function-whitelist body)))

(defn compile-univariate-function
  [f]
  (let [var (gensym)]
    (->> var
         f
         g/simplify
         (construct-univariate-function-exp var)
         eval)))
