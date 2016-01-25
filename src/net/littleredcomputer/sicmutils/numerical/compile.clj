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

(ns net.littleredcomputer.sicmutils.numerical.compile
  (:require [clojure.walk :refer [postwalk-replace]]
            [net.littleredcomputer.sicmutils
             [structure :as struct]
             [generic :as g]]
            [clojure.tools.logging :as log])
  (:import (com.google.common.base Stopwatch)))

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

(def ^:private compiled-function-cache (atom {}))

(defn- construct-state-function-exp
  "Given a state model (a structure which is in the domain and range
  of the function) and its body, produce a function of the flattened
  form of the argument structure as a sequence.
  FIXME: give an example here, since nobody could figure out what's
  going on just by reading this"
  [generic-parameters state-model body]
  `(fn [~(into [] (concat (-> state-model flatten) generic-parameters))]
     ~(postwalk-replace compiled-function-whitelist body)))

(defn- compile-state-function2
  [f parameters initial-state]
  (let [sw (Stopwatch/createStarted)
        generic-parameters (for [_ parameters] (gensym 'p))
        generic-initial-state (struct/mapr (fn [_] (gensym 'y)) initial-state)
        g (apply f generic-parameters)
        compiled-function (->> generic-initial-state
                               g
                               g/simplify
                               (construct-state-function-exp
                                 generic-parameters
                                 generic-initial-state)
                               eval)]
    (log/info "compiled state function in" (str sw))
    compiled-function))

(defn compile-state-function
  [f parameters initial-state]
  (if-let [cached (@compiled-function-cache f)]
    (do
      (log/info "compiled state function cache hit")
      cached)
    (let [compiled-function (compile-state-function2 f parameters initial-state)]
      (swap! compiled-function-cache assoc f compiled-function)
      compiled-function)))

(defn- construct-univariate-function-exp
  [x body]
  `(fn [~x] ~(postwalk-replace compiled-function-whitelist body)))

(defn- compile-univariate-function2
  [f]
  (let [sw (Stopwatch/createStarted)
        var (gensym 'x)
        compiled-function (->> var
                               f
                               g/simplify
                               (construct-univariate-function-exp var)
                               eval)]
    (log/info "compiled univariate function in" (str sw))
    compiled-function))

(defn compile-univariate-function
  [f]
  (if-let [cached (@compiled-function-cache f)]
    (do
      (log/info "compiled univariate function cache hit")
      cached)
    (let [compiled-function (compile-univariate-function2 f)]
      (swap! compiled-function-cache assoc f compiled-function)
      compiled-function)))
