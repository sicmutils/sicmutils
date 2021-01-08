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

(ns sicmutils.util.vector-set-test
  (:require [clojure.set :as cs]
            [clojure.test :refer [is deftest testing]]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]
             #?@(:cljs [:include-macros true])]
            [sicmutils.generators :as sg]
            [sicmutils.util.vector-set :as vs]))

(deftest vector-set-tests
  (checking "disj/conj acts as identity" 100
            [v (sg/vector-set gen/nat)
             x gen/nat]
            (if (vs/contains? v x)
              (is (= v (-> (vs/disj v x)
                           (vs/conj x))))
              (is (= v (-> (vs/conj v x)
                           (vs/disj x))))))

  (testing "conj for existing element throws"
    (is (thrown? #?(:clj IllegalArgumentException :cljs js/Error)
                 (vs/conj [1 2 3] 1))))

  (checking "disj, conj maintain sort" 100
            [v (sg/vector-set gen/nat)]
            (let [x   (inc' (apply max (or (seq v) [0])))
                  v+x (vs/conj v x)
                  v'  (vs/disj v+x x)]
              (is (not (vs/contains? v+x))
                  "check that the element greater than the max isn't present")

              (is (= v+x (sort v+x))
                  "conj maintains sort")

              (is (= v' (sort v'))
                  "disj maintains sort")

              (is (= (count v+x)
                     (inc (count v)))
                  "conj increases length")

              (is (= x (peek v+x))
                  "vector-set is a vector and new max elements go on the end.")))

  (checking "vs/intersection is correct" 100
            [v1 (sg/vector-set gen/nat)
             v2 (sg/vector-set gen/nat)]
            (is (= (sort (cs/intersection (set v1) (set v2)))
                   (vs/intersection v1 v2))
                "vs/union matches clojure.set/intersection"))

  (checking "intersection with empty == empty" 100
            [v (sg/vector-set gen/nat)]
            (is (= vs/empty-set (vs/intersection vs/empty-set v)))
            (is (= vs/empty-set (vs/intersection v vs/empty-set))))

  (checking "vs/union is correct" 100
            [v1 (sg/vector-set gen/nat)
             v2 (sg/vector-set gen/nat)]
            (is (= (vs/make (into v1 v2))
                   (vs/union v1 v2))
                "optimized way matches cheap way")

            (is (= (sort (cs/union (set v1) (set v2)))
                   (vs/union v1 v2))
                "vs/union matches clojure.set/union"))

  (checking "union(x, empty) == x " 100
            [v (sg/vector-set gen/nat)]
            (is (= v (vs/union vs/empty-set v)))
            (is (= v (vs/union v vs/empty-set)))))
