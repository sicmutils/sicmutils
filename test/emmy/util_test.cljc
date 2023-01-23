#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.util-test
  (:require [clojure.test :refer [is deftest testing]]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]]
            [emmy.util :as u]))

(deftest dict-tests
  (testing "keyset"
    (checking "keyset works" 100 [m (gen/map gen/keyword gen/nat)]
              (is (= (set (keys m))
                     (u/keyset m))))

    (is (= #{} (u/keyset {}))
        "keyset of an empty map is the empty set"))

  (testing "map-vals"
    (checking "map-vals works" 100 [m (gen/map gen/keyword gen/nat)]
              (is (= (into {}
                           (map (fn [[k v]]
                                  [k (str v)]))
                           m)
                     (u/map-vals str m))
                  "naive implementation vs actual fn"))

    (is (= {} (u/map-vals inc {}))
        "u/map-vals on the empty dict returns empty.")))
