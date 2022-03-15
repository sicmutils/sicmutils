#_
"Copyright © 2021 Sam Ritchie.
This work is based on the Scmutils system of MIT/GNU Scheme:
Copyright © 2002 Massachusetts Institute of Technology

This is free software;  you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or (at
your option) any later version.

This software is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this code; if not, see <http://www.gnu.org/licenses/>."

(ns sicmutils.util-test
  (:require [clojure.test :refer [is deftest testing]]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]
             #?@(:cljs [:include-macros true])]
            [sicmutils.util :as u]))

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
