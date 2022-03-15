#_
"Copyright © 2017 Colin Smith.
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

(ns sicmutils.util.stream-test
  "Tests of the various sequence convergence and generation utilities in the SICM
  library."
  (:require [clojure.test :refer [is deftest testing]]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]
             #?@(:cljs [:include-macros true])]
            [same :refer [ish?]
             #?@(:cljs [:include-macros true])]
            [sicmutils.generators :as sg]
            [sicmutils.generic :as g]
            [sicmutils.numbers]
            [sicmutils.util.stream :as us]))

(deftest zeno-powers-tests
  (testing "zeno streams"
    (is (ish? (/ 5 (g/expt 2 10))
              (nth (us/zeno 2 5) 10)))

    (is (ish? (/ 1 (g/expt 2 10))
              (nth (us/zeno 2) 10))))

  (testing "powers"
    (is (ish? (* 5 (g/expt 2 10))
              (nth (us/powers 2 5) 10)))

    (is (ish? (g/expt 2 10)
              (nth (us/powers 2) 10)))))

(deftest vector-tests
  (checking "vector:generate" 100 [v (gen/vector sg/real)]
            (is (= v (us/vector:generate
                      (count v) (partial get v)))
                "use generate to rebuild."))

  (checking "separatev" 100 [v (gen/vector gen/nat)]
            (let[[evens odds] (us/separatev even? v)]
              (is (= [(filterv even? v)
                      (filterv odd? v)]
                     [evens odds])
                  "separatev runs a filterv and filterv on a predicate's
                  complement in parallel.")

              (is (and (vector? evens) (vector? odds))
                  "both returned elements are vectors."))))

(deftest convergence-tests
  (testing "empty sequence behavior."
    (is (= {:converged? false, :terms-checked 0, :result nil}
           (us/seq-limit []))))

  (testing "normal usage."
    (is (= {:converged? true, :terms-checked 11, :result (/ 1 1024)}
           (us/seq-limit (us/zeno 2)
                         {:tolerance (/ 1 (g/expt 2 10))}))))

  (testing "maxterms stops evaluation."
    (is (= {:converged? false, :terms-checked 4, :result (/ 1 8)}
           (us/seq-limit (us/zeno 2)
                         {:maxterms 4}))))

  (testing "minterms forces that number of terms to be evaluated."
    (is (= {:converged? true, :terms-checked 20, :result (/ 1 (g/expt 2 19))}
           (us/seq-limit (us/zeno 2)
                         {:tolerance (/ 1 (g/expt 2 10))
                          :minterms 20}))))

  (testing "If the sequence runs out, convergence tests stop and the final
     item's returned."
    (is (= {:converged? false, :terms-checked 3, :result 3}
           (us/seq-limit [1 2 3]
                         {:tolerance (/ 1 (g/expt 2 10))
                          :minterms 20})))))
