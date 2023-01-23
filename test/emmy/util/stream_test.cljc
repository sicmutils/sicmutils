#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.util.stream-test
  "Tests of the various sequence convergence and generation utilities in the SICM
  library."
  (:require [clojure.test :refer [is deftest testing]]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]]
            [same :refer [ish?] :include-macros true]
            [emmy.generators :as sg]
            [emmy.generic :as g]
            [emmy.numbers]
            [emmy.util.stream :as us]))

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
