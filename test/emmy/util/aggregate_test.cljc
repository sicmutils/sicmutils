#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.util.aggregate-test
  (:require [clojure.test :refer [is deftest testing]]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]]
            [emmy.algebra.fold :as af]
            [emmy.numbers]
            [emmy.util.aggregate :as ua]))

(deftest sum-tests
  (is (= [0.0 1.0 5.0 14.0 30.0]
         (ua/scan #(* % %) 0 5))
      "scan the running sum of squares using the three-arity version of
      ua/scan.")

  (is (= 499500 (ua/pairwise-sum (range 0 1000)))
      "pairwise-sum with a non-vector still works.")

  (testing "compensated summation examples"
    (let [xs [1.0 1e-8 -1e-8]]
      (testing "Naive summation"
        (binding [ua/*fold* af/generic-sum-fold]
          (is (not= 1 (ua/sum xs))
              "Without the summation trick, errors build up.")

          (is (= (ua/generic-sum xs)
                 (ua/sum xs))
              "generic-sum matches sum when the binding is generic-sum-fold.")))

      (testing "Kahan summation"
        (binding [ua/*fold* af/kahan]
          (is (= 1.0 (ua/sum xs))
              "Kahan's summation trick allows us to keep precision.")

          (is (= [1.0 1.00000001 1.0] (ua/scan xs)))

          (is (= (ua/scan xs)
                 ((af/fold->scan-fn af/kahan) xs))
              "scan acts just like an actual `scan` call.")))

      (testing "KBN Summation"
        (binding [ua/*fold* af/kbn]
          (is (= 1.0 (ua/sum xs))
              "KBN's summation trick also allows us to keep precision.")

          (is (= [1.0 1.00000001 1.0] (ua/scan xs)))

          (is (= (ua/scan xs)
                 ((af/fold->scan-fn af/kbn) xs))
              "scan acts just like an actual `scan` call.")))))

  (testing "investigation from scmutils"
    ;; When adding up 1/n large-to-small we get a different answer than when
    ;; adding them up small-to-large, which is more accurate.
    (let [n 10000000
          z->n (into [] (range n))
          n->z (reverse z->n)
          f #(/ 1.0 (inc %))
          sum-inverse (fn [xs]
                        (binding [ua/*fold* af/generic-sum-fold]
                          (ua/sum (map f xs))))
          large->small (sum-inverse z->n)
          small->large (sum-inverse n->z)]
      (is (= 16.695311365857272
             large->small)
          "Naive summation ")

      (is (= 16.695311365859965
             small->large)
          "second example...")

      (is (= 2.6929569685307797e-12
             (- small->large large->small))
          "error!")

      (binding [ua/*fold* af/kahan]
        (is (= 1.1368683772161603e-13
               (- small->large
                  (ua/sum f 0 n)))
            "From GJS: Kahan's compensated summation formula is much better, but
      slower..."))

      (binding [ua/*fold* af/kbn]
        (is (= 1.1368683772161603E-13
               (- small->large
                  (ua/sum f 0 n)))
            "kbn sum, good, faster by 2x."))

      (is (= 1.1368683772161603E-13
             (- small->large
                (ua/pairwise-sum f 0 n)))
          "pairwise summation matches that error in this example, and is
          slightly faster (including the time to generate the vector of
          inputs).")))

  (testing "any monoid works as a fold binding, if no `present` beyond identity
            is needed."
    (is (= [1 2]
           (binding [ua/*fold* (ua/monoid into [])]
             (ua/sum [[1] [2]]))))))

(deftest monoid-group-tests
  (let [plus (ua/monoid + 0)]
    (checking "monoid" 100 [xs (gen/vector gen/nat)]
              (is (= (apply + xs)
                     (apply plus xs))
                  "monoid version built out of binary `+` matches built-in `+`"))

    (testing "* monoid bails early"
      (let [mul (ua/monoid * 1 zero?)]
        (is (= 6 (mul 1 2 3)))
        (is (= 0 (mul 1 2 0 :keyword))))))

  (let [minus (ua/group - + - 0)]
    (checking "group" 100 [xs (gen/vector gen/nat)]
              (if (seq xs)
                (is (= (apply - xs)
                       (apply minus xs))
                    "group version built out of binary `-` matches built-in `-`")
                (is (= 0 (apply minus xs))
                    "group version built out of binary `-` matches built-in `-`")))))
