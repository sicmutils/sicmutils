;;
;; Copyright © 2017 Colin Smith.
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

(ns sicmutils.util.aggregate-test
  (:require [clojure.test :refer [is deftest testing]]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]
             #?@(:cljs [:include-macros true])]
            [same :refer [ish?]]
            [sicmutils.numbers]
            [sicmutils.util.aggregate :as ua]
            [sicmutils.util.stream :as us]))

(deftest sum-tests
  (testing "Naive summation"
    (binding [ua/*fold* ua/naive-fold]
      (is (not= 1 (ua/sum [1.0 1e-8 -1e-8]))
          "Without the summation trick, errors build up.")))

  (testing "Kahan summation"
    (binding [ua/*fold* ua/kahan-fold]
      (is (= 1.0 (ua/sum [1.0 1e-8 -1e-8]))
          "Kahan's summation trick allows us to keep precision.")

      (let [xs [1.0 1e-8 -1e-8]]
        (is (= [1.0 1.00000001 1.0] (ua/scanning-sum xs)))

        (is (= (ua/scanning-sum xs)
               ((us/scan ua/kahan-fold :present ua/kahan-fold) xs))
            "scanning-sum acts just like an actual `scan` call."))))

  (testing "KBN Summation"
    (binding [ua/*fold* ua/kbn-fold]
      (is (= 1.0 (ua/sum [1.0 1e-8 -1e-8]))
          "KBN's summation trick also allows us to keep precision.")

      (let [xs [1.0 1e-8 -1e-8]]
        (is (= [1.0 1.00000001 1.0] (ua/scanning-sum xs)))

        (is (= (ua/scanning-sum xs)
               ((us/scan ua/kbn-fold :present ua/kbn-fold) xs))
            "scanning-sum acts just like an actual `scan` call."))))

  (testing "investigation from scmutils"
    ;; When adding up 1/n large-to-small we get a different answer than when
    ;; adding them up small-to-large, which is more accurate.
    (let [n    10000000
          z->n (into [] (range n))
          n->z (reverse z->n)
          f    #(/ 1.0 (inc %))
          sum-inverse (fn [xs]
                        (binding [ua/*fold* ua/naive-fold]
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

      (binding [ua/*fold* ua/kahan-fold]
        (is (= 1.1368683772161603e-13
               (- small->large
                  (ua/sum f 0 n)))
            "From GJS: Kahan's compensated summation formula is much better, but
      slower..."))

      (binding [ua/*fold* ua/kbn-fold]
        (is (= 2.6929569685307797E-12
               (- small->large
                  (ua/sum f 0 n)))
            "kbn sum, seemingly just as bad??")))))

(deftest monoid-group-tests
  (let [plus (ua/monoid (fn [a b] (+ a b)) 0)]
    (checking "monoid" 100 [xs (gen/vector gen/nat)]
              (is (= (apply + xs)
                     (apply plus xs))
                  "monoid version built out of binary `+` matches built-in `+`"))

    (testing "* monoid bails early"
      (let [mul (ua/monoid (fn [a b] (* a b)) 1 zero?)]
        (is (= 6 (mul 1 2 3)))
        (is (= 0 (mul 1 2 0 :keyword))))))

  (let [minus (ua/group (fn [a b] (- a b))
                        (fn [a b] (+ a b))
                        (fn [b] (- b))
                        0)]
    (checking "group" 100 [xs (gen/vector gen/nat)]
              (if (seq xs)
                (is (= (apply - xs)
                       (apply minus xs))
                    "group version built out of binary `-` matches built-in `-`")
                (is (= 0 (apply minus xs))
                    "group version built out of binary `-` matches built-in `-`")))))
