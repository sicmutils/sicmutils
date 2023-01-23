#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.util.permute-test
  (:require [clojure.test :refer [is deftest testing]]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]]
            [emmy.generic :as g]
            [emmy.util.permute :as p]))

(deftest misc-tests
  (testing "permutations"
    (is (= '((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1))
           (p/permutations [1 2 3])))

    (is (= [[]] (p/permutations [])))
    (checking "permutation laws" 100
              [xs (gen/vector gen/keyword 3)]
              (let [perms (p/permutations xs)
                    elems (distinct (map set perms))]
                (is (every? #(= (count %) 3) perms)
                    "every permutation has the same number of elements.")

                (is (= 1 (count elems))
                    "They all have the same enties..")

                (is (= (set xs) (first elems))
                    "equal to the original."))))

  (testing "combinations"
    (checking "empty input always returns empty output" 100
              [p (gen/fmap inc gen/nat)]
              (is (= [] (p/combinations [] p))))

    (checking "p == 0 always returns a singleton with the empty set." 100
              [xs (gen/vector gen/any-equatable)]
              (is (= [[]] (p/combinations xs 0))))

    (is (= '((a b c)
             (a b d)
             (a b e)
             (a c d)
             (a c e)
             (a d e)
             (b c d)
             (b c e)
             (b d e)
             (c d e))
           (p/combinations
            '[a b c d e] 3))))

  (testing "cartesian-product"
    (is (= '((a c) (b c) (a d) (b d))
           (p/cartesian-product [['a 'b]
                                 ['c 'd]])))

    (is (= '(()) (p/cartesian-product []))
        "base case."))

  (testing "list-interchanges, permutation-sequence"
    (let [xs ['a 'b 'c 'd 'e]
          changes (map #(p/list-interchanges % xs)
                       (p/permutation-sequence xs))]
      (is (every? true?
                  (for [[a b] (partition 2 changes)]
                    (= 1 (g/abs (- b a)))))
          "p/permutation-sequence generates a sequence of permutations that each
           differ from the previous by a single transposition.")))

  (testing "permutation-parity"
    (is (= 1 (p/permutation-parity [1 2 3] [1 2 3]))
        "Same elements returns 1 (even parity)")

    (is (= 0 (p/permutation-parity [1 2 3] [1 2 3 3]))
        "Same elements but different length gives 0.")

    (let [xs       ['a 'b 'c 'd 'e 'f]
          parities (map #(p/permutation-parity % xs)
                        (p/permutation-sequence xs))]
      (is (= (take (count parities)
                   (cycle [1 -1]))
             parities)
          "parity cycles between 1 and -1.")))

  (checking "permutation-parity laws" 100 [xs (gen/shuffle (range 6))]
            (let [sorted (sort xs)]
              (is (= 1 (p/permutation-parity sorted))
                  "sorted lists have parity == 1")

              (let [changes (p/permutation-interchanges xs)]
                (is (= (if (odd? changes) -1 1)
                       (p/permutation-parity xs))
                    "given odd interchanges, permutation-parity returns -1, else
                    1. Never 0 in the single-arg case."))))

  (checking "permutation-interchanges" 100
            [xs (gen/vector gen/nat 6)]
            (is (= (p/list-interchanges xs (sort xs))
                   (p/permutation-interchanges xs))))

  (testing "permute unit"
    (is (= [] (p/permute [] [])))
    (is (= [0 1 3 2]
           (p/permute [3 0 1 2] [1 3 2 0]))))

  (checking "permute" 100 [xs (gen/shuffle (range 6))]
            (let [sorted (sort xs)]
              (is (= xs (p/permute xs sorted))
                  "applying a permutation to a sorted list returns the
                permutation.")

              (is (= xs (p/permute sorted xs))
                  "applying the sorted list to the permutation acts as id.")))

  (testing "sort-and-permute"
    (is (= [[0 2 0 0 1 2 0 0]
            [0 0 0 0 0 1 2 2]
            [0 0 0 0 0 1 2 2]
            [0 2 0 0 1 2 0 0]]
           (p/sort-and-permute
            [0 2 0 0 1 2 0 0]
            <
            (fn [unsorted sorted permuter unpermuter]
              [unsorted
               sorted
               (permuter unsorted)
               (unpermuter sorted)])))))

  (testing "subpermute"
    (is (= ['a 'e 'd 'b 'c]
           (p/subpermute {1 4, 4 2, 2 3, 3 1}
                         '[a b c d e]))))

  (checking "number-of-permutations" 100
            [xs (gen/let [n (gen/choose 0 6)]
                  (gen/vector gen/nat n))]
            (is (= (p/number-of-permutations (count xs))
                   (count (p/permutations xs)))))

  (checking "number-of-combinations" 100
            [[xs k] (gen/let [n (gen/choose 1 6)]
                      (gen/tuple
                       (gen/vector gen/nat n)
                       (gen/choose 0 n)))]
            (is (= (p/number-of-combinations (count xs) k)
                   (count
                    (p/combinations xs k)))))

  (checking "multichoose" 100
            [n gen/nat k (gen/fmap inc gen/nat)]
            (is (= (p/multichoose n k)
                   (p/number-of-combinations
                    (+ n k -1) k))
                "Definition from https://mathworld.wolfram.com/Multichoose.html")))

(deftest permutation-test
  (testing "permutation-sequence"
    (is (thrown? #?(:clj Exception :cljs js/Error)
                 (p/permutation-sequence 0)))

    (is (= '[[a]]
           (p/permutation-sequence '[a])))

    (is (= '[[a b] [b a]]
           (p/permutation-sequence '(a b))))

    (is (= [[0 1 2]
            [0 2 1]
            [2 0 1]
            [2 1 0]
            [1 2 0]
            [1 0 2]]
           (p/permutation-sequence [0 1 2])))

    (is (= [[[0 1 2] 1]
            [[0 2 1] -1]
            [[2 0 1] 1]
            [[2 1 0] -1]
            [[1 2 0] 1]
            [[1 0 2] -1]]
           (map vector
                (p/permutation-sequence (range 3))
                (cycle [1 -1]))))

    (is (= [[0 1 2 3] [0 1 3 2] [0 3 1 2] [3 0 1 2]
            [3 0 2 1] [0 3 2 1] [0 2 3 1] [0 2 1 3]
            [2 0 1 3] [2 0 3 1] [2 3 0 1] [3 2 0 1]
            [3 2 1 0] [2 3 1 0] [2 1 3 0] [2 1 0 3]
            [1 2 0 3] [1 2 3 0] [1 3 2 0] [3 1 2 0]
            [3 1 0 2] [1 3 0 2] [1 0 3 2] [1 0 2 3]]
           (p/permutation-sequence (range 4))))))
