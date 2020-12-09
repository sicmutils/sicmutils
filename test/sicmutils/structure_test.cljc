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

(ns sicmutils.structure-test
  (:refer-clojure :exclude [+ - * /])
  (:require [clojure.test :refer [is deftest testing]]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]
             #?@(:cljs [:include-macros true])]
            [sicmutils.abstract.number]
            [sicmutils.complex :as c]
            [sicmutils.generators :as sg]
            [sicmutils.generic :as g :refer [+ - * / cube expt negate square]]
            [sicmutils.structure :as s]
            [sicmutils.util :as u]
            [sicmutils.value :as v]))

(deftest value-protocol-tests
  (testing "zero?"
    (is (v/zero? (s/up)))
    (is (v/zero? (s/down)))
    (is (v/zero? (s/down 0)))
    (is (v/zero? (s/up 0 0)))
    (is (v/zero? (s/up 0)))
    (is (v/zero? (s/down 0 0)))
    (is (v/zero? (s/up 0 (s/down (s/up 0 0) (s/up 0 0)))))
    (is (v/zero? (s/up 0 (u/long 0) (u/int 0)))))

  (testing "zero-like"
    (is (v/zero? (v/zero-like (s/up 1 2 3))))
    (is (= (s/up 0 0 0) (v/zero-like (s/up 1 2 3))))
    (is (= (s/up) (v/zero-like (s/up))))
    (is (= (s/down 0 0 0) (v/zero-like (s/down 1 2 3))))
    (is (= (s/down) (v/zero-like (s/down))))
    (is (= (s/up 0 (s/down (s/up 0 0) (s/up 0 0))) (v/zero-like (s/up 1 (s/down (s/up 2 3) (s/up 4 5))))))
    (is (= (s/up (u/long 0) (u/int 0) 0)
           (v/zero-like (s/up (u/long 1) (u/int 2) 3)))))

  (testing "one-like"
    (is (thrown? #?(:clj UnsupportedOperationException :cljs js/Error)
                 (v/one-like (s/up 1 2 3)))))

  (testing "identity-like"
    (is (thrown? #?(:clj UnsupportedOperationException :cljs js/Error)
                 (v/identity-like (s/up 1 2 3)))))

  (testing "exact?"
    (is (v/exact? (s/up 1 2 3 4)))
    (is (not (v/exact? (s/up 1.2 3 4))))
    (is (v/exact? (s/up 0 1 #sicm/ratio 3/2)))
    (is (not (v/exact? (s/up 0 0 0.00001)))))

  (testing "freeze"
    (is (= '(up 1 2 3) (v/freeze (s/up 1 2 3)))))

  (testing "kind"
    (is (= ::s/up (v/kind (s/up 1 2))))
    (is (= ::s/down (v/kind (s/down (s/up 1 2)
                                    (s/up 2 3))))
        "Kind only depends on the outer wrapper, not on the contents.")))

(deftest structure-interfaces
  (testing "count"
    (is (= 3 (count (s/up 1 2 3)))))

  (testing "can be counted"
    (is (= 3 (count (s/up 4 5 6))))
    (is (= 2 (count (s/down (s/up 1 2) (s/up 3 4)))))
    (is (= [2 3] (map count (s/down (s/up 1 2) (s/up 3 4 5))))))

  (testing "support take"
    (is (= (s/up 1 2) (take 2 (s/up 1 2 3))))

    (let [first-two (take 2 (s/down (s/up 1 2)
                                    (s/up 3 4)
                                    (s/up 5 6)))]
      (is (= (s/up (s/up 1 2)
                   (s/up 3 4))
             first-two)
          "taking creates a lazy sequence and loses orientation information, but
          bare sequences are interpreted as up.")
      (is (not= (s/down (s/up 1 2)
                        (s/up 3 4))
                first-two)
          "the bare sequence is NOT equal to this down.")))

  (testing "support drop"
    (is (= (s/up 3) (drop 2 (s/up 1 2 3))))
    (let [dropped (drop 1 (s/down (s/up 1 2)
                                  (s/up 3 4)
                                  (s/up 5 6)))]
      (is (not= (s/down (s/up 3 4)
                        (s/up 5 6))
                dropped)
          "bare sequences can't equal downs.")
      (is (= (s/up (s/up 3 4) (s/up 5 6))
             dropped))))

  (testing "can be mapped"
    (is (= (s/up 1 4 9) (map square (s/up 1 2 3)))))

  (testing "a structure can produce a seq"
    (is (= [1 2 3] (seq (s/up 1 2 3))))
    (is (= [4 5 6] (seq (s/down 4 5 6))))
    (is (= [(s/up 1 2) (s/up 3 4)] (seq (s/down (s/up 1 2) (s/up 3 4)))))
    (is (= [1 2 3 4] (flatten (s/down (s/up 1 2) (s/up 3 4))))))

  (testing "seqable"
    (is (= [1 2 3] (into [] (s/up 1 2 3)))))

  (testing "a structure has a nth element (ILookup)"
    (is (= 14 (nth (s/up 10 12 14) 2)))
    (is (= 5 (nth (s/up 4 5 6) 1)))
    (is (thrown? #?(:clj IndexOutOfBoundsException :cljs js/Error) (nth (s/up 4 5 6) 4))))

  (testing "IFn"
    (is (= (s/up 6 9 1) ((s/up + * /) 3 3)))
    (is (= (s/up 22 2048 (g/expt 2 -9)) ((s/up + * /) 2 2 2 2 2 2 2 2 2 2 2))))

  (testing "print representation"
    (let [s (pr-str (s/up 1 2 3))]
      (is #?(:clj (clojure.string/includes? s "\"(up 1 2 3)\"")
             :cljs (= s "#object[sicmutils.structure.Structure \"(up 1 2 3)\"]"))))
    (is (= "(up 1 2 3)" (str (s/up 1 2 3)))))

  (testing "equality"
    (= (s/up 1 2 3) [1 2 3])
    (= (s/up 1 2 3) (s/up 1 2 3))))

(deftest structural-operations
  (testing "to vector"
    (is (= [1 2 3] (s/structure->vector (s/up 1 2 3))))
    (is (= [1 2 3] (s/structure->vector (s/down 1 2 3))))
    (is (= (type []) (type (s/structure->vector (s/up 1 2 3)))))
    (is (= (type []) (type (s/structure->vector (s/down 1 2 3)))))
    (is (= [(s/up 1 2) (s/up 3 4)] (s/structure->vector (s/down (s/up 1 2) (s/up 3 4))))))

  (testing "from vector"
    (is (= (s/up 1 2 3) (s/vector->up [1 2 3])))
    (is (= (s/down 4 5 6) (s/vector->down [4 5 6])))
    (is (thrown? #?(:clj AssertionError :cljs js/Error) (s/vector->up '(1 2 3)))))

  (testing "structure?"
    (is (s/structure? [1 2 3]))
    (is (s/structure? (s/up 1 2 3)))
    (is (s/structure? (s/down 1 2 3)))
    (is (not (s/structure? 10))))

  (testing "up?"
    (is (s/up? [1 2]))
    (is (s/up? (s/up 3 4)))
    (is (not (s/up? (s/down 3 4)))))

  (testing "orientation"
    (is (= ::s/up (s/orientation (s/up 1 2 3))))
    (is (= ::s/down (s/orientation (s/down 1 2 3))))
    (is (= ::s/up (s/orientation [1 2 3]))))

  (testing "opposite"
    (is (= (s/down 3 2 1)
           (s/opposite (s/up 1 2 3) [3 2 1])))
    (is (= (s/up 4 5 6)
           (-> (s/up 1 2 3)
               (s/opposite [3 2 1])
               (s/opposite [4 5 6])))))

  (testing "same"
    (is (= (s/up 3 2 1)
           (s/same (s/up 1 2 3) [3 2 1])))
    (is (= (s/down 3 2 1)
           (s/same (s/down) [3 2 1]))))

  (testing "flip-indices"
    (is (= (s/down 1 2 3)
           (s/flip-indices (s/up 1 2 3))))
    (is (= (s/up 1 2 3)
           (s/flip-indices (s/flip-indices (s/up 1 2 3)))))
    (is (= (s/down (s/up 1 2 3)
                   (s/up 4 5 6)
                   (s/up 7 8 9))
           (s/flip-indices
            (s/up (s/down 1 2 3)
                  (s/down 4 5 6)
                  (s/down 7 8 9))))))

  (testing "generate"
    (is (thrown? #?(:clj AssertionError :cljs js/Error)
                 (s/generate 5 ::random identity))
        "s/generate guards against wrong orientations.")

    (is (= (s/up 1 4 9 16 25)
           (s/generate 5 ::s/up (comp #(* % %) inc)))))

  (testing "literal-up,down"
    (is (thrown? #?(:clj AssertionError :cljs js/Error)
                 (s/literal 'x 3 ::random))
        "s/literal guards against invalid orientations.")

    (is (= (s/literal 'x 3 ::s/up)
           (s/literal-up 'x 3))
        "s/literal allows for creation of literal structures with a specified,
        correct orientation.")

    (is (= (s/up 'x↑0 'x↑1 'x↑2)
           (s/literal-up 'x 3)))

    (is (= (s/down 'x_0 'x_1 'x_2)
           (s/literal-down 'x 3)))

    (is (= '(+ (* x↑0 x_0)
               (* x↑1 x_1)
               (* x↑2 x_2))
           (v/freeze
            (g/* (s/literal-up 'x 3)
                 (s/literal-down 'x 3))))
        "It can be convenient to generate symbolic structures if you don't care
        about the entries."))

  (testing "mapr"
    (is (= (s/up (s/down 1  4  9)
                 (s/down 16 25 36)
                 (s/down 49 64 81))
           (s/mapr #(* % %)
                   (s/up (s/down 1 2 3)
                         (s/down 4 5 6)
                         (s/down 7 8 9))))
        "mapr should square every element without changing the structure.")

    (let [S0 (s/up 2)
          S1 (s/up 2 3)
          S2 (s/down (s/up 1 2) (s/up 3 4))
          S3 (s/up (s/down 1 2) (s/down 3 4))]
      (is (= (s/up 4) (s/mapr square S0)))
      (is (= (s/up 4 9) (s/mapr square S1)))
      (is (= (s/down (s/up 1 4) (s/up 9 16)) (s/mapr square S2)))
      (is (= (s/up (s/down 1 4) (s/down 9 16)) (s/mapr square S3)))
      (is (= (s/up 4 9) (s/mapr square [2 3])))))

  (testing "mapr - two arg fn"
    (let [S (s/down 'a 'b (s/up 'c 'd) (s/down 'e (s/down 'f 'g)) 'h)]
      (is (= (s/down '(a 0) '(b 1)
                     (s/up '(c 2 0) '(d 2 1))
                     (s/down '(e 3 0) (s/down '(f 3 1 0) '(g 3 1 1))) '(h 4))
             (s/mapr cons S (s/structure->access-chains S))))))

  (testing "mapr - etc"
    (is (= (s/up 1 4 9) (s/mapr square [1 2 3])))
    (is (= (s/up 11 22) (s/mapr + (s/up 1 2) (s/up 10 20))))
    (is (= (s/up 11 22) (s/mapr + [1 2] (s/up 10 20))))
    (is (= (s/up 11 22) (s/mapr + (s/up 10 20) [1 2]))))

  (testing "access-chains"
    (is (= (s/up [0] [1] [2]) (s/structure->access-chains (s/up 1 2 3))))
    (is (= (s/up [0] (s/up [1 0] [1 1]) (s/down [2 0] [2 1]))
           (s/structure->access-chains
            (s/up 't (s/up 'x 'y) (s/down 'p_x 'p_y)))))
    (is (= (s/up (s/down (s/up [0 0 0] [0 0 1])
                         (s/up [0 1 0] [0 1 1]))
                 (s/down (s/up [1 0 0] [1 0 1])
                         (s/up [1 1 0] [1 1 1])))
           (s/structure->access-chains
            (s/up (s/down (s/up 1 2) (s/up 2 3))
                  (s/down (s/up 3 4) (s/up 4 5)))))))

  (testing "component"
    (let [m (s/up (s/down (s/up 1 2) (s/up 2 3))
                  (s/down (s/up 3 4) (s/up 4 5)))
          chains (s/structure->access-chains m)]
      (is (= m (s/mapr (fn [chain]
                         ((apply s/component chain) m))
                       chains))
          "In a structure of chains, check that every element (chain) can look
          up the original value in the original structure.")

      (is (= m (->> chains
                    (s/mapr (partial apply s/component))
                    (s/mapr #(% m))))
          "Identical statement built a different way.")))

  (testing "get-in"
    (is (= 5 (get-in (s/up 4 5 6) [1])))
    (is (= 4 (get-in (s/up 4 5 6) [0])))
    (is (= 4 (get-in (s/down (s/up 1 2) (s/up 3 4)) [1 1])))
    (is (= 2 (get-in (s/down (s/up 1 2) (s/up 3 4)) [0 1]))))

  (testing "assoc-in"
    (is (= (s/up 4 55 6)
           (assoc-in (s/up 4 5 6) [1] 55)))
    (is (= (s/down (s/up 1 22) (s/up 3 4))
           (assoc-in (s/down (s/up 1 2) (s/up 3 4)) [0 1] 22))))

  (testing "unflatten"
    (is (= (s/up (s/down 0 1) (s/down 2 3))
           (s/unflatten (range) (s/up (s/down 'x 'y) (s/down 'z 't)))))
    (is (= (s/down 3 (s/up 4 5) (s/down (s/up (s/down 6 7) (s/up 8 9) 10)) 11)
           (s/unflatten (range 3 12)
                        (s/down 'a (s/up 'b 'c) (s/down (s/up (s/down 'd 'e) (s/up 'f 'g) 'h)) 'i))))
    (is (= 9 (s/unflatten [9] 3)))
    (is (= (s/up 2) (s/unflatten [2] (s/up 0.0))))
    ;; gjs examples from structs.scmutils
    (is (= (s/up 1 2 'a (s/down 3 4) (s/up (s/down 'c 'd) 'e))
           (s/unflatten '(1 2 a 3 4 c d e)
                        (s/up 'x 'x 'x (s/down 'x 'x) (s/up (s/down 'x 'x) 'x))))))

  ;; this is wrong and needs to be fixed.
  (testing "compatible-shape"
    (let [o (s/compatible-shape (s/up 1 2))]
      (is (= ::s/down (v/kind o)))
      (is (every? symbol? o)))
    (let [o (s/compatible-shape (s/down 3 (s/up 1 2) (s/up 3 4)))]
      (is (= ::s/up (v/kind o)))
      (is (symbol? (get o 0)))
      (is (= ::s/down (v/kind (get o 1))))
      (is (= ::s/down (v/kind (get o 2))))))

  (testing "dimension"
    (let [A (s/up 1 2 'a (s/down 3 4) (s/up (s/down 'c 'd) 'e))]
      (is (= 8 (g/dimension A)))
      (is (= 1 (g/dimension 99))))))

(deftest structure-generics
  (testing "up/down +, same kind"
    (is (= (+ (s/up 1 2) (s/up 2 3))
           (s/up 3 5)))
    (is (= (+ (s/down 3 4) (s/down 1 2))
           (s/down 4 6)))
    (is (= (s/down (+ 'u 4) (+ 2 'v))
           (+ (s/down 'u 2) (s/down 4 'v)))))

  (testing "up/down -, same kind"
    (is (= (- (s/up 1 2) (s/up 2 3))
           (s/up -1 -1)))
    (is (= (- (s/down 8 5) (s/down 4 -1))
           (s/down 4 6)))
    (is (= (- (s/down 8 5))
           (s/down -8 -5)))
    (is (= (- (s/up 10 10) (s/up 2 3) (s/up 3 4))
           (s/up 5 3))))

  (testing "s +/- t mixed"
    (is (= (+ (s/up (s/down 1 2) (s/down 3 4))
              (s/up (s/down 2 3) (s/down -7 2)))
           (s/up (s/down 3 5) (s/down -4 6))))
    (is (= (- (s/up (s/down 1 2) (s/down 3 4))
              (s/up (s/down 2 3) (s/down -7 2)))
           (s/up (s/down -1 -1) (s/down 10 2))))
    (is (= (+ (s/down (s/up 1 2) (s/up 3 4))
              (s/down (s/up 2 3) (s/up -7 2)))
           (s/down (s/up 3 5) (s/up -4 6))))
    (is (= (- (s/down (s/up 1 2) (s/up 3 4))
              (s/down (s/up 2 3) (s/up -7 2)))
           (s/down (s/up -1 -1) (s/up 10 2)))))

  (testing "a*s"
    (is (= (s/up 2 4 6) (* 2 [1 2 3])))
    (is (= (s/down 3 6 9) (* 3 (s/down 1 2 3))))
    (is (= (s/down 12 24 36) (* 3 4 (s/down 1 2 3)))))

  (testing "s/a"
    (is (= (s/up 1 2 -3) (/ (s/up 2 4 -6) 2)))
    (is (= (s/up 1 2 -3)
           (/ (s/up (u/long 2) 4 -6)
              (u/long 2)))))

  (testing "neg"
    (is (= (s/up -1 2 -3) (- (s/up 1 -2 3))))
    (is (= (s/up -1 2 -3) (negate (s/up 1 -2 3)))))

  (testing "a*s with literals"
    (is (= (s/up 2 (* 2 't) 6) (* 2 (s/up 1 't 3))))
    (is (= (s/down (* 3 'x_0) (* 3 'x_1)) (* 3 (s/down 'x_0 'x_1)))))

  (testing "s*t outer simple"
    (is (= (s/up (s/up 3 6)
                 (s/up 4 8))
           (* (s/up 1 2)
              (s/up 3 4))))
    (is (= (s/down (s/down 3 6) (s/down 4 8))
           (* (s/down 1 2)
              (s/down 3 4))))
    (is (= (s/down (s/up 3 6)
                   (s/up 4 8)
                   (s/up 5 10))
           (* (s/up 1 2)
              (s/down 3 4 5)))))

  (testing "s*t inner simple"
    (is (= 11 (* (s/up 1 2) (s/down 3 4))))
    (is (= 22 (* (s/down 2 3) (s/up 5 4)))))

  (testing "s*t inner with vars"
    (is (= (+ 'y (* 'x 4)) (* (s/up 1 'x) (s/down 'y 4)))))

  (testing "cross-product with fns"
    (let [deferred (g/cross-product #(g/* 2 %)
                                    #(g/+ (s/up 4 3 1) %))
          v (s/up 1 2 3)]
      (is (= (g/cross-product (g/* 2 v)
                              (g/+ (s/up 4 3 1) v))
             (deferred v))
          "Slightly tougher since this works with structures")))

  (testing "examples from refman"
    (is (= 652 (* (s/up (s/up 2 3) (s/down 5 7 11))
                  (s/down (s/down 13 17) (s/up 19 23 29)))))
    (is (= (s/up (s/up 10 15) (s/up 14 21) (s/up 22 33)) (* (s/up 2 3) (s/up 5 7 11))))
    (is (= (s/up (s/up 10 14 22) (s/up 15 21 33)) (* (s/up 5 7 11) (s/up 2 3)))))

  (testing "square/cube"
    (is (= 14 (square (s/up 1 2 3))))
    (is (= (s/up (s/up (s/up 1 2 3) (s/up 2 4 6) (s/up 3 6 9))
                 (s/up (s/up 2 4 6) (s/up 4 8 12) (s/up 6 12 18))
                 (s/up (s/up 3 6 9) (s/up 6 12 18) (s/up 9 18 27))) (cube (s/up 1 2 3)))))


  (testing "matrix-like"
    (let [M (s/down (s/up 'a 'c) (s/up 'b 'd))
          S (s/up (s/down 'a 'b) (s/down 'c 'd))
          x (s/up 'x 'y)
          xt (s/down 'x 'y)]
      (is (= (s/up (+ (* 'a 'x) (* 'b 'y))
                   (+ (* 'c 'x) (* 'd 'y)))
             (* M x)))

      (is (= (s/up (+ (* 'x 'a) (* 'y 'b))
                   (+ (* 'x 'c) (* 'y 'd)))
             (* x S)))

      (is (= (s/down (+ (* 'x 'a) (* 'y 'c))
                     (+ (* 'x 'b) (* 'y 'd)))
             (* xt M)))

      (is (= (+ (* (+ (* 'x 'a) (* 'y 'c)) 'x)
                (* (+ (* 'x 'b) (* 'y 'd)) 'y))
             (* xt M x)))

      (is (= (+ (* (+ (* 'x 'a) (* 'y 'c)) 'x)
                (* (+ (* 'x 'b) (* 'y 'd)) 'y))
             (* (* xt M) x)))

      (is (= (+ (* 'x (+ (* 'a 'x) (* 'b 'y)))
                (* 'y (+ (* 'c 'x) (* 'd 'y))))
             (* xt (* M x)))))

    (let [M (s/up (s/down 'a 'b)
                  (s/down 'c 'd))
          x (s/down 'x 'y)]
      (is (= (s/down (+ (* 'a 'x) (* 'c 'y))
                     (+ (* 'b 'x) (* 'd 'y)))
             (* M x)))
      (is (= (s/down (+ (* 'x 'a) (* 'y 'c))
                     (+ (* 'x 'b) (* 'y 'd)))
             (* x M))))

    (let [M (s/up (s/down 'a 'c)
                  (s/down 'b 'd))]
      (is (= (s/up (s/down (s/up (s/down (* 'a 'a) (* 'a 'c))
                                 (s/down (* 'a 'b) (* 'a 'd)))
                           (s/up (s/down (* 'c 'a) (* 'c 'c))
                                 (s/down (* 'c 'b) (* 'c 'd))))
                   (s/down (s/up (s/down (* 'b 'a) (* 'b 'c))
                                 (s/down (* 'b 'b) (* 'b 'd)))
                           (s/up (s/down (* 'd 'a) (* 'd 'c))
                                 (s/down (* 'd 'b) (* 'd 'd)))))
             (g/outer-product M M)))

      (is (= (s/up (s/down (+ (* 'a 'a) (* 'b 'c))
                           (+ (* 'c 'a) (* 'd 'c)))
                   (s/down (+ (* 'a 'b) (* 'b 'd))
                           (+ (* 'c 'b) (* 'd 'd))))
             (* M M)))))

  (testing "fibonacci-matrix"
    (let [n 20
          fibs (map first (iterate (fn [[a b]] [b (+ a b)]) [0 1]))
          fib (fn [i] (nth fibs i))
          M (s/down (s/up 1 1) (s/up 1 0))]
      (is (= (fib n) (-> (expt M n) first second)))))

  (testing "expt"
    (is (= (s/up
            (s/up
             (s/up (s/up 1 2) (s/up 2 4))
             (s/up (s/up 2 4) (s/up 4 8)))
            (s/up
             (s/up (s/up 2 4) (s/up 4 8))
             (s/up (s/up 4 8) (s/up 8 16))))
           (expt (s/up 1 2) 4)))
    (is (= (* (s/up 1 2) (s/up 1 2) (s/up 1 2) (s/up 1 2))
           (expt (s/up 1 2) 4)))))

(deftest some-tensors
  (let [ε_ijk (s/down (s/down (s/down  0  0  0)
                              (s/down  0  0  1)
                              (s/down  0 -1  0))
                      (s/down (s/down  0  0 -1)
                              (s/down  0  0  0)
                              (s/down  1  0  0))
                      (s/down (s/down  0  1  0)
                              (s/down -1  0  0)
                              (s/down  0  0  0)))
        δ-il (s/up (s/up 1 0 0)
                   (s/up 0 1 0)
                   (s/up 0 0 1))]
    (is (= (s/down 0 0 0) (* δ-il ε_ijk)))))

(deftest matrices
  (let [A (s/up (s/up 1 2) (s/up 3 4))
        B (s/down (s/up 1 2 3) (s/up 3 4 5))
        C (s/down (s/up 1 2 3) (s/up 0 4 5) (s/up 1 0 6))
        D (s/up (s/down 3))
        E (s/up 1)
        F (s/down (s/up 1 2) (s/up 3 4))]

    (testing "transpose"
      (is (= (s/down (s/up 1 2) (s/up 3 4)) (g/transpose A)))
      (is (= (s/up (s/up 1 2 3) (s/up 3 4 5)) (g/transpose B)))
      (is (= (s/up (s/up 1 2 3) (s/up 0 4 5) (s/up 1 0 6)) (g/transpose C)))
      (is (= (s/down (s/down 3)) (g/transpose D)))
      (is (= (s/down 1) (g/transpose E)))
      (is (= (s/up (s/up 1 2) (s/up 3 4)) (g/transpose F))))

    (testing "flip-indices"
      (is (= (s/down (s/down 1 2) (s/down 3 4)) (s/flip-indices A)))
      (is (= (s/up (s/down 1 2 3) (s/down 3 4 5)) (s/flip-indices B)))
      (is (= (s/down 1) (s/flip-indices E))))))

(def ^:private near
  (v/within 1e-12))

(deftest struct-complex-tests
  (testing "magnitude of structures as per GJS - 'plain' vectors"
    (is (= (g/magnitude [3 4]) 5))
    (is (near (g/magnitude [3 4 5]) (g/sqrt 50))))

  (testing "magnitude of a structure with complex entries"
    (let [m (g/magnitude [#sicm/complex "3+4i" (g/sqrt 11)])]
      (is (near 6 (g/real-part m)))
      (is (near 0 (g/imag-part m)))))

  (testing "g/abs"
    (is (near (g/abs [3 4 5]) (g/sqrt 50)))

    (let [m (g/magnitude [#sicm/complex "3+4i" (g/sqrt 11)])]
      (is (= (g/sqrt (g/square m))
             (c/complex (g/abs m))))))

  (testing "g/conjugate"
    (is (= (s/up 3 4 5) (g/conjugate [3 4 5])))
    (is (= (s/up #sicm/complex "3-4i")
           (g/conjugate [#sicm/complex "3+4i"]))))

  (testing "magnitude of structures as per GJS - structures"
    (is (= (g/magnitude (s/up 3 4)) 5))
    (is (= (g/magnitude (s/down 3 4)) 5))
    (is (near (g/magnitude (s/up 3 4 5)) (g/sqrt 50)))
    (is (near (g/magnitude (s/down 3 4 5)) (g/sqrt 50)))))

(deftest tests-to-file
  (testing "transpose-outer unit"
    (let [foo (s/down (s/down (s/up 'x 'y)
                              (s/up 'z 'w))
                      (s/down (s/up 'a 'b)
                              (s/up 'c 'd)))]
      (is (= (s/down (s/down (s/up 'x 'y)
                             (s/up 'a 'b))
                     (s/down (s/up 'z 'w)
                             (s/up 'c 'd)))
             (s/transpose-outer foo)))))

  (checking "transpose-outer roundtrip" 100
            [s (sg/up1 (sg/down1 sg/real 5))]
            (is (= s (s/transpose-outer
                      (s/transpose-outer s)))))

  (testing "structure->prototype"
    (let [s (s/up 't (s/up 'u 'v) (s/down 'r 's) (s/up 'v1 'v2))]
      (is (= (s/up 'x0
                   (s/up 'x1:0 'x1:1)
                   (s/down 'x2:0 'x2:1)
                   (s/up 'x3:0 'x3:1))
             (s/structure->prototype 'x s)))))

  (checking "s/map-chain with get-in is identity" 100
            [s (sg/structure sg/real)]
            (is (= s (s/map-chain #(get-in s %2) s)))

            (is (= (s/map-chain (fn [_ chain] (seq chain)) s)
                   (s/structure->access-chains s))
                "map-chain and structure->access-chains are equiv"))

  (checking "s/mapr get-in from structure->access-chains is identity" 100
            [s (sg/structure sg/real)]
            (let [chains (s/structure->access-chains s)]
              (is (= s (s/mapr #(get-in s %) chains)))))

  (checking "s/compatible-zero works" 100
            [s (sg/structure sg/real)]
            (is (v/zero? (g/* s (s/compatible-zero s))))
            (is (v/zero? (g/* (s/compatible-zero s) s)))))

(deftest product-tests
  (testing "dot-product unit"
    (is (= 11 (g/dot-product
               (s/up (s/down 1 2))
               (s/up (s/down 3 4))))))

  (checking "g/dot-product of all 1s == dimension" 100
            [x (sg/structure (gen/return 1))]
            (is (= (s/dimension x)
                   (g/dot-product x x))))

  (checking "g/dot-product of complex" 100
            [x (sg/structure (gen/return
                              (g/make-rectangular 1 1)))]
            (let [dot   (g/dot-product x x)
                  two*d (g/* 2 (s/dimension x))]
              (is (== two*d (g/imag-part dot))
                  "dot product doesn't conjugate, so all magnitude gets rotated
                  up to the imaginary axis.")

              (is (zero? (g/real-part dot))
                  "real part is zero!")))

  (checking "g/inner-product of complex" 100
            [x (sg/structure (gen/return
                              (g/make-rectangular 1 1)))]
            (let [ip (g/inner-product x x)]
              (is (zero? (g/imag-part ip))
                  "inner product always returns a real.")

              (is (== (g/* 2 (s/dimension x))
                      (g/real-part ip))
                  "every entry had magnitude (sqrt 2), so the inner product
                  should be double the dimension.")))

  (testing "{dot,inner}-product throws at incompatible lengths"
    (is (thrown? #?(:clj IllegalArgumentException :cljs js/Error)
                 (g/inner-product
                  (s/up (s/down #sicm/complex "1+2i" 2))
                  (s/up (s/down #sicm/complex "1+2i" 2 3 4)))))

    (is (thrown? #?(:clj IllegalArgumentException :cljs js/Error)
                 (g/dot-product
                  (s/up (s/down 1 2))
                  (s/up (s/down 1 2 3 4)))))))
