#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.polynomial.gcd-test
  (:require [clojure.test :refer [is deftest testing]]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]]
            [emmy.generators :as sg]
            [emmy.generic :as g]
            [emmy.modint :as mi]
            [emmy.polynomial :as p]
            [emmy.polynomial.gcd :as pg]
            [emmy.util :as u]
            [emmy.util.stopwatch :as us]
            [emmy.value :as v]
            [taoensso.timbre :as log]))

(deftest gcd-tests
  (checking "gcd matches pg behavior for rationals" 100
            [x sg/rational
             y sg/rational]
            (is (= (pg/gcd x y)
                   (g/gcd x y))))

  (checking "primitive-gcd matches normal gcd, for more than ONE input." 100
            [xs (gen/vector sg/any-integral)]
            (let [xs (if (= 1 (count xs))
                       (concat xs xs)
                       xs)]
              (is (= (reduce g/gcd 0 xs)
                     (apply pg/primitive-gcd xs)))))

  (testing "monomial-gcd"
    (is (= (p/make 2 {[1 2] 3})
           (pg/monomial-gcd
            (p/make 2 {[1 2] 12})
            (p/make 2 {[4 3] 15})))))

  (testing "inexact coefficients"
    (is (= 1 (g/gcd
              (p/make [0.2 0.4 0.6])
              (p/make [0.4 0.6 0.8]))))
    (is (= (p/make [0 1])
           (g/gcd
            (p/make [0 1])
            (p/make [0 0.2 1])))))

  (testing "divide constant arity 2"
    (let [X (p/make 2 [[[1 0] 1]])
          Y (p/make 2 [[[0 1] 1]])]
      (is (= [0 X] (p/divide X Y)))
      (is (= [0 Y] (p/divide Y X)))))

  (checking "constant case" 100
            [x sg/any-integral
             y sg/any-integral]
            (is (v/= (g/gcd x y)
                     (g/gcd
                      (p/constant x)
                      (p/constant y)))))

  (testing "GCD: arity 1 case"
    (let [x+1 (p/make [1 1])
          x+2 (p/make [2 1])
          x+3 (p/make [3 1])
          x+4 (p/make [4 1])
          U (g/* x+1 x+2 x+1 x+4)
          V (g/* x+1 x+2 x+3)]
      (is (= (g/* x+1 x+2)
             (g/gcd U V))))

    (let [x+4 (p/make [4 1])
          x+3 (p/make [3 1])
          x-2 (p/make [-2 1])
          x+1 (p/make [1 1])
          U (g/* x+4 x+4 x+3 x+3 x-2 x-2 x-2 x-2 x-2 x+1)
          V (g/* x+4 x+3 x+3 x+3 x-2 x-2 x+1 x+1)
          W (g/* x+4 x+3 x+3 x-2 x-2 x+1)]
      (is (= W (g/gcd U V)))
      (is (= W (g/gcd V U)))

      (testing "TODO, generative, GCD of anything with itself is itself."
        (is (= U (g/gcd U U)))
        (is (= V (g/gcd V V)))
        (is (= W (g/gcd W W))))

      (testing "TODO, generative, gcd of anything with 0 itself."
        (is (= U (g/gcd U 0)))
        (is (= U (g/gcd 0 U)))
        (is (= V (g/gcd V 0)))
        (is (= V (g/gcd 0 V))))))

  (testing "GCD: arity 2 case"
    (let [I (p/make 2 [[[0 0] 1]])
          X (p/make 2 [[[1 0] 1]])
          Y (p/make 2 [[[0 1] 1]])
          X+Y (g/+ X Y)
          X+1 (g/+ X I)
          Y+1 (g/+ Y I)
          X+Y_2 (g/* X+Y X+Y)
          X+Y_3 (g/* X+Y_2 X+Y)
          U (g/* (g/expt X+1 3) (g/expt X+Y 2) (g/expt Y+1 4))
          V (g/* (g/expt X+1 2) (g/expt X+Y 5) (g/expt Y+1 3))
          G (g/* (g/expt X+1 2) (g/expt X+Y 2) (g/expt Y+1 3))]
      (is (= X+Y_2 (g/gcd X+Y_2 X+Y_3)))
      (is (= X+Y_3 (g/gcd X+Y_3 X+Y_3)))
      (is (= G (g/gcd U V)))))

  (testing "GCD: arity 3 case"
    (binding [pg/*poly-gcd-time-limit* #?(:clj  [2 :seconds]
                                          :cljs [20 :seconds])]
      (let [[X Y Z] (p/new-variables 3)
            X+Y (g/+ X Y)
            X+Z (g/+ X Z)
            Y+Z (g/+ Y Z)
            X+Y+Z (g/+ X+Y Z)
            X+1 (g/+ X 1)
            Y+1 (g/+ Y 1)
            Z+1 (g/+ Z 1)
            U (g/* (g/expt X+1 3)
                   (g/expt X+Y 2)
                   (g/expt Y+Z 5)
                   (g/expt X+Y+Z 4)
                   (g/expt Y+1 4)
                   (g/expt Z+1 3))
            V (g/* (g/expt X+1 2)
                   (g/expt X+Y 5)
                   (g/expt Y+Z 3)
                   (g/expt X+Y+Z 5)
                   (g/expt Y+1 2)
                   (g/expt Z+1 1)
                   X+1)
            G (g/* (g/expt X+1 3)
                   (g/expt X+Y 2)
                   (g/expt Y+Z 3)
                   (g/expt X+Y+Z 4)
                   (g/expt Y+1 2)
                   Z+1)]
        (is (= [(g/* (g/expt Y+Z 2)
                     (g/expt Y+1 2)
                     (g/expt Z+1 2))
                0]
               (p/divide U G)))
        (is (= [(g/* (g/expt X+Y 3) X+Y+Z) 0]
               (p/divide V G)))

        (is (= X+Z (g/gcd (g/* X+Y X+Z)
                          (g/* Y+Z X+Z))))

        (is (= (g/* X+Z X+Y+Z)
               (g/gcd (g/* X+Y X+Z X+Y+Z)
                      (g/* X+Z X+Y+Z Y+Z))))

        (is (= (g/* X+Z (g/* X+Z X+Y))
               (g/gcd (g/* X+Z X+Z X+Y X+Y+Z Y+1)
                      (g/* X+Z X+Z X+Y X+1 Z+1 X+Z))))

        (is (= G (g/gcd U V))))))

  (testing "modular polynomial reduction"
    (let [A (p/make [-360 -171 145 25 1])
          B (p/make [-15 -14 -1 15 14 1])
          Z5 #(mi/make % 5)
          A:Z5 (p/map-coefficients Z5 A)
          B:Z5 (p/map-coefficients Z5 B)
          G5 (g/gcd A:Z5 B:Z5)]
      (is (= (p/make [(Z5 0) (Z5 -1) (Z5 0) (Z5 0) (Z5 1)]) A:Z5))
      (is (= (p/make [(Z5 0) (Z5 1) (Z5 -1) (Z5 0) (Z5 -1) (Z5 1)]) B:Z5))
      (is (= (p/make [(Z5 0) (Z5 -1) (Z5 0) (Z5 0) (Z5 1)]) G5)))))

(deftest simple-gcd-3
  (testing "GCD: arity 3 case"
    (let [[X Y Z] (p/new-variables 3)
          X+Y     (g/+ X Y)
          X+Z     (g/+ X Z)
          Y+Z     (g/+ Y Z)]
      (is (= X+Z (g/gcd
                  (g/* X+Y X+Z)
                  (g/* Y+Z X+Z))))
      (is (= 2 (g/gcd 2 (g/+ Z Z))))
      (is (= 2 (g/gcd (g/+ Z Z) 2)))
      (is (= 2 (g/gcd 2 (g/+ X X Z Z)))))))

(defn ->poly [x]
  (p/expression-> x (fn [p _] p)))

(defn gcd-test [name dx fx gx]
  (is (zero? (pg/gcd)))

  (let [d  (->poly dx)
        f  (->poly fx)
        g  (->poly gx)
        df (g/* d f)
        dg (g/* d g)
        sw (us/stopwatch)]
    (is (= df (pg/gcd df)))
    (is (= dg (pg/gcd dg)))
    (binding [pg/*poly-gcd-time-limit* [30 :seconds]
              pg/*poly-gcd-cache-enable* false]
      (is (= d (pg/gcd df dg))))
    (log/info "gcd-test" name (us/repr sw))))

(deftest ^:long ^:benchmark gjs
  (testing "GJS cases (see sparse-gcd.scm:666)"
    (let [d1 '(+ (expt x1 2) x1 3)
          f1 '(+ (* 2 (expt x1 2)) (* 2 x1) 1)
          g1 '(+ (expt x1 2) (* 2 x1) 2)]
      (gcd-test "D1" d1 f1 g1))

    (let [d2 '(+ (* 2 (expt x1 2) (expt x2 2))
                 (* x1 x2)
                 (* 2 x1))
          f2 '(+ (expt x2 2)
                 (* 2 (expt x1 2) x2)
                 (expt x1 2)
                 1)
          g2 '(+ (* (expt x1 2) (expt x2 2))
                 (* (expt x1 2) x2)
                 (* x1 x2)
                 (expt x1 2)
                 x1)]
      (gcd-test "D2" d2 f2 g2))

    (let [d3 '(+ (* x2 x2 x3 x3)
                 (* x2 x2 x3)
                 (* 2 x1 x1 x2 x3)
                 (* x1 x3))
          f3 '(+ (* x3 x3)
                 (* x2 x2 x3)
                 (* x1 x1 x2 x3)
                 (* x1 x3)
                 (* x1 x1 x2 x2))
          g3 '(+ (* x2 x3)
                 (* 2 x1 x3)
                 x3
                 x1)]
      (gcd-test "D3" d3 f3 g3))

    (let [d4 '(+ (* x1 x1 x4 x4)
                 (* x2 x2 x3 x4)
                 (* x1 x1 x2 x4)
                 (* x2 x4)
                 (* x1 x1 x2 x3))
          f4 '(+ (* x1 x2 x3 x3 x4 x4)
                 (* x1 x3 x3 x4 x4)
                 (* x1 x4 x4)
                 (* x4 x4)
                 (* x1 x3 x4))
          g4 '(+ (* x1 x3 x3 x4 x4)
                 (* x3 x3 x4 x4)
                 (* x4 x4)
                 (* x1 x2 x2 x3 x4)
                 (* x1 x2 x2))]
      (gcd-test "D4" d4 f4 g4))

    (let [d5 '(+ (* x1 x1 x1 x2 x2 x3 x3 x4 x5 x5)
                 (* x1 x2 x2 x5 x5)
                 (* x1 x1 x1 x3 x4 x4 x5)
                 (* x1 x1 x1 x2 x3 x3 x4 x5)
                 (* x1 x1 x2 x3 x3 x4 x4))
          f5 '(+ (* x1 x2 x2 x5 x5)
                 (* x1 x2 x3 x3 x4 x5)
                 (* x1 x2 x3 x3 x4 x4)
                 (* x1 x2 x2 x4 x4)
                 1)
          g5 '(+ (* x1 x3 x3 x4 x5 x5)
                 (* x2 x5 x5)
                 (* x1 x2 x4 x5)
                 (* x2 x5)
                 (* x1 x2 x3 x4 x4))]
      (gcd-test "D5" d5 f5 g5))

    ;; the following two are too big for our naive algorithm.
    (comment
      (let [d6 '(+ (* x1 x2 x4 x4 x5 x5 x6 x6)
                   (* x1 x2 x2 x3 x3 x4 x5 x5 x6 x6)
                   (* x1 x1 x3 x6 x6)
                   (* x1 x1 x2 x3 x3 x4 x5 x5 x6)
                   (* x1 x1 x3 x5 x6))
            f6 '(+ (* x1 x1 x2 x4 x5 x5 x6 x6)
                   (* x1 x3 x5 x5 x6 x6)
                   (* x1 x2 x2 x6 x6)
                   (* x1 x1 x2 x2 x3 x3 x5 x6)
                   (* x1 x3 x3 x4 x5))
            g6 '(+ (* x2 x2 x3 x3 x4 x5 x5 x6)
                   (* x1 x4 x4 x5 x6)
                   (* x2 x2 x3 x3 x4 x5 x6)
                   (* x1 x2 x2 x3 x4 x4 x6)
                   (* x1 x1 x3 x5 x5))]
        (gcd-test "D6" d6 f6 g6)))

    (comment
      (let [d7 '(+ (* x1 x2 x2 x4 x4 x6 x6 x7 x7)
                   (* x1 x1 x3 x4 x6 x6 x7 x7)
                   (* x3 x3 x4 x4 x7 x7)
                   (* x1 x1 x2 x4 x4 x6)
                   (* x3 x4 x5 x5))
            f7 '(+ (* x1 x1 x2 x4 x4 x5 x6 x6 x7 x7)
                   (* x1 x2 x3 x6 x7)
                   (* x3 x4 x4 x5 x5 x7)
                   (* x1 x1 x2 x3 x4 x4 x5 x6))
            g7 '(+ (* x1 x3 x5 x6 x6 x7 x7)
                   (* x2 x2 x3 x3 x4 x4 x5 x6 x7 x7)
                   (* x4 x6 x7 x7)
                   (* x1 x1 x2 x3 x5 x6 x7)
                   (* x1 x1 x3 x3 x4 x5 x5))]
        (gcd-test "D7" d7 f7 g7)))
    (pg/gcd-stats)))

(deftest big-gcd
  (let [u (p/make 10 [[[0 0 1 0 0 0 1 1 0 1] 1]
                      [[0 1 1 0 0 0 1 1 1 0] 2]
                      [[0 0 1 2 0 1 0 1 0 1] 1]
                      [[0 0 1 2 1 0 0 1 0 1] -1]
                      [[1 1 0 1 0 1 0 2 0 0] 1]
                      [[1 1 0 1 1 0 0 2 0 0] -1]
                      [[2 0 1 0 0 1 0 1 0 1] -1]
                      [[2 0 1 0 1 0 0 1 0 1] 1]
                      [[0 1 1 2 0 1 0 1 1 0] -2]
                      [[1 0 2 1 0 1 0 0 1 1] 2]
                      [[1 0 2 1 1 0 0 0 1 1] -2]])
        v (p/make 10 [[[0 0 1 4 1 1 0 0 0 0] 1]
                      [[2 0 1 2 1 1 0 0 0 0] 2]
                      [[4 0 1 0 1 1 0 0 0 0] 1]])
        t (fn []
            (let [g (g/gcd u v)]
              (pg/gcd-stats)
              g))]
    (is (= (p/make 10 [[[0 0 0 0 0 0 0 0 0 0] 1]]) (t)))
    ;; for profiling
    (binding [pg/*poly-gcd-cache-enable* false]
      (dotimes [_ 0] (t)))))

(deftest ^:long troublesome-gcd
  (let [u (p/make 10 [[[0 1 1 2 1 0 1 1 0 1] -1]
                      [[2 1 1 0 0 1 1 1 0 1] -1]
                      [[0 2 1 2 1 0 1 1 1 0] -2]
                      [[1 1 2 1 0 1 1 0 1 1] 1]
                      [[1 1 2 1 1 0 1 0 1 1] -1]
                      [[2 2 1 0 0 1 1 1 1 0] -2]
                      [[0 1 1 4 1 1 0 1 0 1] -1]
                      [[0 1 1 4 2 0 0 1 0 1] 1]
                      [[1 2 0 3 1 1 0 2 0 0] -1]
                      [[1 2 0 3 2 0 0 2 0 0] 1]
                      [[1 2 2 1 0 1 1 0 2 0] 1]
                      [[1 2 2 1 1 0 1 0 2 0] -1]
                      [[2 1 1 2 0 2 0 1 0 1] 1]
                      [[2 1 1 2 1 1 0 1 0 1] -2]
                      [[2 1 1 2 2 0 0 1 0 1] 1]
                      [[3 2 0 1 0 2 0 2 0 0] -1]
                      [[3 2 0 1 1 1 0 2 0 0] 1]
                      [[4 1 1 0 0 2 0 1 0 1] 1]
                      [[4 1 1 0 1 1 0 1 0 1] -1]
                      [[0 2 1 4 1 1 0 1 1 0] 2]
                      [[1 1 2 3 0 2 0 0 1 1] -1]
                      [[1 1 2 3 2 0 0 0 1 1] 1]
                      [[2 2 1 2 0 2 0 1 1 0] 2]
                      [[2 2 1 2 2 0 0 1 1 0] 2]
                      [[3 1 2 1 0 2 0 0 1 1] -1]
                      [[3 1 2 1 2 0 0 0 1 1] 1]
                      [[4 2 1 0 1 1 0 1 1 0] 2]
                      [[1 2 2 3 0 2 0 0 2 0] -1]
                      [[1 2 2 3 1 1 0 0 2 0] 1]
                      [[3 2 2 1 1 1 0 0 2 0] -1]
                      [[3 2 2 1 2 0 0 0 2 0] 1]])
        v (p/make 10 [[[0 0 1 4 1 1 0 0 0 0] 1]
                      [[2 0 1 2 1 1 0 0 0 0] 2]
                      [[4 0 1 0 1 1 0 0 0 0] 1]])]
    (is (= (p/constant 10 1)
           (g/gcd u v)))))

(deftest ^:benchmark kuniaki-tsuji-examples
  ;; (only have a few of these, will add more)
  ;;
  ;; TODO add more?
  ;; http://www.sciencedirect.com/science/article/pii/S0747717108001016
  (testing "ex1"
    (let [d '(+ (* x x) (* 2 (expt y 23) (expt z 24)))
          p '(+ (* x x) (* (expt y 23) (expt z 22)))
          q '(+ (* y z (expt x 3))
                (* 2 (expt z 35) (expt y 41) (expt x 2))
                (* (expt z 3) (expt y 5) x)
                525)]
      (gcd-test "K1" d p q)))

  (testing "ex2"
    ;; a hack, using (expt z 0) to make the arities equal
    (let [d '(+ (expt x 3)
                (* 2 (expt z 0)
                   (+ (expt y 34) (expt y 75))
                   (expt x 2)) (expt y 84))
          p '(+ (expt x 2)
                (* (expt y 53)
                   (expt z 62)))
          q '(+ (* y (expt x 8))
                (* 2 (expt y 47) (expt x 6))
                (* (+ (expt y 20) (expt y 14))
                   (expt x 4))
                (* (expt y 69) (expt x 3))
                (* (expt y 55) (expt x 2))
                (expt y 99)
                (* (expt y 45) x)
                54
                (expt z 0))]
      (gcd-test "K2" d p q))))

(deftest ^:benchmark some-interesting-small-examples
  ;; Clojure.test.check's awesome problem-shrinking feature found some small
  ;; examples of polynomials whose GCD is difficult to compute with this
  ;; code (at the time of this writing). Recording them here as they should
  ;; provide excellent examples for performance experiments.
  (testing "ex1"
    (let [u (p/make 3 {[0 0 0] -1, [0 0 3] 1})
          v (p/make 3 {[0 0 0] 1, [2 3 0] 2, [0 8 1] 1, [7 0 5] -1})]
      (gcd-test "S1" (p/make 3 {[0 0 0] 1}) u v)))

  (testing "ex2"
    (let [u (p/make 2 {[0 0] -1
                       [0 7] -1})
          v (p/make 2 {[0 0] 1
                       [0 1] -1
                       [0 4] 1
                       [3 3] -11
                       [1 9] 8
                       [8 5] -9
                       [12 1] 1})]
      (gcd-test "S2" 1 u v)))

  (testing "example 3: this stresses => bigint conversion in pseudo-remainder."
    (let [u (p/make 1 {{} 1 {0 3} 1})
          v (p/make 1 {{} 1
                       {0 1} (u/long 21)})
          d (p/make 1 {{} (u/long 4571)
                       {0 1} (u/long 597)})
          ud (g/* u d)
          vd (g/* v d)
          g (g/gcd ud vd)]
      (is (g/exact-divide ud g))
      (is (g/exact-divide vd g))
      (is (g/exact-divide g d)))))

;; Currently we only do GCD testing of univariate polynomials, because we find
;; that unfortunately clojure.test.check is very good at finding polynomials
;; even of arity 2 that will exceed the time allotment for finding GCDs.
;; Hopefully we can fix that, but for the present this explains why we draw
;; arities from the singleton set [1].

(def num-tests 20)

(deftest gcd-laws
  (checking "g-divides-u-and-v" num-tests
            [[u v] (gen/let [arity (gen/elements [1])]
                     (gen/tuple (sg/polynomial :arity arity)
                                (sg/polynomial :arity arity)))]
            (let [g (g/gcd u v)]
              (is (or (and (v/zero? u)
                           (v/zero? v)
                           (v/zero? g))
                      (and (g/exact-divide u g)
                           (g/exact-divide v g))))))

  (checking "d-divides-gcd-ud-vd" num-tests
            [[u v d] (gen/let [arity (gen/elements [1])]
                       (gen/tuple (sg/polynomial :arity arity)
                                  (sg/polynomial :arity arity)
                                  (sg/polynomial :arity arity)))]
            (let [ud (g/* u d)
                  vd (g/* v d)
                  g (g/gcd ud vd)]
              (is (g/exact-divide ud g))
              (is (g/exact-divide vd g))
              (is (g/exact-divide g d)))))
