#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.series-test
  (:require [clojure.test :refer [is deftest testing use-fixtures]]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]]
            [same :refer [ish? with-comparator] :include-macros true]
            [emmy.calculus.derivative]
            [emmy.function :as f]
            [emmy.generators :as sg]
            [emmy.generic :as g]
            [emmy.series :as s]
            [emmy.simplify :refer [hermetic-simplify-fixture]]
            [emmy.value :as v]))

(use-fixtures :each hermetic-simplify-fixture)

(defn check-series [series]
  (testing "v/kind"
    (if (s/power-series? series)
      (= ::s/power-series (v/kind series))
      (= ::s/series (v/kind series))))

  (testing "v/numerical?"
    (is (not (v/numerical? series))))

  (testing "v/exact?"
    (is (not (v/exact? series))))

  (testing "zero-like"
    (is (= (take 10 s/zero)
           (take 10 (g/* series (v/zero-like series))))))

  (testing "one-like"
    (is (= (take 10 series)
           (take 10 (g/* series (v/one-like series))))))

  (testing "identity-like"
    (let [id (if (s/power-series? series)
               (s/power-series* [0 1])
               (s/series* [0 1]))]
      (is (= (take 10 (g/* id series))
             (take 10 (g/* series (v/identity-like series))))
          "the identity-series is an identity on APPLICATION, not for
        multiplication with other series.")))

  (testing "meta / with-meta work")
  (testing "one? zero? identity? always return false (for now!)"
    (is (not (v/zero? (v/zero-like series))))
    (is (not (v/one? (v/one-like series))))
    (is (not (v/identity? (v/identity-like series)))))

  (checking "f/arity" 100 [v (sg/power-series sg/real)]
            (is (= [:exactly 1]
                   (f/arity v))
                "all power-series instances have arity == 1."))

  (testing "non-power series arity"
    (let [s (s/series* (cycle [g/add g/sub g/mul]))]
      (is (= [:exactly 2] (f/arity s))
          "arity only checks the first element.")

      (is (= [6 0 9 6 0 9 6 0 9]
             (take 9 (s 3 3)))
          "applying a series containing functions returns a new series with the
          results.")))

  (checking "simplify returns a series" 100
            [s (sg/series)]
            (is (s/series? (g/simplify s))))

  (checking "simplify returns a power series" 100
            [s (sg/power-series)]
            (is (s/series? (g/simplify s)))
            (is (s/power-series? (g/simplify s)))))

(deftest value-protocol-tests
  (testing "power series"
    (check-series s/sin-series)
    (check-series (s/power-series 1 2 3 4)))

  (testing "normal (non-power) series"
    (check-series (s/series 1 2 3 4)))

  (checking "identity-like power-series application" 100 [n sg/real]
            (is (= n (-> ((v/identity-like s/sin-series) n)
                         (s/sum 50)))
                "evaluating the identity series at `n` will return a series that sums to `n`.")))

(deftest generic-series-tests
  (checking "metadata arity of constructors works" 100
            [xs (gen/vector gen/nat)
             m (gen/map gen/keyword gen/nat)]
            (is (nil? (meta (s/->PowerSeries xs nil))))
            (is (= m (meta (s/->PowerSeries xs m))))

            (is (nil? (meta (s/->Series xs nil))))
            (is (= m (meta (s/->Series xs m)))))

  (checking "with-meta / meta for [[Series]], [[PowerSeries]]" 100
            [m (gen/map gen/keyword gen/any)
             s (gen/one-of [(sg/series)
                            (sg/power-series)])]
            (is (nil? (meta s))
                "meta is empty by default")

            (is (= m (meta (with-meta s m)))
                "with-meta works"))

  (let [Q (s/power-series 4)
        R (s/power-series 4 3)
        S (s/power-series 4 3 2)
        ones (s/generate (constantly 1))
        nats0 (s/generate identity)
        nats (s/generate inc)]

    (testing "series act as seqs"
      (is (= [4 0 0 0 0 0 0 0] (take 8 Q)))
      (is (= [4 3 0 0 0 0 0 0] (take 8 R)))
      (is (= [4 3 2 0 0 0 0 0] (take 8 S)))
      (is (= [0 1 2 3] (take 4 nats0)))
      (is (= 4 (nth nats0 4))))

    (testing "generating series"
      (is (= [0 1 4 9]
             (take 4 (s/generate g/square))))

      (is (= [0 2 6 12]
             (->> (g/+ nats0 (s/generate g/square))
                  (take 4)))))

    (testing "series addition"
      (is (= [8 6 2 0 0 0 0 0]
             (take 8 (g/+ R S))))

      (is (= [3 5 7 0 0 0 0 0]
             (take 8 (g/+ (s/series 1 2 3)
                          (s/series 2 3 4)))))

      (is (= [0 2 4 6 8]
             (take 5 (g/+ nats0 nats0))))

      (let [xs (s/series 1 2 3 4)]
        (is (= [11 2 3 4 0 0]
               (take 6 (g/+ xs 10))
               (take 6 (g/+ 10 xs))))))

    (testing "negate"
      (is (= [-1 -2 -3 -4 0 0 0]
             (take 7 (g/negate (s/series 1 2 3 4))))))

    (testing "series subtraction"
      (is (= [0 0 0 0 0]
             (take 5 (g/- nats0 nats0))))

      (is (= [-10 1 2 3 4]
             (take 5 (g/- nats0 10))))

      (is (= [10 -1 -2 -3 -4]
             (take 5 (g/- 10 nats0))))

      (is (= [-3 -6 -9 -12]
             (take 4 (g/negate (g/* nats 3)))))

      (is (= '(-3 -6 -9 -12)
             (take 4 (g/negate (g/* 3 nats))))))

    (testing "series multiplication"
      (is (= '(3 6 9 12) (take 4 (g/* 3 nats))))
      (is (= '(3 6 9 12) (take 4 (g/* nats 3))))
      (is (= [1 4 10 12 9 0 0]
             (g/simplify
              (take 7 (g/*
                       (s/power-series 1 2 3)
                       (s/power-series 1 2 3))))))
      (is (= [0 4 11 20 30 40 50 60 70 80]
             (take 10 (g/* nats0 (s/power-series 4 3 2 1)))))

      (testing "rational"
        (is (= [#sicm/ratio 17/4
                #sicm/ratio 7/2
                #sicm/ratio 11/4
                1]
               (take 4 (g/+ (g/* #sicm/ratio 1/4 nats) S)))))

      (is (v/= '(ε (* 2 ε) (* 3 ε) (* 4 ε))
               (g/simplify
                (take 4 (g/* nats 'ε)))))

      (is (v/= '(0 r (* 2 r) (* 3 r))
               (g/simplify
                (take 4 (g/* 'r nats0)))))

      (is (v/= '(ε (* 2 ε) (* 3 ε) (* 4 ε))
               (g/simplify
                (take 4 (g/* 'ε nats)))))

      (is (v/= '(0 m (* 2 m) (* 3 m))
               (g/simplify
                (take 4 (g/* 'm nats0))))))

    (testing "division"
      (let [series (s/series 0 0 0 4 3 2 1)]
        (is (= [1 0 0 0 0]
               (take 5 (g/div series series))
               (take 5 (g/solve-linear series series))
               (take 5 (g/solve-linear-right series series))))))

    (testing "constant division"
      (is (= [4 -8 4 0 0 0]
             (take 6 (g// 4 nats))))

      (let [divided    (g// 4 nats)
            seq-over-4 (g/invert divided)
            original   (g/* seq-over-4 4)]
        (is (= (take 5 nats)
               (take 5 original))
            "We can recover the nats as expected."))

      (is (= [1 2 3 4 5]
             (take 5 (g// (g/* nats 2) 2)))))

    (testing "series invert, solve linear"
      (let [series (s/->PowerSeries (iterate inc 3) nil)]
        (is (= (take 5 (g/invert series))
               (take 5 (g/div s/one series))
               (take 5 (g/solve-linear-right s/one series))
               (take 5 (g/solve-linear series s/one)))
            "invert(xs) matches div(1, xs)")

        (is (= [1 0 0 0 0]
               (take 5 (g/* series (g/invert series)))
               (take 5 (g/div series series))))))

    (testing "square"
      (is (= '(1 2 3 4 5 6)
             (take 6 (g/square ones)))))

    (testing "expt"
      (is (= (take 11 (s/binomial-series 10))
             (take 11 (g/expt (s/series 1 1) 10)))
          "(1 + x)^10 = binomial expansion"))

    (testing "series sqrt"
      (is (= [1 2 3 4 5 6]
             (take 6 (g/* (g/sqrt nats)
                          (g/sqrt nats)))))

      (let [xs (s/->PowerSeries (iterate inc 9) nil)]
        (is (= [9 10 11 12 13 14]
               (take 6 (g/* (g/sqrt xs)
                            (g/sqrt xs))))))

      (let [xs (s/->PowerSeries (concat [0 0] (iterate inc 9)) nil)]
        (is (= [0 0 9 10 11 12]
               (take 6 (g/* (g/sqrt xs)
                            (g/sqrt xs)))))))

    (testing "series derivative"
      (is (= [1 2 3 4 5 6] ;; 1 + 2x + 3x^2 + ...
             (take 6 (-> (s/generate (constantly 1))
                         (g/partial-derivative []))))))))

(deftest series-as-fn-tests
  (let [f (fn [i] #(g/* %1 %2 i))
        square-series (s/generate f ::s/series)
        nats (s/generate inc)]
    (testing "a series of fns is a fn too"
      (is (= [0 6 12 18 24 30]
             (take 6 (square-series 2 3)))))

    (testing "a series of fns is a fn too"
      (let [nats*index-series (-> (fn [i] (g/* i nats))
                                  (s/generate ::s/series))]
        (take 6 (nats*index-series 'x))))))

(deftest series-specific-tests
  (let [Q (s/power-series 4)
        R (s/power-series 4 3)
        S (s/power-series 4 3 2)
        ones (s/generate (constantly 1))
        nats0 (s/generate identity)
        nats (s/generate inc)]

    (testing "fmap"
      (is (= '(1 4 9 16) (take 4 (s/fmap g/square nats))))
      (is (s/power-series? (s/fmap g/square nats)))
      (let [s (s/fmap g/square (s/generate inc ::s/series))]
        (testing "fmap keeps the proper type"
          (is (not (s/power-series? s)))
          (is (s/series? s)))))

    (testing "->function"
      (let [ps (s/->function
                (s/series 1 2 3))]
        (is (= '(1 (* 2 x) (* 3 x x) 0 0)
               (v/freeze (take 5 (ps 'x))))
            "->function converts a series to a power series."))

      (testing "function->"
        (is (= '(+ (* (/ 1 362880) (expt x 9))
                   (* (/ -1 5040) (expt x 7))
                   (* (/ 1 120) (expt x 5))
                   (* (/ -1 6) (expt x 3))
                   x)
               (-> ((s/function-> g/sin) 'x)
                   (s/sum 10)
                   (g/simplify)
                   (v/freeze)))
            "power series expansion of g/sin around 0, evaluated at 'x")

        (is (= '(+ (* (/ 1 6) (expt dx 3) (exp a))
                   (* (/ 1 2) (expt dx 2) (exp a))
                   (* dx (exp a))
                   (exp a))
               (-> ((s/function-> g/exp 'a) 'dx)
                   (s/sum 3)
                   (g/simplify)
                   (v/freeze)))
            "power series expansion of g/exp around 'a, evaluated at 'dx")

        (is (= '(/ (+
                    (* (expt a 4) (atan a b))
                    (* 2 (expt a 2) (expt b 2) (atan a b))
                    (* (expt b 4) (atan a b))
                    (* -1 (expt a 3) db)
                    (* (expt a 2) b da)
                    (* (expt a 2) da db)
                    (* -1 a (expt b 2) db)
                    (* -1 a b (expt da 2))
                    (* a b (expt db 2))
                    (* (expt b 3) da)
                    (* -1 (expt b 2) da db))
                   (+ (expt a 4) (* 2 (expt a 2) (expt b 2)) (expt b 4)))
               (-> ((s/function-> g/atan 'a 'b) ['da 'db])
                   (s/sum 2)
                   (g/simplify)
                   (v/freeze)))
            "power series expansion of g/atan around 'a and 'b, evaluated
            at (the vector value) ['da 'db]. Shows that function-> can handle
            multiple arguments without a structural wrapping.")

        (letfn [(check [f n]
                  (is (= (take n (g/simplify
                                  ((f s/identity) 'x)))
                         (take n (g/simplify
                                  ((s/function-> f) 'x))))
                      "checking generic passed to function-> against an explicit
                      series constructed for that function."))]
          (check g/cos 10)
          (check g/sin 10)
          (check g/exp 10)))

      (let [i*nats (fn [_] nats)
            series (s/generate i*nats ::s/series)
            power-series (s/generate i*nats ::s/power-series)]
        (is (= [[1 2 3 4 5]
                [2 4 6 8 10]
                [4 8 12 16 20]
                [8 16 24 32 40]
                [16 32 48 64 80]]
               (map (fn [s] (take 5 s))
                    (take 5 (power-series 2))))
            "if you have a power series of functions that return power series,
            applying it to some value will return a series of series!")

        (is (= [1 5 17 49 129 321 769 1793 4097 9217]
               (take 10 (series 2)))
            "Applying a number to a `Series` of functions that return
        `series?`-true objects returns a running sum across the diagonal of the
        first test's return value.") ))

    (testing "inflate"
      (is (= [0 0 0 1 0]
             (take 5 (s/inflate s/identity 3))))

      (is (= [1 0 2 0 3 0]
             (take 6 (s/inflate (s/generate inc) 2)))))

    (testing "series composition"
      (is (= [1 0 1 0 1 0 1 0 1 0]
             (take 10 (s/compose
                       (s/generate (constantly 1))
                       (s/xpow 2))))
          "square all entries"))

    (testing "series reversion"
      (is (= [0 1 0 0 0]
             (take 5 (s/compose nats0 (s/revert nats0))))
          "compose with reversion gives identity")

      (is (= [0 1 -2 5 -14]
             (take 5 (s/revert nats0)))))

    (testing "series integral"
      (is (= [5 1 1 1 1 1]
             (take 6 (s/integral nats 5))))

      (is (= [0 1 1 1 1 1]
             (take 6 (s/integral nats)))
          "By default, constant is 0."))

    (testing "arg-scale on power series"
      (let [base (s/generate (fn [_] 1))
            scaled (s/arg-scale base [2])]
        (is (thrown? #?(:clj AssertionError :cljs js/Error)
                     (s/arg-scale base [2 3]))
            "multiple scale factors trigger an error.")

        (is (s/power-series? scaled))
        (is (= (g/simplify (take 10 (scaled 'x)))
               (g/simplify (take 10 (base (g/* 2 'x))))))))

    (testing "arg-shift on power series"
      (let [base (s/generate (fn [_] 1))
            shifted (s/arg-shift base [2])]
        (is (thrown? #?(:clj AssertionError :cljs js/Error)
                     (s/arg-shift base [2 3]))
            "multiple shifts trigger an error.")

        (is (fn? shifted))
        (is (= (g/simplify (take 10 (shifted 'x)))
               (g/simplify (take 10 (base (g/+ 2 'x))))))))

    (testing "summing N elements of a series"
      (is (= 4 (s/sum S 0)))
      (is (= 7 (s/sum S 1)))
      (is (= 9 (s/sum S 2)))
      (is (= 7 (s/sum R 8)))
      (is (= 4 (s/sum Q 20)))
      (is (= 9 (s/sum S 3)))
      (is (= 9 (s/sum S 4))))

    (testing "partial-sums"
      (is (= '(1 2 3 4 5 6)
             (take 6 (s/partial-sums ones)))))

    (testing "miscellaneous"
      (is (= '(0 -2 -6 -12)
             (take 4 (g/negate
                      (g/+ nats0 (s/generate g/square)))))))

    (let [triangular (g/* ones nats)]
      (testing "triangular numbers https://en.wikipedia.org/wiki/Triangular_number"
        (is (= '(1 3 6 10 15 21)
               (take 6 triangular))
            "via convolution")

        (is (= '(1 3 6 10 15 21)
               (take 6 (s/partial-sums nats)))
            "via partial sums"))

      (testing "tetrahedral numbers https://en.wikipedia.org/wiki/Tetrahedral_number"
        (is (= '(1 4 10 20 35 56 84)
               (take 7 (g/square nats))))

        (is (v/= '(m (* 4 m) (* 10 m) (* 20 m))
                 (->> (s/generate inc)
                      (g/square)
                      (g/* 'm)
                      (take 4)
                      (g/simplify))))

        (is (= [1 4 10 20 35 56 84]
               (take 7 (s/partial-sums triangular)))
            "The tetrahedral numbers are the partial sums of the triangular
              numbers")))))

(deftest power-series-examples
  (testing "g/exp acts like composition"
    (with-comparator (v/within 1e-6)
      (ish? (Math/exp (Math/sin 2.2))
            (s/sum ((g/exp s/sin-series) 2.2) 50))))

  (testing "correct expansion of (1-2x^2)^3"
    (is (= [1 0 -6 0 12 0 -8 0 0 0]
           (take 10 (g/expt (s/series 1 0 -2) 3)))))

  (testing "power series of 1/(1-x)"
    (is (= (take 10 (repeat 1))
           (take 10 (g/invert (s/power-series 1 -1))))))

  (testing "deriv gives series of 1/(1-x)^2"
    (is (= (take 10 (iterate inc 1))
           (take 10 (g/invert
                     (g/square
                      (s/power-series 1 -1)))))))

  (testing "expansion of e^x"
    (is (= '(1
             1
             (/ 1 2)
             (/ 1 6)
             (/ 1 24)
             (/ 1 120)
             (/ 1 720)
             (/ 1 5040)
             (/ 1 40320)
             (/ 1 362880))
           (v/freeze (take 10 s/exp-series)))))

  (testing "sine expansion"
    (is (= '(0
             1
             0
             (/ -1 6)
             0
             (/ 1 120)
             0
             (/ -1 5040)
             0
             (/ 1 362880))
           (v/freeze (take 10 s/sin-series)))))

  (testing "cosine expansion"
    (is (= '(1
             0
             (/ -1 2)
             0
             (/ 1 24)
             0
             (/ -1 720)
             0
             (/ 1 40320)
             0)
           (v/freeze (take 10 s/cos-series)))))

  (testing "catalan numbers"
    (is (= [1 1 2 5 14 42 132 429 1430 4862]
           (take 10 s/catalan-series))))

  (testing "harmonic numbers"
    (is (= [1
            #sicm/ratio 3/2
            #sicm/ratio 11/6
            #sicm/ratio 25/12
            #sicm/ratio 137/60]
           (take 5 s/harmonic-series))))

  (testing "bell numbers"
    (is (= [1 2 5 15 52 203 877 4140 21147 115975]
           (take 10 s/bell-series)))))

(deftest series-identity-tests
  (is (->> (g/- s/sin-series
                (g/sqrt (g/- 1 (g/expt s/cos-series 2))))
           (take 30)
           (every? v/zero?))
      "sin(x) = sqrt(1-cos(x)^2) to 30 terms")

  (is (->> (g/- s/tan-series (s/revert s/atan-series))
           (take 30)
           (every? v/zero?))
      "tan(x) = revert(arctan(x))")

  (is (->> (g/- s/atan-series
                (s/integral
                 (g/invert (s/power-series 1 0 1))))
           (take 30)
           (every? v/zero?))
      "atan(x) = integral(1/(1+x^2))"))

(deftest series-trig-tests
  (testing "A few tests of various manipulations of the trig functions to flex a
  bit of the library."
    (is (= [0 1 0 0 0]
           (take 5 (g/sin s/asin-series))))
    (is (= [0 1 0 0 0]
           (take 5 (g/tan s/atan-series))))

    (is (= [0 1 0 0 0]
           (take 5 (g/sinh s/asinh-series))))
    (is (= [0 1 0 0 0]
           (take 5 (g/tanh s/atanh-series))))

    (is (= (take 20 s/sec-series)
           (take 20 (g/invert s/cos-series))))

    (is (= (take 20 s/tan-series)
           (take 20 (g/div s/sin-series s/cos-series))))))
