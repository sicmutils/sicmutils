#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.numerical.quadrature.riemann-test
  (:require [clojure.test :refer [is deftest testing]]
            [same :refer [ish?]]
            [emmy.generic :as g]
            [emmy.numbers]
            [emmy.numerical.quadrature.riemann :as qr]
            [emmy.polynomial.richardson :as pr]
            [emmy.util :as u]
            [emmy.util.aggregate :as ua]
            [emmy.util.stream :as us]
            [emmy.value :as v]))

(deftest windowed-sum-tests
  (testing "windowed-sum makes for inefficient integrals, but they're
  conceptually nice and simple."
    (let [area-fn   (fn [_l _r] 2)
          estimator (qr/windowed-sum area-fn 0 10)]
      (is (= 20.0 (estimator 10)) "10 blocks of 2 == 20.")
      (is (= 40.0 (estimator 20)) "20 blocks of 2 == 40.")))

  (testing "sum implementations with windowed-sum"
    (let [example-sum (fn [sum-fn f n]
                        (->> (us/powers 2)
                             (map (sum-fn f 0 10))
                             (take n)))]
      (is (ish? [0.0 125.0 218.75 273.4375 302.734375]
                (example-sum @#'qr/left-sum g/square 5))
          "Successively tighter left estimates.")

      (is (ish? [1000.0 625.0 468.75 398.4375 365.234375]
                (example-sum @#'qr/right-sum g/square 5))
          "Successively tighter right estimates.")

      (is (ish? (example-sum @#'qr/right-sum g/square 5)
                (example-sum @#'qr/upper-sum g/square 5))
          "for square, upper sum is always right.")

      (is (ish? (example-sum @#'qr/left-sum g/square 5)
                (example-sum @#'qr/lower-sum g/square 5))
          "for square, lower sum is always left.")

      (let [f (fn [x] (- (* x x)))]
        (is (ish? (example-sum @#'qr/left-sum f 5)
                  (example-sum @#'qr/upper-sum f 5))
            "left == upper if we negate square."))))

  (testing "slow convergence of bare fns"
    (let [sum (fn [sum-fn f]
                (map (sum-fn f 0 10)
                     (us/powers 2)))]
      (is (ish? {:converged? false
                 :terms-checked 16
                 :result 333.31807469949126}
                (-> (sum @#'qr/left-sum g/square)
                    (us/seq-limit {:maxterms 16})))
          "left-sum does not converge at 2^16 slices.")

      (is (ish? {:converged? false
                 :terms-checked 16
                 :result 333.34859227761626}
                (-> (sum @#'qr/right-sum g/square)
                    (us/seq-limit {:maxterms 16})))
          "right-sum does not converge at 2^16 slices."))))

(deftest riemann-sum-tests
  (is (ish? {:converged? true
             :terms-checked 4
             :result 333.3333333333333}

            (let [f (fn [x] (* x x))]
              (-> (map (@#'qr/left-sum f 0 10)
                       (us/powers 2))
                  (pr/richardson-sequence 2)
                  (us/seq-limit))))
      "Richardson extrapolation speeds up convergence.")

  (testing "incremental implementations"
    ;; left-sequence and right-sequence both internally rely on incremental
    ;; bumps for every even `n`.
    (let [f     g/square
          n-seq (take 20 (iterate inc 1))]
      (is (ish? (qr/left-sequence f 0 10 {:n n-seq})
                (map (@#'qr/left-sum f 0 10) n-seq))
          "Incremental implementation of left-sequence works.")

      (is (ish? (qr/right-sequence f 0 10 {:n n-seq})
                (map (@#'qr/right-sum f 0 10) n-seq))
          "Incremental implementation of right-sequence works.")

      (let [[counter1 f1] (u/counted g/square)
            [counter2 f2] (u/counted g/square)]

        ;; run each sequence fully through, counting the function invocations
        ;; required by each.
        (dorun (qr/left-sequence f1 0 10 {:n n-seq}))
        (run! (@#'qr/left-sum f2 0 10) n-seq)
        (is (= 210
               (int (ua/sum identity 1 21))
               @counter2)
            "The non-incremental version requires `n` evaluations for `n`
        windows. 20 items requires 1 + 2 + ... + 20 = 210.")

        (is (= 155
               (int (- (ua/sum identity 1 21)
                       (ua/sum (map #(/ % 2) (range 2 21 2)))))
               @counter1)
            "every EVEN evaluation from 2 => n saves half of its function
            evaluations. So the total is

 (1 + 2 + ... 20) - (2/2 + 4/2 + ... 20/2) = 155")))))

(deftest interface-tests
  (testing "left-integral"
    (is (ish?
         {:converged? true
          :terms-checked 14
          :result 2}
         (qr/left-integral
          g/sin 0 Math/PI {:accelerate? true
                           :tolerance v/machine-epsilon}))
        "left-integral converges for sin over 0 => pi.")

    (is (ish?
         {:converged? false
          :terms-checked 14
          :result 10.333533164162947}
         (qr/left-integral g/square 0 Math/PI {:maxterms 14}))
        "left-integral does very poorly, non-accelerated."))

  (testing "right-integral"
    (is (ish?
         {:converged? true
          :terms-checked 14
          :result 2}
         (qr/right-integral
          g/sin 0 Math/PI {:accelerate? true
                           :tolerance v/machine-epsilon}))
        "right-integral converges for sin over 0 => pi.")

    (is (ish?
         {:converged? true
          :terms-checked 15
          :result 1.9999999938721431}
         (qr/right-integral
          g/sin 0 Math/PI {:maxterms 20}))
        "right-integral does BETTER, converging to not quite the right
        answer, with no acceleration."))

  (testing "lower-integral"
    (is (ish?
         {:converged? true
          :terms-checked 2
          :result 0}
         (qr/lower-integral g/sin 0 Math/PI))
        "lower-integral breaks for this interval! BOTH of 1 and 2 have minimum
        endpoints of 0, so the sequence converges after checking the minimum of
        two terms.")

    (is (ish?
         {:converged? true
          :terms-checked 14
          :result 2}
         (qr/lower-integral
          g/sin 0 Math/PI {:accelerate? true
                           :minterms 3
                           :tolerance v/machine-epsilon}))
        "lower-integral converges for sin over 0 => pi when you force it to
        consider more than 3 terms."))

  (testing "upper-integral"
    (is (ish?
         {:converged? true
          :terms-checked 13
          :result 2}
         (qr/upper-integral
          g/sin 0 Math/PI {:accelerate? true
                           :tolerance v/machine-epsilon}))
        "upper-integral converges (at machine epsilon!)")))
