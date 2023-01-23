#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.numerical.quadrature.midpoint-test
  (:require [clojure.test :refer [is deftest testing]]
            [same :refer [ish?]]
            [emmy.numerical.quadrature.midpoint :as qm]
            [emmy.numerical.quadrature.riemann :as qr]
            [emmy.numsymb]
            [emmy.util :as u]))

(deftest midpoint-tests
  (testing "midpoint-sum does the job"
    (is (= (* 0.5 10 10)
           ((qr/midpoint-sum identity 0.0 10.0) 10))))

  (testing "incremental midpoint is more efficient, even with arbitrary
  sequences"
    (let [f (fn [x] (/ 4 (+ 1 (* x x))))
          [counter1 f1] (u/counted f)
          [counter2 f2] (u/counted f)
          n-seq (interleave
                 (iterate (fn [x] (* 2 x)) 2)
                 (iterate (fn [x] (* 2 x)) 3))]
      (is
       (ish? (take 12 (qm/midpoint-sequence f1 0 1 {:n n-seq}))
             (take 12 (map (qr/midpoint-sum f2 0 1) n-seq)))
       "The incremental and non-incremental versions of the midpoint method
       produce ~identical results.")

      (is (= [253 315]
             [@counter1 @counter2])
          "The incremental method requires many fewer evaluations."))))

(deftest api-tests
  (testing "midpoint integrator never evaluates the endpoints"
    (let [f (fn [x] (condp = x
                     0 (u/illegal "Zero!!")
                     1 (u/illegal "One!")
                     (/ 4 (+ 1 (* x x)))))]
      (is (ish? {:converged? true
                 :terms-checked 9
                 :result 3.141592655525674}
                (qm/integral f 0 1))
          "convergence happens fairly fast!")

      (is (ish? {:converged? true
                 :terms-checked 5
                 :result 3.141592653582428}
                (qm/integral f 0 1 {:accelerate? true}))
          "convergence happens faster when accelerated."))))
