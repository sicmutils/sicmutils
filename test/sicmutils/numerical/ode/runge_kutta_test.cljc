#_"SPDX-License-Identifier: GPL-3.0"


(ns sicmutils.numerical.ode.runge-kutta-test
  (:require [clojure.test :refer [is deftest testing]]
            [same :refer [ish? with-comparator] :include-macros true]
            [sicmutils.numerical.ode.runge-kutta :as rk]
            [sicmutils.structure :refer [up]]
            [sicmutils.value :as v]))

(deftest rk
  (let [sincos (fn [_x [y1 y2]] [y2 (- y1)])
        expo (fn [_x [y]] [y])]
    (testing "stream interface"
      (doseq [tolerance (map #(Math/pow 10 (- %)) (range 1 13))]
        (let [intervals (rk/dormand-prince-5 tolerance sincos 0 [0 1])
              xs (range 0 6.28 0.1)
              ys (rk/apply-solution-stream intervals xs)
              expected-ys (for [x xs] [x (up (Math/sin x) (Math/cos x))])]
          (with-comparator (v/within (* 3 tolerance))
            (is (ish? expected-ys ys) "sincos integrated values agree with known solution"))
          (let [ivals (take 20 intervals)
                z (map #(= (:x1 %1) (:x0 %2)) ivals (rest ivals))]
            (is (every? true? z) "solution intervals should be connected")))
        (let [intervals (rk/dormand-prince-5 tolerance expo 0 [1])
              xs (range 0 2 0.01)
              ys (rk/apply-solution-stream intervals xs)
              expected-ys (for [x xs] [x (up (Math/exp x))])]
          (with-comparator (v/within (* 4 tolerance))
            (is (ish? expected-ys ys) "exp integrated values agree with known solution")))))
    (testing "arenstorf orbit"
      (let [mu 0.012277471
            nu (- 1 mu)
            arenstorf (fn [_ [y1 y1p y2 y2p]]
                        (let [y2sq (Math/pow y2 2)
                              D1 (Math/pow
                                  (+ (Math/pow (+ y1 mu) 2) y2sq) 3/2)
                              D2 (Math/pow
                                  (+ (Math/pow (- y1 nu) 2) y2sq) 3/2)]
                          [y1p
                           (+ y1 (* 2 y2p) (* -1 nu (/ (+ y1 mu) D1)) (* -1 mu (/ (- y1 nu) D2)))
                           y2p
                           (- y2 (* 2 y1p) (* nu (/ y2 D1)) (* mu (/ y2 D2)))]))
            one-orbit 17.0652165601579625588917206249
            initial-state [0.994 0 0 -2.00158510637908252240537862224]
            intervals (rk/dormand-prince-5 1e-13 arenstorf 0 initial-state)
            ;; Run the orbit simulation through three whole orbits and 
            ;; verify that the satellite returns to the initial conditions (ish)
            ;; at the end of each expected orbital period
            test-points (for [i (range 1 4)] (* i one-orbit))
            rendezvous (rk/apply-solution-stream intervals test-points)
            ]
        (with-comparator (v/within 2e-4)
          (is (every? 
               #(ish? initial-state (nth % 1))
               rendezvous)
              "arenstorf orbit should be periodic"))))))