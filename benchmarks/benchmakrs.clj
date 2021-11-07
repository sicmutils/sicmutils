(ns benchmarks
  (:refer-clojure :exclude [partial zero? + - * / ref])
  (:require [sicmutils.env :refer :all]
            [sicmutils.generic :as g]
            [criterium.core :as crit]
            [clojure.core.reducers :as reducers]))

(defn multivariate-argmin [f x]
  (let [g (D f)]
    (loop [x   x
           fx  (f x)
           gx  (g/transpose (g x))
           eta 1e-5
           i   0]
      (cond (<= (compare (g/abs gx) 1e-5) 0)
            x

            (= i 10)
            (recur x fx gx (g/* 2.0 eta) 0)

            :else
            (let [x-prime (g/- x (g/* eta gx))]
              (if (<= (compare (g/abs (- x x-prime)) 1e-5) 0)
                x
                (let [fx-prime (f x-prime)]
                  (if (< (compare fx-prime fx) 0)
                    (recur x-prime fx-prime (g/transpose (g x-prime)) eta (+ i 1))
                    (recur x fx gx (/ eta 2.0) 0)))))))))

(defn naive-euler [w]
  (let [charges      [[10.0 (- 10.0 w)] [10.0 0.0]]
        x-initial    [0.0 8.0]
        xdot-initial [0.75 0.0]
        delta-t      1e-1
        p (fn [x]
            (transduce (map (fn [c] (/ 1.0 (g/abs (- x c)))))
                       g/+
                       0.0
                       charges))]
    (loop [x    x-initial
           xdot xdot-initial]
      (let [[x' y' :as x-new] (g/+ x (g/* delta-t xdot))]
        (if (g/negative? y')
          (let [delta-t-f (g// (g/- 0.0 (second x))
                               (second xdot))
                x-t-f     (g/+ x (g/* delta-t-f xdot))]
            (g/square (first x-t-f)))
          (let [xddot (g/* -1.0 (g/transpose ((D p) x)))]
            (recur x-new (g/+ xdot (g/* delta-t xddot)))))))))

(defn multivariate-argmax [f x]
  (multivariate-argmin (fn [x] (g/- (f x))) x))

(defn multivariate-max [f x]
  (f (multivariate-argmax f x)))

(defn particle-FF []
  (multivariate-argmin naive-euler 0.0))

(defn saddle-FF []
  (let [start [1.0 1.0]
        f     (fn [[x1 y1] [x2 y2]]
                (- (+ (g/square x1) (g/square y1))
                   (+ (g/square x2) (g/square y2))))
        x1* (multivariate-argmin
             (fn [x1]
               (multivariate-max
                (fn [x2] (f x1 x2))
                start))
             start)
        x2* (multivariate-argmax
             (fn [x2] (f x1* x2)) start)]
    [x1* x2*]))

;; initial implementation of the backprop benchmark

(def xor-ws0
  [[[1.16054 -0.284227 0] [1.30467 0.617194 0]]
   [[0.648461 -0.084395 0]]])


(def xor-data
  [[0 0]
   [0 1]
   [1 0]
   [1 1]])

(def xor-data-with-targets
  [[[0 0] [0]]
   [[0 1] [1]]
   [[1 0] [1]]
   [[1 1] [0]]])

(defn sum-activities [activities]
  (fn [bias-ws]
    (let [bias (peek bias-ws)
          ws (pop bias-ws)]
      (g/+ bias (g/dot-product ws activities)))))

(defn sum-layer [activities ws-layer]
  (vec (map (sum-activities activities) ws-layer)))

(defn sigmoid [x]
  (map #(g// 1 (g/+ (g/exp (g/negate %)) 1)) x))

(defn forward-pass [ws-layers]
  (fn [input]
    (vec (flatten (map #(reducers/reduce sum-layer % ws-layers) input)))))

(defn error-on-dataset [dataset]
   (fn [ws-layers]
     (let [inputs (vec (map first dataset))
           predictions ((forward-pass ws-layers) inputs)
           targets (vec (flatten (map peek dataset)))]
       (* 0.5 (g/square (g/- predictions targets))))))

(crit/bench ((error-on-dataset xor-data-with-targets) xor-ws0))