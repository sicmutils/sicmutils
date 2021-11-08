;;
;; Copyright © 2021 Adam Haber.
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

(ns benchmarks
  "This namespace contains various AD benchmarks, based on Sisnkind and Pearlmutter's work on Stalingrad.
   - An overview of the different benchmarks - https://engineering.purdue.edu/~qobi/papers/ad2008.pdf
   - Implementions of the benchmarks in different languages - https://engineering.purdue.edu/~qobi/stalingrad-examples2009/
   - NIPS 2017 presentation on the benchmarks - https://autodiff-workshop.github.io/slides/JeffreyMarkSiskind.pdf 
   Only forward AD benchmarks are implemented."
  (:refer-clojure :exclude [partial zero? + - * / ref])
  (:require [sicmutils.env :refer :all]
            [sicmutils.generic :as g]
            [criterium.core :as crit]
            [clojure.core.reducers :as reducers]))

(defn multivariate-argmin [f x]
  "A function which finds the multivariate argmin x of f.
   Implements a multivariate optimizer using adaptive naive gradient descent. 
   This iterates xi+1 = xi − η∇*f(xi) until either ∇f(x) or |xi+1 − xi| is small,
   increasing η when progress is made and decreasing η when no progress is made."
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
  "Naive Euler ODE integration of a particle's path.
   The charged particle is traveling non-relativistically in a plane with position x(t) 
   and velocity x'(t) and accelerated by an electric field formed by a pair of repulsive bodies
   at positions (10, 10-w) and (10, 0) where w is a modifiable control parameter of the system."
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
  "A function which finds the multivariate argmax x of f."
  (multivariate-argmin (fn [x] (g/- (f x))) x))

(defn multivariate-max [f x]
  "A function which finds the maximum of a multivariate function f."
  (f (multivariate-argmax f x)))

(defn particle-FF []
  "Finding a value for w that causes the particle’s path to intersect the origin."
  (multivariate-argmin naive-euler 0.0))

;; Evaluation count : 6 in 6 samples of 1 calls.
;;              Execution time mean : 28.701592 sec
;;     Execution time std-deviation : 77.929896 ms
;;    Execution time lower quantile : 28.600724 sec ( 2.5%)
;;    Execution time upper quantile : 28.769411 sec (97.5%)
;;                    Overhead used : 2.172762 ns
(crit/quick-bench (particle-FF))

(defn saddle-FF []
  "Computes a saddle point: min(x1,y1) max(x2,y2) f(x,y)
   for the function f(x,y)= (x1^2 + y1^2) - (x2^2 + y2^2)."
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

;; Evaluation count : 6 in 6 samples of 1 calls.
;;              Execution time mean : 11.263608 sec
;;     Execution time std-deviation : 27.629932 ms
;;    Execution time lower quantile : 11.226264 sec ( 2.5%)
;;    Execution time upper quantile : 11.297167 sec (97.5%)
;;                    Overhead used : 2.172762 ns
(crit/quick-bench (saddle-FF))

;; initial implementation of the backprop benchmark
(def xor-ws0
  "Initial weights and biases for a 1-hidden layer neural network."
  [[[1.16054 -0.284227 0] [1.30467 0.617194 0]]
   [[0.648461 -0.084395 0]]])

(def xor-data-with-targets
  "Inputs and outputs for a XOR function."
  [[[0 0] [0]]
   [[0 1] [1]]
   [[1 0] [1]]
   [[1 1] [0]]])

(def xor-data
  "Extracts a vectors of inputs from the XOR dataset"
  (mapv peek (mapv pop xor-data-with-targets)))

(defn sum-activities [activities]
  "Compute the activation of a neuron in the network given the inputs
   it receives, their weights and its bias."
  (fn [bias-ws]
    (let [bias (peek bias-ws)
          ws (pop bias-ws)]
      (g/+ bias (g/dot-product ws activities)))))

(defn sum-layer [activities ws-layer]
  "Compute the activations of all neurons in a given layer."
  (mapv (sum-activities activities) ws-layer))

(defn sigmoid [x]
  "Implements sigmoid activation function."
  (#(g// 1 (g/+ (g/exp (g/negate %)) 1)) x))

(defn sum-layer-with-nonlinearity [activities ws-layer]
  "Compose the non-linear function with the activations of all neurons in a given layer."
  (mapv sigmoid (mapv (sum-activities activities) ws-layer)))

(defn forward-pass [ws-layers]
  "Compute the outputs of a neural network to a given datasets"
  (fn [input]
    (vec (mapcat #(reducers/reduce sum-layer-with-nonlinearity % ws-layers) input))))

(defn error-on-dataset [dataset]
  "Compute the half of the sum of squares between the networks predictions and the targets."
   (fn [ws-layers]
     (let [inputs (mapv first dataset)
           predictions ((forward-pass ws-layers) inputs)
           targets (vec (flatten (map peek dataset)))]
       (* 0.5 (apply + (mapv g/square (g/- predictions targets)))))))

;; Evaluation count : 6756 in 6 samples of 1126 calls.
;;              Execution time mean : 90.044636 µs
;;     Execution time std-deviation : 1.265485 µs
;;    Execution time lower quantile : 88.763128 µs ( 2.5%)
;;    Execution time upper quantile : 91.422184 µs (97.5%)
;;                    Overhead used : 2.172762 ns
(crit/quick-bench ((error-on-dataset xor-data-with-targets) xor-ws0))