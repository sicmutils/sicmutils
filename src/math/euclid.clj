(ns math.euclid
  (:require [math.generic :as g]))

(defn extended-euclid
  "The extended Euclidean algorithm
  (see http://en.wikipedia.org/wiki/Extended_Euclidean_algorithm)"
  [a b]
  (loop [s 0 s0 1 t 1 t0 0 r b r0 a]
    (if (zero? r)
      [r0 s0 t0]
      (let [q (quot r0 r)]
        (recur (- s0 (* q s)) s
               (- t0 (* q t)) t
               (- r0 (* q r)) r)))))
