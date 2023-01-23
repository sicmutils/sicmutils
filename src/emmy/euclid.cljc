#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.euclid
  "Implementations of various [greatest common
  divisor](https://en.wikipedia.org/wiki/Greatest_common_divisor) algorithms."
  (:require [emmy.generic :as g]
            [emmy.value :as v]))

(defn extended-gcd
  "Returns a vector containing the [greatest common
  divisor](https://en.wikipedia.org/wiki/Greatest_common_divisor) and
  the [Bézout coefficients](https://en.wikipedia.org/wiki/Bézout%27s_identity)
  corresponding to the inputs `a` and `b`.

  For more info, see the Wikipedia article on the [Extended Euclidean
  algorithm](http://en.wikipedia.org/wiki/Extended_Euclidean_algorithm)."
  [a b]
  (cond (v/zero? a) [(g/abs b) 0 1]
        (v/zero? b) [(g/abs a) 1 0]
        :else (loop [s 0 s0 1 t 1 t0 0 r (g/abs b) r0 (g/abs a)]
                (if (v/zero? r)
                  [r0 s0 t0]
                  (let [q (g/quotient r0 r)]
                    (recur (g/- s0 (g/* q s)) s
                           (g/- t0 (g/* q t)) t
                           (g/- r0 (g/* q r)) r))))))

(defn gcd
  "Returns the [greatest common
  divisor](https://en.wikipedia.org/wiki/Greatest_common_divisor) of the two
  inputs `a` and `b`."
  [a b]
  (cond (v/zero? a) (g/abs b)
        (v/zero? b) (g/abs a)

        (or (v/= a b) (v/= a (g/negate b)))
        (g/abs a)

        (not (and (v/integral? a) (v/integral? b))) 1
        :else (loop [a (g/abs a) b (g/abs b)]
                (if (v/zero? b)
                  a
                  (recur b (g/remainder a b))))))

;; multimethod implementation for basic numeric types.

(defmethod g/gcd :default [a b]
  (gcd a b))
