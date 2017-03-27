;
; Copyright (C) 2016 Colin Smith.
; This work is based on the Scmutils system of MIT/GNU Scheme.
;
; This is free software;  you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3 of the License, or (at
; your option) any later version.
;
; This software is distributed in the hope that it will be useful, but
; WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
; General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this code; if not, see <http://www.gnu.org/licenses/>.
;

(ns sicmutils.euclid
  (:require [sicmutils.generic :as g]
            [sicmutils.value :as v]))

(defn extended-gcd
  "The extended Euclidean algorithm
  (see http://en.wikipedia.org/wiki/Extended_Euclidean_algorithm)
  Returns a list containing the GCD and the Bézout coefficients
  corresponding to the inputs."
  [a b]
  (cond (v/nullity? a) [(g/abs b) 0 1]
        (v/nullity? b) [(g/abs a) 1 0]
        :else (loop [s 0 s0 1 t 1 t0 0 r (g/abs b) r0 (g/abs a)]
                (if (v/nullity? r)
                  [r0 s0 t0]
                  (let [q (g/quotient r0 r)]
                    (recur (g/- s0 (g/* q s)) s
                           (g/- t0 (g/* q t)) t
                           (g/- r0 (g/* q r)) r))))))

(defn gcd
  [a b]
  (cond (v/nullity? a) (g/abs b)
        (v/nullity? b) (g/abs a)
        :else (loop [a a b b]
                (if (v/nullity? b)
                  a
                  (recur b (g/remainder a b))))))

(defn lcm
  [a b]
  (g/abs (g/divide (g/* a b) (gcd a b))))
