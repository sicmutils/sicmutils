;
; Copyright © 2017 Colin Smith.
; This work is based on the Scmutils system of MIT/GNU Scheme:
; Copyright © 2002 Massachusetts Institute of Technology
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

(ns sicmutils.euclid)

(defn gcd
  [m n]
  (let [r (mod m n)]
    (if (zero? r) (if (neg? n) (- n) n)
        (recur n r))))

(defn extended-gcd
  "The extended Euclidean algorithm
  (see http://en.wikipedia.org/wiki/Extended_Euclidean_algorithm)
  Returns a list containing the GCD and the Bézout coefficients
  corresponding to the inputs."
  [m n]
  (loop [a' 1
         b 1
         a 0
         b' 0
         c (if (neg? m) (- m) m)
         d (if (neg? n) (- n) n)]
    (let [q (quot c d)
          r (mod c d)]
      (if (zero? r) [d a b]
          (recur a (- b' (* q b)) (- a' (* q a)) b d r)))))

(defn modular-inverse
  [p a]
  (second (extended-gcd a p)))

(defn lcm
  [a b]
  (/ (*' a b) (gcd a b)))
