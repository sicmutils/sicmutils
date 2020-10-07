;;
;; Copyright © 2020 Sam Ritchie.
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

(ns sicmutils.numerical.interpolate.richardson-test
  (:require [clojure.test :refer [is deftest testing]]
            [same :refer [ish?]]
            [sicmutils.generic :as g]
            [sicmutils.numerical.interpolate.polynomial :as ip]
            [sicmutils.numerical.interpolate.richardson :as ir]
            [sicmutils.util.stream :as us]))

;; This is mostly ported from
;; https://dspace.mit.edu/bitstream/handle/1721.1/6060/AIM-997.pdf?sequence=2,
;; Abstraction in Numerical Methods from Sussman.

(defn refine-by-doubling [s]
  (/ s (g/sqrt (+ 2 (g/sqrt (- 4 (g/square s)))))))

(def side-lengths
  (iterate refine-by-doubling (g/sqrt 2)))

;; This is successive doublings of the number of sides of a polygon.

(def side-numbers
  (iterate (fn [x] (* 2 x)) 4))

(defn semi-perimeter [length-of-side number-of-sides]
  (* (/ number-of-sides 2) length-of-side))

(def archimedean-pi-sequence
  (map semi-perimeter side-lengths side-numbers))

;; This totally works... but takes 24 terms to reach full machine
;; precision. (TODO how do we know?)
;;
(deftest basics
  (is (ish? [2.8284271247461903
             3.1391475703122276
             3.1415903931299374
             3.141592653286045
             3.1415926535897865]
            (take 5 (ir/richardson-sequence archimedean-pi-sequence 4))))

  (is (= (take 7 (ir/richardson-sequence archimedean-pi-sequence 4))
         (take 7 (ir/richardson-poly-sequence archimedean-pi-sequence 4))))

  (is (ish? {:converged? true
             :terms-checked 4
             :result 3.141592653286045}
            (ir/richardson-limit archimedean-pi-sequence
                                 4 1 1 {:tolerance 1e-6}))))


;; ## Continuation
;;
;; TODO get this into the normal arena.
;;
;; okay... so that's the version from the paper. Let's get more general. Why do
;; it with these streams, by the way? Because you have to do this sequential
;; pulling game anyway. This... is pretty interesting. And there might be a way
;; to make the other bullshit way more elegant this way.
;;
;; This gives me the first 10 zeno-extrapolated terms. The KEY is to realize
;; that I'm...

;;  'The error of the Trapezoidal method can be expressed in terms of odd powers so
;;  that the error over multiple steps can be expressed in even powers; this leads
;;  us to raise $t$ to the second power and to take powers of $4=2^2=t^2$ in
;;  the pseudocode.' ~ https://en.wikipedia.org/wiki/Richardson_extrapolation

;;  If I do that, then I increase the error by a single term each time. AND that
;;  makes the derivation way easier, oolala. Suddenly, I'm doing a polynomial
;;  extrapolation on the SQUARE of the error term.

;; \"The leading error term in the second evaluation will be 1/4 the size of the
;; error in the first evaluation. Therefore the combination 4 blah... is a weighted
;; average, and works pretty well.

;;  Which matches almost totally what I was doing with rational function
;;  extrapolation.

(deftest final-t
  (let [expected [1 0.3333333333333333 0.15555555555555553 0.07654320987654319
                  0.038121520213023466 0.019042127887443784]]
    (is (ish? expected
              (let [points (us/zeno 2.0)]
                (->> (#'ir/make-tableau points 4 1 1)
                     (ir/first-terms-of-tableau)
                     (take 6)))))

    (let [points (map (juxt g/square identity) (us/zeno 2.0))
          fold (ip/modified-neville-fold 0.0)
          scan (ip/modified-neville-scan 0.0)]
      (is (ish? expected (fold (reverse (take 6 points))))
          "folding the reversed first six points works too.")

      (is (ish? expected (take 6 (map last (scan points))))
          "the FINAL elements of the scan build up! Laziness still allowed.")

      (is (ish? expected (take 6 (ip/modified-neville points 0.0)))
          "the column-wise version feels more natural."))))

(comment
  "


"




  )
