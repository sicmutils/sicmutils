;;
;; Copyright © 2021 Sam Richie.
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

(ns sicmutils.statistics.moments)

;; When combining averages, if the counts sizes are too close we should use a
;; different algorithm. This constant defines how close the ratio of the smaller
;; to the total count can be:

(def ^:no-doc STABILITY-CONSTANT 0.1)

;; Given two streams of doubles (weightN, an) and (weightK, ak) of
;; form (weighted count, mean), calculates the mean of the combined stream.

;; Uses a more stable online algorithm which should be suitable for large
;; numbers of records similar to:
;; http://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Parallel_algorithm

(defn- get-combined-mean [weight-n an weight-k ak]
  (if (< weight-n weight-k)
    (recur weight-k ak weight-n an)
    (let [weight-sum (+ weight-n weight-k)]
      (cond (zero? weight-sum)      0.0
            (= weight-sum weight-n) an
            :else
            (let [scaling (/ weight-k weight-sum)]
              (if (< scaling STABILITY-CONSTANT)
                ;; a_n + (a_k - a_n)*(k/(n+k)) is only stable if n is not
                ;; approximately k
                (+ an (* (- ak an) scaling))
                (/ (+ (* weight-n an)
                      (* weight-k ak))
                   weight-sum)))))))

(def average-zero [0.0 0])

(defn average-fold
  "DOCS!"
  ([] average-zero)
  ([[avg _]] avg)
  ([[avg n] x]
   [(get-combined-mean n avg 1 x)
    (inc n)]))

(defn average-monoid
  "adds two averaged values together."
  ([] [0.0 0])
  ([[vl nl] [vr nr]]
   [(get-combined-mean nl vl nr vr)
    (+ nl nr)]))

;; ## Five Moments

(def moments-zero
  [0.0 0.0 0.0 0.0 0.0])

(defn- present-moments
  "TODO this or functions that return each?"
  [[n m1 m2 m3 m4 :as acc]]
  ;; if n == 1 return 0 for variance!
  (let [variance (/ m2 (dec n))]
    {:count n
     :mean m1
     ;; sample, not population, as written here.
     :variance variance
     :std-dev (Math/sqrt variance)
     :skewness (/ (* m3 (Math/sqrt n))
                  (Math/pow m2 1.5))
     :kurtosis (- (/ (* n m4)
                     (*  m2 m2))
                  3)}))

(defn moments-monoid
  "merges the five moments data structures."
  ([] moments-zero)
  ([[ln l-mean l2 l3 l4] [rn r-mean r2 r3 r4]]
   (let [count (+ ln rn)]
     (if (zero? count)
       moments-zero
       (let [delta    (- r-mean l-mean)
             delta_n  (/ delta count)
             delta_n2 (* delta_n delta_n)
             ln**2     (Math/pow ln 2)
             rn**2     (Math/pow rn 2)
             mean      (get-combined-mean ln l-mean rn r-mean)
             m2 (+ l2 r2 (* delta delta_n ln rn))
             m3 (+ l3 r3
                   (* delta delta_n2 ln rn (- ln rn))
                   (* 3 (- (* ln r2) (* rn l2)) delta_n))
             m4 (+ l4 r4
                   (* delta delta_n2 delta_n ln rn
                      (+ (- ln**2 (* ln rn)) rn**2))
                   (* 6 delta_n2 (+ (* ln**2 r2)
                                    (* rn**2 l2)))
                   (* 4 delta_n (- (* ln r3) (* rn l3))))]
         [count mean m2 m3 m4])))))

(defn moments-fold
  "merges new doubles into the five moments."
  ([] moments-zero)
  ([acc] (present-moments acc))
  ([[n mean m2 m3 m4] x]
   (let [count (inc n)]
     (let [n'       (inc n)
           delta    (- x mean)
           delta_n  (/ delta n')
           delta_n2 (* delta_n delta_n)
           term1    (* delta delta_n n)
           mean'    (get-combined-mean n mean 1.0 x)
           m4' (+ m4
                  (* term1 delta_n2 (+ (- (* n' n') (* 3 n'))
                                       3))
                  (- (* 6 delta_n2 m2)
                     (* 4 delta_n m3)))
           m3' (+ m3 (- (* term1 delta_n (- n' 2)) (* 3 delta_n m2)))
           m2' (+ m2 term1)]
       [n' mean' m2' m3' m4']))))

(def variance-example
  [(+ 10e9 4) (+ 10e9 7) (+ 10e9 13) (+ 10e9 16)])
