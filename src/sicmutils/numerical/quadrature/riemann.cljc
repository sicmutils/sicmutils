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

(ns sicmutils.numerical.quadrature.riemann
  (:require [sicmutils.util.aggregate :as ua]))

;; Riemann Quadrature
;;
;; First, let's talk about using the midpoint, etc methods...
;;
;; TODO whoops, multiply by the width of the box!!

(defn windowed-sum
  "This thing takes a function of two arguments, ie, that gets successive windows
  into the range $[a, b]$, and returns some aggregate value."
  [f a b]
  (fn [n]
    (let [grid-points (concat (range a b (/ (- b a) n))
                              (lazy-seq [b]))]
      (ua/sum
       (map f grid-points (rest grid-points))))))

(defn left-riemann-sum [f a b]
  (-> (fn [l r] (* (- r l) (f l)))
      (windowed-sum a b)))

(defn right-riemann-sum [f a b]
  (-> (fn [l r] (* (- r l) (f r)))
      (windowed-sum a b)))

(defn upper-riemann-sum [f a b]
  (-> (fn [l r] (* (- r l)
                  (max (f l) (f r))))
      (windowed-sum a b)))

(defn lower-riemann-sum [f a b]
  (-> (fn [l r] (* (- r l)
                  (min (f l) (f r))))
      (windowed-sum a b)))

(defn midpoint-rule [f a b]
  (letfn [(f-midpoint [l r]
            (let [half-range (/ (- r l) 2)
                  midpoint (+ l half-range)]
              (* (f midpoint)
                 (- r l))))]
    (windowed-sum f-midpoint a b)))

(defn trapezoid-rule [f a b]
  (letfn [(trapezoid-area [l r]
            (-> (+ (f l) (f r))
                (* (- r l))
                (/ 2)))]
    (windowed-sum trapezoid-area a b)))

;; TODO reference simpsons-38 rule, and boole's rule! AND show that these are just
;; sub-examples of the richardson thing.
;; http://www.learningclojure.com/2011/05/numerical-integration-better-rules.html

(defn simpson-rule
  "http://www.learningclojure.com/2011/05/numerical-integration-harder-functions.html

  the leading error term disappears if we take a weighted average of the
  midpoint and trapezoid rules...

  but we can show also that it's equivalent to the first richardson
  extrapolation."
  [f a b]
  (letfn [(simpson-area [l r]
            (let [half-range (/ (- r l) 2)
                  midpoint (+ l half-range)
                  sum      (+ (f l)
                              (* 4 (f midpoint))
                              (f r))]
              (/ (* sum (- r l)) 6)))]
    (windowed-sum simpson-area a b)))
