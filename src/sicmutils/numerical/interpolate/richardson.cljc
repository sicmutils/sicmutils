;;
;; Copyright © 2017 Colin Smith.
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

(ns sicmutils.numerical.interpolate.richardson
  "Richardson interpolation. Super cool to talk about how this is EQUIVALENT to
  interpolating a polynomial! http://www.cs.rpi.edu/~flaherje/pdf/ode4.pdf TODO
  this is a great resource."
  (:require [sicmutils.numerical.interpolate.polynomial :as ip]
            [sicmutils.generic :as g]
            [sicmutils.util :as u]
            [sicmutils.util.aggregate :as ua]
            [sicmutils.util.stream :as us]
            [sicmutils.value :as v]))

;; ## Richardson Interpolation
;;
;; This is polynomial interpolation, when we can make some assumptions about the
;; spacing, or ratios, between successive points in the sequence.

;; So next they move on to something interesting, how to kill the error term...
;; and then on to "Richardson extrapolation":
;; https://en.wikipedia.org/wiki/Richardson_extrapolation

;; Another post on Richardson extrapolation:
;; https://calculus7.org/2015/12/27/richardson-extrapolation-and-midpoint-rule/
;; where... apparently it's called Milne's rule if you apply richardson to the
;; midpoint rule.
;;
;; Great... now, we ACCELERATE this, by using Richardson extrapolation:

(defn- accelerate-sequence
  "`xs` is a sequence of evaluations of some function of `A(h/t)` each time."
  [xs t p]
  (let [t**p (Math/pow t p)]
    (map (fn [fl fr]
           (/ (- (* t**p fr) fl)
              (dec t**p)))
         xs
         (rest xs))))

(defn- make-tableau
  "This makes the assumption that the exponent increases arithmetically... ie, you
  can generate it with an initial `p` and then an increment `q`.

  TODO get into why it's called a tableau, and why you want the first terms, if
  you're specifically interested in doing Richardson extrapolation."
  [xs t p q]
  (iterate (fn [[xs p]]
             [(accelerate-sequence xs t p) (+ p q)])
           [xs p]))

(defn first-terms-of-tableau [tableau]
  (map (comp first first) tableau))

(defn richardson-sequence
  ([points t]
   (richardson-sequence points t 1 1))
  ([points t p q]
   (first-terms-of-tableau
    (make-tableau points t p q))))

(defn richardson-poly-sequence
  "Less efficient... but this shows that this is identical to a polynomial
  interpolation."
  ([points t] (richardson-poly-sequence points t 1 1))
  ([points t p q]
   (ip/tableau-fn (fn [fx] [p fx])
                  (fn [[p fl] [_ fr]]
                    (let [t**p (Math/pow t p)]
                      [(+ p q) (/ (- (* t**p fr) fl)
                                  (dec t**p))]))
                  (fn [row] (map second row))
                  points)))

(defn richardson-limit
  ([xs t p q]
   (richardson-limit xs t p q {}))
  ([xs t p q opts]
   (let [rs (richardson-sequence xs t p q)]
     (us/seq-limit rs opts))))
