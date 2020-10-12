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

(ns sicmutils.numerical.quadrature.trapezoid
  "Trapezoid method."
  (:require [sicmutils.numerical.quadrature.riemann :as qr]
            [sicmutils.numerical.interpolate.richardson :as ir]
            [sicmutils.function :as f]
            [sicmutils.generic :as g]
            [sicmutils.util :as u]
            [sicmutils.util.aggregate :as ua]
            [sicmutils.util.stream :as us]))

;; ## The Trapezoid Method
;;
;; We can do some algebra and save ourselves some computation in the trapezoid
;; method. Here's a nice function that gets more efficient, for any N.

(defn single-trapezoid [f a b]
  (* (/ (+ (f a) (f b)) 2.0)
     (- b a)))

;; What's the area of a trapezoid? Two points, `(a, f(a))` and `(b, f(b))`. Add
;; the area together for the

;; - lower square
;; - upper triangle

;; the lower square area is

;; $$(b - a) f(a)$$.

;; The upper triangle is 0.5 base * height:

;; $$ {1 \\over 2} (b - a) (f(b) - f(a))$$

;; Add these together to get:

;; $${1 \\over 2} {(b - a) (f(a) + f(b))}$$

;; This is true because the square area is:
;;
;; $$(- b a) f(a)$$
;;
;; and the triangle is:
;;
;; $${1 \over 2} (- b a) (f(b) - f(a))$$
;;
;; We can verify that adding these two simplifies to our trapezoid rule:

#_
(let [f (f/literal-function 'f)
      square    (g/* (f 'a)
                     (g/- 'b 'a))
      triangle  (g/* (g// 1 2)
                     (g/- 'b 'a)
                     (g/- (f 'b) (f 'a)))
      trapezoid (g/* (g// (g/+ (f a) (f b)) 2.0)
                     (g/- b a))]
  (zero?
   (g/simplify (g/- formula (g/+ square triangle)))))
;; => true

;; Full sum:

(defn- trapezoid-sum* [f a b]
  (qr/windowed-sum (partial single-trapezoid f)
                   a b))

;; In fact... the trapezoid rule is equal to the AVERAGE of the left and right
;; Riemann sums. You can see that in the equation, but lets verify:

#_
(let [f      (fn [x] (* x x))
      points (iterate inc 1)
      [a b]  [0 10]

      left-estimates  (map (@#'qr/left-sum f a b) points)
      right-estimates (map (@#'qr/right-sum f a b) points)
      midpoints       (map (trapezoid-sum f a b) points)]
  (map - (take 5 midpoints)
       (take 5 (map (fn [l r]
                      (/ (+ l r) 2))
                    left-estimates
                    right-estimates))))

;; Turns out we can definitely make the trapezoid calculation more efficient.
;;
;; This page https://en.wikipedia.org/wiki/Trapezoidal_rule makes it more clear
;; what it going on, and why we \"double\" the interior estimates. Though I want
;; to add the goods myself.
;;
;; We need to start adding interior trapezoids. The nice observation we can make
;; is that when we add them together, we're going to double-count the interior
;; sides.

;; SO, all we have to do is reserve that endpoint calculation... and then go add
;; the actual function evaluations all the way through.

(defn trapezoid-sum
  "More efficient version of trapezoid summation."
  [f a b]
  (fn [n]
    (let [h  (/ (- b a) n)
          fx (fn [i] (f (+ a (* i h))))]
      (* h (+ (/ (+ (f a) (f b)) 2)
              (ua/sum fx 1 n))))))

;; Then use this to estimate pi:

(def ^:private pi-estimator
  (let [f (fn [x] (/ 4 (+ 1 (* x x))))]
    (trapezoid-sum f 0.0 1.0)))

#_
(= 3.1399259889071587
   (pi-estimator 10))

#_
(= 3.1415926519231268
   (pi-estimator 10000))

;; Now we can follow
;; https://dspace.mit.edu/bitstream/handle/1721.1/6060/AIM-997.pdf?sequence=2
;; and make a SEQUENCE of estimates...

(defn- pi-estimator-sequence [n]
  (map pi-estimator
       (us/powers 2 n)))

#_
(->> (ir/richardson-sequence (pi-estimator-sequence 10) 2 2 2)
     (us/pprint 10))

;; 3.1399259889071587
;; 3.1415926529697855
;; 3.1415926536207928
;; 3.141592653589793
;; 3.141592653589794
;; ...

;; So this is pretty good! But not TOTALLY amazing... because we double every
;; time, we re-evaluate each grid point, just like we did in `riemann.cljc`.
;; Let's try to make an efficient upgrader...
;;
;; Boom, it totally works and re-uses the tricks from before.

(defn trapezoid-stream
  ([f a b] (trapezoid-stream f a b 1))
  ([f a b n0]
   (let [S      (trapezoid-sum f a b)
         next-S (qr/Sn->S2n f a b)]
     (if (number? n0)
       (qr/power-seq S next-S 2 n0)
       (qr/incrementalize S next-S 2 n0)))))

;; Final integrator interface:

(defn integrator
  ([f a b] (integrator f a b {}))
  ([f a b opts]
   (let [xs (trapezoid-stream f a b)]
     (-> (if (:accelerate? opts)
           (ir/richardson-sequence xs 2 2 2)
           xs)
         (us/seq-limit opts)))))

;; And we can check it on our previous example:

#_
(let [f (fn [x] (/ 4 (+ 1 (* x x))))]
  (= {:converged? true
      :terms-checked 13
      :result 3.141592643655686}
     (integrator f 0 1)))

;; Then accelerate:

#_
(let [f (fn [x] (/ 4 (+ 1 (* x x))))
      [counter f ] (u/counted f)]
  (= {:converged? true
      :terms-checked 6
      :result 3.141592653638244}
     (integrator f 0 1 {:accelerate? true}))
  @counter)
