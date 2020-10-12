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
  (:require [sicmutils.numerical.interpolate.richardson :as ir]
            [sicmutils.generic :as g]
            [sicmutils.util :as u]
            [sicmutils.util.aggregate :as ua]
            [sicmutils.util.stream :as us]
            [sicmutils.numsymb]))

;; Riemann Quadrature
;;
;; This namespace includes functions for calculating the Riemann integral of a
;; single-variable function. These are /not/ methods that you'll want to
;; actually use; see the documentation and defaults in
;; `sicmutils.numerical.quadrature` for good recommendations. But they're clear
;; and understandable. The goal of this namespace is to lay the groundwork for
;; visualizable routines that you can use to step toward understanding of the
;; tougher methods.
;;
;; Each of the methods below works like this:
;;
;; - partition the area under the curve of some function $f$ into $n$ "slices"
;; - generate some area estimate for each slice
;; - add up all of the slices to form an estimate of the integral
;; - increase the number of slices, and stop when the estimate stops changing.
;;
;; The integral of a function $f$ is the limit of this process as $n \to
;; \infty$.
;;
;; `windowed-sum` gives us this behavior:

(defn windowed-sum
  "Takes:

  - `area-fn`, a function of the left and right endpoints of some integration
  slice
  - definite integration bounds `a` and `b`

  and returns a function of `n`, the number of slices to use for an integration
  estimate.

  `area-fn` should return an estimate of the area under some curve between the
  `l` and `r` bounds it receives."
  [area-fn a b]
  (fn [n]
    (let [width       (/ (- b a) n)
          grid-points (concat (range a b width)
                              (lazy-seq [b]))]
      (ua/sum
       (map area-fn grid-points (rest grid-points))))))

;; Test this out with a function that returns `2` for every slice, and we get
;; back an estimate (from the function returned by `windowed-sum`) of 2x the
;; number of slices:

#_
(let [area-fn   (fn [l r] 2)
      estimator (windowed-sum area-fn 0 10)]
  (and (= 20.0 (estimator 10))
       (= 40.0 (estimator 20))))
;; => true

;; Now, let's implement the four classic ["Riemann
;; Integral"](https://en.wikipedia.org/wiki/Riemann_integral) methods.
;;
;; Let's say we want to integrate a function $f$. The left and right Riemann
;; sums estimate a slice's area as a rectangle with:
;;
;; - width == $x_r - x_l$, and
;; - height == $f(x_l)$ or $f(x_r)$, respectively.
;;
;; `left-riemann-sum` is simple to implement, given `windowed-sum`:

(defn- left-riemann-sum
  "Returns a function of `n`, some number of slices of the total integration
  range, that returns an estimate for the definite integral of $f$ over the
  range $[a, b)$ using a left Riemann sum.

  "
  [f a b]
  (-> (fn [l r] (* (f l) (- r l)))
      (windowed-sum a b)))

;; `right-riemann-sum` is almost identical, except that it uses $f(x_r)$ as the
;; estimate of each rectangle's height:

(defn- right-riemann-sum
  "Returns a function of `n`, some number of slices of the total integration
  range, that returns an estimate for the definite integral of $f$ over the
  range $(a, b]$ using a right Riemann sum."
  [f a b]
  (-> (fn [l r] (* (f r) (- r l)))
      (windowed-sum a b)))

;; The upper Riemann sum generates a slice estimate by taking the maximum of
;; $f(x_l)$ and $f(x_r)$:

(defn- upper-riemann-sum
  "Returns an estimate for the definite integral of $f$ over the range $[a, b]$
  using an upper Riemann sum.

  This function may or may not make an evaluation at the endpoints $a$ or $b$,
  depending on whether or not the function is increasing or decreasing at the
  endpoints."
  [f a b]
  (-> (fn [l r] (* (- r l)
                  (max (f l) (f r))))
      (windowed-sum a b)))

;; Similarly, the lower Riemann sum uses the /minimum/ of $f(x_l)$ and $f(x_r)$:

(defn- lower-riemann-sum
  "Returns an estimate for the definite integral of $f$ over the range $[a, b]$
  using a lower Riemann sum.

  This function may or may not make an evaluation at the endpoints $a$ or $b$,
  depending on whether or not the function is increasing or decreasing at the
  endpoints."
  [f a b]
  (-> (fn [l r] (* (- r l)
                  (min (f l) (f r))))
      (windowed-sum a b)))

;; ## Sequence Acceleration
;;
;; Given the tools above, let's attempt to estimate the integral of $f(x) = x^2$
;; using the left and right Riemann sum methods. (The actual equation for the
;; integral is $x^3 \over 3$).
;;
;; The functions above return functions of `n`, the number of slices. We can
;; use `(us/powers 2)` to return a sequence of `(1, 2, 4, 8, ...)` and map the
;; function of `n` across this sequence to obtain successively better estimates
;; for $\int_0^{10} x^2$. The true value is $10^3 \over 3 = 333.333...$:

#_
(let [f              (fn [x] (* x x))
      left-estimates  (map (left-riemann-sum f 0 10)
                           (us/powers 2))
      right-estimates (map (right-riemann-sum f 0 10)
                           (us/powers 2))]
  (= [0 125 218.75 273.4375 302.734375]
     (take 5 left-estimates))

  (= [1000 625 468.75 398.4375 365.234375]
     (take 5 right-estimates)))

;; Both estimates are bad at 32 slices and don't seem to be getting better. Even
;; up to $2^16 = 65,536$ slices we haven't converged, and are still far from the
;; true estimate:

#_
(= {:converged? false
    :terms-checked 16
    :result 333.31807469949126}
   (let [f (fn [x] (* x x))]
     (-> (map (left-riemann-sum f 0 10)
              (us/powers 2))
         (us/seq-limit {:maxterms 16}))))

;; The answer to this problem is to use "sequence acceleration" via Richardson
;; acceleration, as described in `richardson.cljc`.
;;
;; `ir/richardson-sequence` takes a sequence of estimates of some function
;; and "accelerates" the sequence by combining successive estimates.
;;
;; The estimates have to be functions of some parameter $n$ that decreases by a
;; factor of $t$ for each new element. In the example above, $n$ doubles each
;; time; this is equivalent to thinking about the window width $h$ halving each
;; time, so $t = 2$.
;;
;; Does Richardson acceleration help?

#_
(= {:converged? true
    :terms-checked 4
    :result 333.3333333333333}

   (let [f (fn [x] (* x x))]
     (-> (map (left-riemann-sum f 0 10)
              (us/powers 2))
         (ir/richardson-sequence 2)
         (us/seq-limit))))

;; We now converge to the actual, true value of the integral in 4 terms!
;;
;; The results look quite nice; but notice how much redundant computation we're
;; doing. Every time we double our number of number of evaluations, half of the
;; windows share a left (or right) endpoint.

(defn- left-riemann-seq [f a b]
  (letfn [(next-S [[Sn n]]
            (let [next-n  (* 2 n)
                  half-h  (/ (+ a b) next-n)
                  offsets (left-riemann-sum f (+ a half-h) (+ b half-h))
                  Sn+1    (/ (+ Sn (offsets n)) 2)]
              [Sn+1 next-n]))]
    (->> (iterate next-S [((left-riemann-sum f a b) 1) 1])
         (map first))))

(defn- right-riemann-seq [f a b]
  (letfn [(next-S [[Sn n]]
            (let [next-n  (* 2 n)
                  half-h  (/ (+ a b) next-n)
                  offsets (right-riemann-sum f (- a half-h) (- b half-h))
                  Sn+1    (/ (+ Sn (offsets n)) 2)]
              [Sn+1 next-n]))]
    (->> (iterate next-S [((right-riemann-sum f a b) 1) 1])
         (map first))))

(defn accelerated-left
  ([f a b] (accelerated-left f a b {}))
  ([f a b opts]
   (-> (left-riemann-seq f a b)
       (ir/richardson-sequence 2)
       (us/seq-limit opts))))

(defn accelerated-right
  ([f a b] (accelerated-right f a b {}))
  ([f a b opts]
   (-> (right-riemann-seq f a b)
       (ir/richardson-sequence 2)
       (us/seq-limit opts))))

;; The same trick works for upper and lower, except we don't have a clever way
;; of combining previous evaluations.

(defn accelerated-upper
  ([f a b] (accelerated-upper f a b {}))
  ([f a b opts]
   (-> (map (upper-riemann-sum f a b)
            (us/powers 2))
       (ir/richardson-sequence 2)
       (us/seq-limit opts))))

(defn accelerated-lower
  ([f a b] (accelerated-lower f a b {}))
  ([f a b opts]
   (-> (map (lower-riemann-sum f a b)
            (us/powers 2))
       (ir/richardson-sequence 2)
       (us/seq-limit opts))))
