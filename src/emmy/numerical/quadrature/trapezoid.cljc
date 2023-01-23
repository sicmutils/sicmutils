#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.numerical.quadrature.trapezoid
  "Trapezoid method."
  (:require [emmy.abstract.function :as f]
            [emmy.generic :as g]
            [emmy.numerical.quadrature.common :as qc]
            [emmy.numerical.quadrature.riemann :as qr]
            [emmy.polynomial.richardson :as pr]
            [emmy.util :as u]
            [emmy.util.aggregate :as ua]))

;; ## The Trapezoid Method
;;
;; This namespace builds on the ideas introduced in `riemann.cljc` and
;; `midpoint.cljc`, and follows the pattern of those namespaces:
;;
;; - implement a simple, easy-to-understand version of the Trapezoid method
;; - make the computation more efficient
;; - write an incremental version that can reuse prior results
;; - wrap everything up behind a nice, exposed API
;;
;; Let's begin.
;;
;; ## Simple Implementation
;;
;; A nice integration scheme related to the Midpoint method is the "Trapezoid"
;; method. The idea here is to estimate the area of each slice by fitting a
;; trapezoid between the function values at the left and right sides of the
;; slice.
;;
;; Alternatively, you can think of drawing a line between $f(x_l)$ and $f(x_r)$
;; and taking the area under the line.
;;
;; What's the area of a trapezoid? The two slice endpoints are
;;
;; - $(x_l, f(x_l))$ and
;; - $(x_r, f(x_r))$
;;
;; The trapezoid consists of a lower rectangle and a capping triangle. The lower
;; rectangle's area is:
;;
;; $$(b - a) f(a)$$.
;;
;; Just like in the left Riemann sum. The upper triangle's area is one half base
;; times height:
;;
;; $$ {1 \over 2} (x_r - x_l) (f(x_r) - f(x_l))$$
;;
;; The sum of these simplifies to:
;;
;; $${1 \over 2} {(x_r - x_l) (f(x_l) + f(x_r))}$$
;;
;; Or, in Clojure:

(defn single-trapezoid [f xl xr]
  (g// (g/* (g/- xr xl)
            (g/+ (f xl) (f xr)))
       2))

;; We can use the symbolic algebra facilities in the library to show that this
;; simplification is valid:

(comment
  (let [f (f/literal-function 'f)
        square    (g/* (f 'x_l)
                       (g/- 'x_r 'x_l))
        triangle  (g/* (g// 1 2)
                       (g/- 'x_r 'x_l)
                       (g/- (f 'x_r) (f 'x_l)))]
    (zero?
     (g/simplify
      (g/- (single-trapezoid f 'x_l 'x_r)
           (g/+ square triangle))))))
;; => true

;; We can use `qr/windowed-sum` to turn this function into an (inefficient)
;; integrator:

(defn- trapezoid-sum* [f a b]
  (qr/windowed-sum (partial single-trapezoid f)
                   a b))

;; Fitting triangles is easy:

(comment
  (= (* 0.5 10 10)
     ((trapezoid-sum* identity 0.0 10.0) 10)))

;; In fact, we can even use our estimator to estimate $\pi$:

(def ^:private pi-estimator*
  (let [f (fn [x] (/ 4 (+ 1 (* x x))))]
    (trapezoid-sum* f 0.0 1.0)))

;; The accuracy is not bad, for 10 slices:

(comment
  (= 3.1399259889071587
     (pi-estimator* 10)))

(comment
  (- Math/PI (pi-estimator* 10)))
;; => 0.0016666646826344333

;; 10000 slices gets us closer:

(comment
  (< (- Math/PI (pi-estimator* 10000))
     1e-8))

;; Fun fact: the trapezoid method is equal to the /average/ of the left and
;; right Riemann sums. You can see that in the equation, but lets verify:

(defn- basically-identical? [l-seq r-seq]
  (every? #(< % 1e-15)
          (map - l-seq r-seq)))

(comment
  (let [points  (take 5 (iterate inc 1))
        average (fn [l r]
                  (/ (+ l r) 2))
        f       (fn [x] (/ 4 (+ 1 (* x x))))
        [a b]   [0 1]
        left-estimates  (qr/left-sequence f a b {:n points})
        right-estimates (qr/right-sequence f a b {:n points})]
    (basically-identical?
     (map (trapezoid-sum* f a b) points)
     (map average
          left-estimates
          right-estimates))))

;; ## Efficient Trapezoid Method
;;
;; Next let's attempt a more efficient implementation. Looking at
;; `single-trapezoid`, it's clear that each slice evaluates both of its
;; endpoints. This means that each point on a border between two slices earns a
;; contribution of $f(x) \over 2$ from each slice.
;;
;; A more efficient implementation would evaluate both endpoints once and then
;; sum (without halving) each interior point.
;;
;; This interior sum is identical to a left Riemann sum (without the $f(a)$
;; evaluation), or a right Riemann sum (without $f(b)$).
;;
;; Here is this idea implemented in Clojure:

(defn trapezoid-sum
  "Returns a function of `n`, some number of slices of the total integration
  range, that returns an estimate for the definite integral of $f$ over the
  range $(a, b)$ using the trapezoid method."
  [f a b]
  (let [width (- b a)]
    (fn [n]
      (let [h  (/ width n)
            fx (fn [i] (f (+ a (* i h))))]
        (* h (+ (/ (+ (f a) (f b)) 2)
                (ua/sum fx 1 n)))))))

;; We can define a new `pi-estimator` and check it against our less efficient
;; version:

(def ^:private pi-estimator
  (let [f (fn [x] (/ 4 (+ 1 (* x x))))]
    (trapezoid-sum* f 0.0 1.0)))

(comment
  (basically-identical?
   (map pi-estimator (range 1 100))
   (map pi-estimator* (range 1 100))))
;; => true

;; ## Incremental Trapezoid Rule
;;
;; Next let's develop an incremental updater for the Trapezoid rule that lets us
;; reuse evaluation points as we increase the number of slices.
;;
;; Because interior points of the Trapezoid method mirror the interior points of
;; the left and right Riemann sums, we can piggyback on the incremental
;; implementations for those two methods in developing an incremental Trapezoid
;; implementation.
;;
;; Consider the evaluation points of the trapezoid method with 2 slices, next to
;; the points of a 4 slice pass:
;;
;; x-------x-------x
;; x---x---x---x---x
;;
;; The new points are simply the /midpoints/ of the existing slices, just like
;; we had for the left (and right) Riemann sums. This means that we can reuse
;; `qr/Sn->S2n` in our definition of the incrementally-enabled
;; `trapezoid-sequence`:

(defn trapezoid-sequence
  "Returns a (lazy) sequence of successively refined estimates of the integral of
  `f` over the open interval $(a, b)$ using the Trapezoid method.

  ### Optional arguments:

  `:n`: If `:n` is a number, returns estimates with $n, 2n, 4n, ...$ slices,
  geometrically increasing by a factor of 2 with each estimate.

  If `:n` is a sequence, the resulting sequence will hold an estimate for each
  integer number of slices in that sequence.

  `:accelerate?`: if supplied (and `n` is a number), attempts to accelerate
  convergence using Richardson extrapolation. If `n` is a sequence this option
  is ignored."
  ([f a b] (trapezoid-sequence f a b {:n 1}))
  ([f a b {:keys [n accelerate?] :or {n 1}}]
   (let [S      (trapezoid-sum f a b)
         next-S (qr/Sn->S2n f a b)
         xs     (qr/incrementalize S next-S 2 n)]
     (if (and accelerate? (number? n))
       (pr/richardson-sequence xs 2 2 2)
       xs))))

;; The following example shows that for the sequence $1, 2, 4, 8, ..., 2^n$, the
;; incrementally-augmented `trapezoid-sequence` only performs $2^n + 1$ function
;; evaluations; ie, the same number of evaluations as the
;; non-incremental `(trapezoid-sum f2 0 1)` would perform for $2^n$ slices. (why
;; $2^n + 1$? each interior point is shared, so each trapezoid contributes one
;; evaluation, plus a final evaluation for the right side.)
;;
;; The example also shows that evaluating /every/ $n$ in the sequence costs
;; $\sum_{i=0}^n{2^i + 1} = 2^{n+1} + n$ evaluations. As $n$ gets large, this is
;; roughly twice what the incremental implementation costs.
;;
;; When $n=11$, the incremental implementation uses 2049 evaluations, while the
;; non-incremental takes 4017.

(comment
  (let [n-elements 11
        f (fn [x] (/ 4 (+ 1 (* x x))))
        [counter1 f1] (u/counted f)
        [counter2 f2] (u/counted f)
        [counter3 f3] (u/counted f)
        n-seq (take (inc n-elements)
                    (iterate (fn [x] (* 2 x)) 1))]
    ;; Incremental version evaluating every `n` in the sequence $1, 2, 4, ...$:
    (dorun (trapezoid-sequence f1 0 1 {:n n-seq}))

    ;; Non-incremental version evaluating every `n` in the sequence $1, 2, 4, ...$:
    (run! (trapezoid-sum f2 0 1) n-seq)

    ;; A single evaluation of the final `n`
    ((trapezoid-sum f3 0 1) (last n-seq))

    (let [two**n+1 (inc (g/expt 2 n-elements))
          n+2**n (+ n-elements (g/expt 2 (inc n-elements)))]
      (= [2049 4107 2049]
         [two**n+1 n+2**n two**n+1]
         [@counter1 @counter2 @counter3]))))
;; => true

;; Another short example that hints of work to come. The incremental
;; implementation is useful in cases where the sequence includes doublings
;; nested in among other values.
;;
;; For the sequence $2, 3, 4, 6, ...$ (used in the Bulirsch-Stoer method!), the
;; incrementally-augmented `trapezoid-sequence` only performs 162 function
;; evaluations, vs the 327 of the non-incremental
;; `(trapezoid-sum f2 0 1)` mapped across the points.
;;
;; This is a good bit more efficient than the Midpoint method's incremental
;; savings, since factors of 2 come up more often than factors of 3.

(comment
  (let [f (fn [x] (/ 4 (+ 1 (* x x))))
        [counter1 f1] (u/counted f)
        [counter2 f2] (u/counted f)
        n-seq (take 12 (interleave
                        (iterate (fn [x] (* 2 x)) 2)
                        (iterate (fn [x] (* 2 x)) 3)))]
    (dorun (trapezoid-sequence f1 0 1 {:n n-seq}))
    (run! (trapezoid-sum f2 0 1) n-seq)
    (= [162 327]
       [@counter1 @counter2])))

;; Final Trapezoid API:
;;
;; The final version is analogous the `qr/left-integral` and friends, including
;; an option to `:accelerate?` the final sequence with Richardson
;; extrapolation. (Accelerating the trapezoid method in this way is
;; called "Romberg integration".)
;;
;; ### Note on Richardson Extrapolation
;;
;; The terms of the error series for the Trapezoid method increase as $h^2, h^4,
;; h^6$... (see https://en.wikipedia.org/wiki/Trapezoidal_rule#Error_analysis).
;; Because of this, we pass $p = q = 2$ into `pr/richardson-sequence` below.
;; Additionally, `integral` hardcodes the factor of `2` and doesn't currently
;; allow for a custom sequence of $n$. This is configured by passing $t = 2$
;; into `pr/richardson-sequence`.
;;
;; If you want to accelerate some other geometric sequence, call
;; `pr/richardson-sequence` with some other value of `t.`
;;
;; To accelerate an arbitrary sequence of trapezoid evaluations, investigate
;; `polynomial.cljc` or `rational.cljc`. The "Bulirsch-Stoer" method uses either
;; of these to extrapolate the Trapezoid method using a non-geometric sequence.

(qc/defintegrator integral
  "Returns an estimate of the integral of `f` over the closed interval $[a, b]$
  using the Trapezoid method with $1, 2, 4 ... 2^n$ windows for each estimate.

  Optionally accepts `opts`, a dict of optional arguments. All of these get
  passed on to [[emmy.util.stream/seq-limit]] to configure convergence
  checking.

  See [[trapezoid-sequence]] for information on the optional args in `opts` that
  customize this function's behavior."
  :area-fn single-trapezoid
  :seq-fn trapezoid-sequence)

;; ## Next Steps
;;
;; If you start with the trapezoid method, one single step of Richardson
;; extrapolation (taking the second column of the Richardson tableau) is
;; equivalent to "Simpson's rule". One step using `t=3`, ie, when you /triple/
;; the number of integration slices per step, gets you "Simpson's 3/8 Rule". Two
;; steps of Richardson extrapolation gives you "Boole's rule".
;;
;; The full Richardson-accelerated Trapezoid method is also known as "Romberg
;; integration" (see `romberg.cljc`).
;;
;; These methods will appear in their respective namespaces in the `quadrature`
;; package.
;;
;; See the wikipedia entry on [Closed Newton-Cotes
;; Formulas](https://en.wikipedia.org/wiki/Newton%E2%80%93Cotes_formulas#Closed_Newton%E2%80%93Cotes_formulas)
;; for more details.
