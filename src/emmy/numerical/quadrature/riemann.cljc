#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.numerical.quadrature.riemann
  (:require [emmy.numerical.quadrature.common :as qc]
            [emmy.polynomial.richardson :as pr]
            [emmy.util.aggregate :as ua]
            [emmy.util.stream :as us]))

;; Riemann Quadrature
;;
;; This namespace includes functions for calculating the Riemann integral of a
;; single-variable function. These are probably /not/ methods that you'll want
;; to use; see the documentation and defaults in
;; `emmy.numerical.quadrature` for good recommendations. But they're clear
;; and understandable. The goal of this namespace is to lay the groundwork for
;; visualizable routines that you can use to step toward understanding of the
;; tougher methods.
;;
;; ["Quadrature"](https://en.wikipedia.org/wiki/Numerical_integration), in this
;; context, means "numerical integration". The word is a historical term for
;; calculating the area inside of some geometry shape. [Riemann
;; sums](https://en.wikipedia.org/wiki/Riemann_sum) are a group of methods for
;; numerical integration that use this strategy:
;;
;; - partition the area under the curve of some function $f$ into $n$ "slices"
;; - generate some area estimate for each slice
;; - add up all of the slices to form an estimate of the integral
;; - increase the number of slices, and stop when the estimate stops changing.
;;
;; The Riemann integral of a function $f$ is the limit of this process as $n \to
;; \infty$.
;;
;; How do you estimate the area of a slice? All of these methods estimate the
;; area by forming a rectangle. For the base, use $x_r - x_l$. For the height,
;; you might use:
;;
;; - the function value at the left point, $f(x_l)$ (Left Riemann sum)
;; - the right point, $f(x_r)$ (Right Riemann sum)
;; - the max of either $max(f(x_l), f(x_r))$ ("upper" Riemann sum)
;; - the minimum, $min(f(x_l), f(x_r))$, called the "lower" Riemann sums
;; - the function value at the midpoint: $f({{x_l + x_r} \over 2})$
;;
;; This namespace builds up to implementations for `left-integral`,
;; `right-integral`, `upper-integral` and `lower-integral`. `midpoint.cljc`
;; holds an implementation of the Midpoint method.
;;
;; A closely related method involves forming a trapezoid for each slice. This is
;; equivalent to averaging the left and right Riemann sums. The trapezoid method
;; lives in `trapezoid.cljc`.
;;
;; ## Riemann Sum Implementation
;;
;; We'll start with an inefficient-but-easily-understandable version of these
;; methods. To form a Riemann sum we need to:
;;
;; - partition some range $[a, b]$ into `n` slices
;; - call some area-generating function on each slice
;;-  add all of the resulting area estimates together
;;
;; `windowed-sum` implements this pattern:

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
          grid-points (concat (range a b width) [b])]
      (ua/sum
       (map area-fn grid-points (rest grid-points))))))

;; Test this out with a function that returns `2` for every slice, and we get
;; back an estimate (from the function returned by `windowed-sum`) of 2x the
;; number of slices:

(comment
  (let [area-fn   (fn [_ _] 2)
        estimator (windowed-sum area-fn 0 10)]
    (and (= 20.0 (estimator 10))
         (= 40.0 (estimator 20)))))
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
;; `left-sum` is simple to implement, given `windowed-sum`:

#_{:clj-kondo/ignore [:unused-private-var]}
(defn- left-sum* [f a b]
  (-> (fn [l r] (* (f l) (- r l)))
      (windowed-sum a b)))

;; Every internal slice has the same width, so we can make the sum slightly more
;; efficient by pulling out the constant and multiplying by it a single time.
;;
;; Internally, we also generate all of the internal "left" points directly from
;; the slice index, instead of pre-partitioning the range. This is fine since we
;; don't need $x_r$.

(defn- left-sum
  "Returns a function of `n`, some number of slices of the total integration
  range, that returns an estimate for the definite integral of $f$ over the
  range $[a, b)$ using a left Riemann sum."
  [f a b]
  (let [width (- b a)]
    (fn [n]
      (let [h  (/ width n)
            fx (fn [i] (f (+ a (* i h))))]
        (* h (ua/sum fx 0 n))))))

;; `right-sum` is almost identical, except that it uses $f(x_r)$ as the
;; estimate of each rectangle's height:

#_{:clj-kondo/ignore [:unused-private-var]}
(defn- right-sum* [f a b]
  (-> (fn [l r] (* (f r) (- r l)))
      (windowed-sum a b)))

;; Same trick here to get a more efficient version. This implementation also
;; generates an internal function `fx` of the window index. The only difference
;; from the `left-sum` implementation is an initial offset of `h`, pushing every
;; point to the right side of the window.

(defn- right-sum
  "Returns a function of `n`, some number of slices of the total integration
  range, that returns an estimate for the definite integral of $f$ over the
  range $(a, b]$ using a right Riemann sum."
  [f a b]
  (let [width (- b a)]
    (fn [n]
      (let [h     (/ width n)
            start (+ a h)
            fx    (fn [i] (f (+ start (* i h))))]
        (* h (ua/sum fx 0 n))))))

;; The upper Riemann sum generates a slice estimate by taking the maximum of
;; $f(x_l)$ and $f(x_r)$:

(defn- upper-sum
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

(defn- lower-sum
  "Returns an estimate for the definite integral of $f$ over the range $[a, b]$
  using a lower Riemann sum.

  This function may or may not make an evaluation at the endpoints $a$ or $b$,
  depending on whether or not the function is increasing or decreasing at the
  endpoints."
  [f a b]
  (-> (fn [l r] (* (- r l)
                  (min (f l) (f r))))
      (windowed-sum a b)))

;; ## Estimating Integrals with Riemann Sums
;;
;; Given the tools above, let's attempt to estimate the integral of $f(x) = x^2$
;; using the left and right Riemann sum methods. (The actual equation for the
;; integral is $x^3 \over 3$).
;;
;; The functions above return functions of `n`, the number of slices. We can
;; use `(us/powers 2)` to return a sequence of `(1, 2, 4, 8, ...)` and map the
;; function of `n` across this sequence to obtain successively better estimates
;; for $\int_0^{10} x^2$. The true value is $10^3 \over 3 = 333.333...$:

(comment
  (let [f              (fn [x] (* x x))
        left-estimates  (map (left-sum f 0 10)
                             (us/powers 2))
        right-estimates (map (right-sum f 0 10)
                             (us/powers 2))]
    (and (= [0.0 125.0 218.75 273.4375 302.734375]
            (take 5 left-estimates))

         (= [1000.0 625.0 468.75 398.4375 365.234375]
            (take 5 right-estimates)))))

;; Both estimates are bad at 32 slices and don't seem to be getting better. Even
;; up to $2^16 = 65,536$ slices we haven't converged, and are still far from the
;; true estimate:

(comment
  (= {:converged? false
      :terms-checked 16
      :result 333.31807469949126}
     (let [f (fn [x] (* x x))]
       (-> (map (left-sum f 0 10)
                (us/powers 2))
           (us/seq-limit {:maxterms 16})))))

;; This bad convergence behavior is why common wisdom states that you should
;; never use left and right Riemann sums for real work.
;;
;; But maybe we can do better.
;;
;;
;; ## Sequence Acceleration
;;
;; One answer to this problem is to use "sequence acceleration" via Richardson
;; extrapolation, as described in `richardson.cljc`.
;;
;; `pr/richardson-sequence` takes a sequence of estimates of some function
;; and "accelerates" the sequence by combining successive estimates.
;;
;; The estimates have to be functions of some parameter $n$ that decreases by a
;; factor of $t$ for each new element. In the example above, $n$ doubles each
;; time; this is equivalent to thinking about the window width $h$ halving each
;; time, so $t = 2$.
;;
;; This library's functional style lets us accelerate a sequence of estimates
;; `xs` by simply wrapping it in a call to `(pr/richardson-sequence xs 2)`.
;; Amazing!
;;
;; Does Richardson extrapolation help?

(comment
  (= {:converged? true
      :terms-checked 4
      :result 333.3333333333333}

     (let [f (fn [x] (* x x))]
       (-> (map (left-sum f 0 10)
                (us/powers 2))
           (pr/richardson-sequence 2)
           (us/seq-limit)))))

;; We now converge to the actual, true value of the integral in 4 terms!
;;
;; This is going to be useful for each of our Riemann sums, so let's make a
;; function that can accelerate a generic sequence of estimates. The following
;; function takes:
;;
;; - the sequence of estimates, `estimate-seq`
;; - a dictionary of "options"
;;
;; This library is going to adopt an interface that allows the user to configure
;; a potentially very complex integration function by sending a single
;; dictionary of options down to each of its layers. Adopting that style now is
;; going to allow this function to grow to accomodate other methods of sequence
;; acceleration, like polynomial or rational function extrapolation.
;;
;; For now, `{:accelerate? true}` configures Richardson extrapolation iff the
;; user hasn't specified a custom sequence of integration slices using the `:n`
;; option.

(defn- accelerate
  "NOTE - this is only appropriate for Richardson-accelerating sequences with t=2,
  p=q=1.

  This only applies to the Riemann sequences in this namespace!"
  [estimate-seq {:keys [n accelerate?] :or {n 1}}]
  (if (and accelerate? (number? n))
    (pr/richardson-sequence estimate-seq 2 1 1)
    estimate-seq))

;; Check that this works:

(comment
  (= {:converged? true
      :terms-checked 4
      :result 333.3333333333333}

     (let [f (fn [x] (* x x))]
       (-> (map (left-sum f 0 10)
                (us/powers 2))
           (accelerate {:accelerate? true})
           (us/seq-limit)))))

;; Excellent!
;;
;; ## Incremental Computation
;;
;; The results look quite nice; but notice how much redundant computation we're
;; doing.
;;
;; Consider the evaluation points of a left Riemann sum with 4 slices, next to a
;; left sum with 8 slices:
;;
;; x---x---x---x----
;; x-x-x-x-x-x-x-x--
;;
;; Every time we double our number of number of evaluations, half of the windows
;; share a left endpoint. The same is true for a right sum:
;;
;; ----x---x---x---x
;; --x-x-x-x-x-x-x-x
;;
;; In both cases, the new points are simply the /midpoints/ of the existing
;; slices.
;;
;; This suggests a strategy for incrementally updating a left or right Riemann
;; sum when doubling the number of points:
;;
;; - Generate a new midpoint estimate of each `n` slices
;; - Add this estimate to the previous estimate
;; - Divide the sum by `2` to scale each NEW slice width down by 2 (since we're
;;   doubling the number of slices)
;;
;; First, implement `midpoint-sum`. This is very close to the implementation for
;; `left-sum`; internally the function adds an offset of $h \over 2$ to each
;; slice before sampling its function value.

(defn midpoint-sum
  "Returns a function of `n`, some number of slices of the total integration
  range, that returns an estimate for the definite integral of $f$ over the
  range $(a, b)$ using midpoint estimates."
  [f a b]
  (let [width (- b a)]
    (fn [n]
      (let [h      (/ width n)
            offset (+ a (/ h 2.0))
            fx     (fn [i] (f (+ offset (* i h))))]
        (* h (ua/sum fx 0 n))))))

;; The next function returns a function that can perform the incremental update
;; to a left or right Riemann sum (and to a midpoint method estimate, as we'll
;; see in `midpoint.cljc`):

(defn Sn->S2n
  "Returns a function of:

  - `Sn`: a sum estimate for `n` partitions, and
  - `n`: the number of partitions

  And returns a new estimate for $S_{2n}$ by sampling the midpoints of each
  slice. This incremental update rule is valid for left and right Riemann sums,
  as well as the midpoint method."
  [f a b]
  (let [midpoints (midpoint-sum f a b)]
    (fn [Sn n]
      (-> (+ Sn (midpoints n))
          (/ 2.0)))))

;; After using `left-sum` to generate an initial estimate, we can use `Sn->S2n`
;; to generate all successive estimates, as long as we always double our slices.
;; This suggests a function that takes an initial number of slices, `n0`, and
;; then uses `reductions` to scan across `(us/powers 2 n0)` with the function
;; returned by `Sn->S2n`:

(defn- left-sequence* [f a b n0]
  (let [first-S ((left-sum f a b) n0)
        steps   (us/powers 2 n0)]
    (reductions (Sn->S2n f a b) first-S steps)))

;; Verify that this function returns an equivalent sequence of estimates to the
;; non-incremental `left-sum`, when mapped across powers of 2:

(comment
  (let [f (fn [x] (* x x))]
    (= (take 10 (left-sequence* f 0 10 1))
       (take 10 (map (left-sum f 0 10)
                     (us/powers 2 1))))))

;; ## Generalizing the Incremental Approach
;;
;; We need to use the same style for `right-sum`, so let's try and extract the
;; pattern above, of:
;;
;; - generating an initial estimate of `n0` slices using some function `S-fn`
;; - refining an estimate of `n0` slices => `n0 / 2` slices using some
;;   incremental updater, `next-S-fn`
;;
;; In fact, because methods like the Midpoint method from `midpoint.cljc` can
;; only incrementally update from `n` => `n/3`, let's make the factor general
;; too.
;;
;; `geometric-estimate-seq` captures the pattern above:

(defn geometric-estimate-seq
  "Accepts:

  - `S-fn`: a function of `n` that generates a numerical integral estimate from
  `n` slices of some region, and
  - `next-S-fn`: a function of (previous estimate, previous `n`) => new estimate
  - `factor`: the factor by which `n` increases for successive estimates
  - `n0`: the initial `n` to pass to `S-fn`

  The new estimate returned b `next-S-fn` should be of `factor * n` slices."
  [S-fn next-S-fn factor n0]
  (let [first-S (S-fn n0)
        steps   (us/powers factor n0)]
    (reductions next-S-fn first-S steps)))

;; And another version of `left-sequence`, implemented using the new function:

#_{:clj-kondo/ignore [:unused-private-var]}
(defn- left-sequence**
  "Returns a (lazy) sequence of successively refined estimates of the integral of
  `f` over the closed-open interval $a, b$ by taking left-Riemann sums with

  n0, 2n0, 4n0, ...

  slices."
  ([f a b] (left-sequence** f a b 1))
  ([f a b n0]
   (geometric-estimate-seq (left-sum f a b)
                           (Sn->S2n f a b)
                           2
                           n0)))

;; ## Incremental Updates with Any Sequence
;;
;; What if we want to combine the ability to reuse old results with the ability
;; to take successively refined estimates that /don't/ look like geometric
;; series? The series 1, 2, 3... of natural numbers is an obvious choice of
;; windows... but only the even powers are able to reuse estimates.
;;
;; Integration methods like the Bulirsch-Stoer approach depend on sequences like
;; 2, 3, 4, 6...
;;
;; We absolutely want to be able to save potentially-expensive function
;; evaluations.
;;
;; One way to do this is to memoize the function `f` that you pass in to any of
;; the methods above.
;;
;; Alternatively, we could implement a version of `geometric-estimate-seq` that
;; takes /any/ sequence of estimate,s and maintains a sort of internal
;; memoization cache.
;;
;; For every `n`, check the cache for `prev == n/factor`. If it exists in the
;; cache, use `next-S-fn`; else, use `S-fn`, just like we did in
;; `geometric-estimate-seq` for the initial value.
;;
;; `general-estimate-seq` does this:

(defn- general-estimate-seq
  "Accepts:

  - `S-fn`: a function of `n` that generates a numerical integral estimate from
  `n` slices of some region, and
  - `next-S-fn`: a function of (previous estimate, previous `n`) => new estimate
  - `factor`: the factor by which `next-S-fn` increases `n` in its returned estimate
  - `n-seq`: a monotonically increasing sequence of `n` slices to use.

  Returns a sequence of estimates of returned by either function for each `n` in
  `n-seq`. Internally decides whether or not to use `S-fn` or `next-S-fn` to
  generate successive estimates."
  [S-fn next-S-fn factor n-seq]
  (let [f (fn [[cache _] n]
            (let [Sn (if (zero? (rem n factor))
                       (let [prev (quot n factor)]
                         (if-let [S-prev (get cache prev)]
                           (next-S-fn S-prev prev)
                           (S-fn n)))
                       (S-fn n))]
              [(assoc cache n Sn) Sn]))]
    (->> (reductions f [{} nil] n-seq)
         (map second)
         (rest))))

;; We can combine `general-estimate-seq` and `geometric-estimate-seq` into a
;; final method that decides which implementation to call, based on the type of
;; the `n0` argument.
;;
;; If it's a number, use it as the `n0` seed for a geometrically increasing
;; series of estimates. Else, assume it's a sequence and pass it to
;; `general-estimate-seq`.

(defn incrementalize
  "Function that generalizes the ability to create successively-refined estimates
  of an integral, given:

  - `S-fn`: a function of `n` that generates a numerical integral estimate from
  `n` slices of some region, and
  - `next-S-fn`: a function of (previous estimate, previous `n`) => new estimate
  - `factor`: the factor by which `next-S-fn` increases `n` in its returned estimate
  - `n`: EITHER a number, or a monotonically increasing sequence of `n` slices to use.

  If `n` is a sequence, returns a (lazy) sequence of estimates generated for
  each entry in `n`.

  If `n` is a number, returns a lazy sequence of estimates generated for each
  entry in a geometrically increasing series of inputs $n, n(factor),
  n(factor^2), ....$

  Internally decides whether or not to use `S-fn` or `next-S-fn` to generate
  successive estimates."
  [S-fn next-S-fn factor n]
  (let [f (if (number? n)
            geometric-estimate-seq
            general-estimate-seq)]
    (f S-fn next-S-fn factor n)))

;; ## Final Incremental Implementations
;;
;; We can use `incrementalize` to write our final version of `left-sequence`,
;; along with a matching version for `right-sequence`.
;;
;; Notice that we're using `accelerate` from above. The interface should make
;; more sense now:

(defn left-sequence
  "Returns a (lazy) sequence of successively refined estimates of the integral of
  `f` over the closed-open interval $a, b$ by taking left-Riemann sums.

  ### Optional Arguments

  `:n`: If `n` is a number, returns estimates with $n, 2n, 4n, ...$ slices,
  geometrically increasing by a factor of 2 with each estimate.

  If `n` is a sequence, the resulting sequence will hold an estimate for each
  integer number of slices in that sequence.

  `:accelerate?`: if supplied (and `n` is a number), attempts to accelerate
  convergence using Richardson extrapolation. If `n` is a sequence this option
  is ignored."
  ([f a b] (left-sequence f a b {}))
  ([f a b opts]
   (let [S      (left-sum f a b)
         next-S (Sn->S2n f a b)]
     (-> (incrementalize S next-S 2 (:n opts 1))
         (accelerate opts)))))

(defn right-sequence
  "Returns a (lazy) sequence of successively refined estimates of the integral of
  `f` over the closed-open interval $a, b$ by taking right-Riemann sums.

  ### Optional Arguments

  `:n`: If `n` is a number, returns estimates with $n, 2n, 4n, ...$ slices,
  geometrically increasing by a factor of 2 with each estimate.

  If `n` is a sequence, the resulting sequence will hold an estimate for each
  integer number of slices in that sequence.

  `:accelerate?`: if supplied (and `n` is a number), attempts to accelerate
  convergence using Richardson extrapolation. If `n` is a sequence this option
  is ignored."
  ([f a b] (right-sequence f a b {}))
  ([f a b opts]
   (let [S      (right-sum f a b)
         next-S (Sn->S2n f a b)]
     (-> (incrementalize S next-S 2 (:n opts 1))
         (accelerate opts)))))

;; `lower-sequence` and `upper-sequence` are similar. They can't take advantage
;; of any incremental speedup, so we generate a sequence of `n`s internally and
;; map `lower-sum` and `upper-sum` directly across these.

(defn lower-sequence
  "Returns a (lazy) sequence of successively refined estimates of the integral of
  `f` over the closed interval $(a, b)$ by taking lower-Riemann sums.

  ### Optional Arguments

  `:n`: If `n` is a number, returns estimates with $n, 2n, 4n, ...$ slices,
  geometrically increasing by a factor of 2 with each estimate.

  If `n` is a sequence, the resulting sequence will hold an estimate for each
  integer number of slices in that sequence.

  `:accelerate?`: if supplied (and `n` is a number), attempts to accelerate
  convergence using Richardson extrapolation. If `n` is a sequence this option
  is ignored."
  ([f a b] (lower-sequence f a b {}))
  ([f a b {:keys [n] :or {n 1} :as opts}]
   (let [n-seq (if (number? n)
                 (us/powers 2 n)
                 n)]
     (-> (map (lower-sum f a b) n-seq)
         (accelerate opts)))))

(defn upper-sequence
  "Returns a (lazy) sequence of successively refined estimates of the integral of
  `f` over the closed interval $(a, b)$ by taking upper-Riemann sums.

  ### Optional Arguments

  `:n`: If `n` is a number, returns estimates with $n, 2n, 4n, ...$ slices,
  geometrically increasing by a factor of 2 with each estimate.

  If `n` is a sequence, the resulting sequence will hold an estimate for each
  integer number of slices in that sequence.

  `:accelerate?`: if supplied (and `n` is a number), attempts to accelerate
  convergence using Richardson extrapolation. If `n` is a sequence this option
  is ignored."
  ([f a b] (upper-sequence f a b {}))
  ([f a b {:keys [n] :or {n 1} :as opts}]
   (let [n-seq (if (number? n)
                 (us/powers 2 n)
                 n)]
     (-> (map (upper-sum f a b) n-seq)
         (accelerate opts)))))

;; ## Integral API
;;
;; Finally, we expose four API methods for each of the {left, right, lower,
;; upper}-Riemann sums.
;;
;; Each of these makes use a special `qc/defintegrator` "macro"; This style
;; allows us to adopt one final improvement. If the interval $a, b$ is below
;; some threshold, the integral API will take a single slice using the supplied
;; `:area-fn` below and not attempt to converge. See `common.cljc` for more
;; details.
;;
;; These API interfaces are necessarily limiting. They force the assumptions
;; that you:
;;
;; - only want to use geometrical sequences that start with n0 = 1
;; - only want to (optionally) accelerate using Richardson extrapolation
;;
;; I can imagine a better API, where it's much easier to configure generic
;; sequence acceleration! This will almost certainly show up in the library at
;; some point. For now, here are some notes:
;;
;; - Richardson extrapolation requires a geometric series of estimates. If you
;;   want to use some /other/ geometry series with `left-sequence` or
;;   `right-sequence`, you can still accelerate with Richardson. Just pass your
;;   new factor as `t`.
;;
;; - For each of {left, right, lower, upper}-Riemann sums, the order of the
;;   error terms is 1, 2, 3, 4..., so always provide `p=1` and `q=1` to
;;   `richardson-sequence`. `accelerate` does this above.
;;
;; - If you want to use some NON-geometric seq, you'll need to use the methods
;;   in `polynomial.cljc` and `rational.cljc`, which are more general forms of
;;   sequence acceleration that use polynomial or rational function
;;   extrapolation. Your sequence of `xs` for each of those methods should be
;;   `n-seq`.

(qc/defintegrator left-integral
  "Returns an estimate of the integral of `f` across the closed-open interval $a,
  b$ using a left-Riemann sum with $1, 2, 4 ... 2^n$ windows for each estimate.

  Optionally accepts `opts`, a dict of optional arguments. All of these get
  passed on to `us/seq-limit` to configure convergence checking.

  See `left-sequence` for information on the optional args in `opts` that
  customize this function's behavior."
  :area-fn (fn [f a b] (* (f a) (- b a)))
  :seq-fn left-sequence)

(qc/defintegrator right-integral
  "Returns an estimate of the integral of `f` across the closed-open interval $a,
  b$ using a right-Riemann sum with $1, 2, 4 ... 2^n$ windows for each estimate.

  Optionally accepts `opts`, a dict of optional arguments. All of these get
  passed on to `us/seq-limit` to configure convergence checking.

  See `right-sequence` for information on the optional args in `opts` that
  customize this function's behavior."
  :area-fn (fn [f a b] (* (f b) (- b a)))
  :seq-fn right-sequence)

;; upper and lower Riemann sums have the same interface; internally, they're not
;; able to take advantage of incremental summation, since it's not possible to
;; know in advance whether or not the left or right side of the interval should
;; get reused.

(qc/defintegrator lower-integral
  "Returns an estimate of the integral of `f` across the closed-open interval $a,
  b$ using a lower-Riemann sum with $1, 2, 4 ... 2^n$ windows for each estimate.

  Optionally accepts `opts`, a dict of optional arguments. All of these get
  passed on to `us/seq-limit` to configure convergence checking.

  See `lower-sequence` for information on the optional args in `opts` that
  customize this function's behavior."
  :area-fn (fn [f a b] (* (min (f a) (f b)) (- b a)))
  :seq-fn lower-sequence)

(qc/defintegrator upper-integral
  "Returns an estimate of the integral of `f` across the closed-open interval $a,
  b$ using an upper-Riemann sum with $1, 2, 4 ... 2^n$ windows for each estimate.

  Optionally accepts `opts`, a dict of optional arguments. All of these get
  passed on to `us/seq-limit` to configure convergence checking.

  See `upper-sequence` for information on the optional args in `opts` that
  customize this function's behavior."
  :area-fn (fn [f a b] (* (max (f a) (f b)) (- b a)))
  :seq-fn upper-sequence)

;; ## Next Steps
;;
;; For a discussion and implementation of the more advanced methods (the
;; workhorse methods that you should actually use!), see `midpoint.cljc` and
;; `trapezoid.cljc`. The midpoint method is the standard choice for open
;; intervals, where you can't evaluate the function at its endpoints. The
;; trapezoid method is standard for closed intervals.
