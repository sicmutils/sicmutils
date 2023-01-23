#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.numerical.derivative
  "This namespace contains implementations of various approaches to [numerical
  differentiation](https://en.wikipedia.org/wiki/Numerical_differentiation).

  Each of these methods uses [Richardson
  extrapolation](https://en.wikipedia.org/wiki/Richardson_extrapolation) to
  accelerate convergence, and roundoff error estimation to bail out of the
  computation when roundoff error threatens to overwhelm the calculation.

  For an implementation of [forward-mode automatic
  differentiation](https://en.wikipedia.org/wiki/Automatic_differentiation),
  see [[emmy.differential]] (for the backing implementation)
  and [[emmy.calculus.derivative]] (for the [[emmy.operator/Operator]]
  instances that make it pleasant to use this method.)"
  (:require [emmy.abstract.function :as af]
            [emmy.calculus.derivative :as d]
            [emmy.expression.render :refer [->infix]]
            [emmy.generic :as g]
            [emmy.polynomial.richardson :as r]
            [emmy.series :as series]
            [emmy.util :as u]
            [emmy.util.stream :as us]
            [emmy.value :as v]))

;; ## Numerical Computation of Derivatives
;;
;; This module builds up to an implementation of numerical derivatives. The
;; final function, `D-numeric`, uses Richardson extrapolation to speed up
;; convergence of successively tighter estimates of $f^{\prime}(x)$ using a few
;; different methods.
;;
;; The inspiration for this style was Sussman's "Abstraction in Numerical
;; Methods", starting on page 10:
;; https://dspace.mit.edu/bitstream/handle/1721.1/6060/AIM-997.pdf?sequence=2
;;
;; We'll proceed by deriving the methods symbolically, and then implement them
;; numerically.
;;
;; First, a function that will print nicely rendered infix versions
;; of (simplified) symbolic expressions:

(defn- show [e]
  (->infix (g/simplify e)))

;; And a function to play with:

(def ^:private func
  (af/literal-function 'f))

;; ## Approximating Derivatives with Taylor Series
;;
;; The key to all of these methods involves the taylor series expansion of an
;; arbitrary function $f$ around a point $x$; we know the taylor series will
;; include a term for $f^{\prime}(x)$, so the goal is to see if we can isolate
;; it.

;; Here's the taylor series expansions of $f(x + h)$:

(def ^:private fx+h
  (-> ((d/taylor-series func 'x) 'h)
      (series/sum 4)))

;; Use `show` to print out its infix representation:

(comment
  (show fx+h))
;; => "1/24 h⁴ D⁴f(x) + 1/6 h³ D³f(x) + 1/2 h² D²f(x) + h Df(x) + f(x)"

;; We can solve this for $Df(x)$ by subtracting $f(x)$ and dividing out $h$:

(comment
  (show (g// (g/- fx+h (func 'x)) 'h)))

;; => "1/24 h³ D⁴f(x) + 1/6 h² D³f(x) + 1/2 h D²f(x) + Df(x)"
;;
;; Voila! The remaining terms include $D f(x)$ along with a series of
;; progressively-smaller "error terms" (since $h \to 0$). The first of these
;; terms is ${1 \over 2} h D^2 f(x)$. It will come to dominate the error as $h
;; \to 0$, so we say that the approximation we've just derived has error of
;; $O(h)$.
;;
;; This particular formula, in the limit as $h \to 0$, is called the "forward
;; difference approximation" to $Df(x)$. Here's the Clojure implementation:

(defn forward-difference
  "Returns a single-variable function of a step size `h` that calculates the
  forward-difference estimate of the the first derivative of `f` at point `x`:

  ```
  f'(x) = [f(x + h) - f(x)] / h
  ```

  Optionally accepts a third argument `fx == (f x)`, in case you've already
  calculated it elsewhere and would like to save a function evaluation."
  ([f x] (forward-difference f x (f x)))
  ([f x fx]
   (fn [h]
     (/ (- (f (+ x h)) fx) h))))

;; We could also expand $f(x - h)$:

(def ^:private fx-h
  (-> ((d/taylor-series func 'x) (g/negate 'h))
      (series/sum 4)))

(comment
  (show fx-h))
;; => "1/24 h⁴ D⁴f(x) -1/6 h³ D³f(x) + 1/2 h² D²f(x) - h Df(x) + f(x)"

;; and solve for $Df(x)$:

(comment
  (show (g// (g/- (func 'x) fx-h) 'h)))
;; => "-1/24 h³ D⁴f(x) + 1/6 h² D³f(x) -1/2 h D²f(x) + Df(x)"
;;
;; To get a similar method, called the "backward difference" formula. Here's the
;; implementation:

(defn backward-difference
  "Returns a single-variable function of a step size `h` that calculates the
  backward-difference estimate of the first derivative of `f` at point `x`:

  ```
  f'(x) = [f(x) - f(x - h)] / h
  ```

  Optionally accepts a third argument `fx == (f x)`, in case you've already
  calculated it elsewhere and would like to save a function evaluation."
  ([f x] (backward-difference f x (f x)))
  ([f x fx]
   (fn [h]
     (/ (- fx (f (- x h))) h))))

;; Notice that the two expansions, of $f(x + h)$ and $f(x - h)$, share every
;; term paired with an even power of $h$. The terms associated with odd powers
;; of $h$ alternate in sign (because of the $-h$ in the expansion of $f(x -
;; h)$).
;;
;; We can find yet another method for approximating $Df(x)$ if we subtract these
;; two series. We're trying to solve for $Df(x)$, and $Df(x)$ appears paired
;; with $h$, an odd-powered term... so subtracting $f(x-h)$ should double that
;; term, not erase it. Let's see:

(comment
  (show (g/- fx+h fx-h)))
;; => "1/3 h³ D³f(x) + 2 h Df(x)"
;;
;; Amazing! Now solve for $Df(x)$:

(comment
  (show (g// (g/- fx+h fx-h)
             (g/* 2 'h))))
;; => "1/6 h² D³f(x) + Df(x)"
;;
;; We're left with $Df(x) + O(h^2)$, a quadratic error term in $h$. (Of course
;; if we'd expanded to more than initial terms in the taylor series we'd see a
;; long error series with only even powers.)
;;
;; This formula is called the "central difference" approximation to the first
;; derivative. Here's the implementation:

(defn central-difference
  "Returns a single-variable function of a step size `h` that calculates the
  central-difference estimate of the first derivative of `f` at point `x`:

  ```
  f'(x) = [f(x + h) - f(x - h)] / 2h
  ```"
  [f x]
  (fn [h]
    (/ (- (f (+ x h)) (f (- x h)))
       (* 2 h))))

;; There's one more approximation we can extract from these two expansions. We
;; noted earlier that the terms associated with odd powers of $h$ alternate in
;; sign. If we add the two series, these odd terms should all cancel out. Let's
;; see:

(comment
  (show (g/+ fx-h fx+h)))
;; => "1/12 h⁴ D⁴f(x) + h² D²f(x) + 2 f(x)"

;; Interesting. The $Df(x)$ term is gone. Remember that we have $f(x)$
;; available; the first unknown term in the series is now $D^2 f(x)$. Solve for
;; that term:

(comment
  (show (g// (g/- (g/+ fx-h fx+h) (g/* 2 (func 'x)))
             (g/square 'h))))
;; => "1/12 h² D⁴f(x) + D²f(x)"

;; This is the "central difference" approximation to the /second/ derivative of
;; $f$. Note that the error term here is quadratic in $h$. Here it is in code:

(defn central-difference-d2
  "Returns a single-variable function of a step size `h` that calculates the
  central-difference estimate of the second derivative of `f` at point `x`:

  f''(x) = [f(x + h) - 2f(x) + f(x - h)] / h^2

  Optionally accepts a third argument `fx == (f x)`, in case you've already
  calculated it elsewhere and would like to save a function evaluation."
  ([f x] (central-difference-d2 f x (f x)))
  ([f x fx]
   (let [fx*2 (* 2 fx)]
     (fn [h]
       (/ (- (+ (f (+ x h))
                (f (- x h)))
             fx*2)
          (* h h))))))

;; ## Taking Derivatives
;;
;; Let's attempt to use these estimates and see how accurate they are. (This
;; section
;; follows [Sussman](https://dspace.mit.edu/bitstream/handle/1721.1/6060/AIM-997.pdf?sequence=2)
;; starting on page 10.)
;;
;; The following function returns a new function that approximates $Df(x)$ using
;; the central difference method, with a fixed value of $h = 0.00001$:

(defn- make-derivative-fn
  [f]
  (fn [x]
    (let [h 1e-5]
      ((central-difference f x) h))))

;; The error here is not great, even for a simple function:

(comment
  ((make-derivative-fn g/square) 3))
;;=> 6.000000000039306

;; Let's experiment instead with letting $h \to 0$. This next function takes a
;; function $f$, a value of $x$ and an initial $h$, and generates a stream of
;; central difference approximations to $Df(x)$ using successively halved values
;; of $h$, ie, $(h, h/2, h/4, h/8, ....)$

(defn- central-diff-stream [f x h]
  (map (central-difference f x)
       (us/zeno 2 h)))

;; Let's print 20 of the first 60 terms (taking every 3 so we see any pattern):

(comment
  (->> (central-diff-stream g/sqrt 1 0.1)
       (take-nth 3)
       (us/pprint 20)))

;; 0.5006277505981893
;; 0.5000097662926306
;; 0.5000001525880649
;; 0.5000000023844109
;; ...
;; 0.5000001192092896
;; 0.4999971389770508
;; 0.500030517578125
;; 0.49957275390625
;; 0.50048828125
;; 0.48828125
;; 0.625
;; 0.0
;; 0.0
;; 0.0

;; At first, the series converges toward the proper value. But as $h$ gets
;; smaller, $f(x + h)$ and $f(x - h)$ get so close together that their
;; difference is less than the minimum epsilon allowed by the system's floating
;; point representation.

;; As Sussman states: "Hence we are in a race between truncation error, which
;; starts out large and gets smaller, and roundoff error, which starts small and
;; gets larger." ~Sussman, p12
;;
;; ## Roundoff Error
;;
;; We can actually analyze and quantify how many halvings we can apply to $h$
;; before roundoff error swamps our calculation.
;;
;; Why does roundoff error occur? From Sussman: "Any real number $x$,
;; represented in the machine, is rounded to a value $x(1 + e)$, where $e$ is
;; effectively a random variable whose absolute value is on the order of the
;; machine epsilon $\epsilon$: that smallest positive number for which 1.0 and
;; $1.0 + \epsilon$ can be distinguished."
;;
;;
;; In the current library, `v/machine-epsilon` holds this value.
;;
;; Our goal, then, is to see if we can figure out when the error due to roundoff
;; grows so large that it exceeds the tolerance we want to apply to our
;; calculation.
;;
;; For the central difference formula:
;;
;; $$f^{\prime}(x) = {f(x + h) - f(x - h)} \over {2h}$$
;;
;; without any roundoff error, the numerator /should/ be equal to $2h f'(x)$. In
;; reality, for small values of $h$, $f(x + h)$ and $f(x - h)$ both have machine
;; representations in error by about $f(x) \epsilon$. Their difference doesn't
;; change the order, so we can say that their difference also has error of $f(x)
;; \epsilon$.
;;
;; Dividing these two together, the relative error is:
;;
;; $$\epsilon\left|\frac{f(x)}{2 h f^{\prime}(x)}\right|$$
;;
;; The relative error doubles each time $h$ is halved. This is technically just
;; the relative error of the numerator of the central difference method, but we
;; know the denominator $2h$ to full precision, so we can ignore it here.
;;
;; If we actually calculate this ratio, we'll find the INITIAL relative error
;; due to roundoff for a given h. Also, because we want to make sure that we're
;; working in integer multiples of machine epsilon, let's actually take the
;; next-highest-integer of the ratio above. The following method takes the ratio
;; above as an argument, and returns:
;;
;; $$1 + floor(\lvert ratio \rvert)$$

(defn- roundoff-units
  "Returns the number of 'roundoff units', ie, multiples of the machine epsilon,
  that roundoff error contributes to the total relative error, given a relative
  error percentage estimated for some initial step size $h$."
  [rel-error-ratio]
  (inc
   (Math/floor
    (Math/abs
     (double rel-error-ratio)))))

;; That calculation, as the documentation states, returns the number
;; of "roundoff units". Let's call it $r$.
;;
;; Each iteration doubles the relative error contributed by roundoff. Given some
;; tolerance, how many roundoff error doublings (or, equivalently, halvings of
;; $h$) can we tolerate before roundoff error swamps our calculation?
;;
;; Here's the solution:

(defn- max-iterations
  "Solution for `n`, in:

  `initial-error` * 2^n <= `tolerance`"
  [units tolerance]
  (let [initial-error (* v/machine-epsilon units)]
    (Math/floor
     (/ (Math/log (/ tolerance initial-error))
        (Math/log 2)))))

;; Let's combine these two ideas into a final function, `terms-before-roundoff`,
;; that calculates how items we can pull from a sequence like
;; `central-diff-stream` above before roundoff swamps us. (This is 1 + max
;; iterations, because we need to include the first point.)

(defn- terms-before-roundoff
  "Generates a default max number of terms, based on roundoff error estimates."
  [ratio tolerance]
  (inc
   (max-iterations (roundoff-units ratio)
                   tolerance)))

;; How many terms are we allowed to examine for an estimate of the derivative of
;; $f(x) = \sqrt(x)$, with an initial $h = 0.1$?

(comment
  (let [f         g/sqrt
        x         1
        h         0.1
        tolerance 1e-13
        ratio     (/ (f x)
                     (- (f (+ x h))
                        (f (- x h))))]
    (terms-before-roundoff ratio tolerance)))
;; => 6

;; 6 terms, or 5 halvings, down to $h = {0.1} \over {2^5} = 0.003125$. How many
;; terms does the sequence take to converge?

(comment
  (= (-> (central-diff-stream g/sqrt 1 0.1)
         (us/seq-limit {:tolerance 1e-13}))

     {:converged? true
      :terms-checked 15
      :result 0.5000000000109139}))

;; 15 is far beyond the level where roundoff error has rendered our results
;; untrustworthy.
;;
;; ## Richardson Extrapolation
;;
;; We need a way to converge more quickly. `richardson.cljc` lays out a general
;; method of "sequence acceleration" that we can use here, since we know the
;; arithmetic progression of the terms in the error series for each of our
;; methods above.
;;
;; For the central difference method, our highest-order error term has an
;; exponent of $p = 2$, and successive terms are all even.
;; `r/richardson-sequence` takes `p` and `q` for an arithmetic sequence of error
;; exponents $p, p + q, p + 2q...$
;;
;; It also needs the initial size $h$ of our sequence progression.
;;
;; Given that information, we can transform our existing sequence of estimates
;; into an accelerated sequence of estimates using Richardson extrapolation.
;; Does it converge in fewer terms?

(= (let [h 0.1, p 2, q 2]
     (-> (central-diff-stream g/sqrt 1 h)
         (r/richardson-sequence h p q)
         (us/seq-limit {:tolerance 1e-13})))

   {:converged? true
    :terms-checked 5
    :result 0.5000000000000159})

;; Happily, it does, in only 5 terms instead of 15! This brings convergence in
;; under our limit of 6 total terms.
;;
;; If you're interested in more details of Richardson extrapolation, please see
;; `richardson.cljc`! For now we'll proceed.
;;
;; ## Putting it All Together
;;
;; We're ready to write our final numeric differentiation routine, `D-numeric`.
;; First, some supporting structure. We support four methods, so let's describe
;; them using keywords in a set:

(def valid-methods
  #{:central :central-d2 :forward :backward})

;; To apply one of the methods, we need to be able to:
;;
;; - generate the method's estimate as a function of $h$
;;
;; - calculate the "relative error ratio" that we used above to calculate a
;;   maximum number of terms to analyze
;; - know the order $p$ of the highest order error term, and
;; - the increment $q$ of successive error terms
;;
;; Once again, `richardson.cljc` for a discussion of $p$ and $q$.
;;
;; This `configs` function bundles all of this together. I don't know that this
;; is the best abstraction, but I don't know yet of any other methods for
;; numeric differentiation, so it'll do for now.
;;
;; Note here that $p = q = 2$ for both central difference methods, just like we
;; determined above. the forward and backward difference methods both have all
;; of the remaining terms from the taylor expansion in their error series, so
;; they only get $p = q = 1$.

(defn- configs [method f x fx]
  (case method
    :forward
    {:p 1
     :q 1
     :function (forward-difference f x fx)
     :ratio-fn (fn [h] (/ fx (- (f (+ x h)) fx)))}

    :central
    {:p 2
     :q 2
     :function (central-difference f x)
     :ratio-fn (fn [h]
                 (/ fx (- (f (+ x h))
                          (f (- x h)))))}

    :backward
    {:p 1
     :q 1
     :function (backward-difference f x fx)
     :ratio-fn (fn [h]
                 (/ fx (- fx (f (- x h)))))}

    :central-d2
    {:p 2
     :q 2
     :function (central-difference-d2 f x fx)
     :ratio-fn (fn [h]
                 (- (+ (f (+ x h))
                       (f (- x h)))
                    (* 2 fx)))}

    (u/illegal
     (str "Invalid method: " method ". Please try one of " valid-methods))))

(defn- fill-defaults
  "Fills in default values required by `D-numeric`. Any option not used by
  `D-numeric` gets passed on to `us/seq-limit`."
  [m]
  (let [defaults {:tolerance v/sqrt-machine-epsilon
                  :method    :central}
        {:keys [method] :as opts} (merge defaults m)]
    (assert (contains? valid-methods method)
            (str method " is not a valid method. Please try one of: " valid-methods))
    opts))

(defn D-numeric
  "Takes a function `f: R => R` (function of a single real variable), and returns
  a new function of `x` that approximates the derivative $Df(x)$ (or $D^2f(x)$
  if you pass `:method :central-d2`).

  Returns the estimated value of the derivative at `x`. If you pass `:info?
  true`, the fn returns a dictionary of the results of `us/seq-limit`:

  ```clojure
  {:converged? <boolean>
   :terms-checked <int>
   :result <derivative estimate>}
  ```

  Make sure to visit [[emmy.calculus.derivative/D]] if you want symbolic or
  automatic differentiation.

  ### Roundoff Estimate

  The returned function will attempt to estimate how many times it can halve the
  step size used to estimate the derivative before roundoff error swamps the
  calculation, and force the function to return (with `:converged? false`, if
  you pass `:info?`)

  ### Optional Arguments

  `D-numeric` takes optional args as its second param. Any of these can be
  overridden by passing a second argument to the function returned by
  `D-numeric`; helpful for setting defaults and then overriding them later.

  The returned function passes through these and any other options to
  `us/seq-limit`, where they control the sequence of richardson
  extrapolation-accelerated estimates.

  Options:

  - `:method`: one of `:central`, `:central-d2`, `:forward` or `:backward`.
  `:central-d2` forces a second derivative estimate; the other methods configure
  a first derivative estimator.

  - `:info?` if false (default), returns the estimated value of `x`. If true,
  returns a dictionary with more information (see `D-numeric`'s docstring for
  more info.)

  - `:initial-h`: the initial `h` to use for derivative estimates before $h \to
  0$. Defaults to `0.1 * abs(x)`.

  - `:tolerance`: see `us/stream-limit` for a discussion of how this value
  handles relative vs absolute tolerance. $\\sqrt(\\epsilon)$ by default, where
  $\\epsilon$ = machine tolerance.

  - `:maxterms`: the maximum number of terms to consider when hunting for a
  derivative estimate. This defaults to an estimate generated internally,
  designed to prevent roundoff error from swamping the result. If you want to
  disable this feature, set `:maxterms` to something moderately large, like
  `:maxterms 100`. But do so carefully! See the surrounding namespace for a
  larger discussion."
  ([f] (D-numeric f {}))
  ([f opts]
   (let [opts (fill-defaults opts)]
     (fn df
       ([x] (df x {}))
       ([x overrides]
        (let [{:keys [maxterms tolerance initial-h method info?] :as opts} (merge opts overrides)
              {:keys [ratio-fn function p q]} (configs method f x (f x))
              h (or initial-h (* 0.1 (g/abs x)))
              n (or maxterms (terms-before-roundoff
                              (ratio-fn h)
                              tolerance))
              estimates (map function (us/zeno 2 h))
              result    (-> (r/richardson-sequence estimates 2 p q)
                            (us/seq-limit (assoc opts :maxterms n)))]
          (if info? result (:result result))))))))

;; More resources about numerical differentiation:
;;
;; - "Abstraction in Numerical Methods", Gerald Sussman, p10+:
;;   https://dspace.mit.edu/bitstream/handle/1721.1/6060/AIM-997.pdf?sequence=2
;; - "Numerical Differentiation and Richardson Extrapolation" lecture notes by
;;   Joseph Mahaffy
;;   https://jmahaffy.sdsu.edu/courses/f16/math541/beamer/richard.pdf
;; - UBC's "Mathematical Python" course:
;;   https://www.math.ubc.ca/~pwalls/math-python/differentiation/differentiation/
