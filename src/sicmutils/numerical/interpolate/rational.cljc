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

(ns sicmutils.numerical.interpolate.rational
  "This namespace contains a discussion of rational function interpolation, and
  different methods for fitting rational functions to `N` points and evaluating
  them at some value `x`."
  (:require [sicmutils.numerical.interpolate.polynomial :as ip]
            [sicmutils.generic :as g]
            [sicmutils.util.aggregate :as ua]
            [sicmutils.util.stream :as us]
            [taoensso.timbre :as log]))

;; ## Rational Function Interpolation
;;
;; This namespace contains implementations of rational function interpolation
;; methods. The [ALGLib](https://www.alglib.net/interpolation/rational.php) user
;; guide has a nice page on [rational function
;; interpolation](https://www.alglib.net/interpolation/rational.php), which
;; suggests that the Bulirsch-Stoer method, included here, is NOT great, and
;; that there are better methods. We'd love implementations of the others if you
;; agree!
;;
;; The main method in this package is an incremental version of the
;; Bulirsch-Stoer algorithm.
;;
;; Just like with polynomial interpolation, let's start with a straightforward
;; implementation of the non-incremental recursive algorithm.

(defn bulirsch-stoer-recursive
  "Returns the value of `P(x)`, where `P` is rational function fit (using the
  Bulirsch-Stoer algorithm, of similar style to Neville's algorithm described in
  [[sicmutils.numerical.interpolate.polynomial]]) to every point in the supplied
  sequence `points`.

  `points`: is a sequence of pairs of the form `[x (f x)]`.

  \"The Bulirsch-Stoer algorithm produces the so-called diagonal rational
  function, with the degrees of numerator and denominator equal (if m is even)
  or with the degree of the denominator larger by one if m is odd.\" ~ Press,
  Numerical Recipes, p105

  The implementation follows [Equation 3.2.3 on on page 105 of
  Press](http://phys.uri.edu/nigh/NumRec/bookfpdf/f3-2.pdf).

  References:

    - Stoer & Bulirsch, ['Introduction to Numerical Analysis'](https://www.amazon.com/Introduction-Numerical-Analysis-Applied-Mathematics/dp/144193006X)
    - [PDF of the same reference](http://www.math.uni.wroc.pl/~olech/metnum2/Podreczniki/(eBook)%20Introduction%20to%20Numerical%20Analysis%20-%20J.Stoer,R.Bulirsch.pdf)
    - Press's Numerical Recipes (p105), [Section 3.2](http://phys.uri.edu/nigh/NumRec/bookfpdf/f3-2.pdf)"
  [points x]
  (letfn [(evaluate [points x]
            (cond (empty? points) 0

                  (= 1 (count points))
                  (let [[[_ y]] points]
                    y)

                  :else
                  (let [l-branch (pop points)
                        r-branch (subvec points 1)
                        center   (pop r-branch)
                        [xl]     (first points)
                        [xr]     (peek points)
                        rl (evaluate l-branch x)
                        rr (evaluate r-branch x)
                        rc (evaluate center x)
                        p  (g/- rr rl)
                        q  (-> (/ (g/- x xl)
                                  (g/- x xr))
                               (g/* (g/- 1 (g// p (g/- rr rc))))
                               (g/- 1))]
                    (g/+ rr (g// p q)))))]
    (let [point-array (vec points)]
      (evaluate point-array x))))

;; We can be a bit more clever, if we reuse the idea of the "tableau" described
;; in the polynomial namespace.

(defn bulirsch-stoer
  "Takes

  - a (potentially lazy) sequence of `points` of the form `[x (f x)]` and
  - a point `x` to interpolate

  and generates a lazy sequence of approximations of `P(x)`. Each entry in the
  return sequence incorporates one more point from `points` into the `P(x)`
  estimate.

  `P(x)` is rational function fit (using the Bulirsch-Stoer algorithm, of
  similar style to Neville's algorithm described
  in [[sicmutils.numerical.interpolate.polynomial]]) to every point in the
  supplied sequence `points`.

  \"The Bulirsch-Stoer algorithm produces the so-called diagonal rational
  function, with the degrees of numerator and denominator equal (if m is even)
  or with the degree of the denominator larger by one if m is odd.\" ~ Press,
  Numerical Recipes, p105

  The implementation follows [Equation 3.2.3 on on page 105 of
  Press](http://phys.uri.edu/nigh/NumRec/bookfpdf/f3-2.pdf).

  ### Column

  If you supply an integer for the third (optional) `column` argument,
  `bulirsch-stoer` will return that /column/ offset the interpolation tableau
  instead of the first row. This will give you a sequence of nth-order
  polynomial approximations taken between point `i` and the next `n` points.

  As a reminder, this is the shape of the tableau:

  ```
  p0 p01 p012 p0123 p01234
  p1 p12 p123 p1234 .
  p2 p23 p234 .     .
  p3 p34 .    .     .
  p4 .   .    .     .
  ```

  So supplying a `column` of `1` gives a sequence of 2-point approximations
  between pairs of points; `2` gives 3-point approximations between successive
  triplets, etc.

  References:

    - Stoer & Bulirsch, ['Introduction to Numerical Analysis'](https://www.amazon.com/Introduction-Numerical-Analysis-Applied-Mathematics/dp/144193006X)
    - [PDF of the same reference](http://www.math.uni.wroc.pl/~olech/metnum2/Podreczniki/(eBook)%20Introduction%20to%20Numerical%20Analysis%20-%20J.Stoer,R.Bulirsch.pdf)
    - Press's Numerical Recipes (p105), [Section 3.2](http://phys.uri.edu/nigh/NumRec/bookfpdf/f3-2.pdf)"
  ([points x] (bulirsch-stoer points x nil))
  ([points x column]
   (let [prepare (fn [[x fx]] [x x 0 fx])
         merge   (fn [[xl _ _ rl] [_ xr rc rr]]
                   (let [p  (- rr rl)
                         q  (-> (/ (- x xl)
                                   (- x xr))
                                (* (- 1 (/ p (- rr rc))))
                                (- 1))]
                     [xl xr rl (+ rr (/ p q))]))
         present (fn [row] (map (fn [[_ _ _ r]] r) row))
         tableau (ip/tableau-fn prepare merge points)]
     (present
      (if column
        (nth tableau column)
        (ip/first-terms tableau))))))

;; ## Incremental Bulirsch-Stoer
;;
;; Press, in [Numerical Recipes section
;; 3.2](http://phys.uri.edu/nigh/NumRec/bookfpdf/f3-2.pdf), describes a
;; modification to the Bulirsch-Stoer that lets you track the differences from
;; the left and left-up entries in the tableau, just like the modified Neville
;; method in `polynomial.cljc`. the algorithm is implemented below.

(defn- bs-prepare
  "Processes an initial point `[x (f x)]` into the required state:

  ```
  [x_l, x_r, C, D]
  ```

  The recursion starts with $C = D = f(x)$."
  [[x fx]] [x x fx fx])

(defn- bs-merge
  "Implements the recursion rules described in Press's Numerical Recipes, [section
  3.2](http://phys.uri.edu/nigh/NumRec/bookfpdf/f3-2.pdf) to generate x_l, x_r,
  C and D for a tableau node, given the usual left and left-up tableau entries.

  This merge function ALSO includes a 'zero denominator fix used by Bulirsch and
  Stoer and Henon', in the words of Sussman from `rational.scm` in the scmutils
  package.

  If the denominator is 0, we pass along `C` from the up-left node and `d` from
  the previous entry in the row. Otherwise, we use the algorithm to calculate.

  TODO understand why this works, or where it helps!"
  [x]
  (fn [[xl _ _ dl] [_ xr cr _]]
    (let [c-d     (- cr dl)
          d*ratio (-> (/ (- x xl)
                         (- x xr))
                      (* dl))
          den  (- d*ratio cr)]
      (if (zero? den)
        (do (log/info "zero denominator!")
            [xl xr cr dl])
        (let [cnum (* d*ratio c-d)
              dnum (* cr c-d)]
          [xl xr (/ cnum den) (/ dnum den)])))))

(defn modified-bulirsch-stoer
  "Similar to [[bulirsch-stoer]] (the interface is identical) but slightly more
  efficient. Internally this builds up its estimates by tracking the delta from
  the previous estimate.

  This non-obvious change lets us swap an addition in for a division,
  making the algorithm slightly more efficient.

  See [[bulirsch-stoer]] for usage information, and info about the required
  structure of the arguments.

  References:

   - Press's Numerical Recipes (p105), [Section 3.2](http://phys.uri.edu/nigh/NumRec/bookfpdf/f3-2.pdf)"
  [points x]
  (ip/mn-present
   (ip/first-terms
    (ip/tableau-fn bs-prepare
                   (bs-merge x)
                   points))))

;; ## Rational Interpolation as a Fold
;;
;; Just like in `polynomial.cljc`, we can write rational interpolation in the
;; style of a functional fold:

(defn modified-bulirsch-stoer-fold-fn
  "Returns a function that accepts:

  - `previous-row`: previous row of an interpolation tableau
  - a new point of the form `[x (f x)]`

  and returns the next row of the tableau using the algorithm described in
  [[modified-bulirsch-stoer]]."
  [x]
  (ip/tableau-fold-fn
   bs-prepare
   (bs-merge x)))

(defn modified-bulirsch-stoer-fold
  "Returns a function that consumes an entire sequence `xs` of points, and returns
  a sequence of successive approximations of `x` using rational functions fitted
  to the points in reverse order."
  [x]
  (ip/tableau-fold
   (modified-bulirsch-stoer-fold-fn x)
   ip/mn-present))

(defn modified-bulirsch-stoer-scan
  "Returns a function that consumes an entire sequence `xs` of points, and returns
  a sequence of SEQUENCES of successive rational function approximations of `x`;
  one for each of the supplied points.

  For a sequence `a, b, c...` you'll see:

  ```clojure
  [([[modified-bulirsch-stoer]] [a] x)
   ([[modified-bulirsch-stoer]] [b a] x)
   ([[modified-bulirsch-stoer]] [c b a] x)
   ...]
  ```"
  [x]
  (ip/tableau-scan
   (modified-bulirsch-stoer-fold-fn x)
   ip/mn-present))
