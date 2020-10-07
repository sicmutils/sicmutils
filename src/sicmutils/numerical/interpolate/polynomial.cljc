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

(ns sicmutils.numerical.interpolate.polynomial
  "This namespace contains a discussion of polynomial interpolation, and different
  methods for fitting a polynomial of degree N-1 to N points and evaluating that
  polynomial at some different `x`."
  (:require [sicmutils.generic :as g]
            [sicmutils.util.aggregate :as ua]
            [sicmutils.util.stream :as us]))

(defn lagrange
  "Generates a lagrange interpolating polynomial that fits every point in the
  supplied sequence `points` (of form `[x (f x)]`) and returns the value of the
  polynomial evaluated at `x`.

  The Lagrange polynomial has this form:

  g(x) =  (f(a) * [(x-b)(x-c)...] / [(a-b)(a-c)...])
        + (f(b) * [(x-a)(x-c)...] / [(b-a)(b-c)...])
        + ...

  for points `[a f(a)], [b f(b)], [c f(c)]` etc.

  This particular method of interpolating `x` into the polynomial is
  inefficient; any new calculation requires fully recomputing. Takes O(n^2)
  operations in the number of points.
  "
  [points x]
  (let [points     (vec points)
        n          (count points)
        build-term (fn [i [a fa]]
                     (let [others (for [j (range n) :when (not= i j)]
                                    (get-in points [j 0]))
                           p (reduce g/* (map #(g/- x %) others))
                           q (reduce g/* (map #(g/- a %) others))]
                       (g// (g/* fa p) q)))]
    (->> (map-indexed build-term points)
         (reduce g/+))))

;; Lagrange's interpolating polynomial is straightforward, but not terribly
;; efficient; every time we change `points` or `x` we have to redo the entire
;; calculation. Ideally we'd like to be able to perform:
;;
;; 1. Some computation on `points` that would let us efficiently evaluate the
;;    fitted polynomial for different values of `x` in O(n) time, or
;;
;; 2. A computation on a particular `x` that would let us efficiently add new
;;    points to the set we use to generate the interpolating polynomial.
;;
;; "Neville's algorithm" lets us generate the same interpolating polynomial
;; recursively. By flipping the recursion around and generating values from the
;; bottom up, we can achieve goal #2 and add new points incrementally.
;;
;; ## Neville's Algorithm
;;
;; Start the recursion with a single point. Any point $(x, f(x))$ has a unique
;; 0th order polynomial passing through it - the constant function $P(x) =
;; f(x)$. For points $x_a$, $x_b$, let's call this $P_a$, $P_b$, etc.
;;
;; $P_{ab}$ is the unique FIRST order polynomial (ie, a line) going through
;; points $x_a$ and $x_b$.
;;
;; this first recursive step gives us this rule:
;;
;; $$P_{ab}(x) = [(x - x_b) P_a(x) - (x - x_a) P_b(x)] / [x_a - x_b]$$
;;
;; For higher order terms like $P_{abcd}$, let's call $P_{abc}$ 'P_l', and
;; $P_{bcd}$ 'P_r' (the polynomial fitted through the left and right set of
;; points).
;;
;; Similarly, the left and rightmost inputs - $x_a$ and $x_b$ - will be $x_l$
;; and $x_r$.
;;
;; Neville's algorithm states that:
;;
;; $$P(x) = [(x - x_r) P_l(x) - (x - x_l) P_r(x)] / [x_l - x_r]$$
;;
;; This recurrence works because the two parents $P_l$ and $P_r$ already agree
;; at all points except $x_l$ and $x_r$.

(defn neville-recursive
  "Top-down implementation of Neville's algorithm.

  Returns the value of `P(x)`, where `P` is a polynomial fit (using Neville's
  algorithm) to every point in the supplied sequence `points` (of form `[x (f
  x)]`)

  The efficiency and results should be identical to
  `sicmutils.numerical.interpolate/lagrange`. This function represents a step on
  the journey toward more incremental methods of polynomial interpolation.

  References:

  - Press's Numerical Recipes (p103), chapter 3: http://phys.uri.edu/nigh/NumRec/bookfpdf/f3-1.pdf
  - Wikipedia: https://en.wikipedia.org/wiki/Neville%27s_algorithm"
  [points x]
  (letfn [(evaluate [points]
            (if (= 1 (count points))
              (let [[[_ y]] points]
                y)
              (let [l-branch (pop points)
                    r-branch (subvec points 1)
                    [xl]     (first points)
                    [xr]     (peek points)]
                (g// (g/+ (g/* (g/- x xr) (evaluate l-branch))
                          (g/* (g/- xl x) (evaluate r-branch)))
                     (g/- xl xr)))))]
    (evaluate (vec points))))

;; ## Tableau-based Methods

;; Neville's algorithm generates each new polynomial from $P_l$ and $P_r$, using
;; this recursion to incorporate the full set of points.
;;
;; You can write these out these relationships in a "tableau":
;;
;; p0
;;  \
;;  p01
;;  /  \
;; p1  p012
;;  \  /  \
;; p12   p0123
;;  /  \  /  \
;; p2  p123   p01234
;;  \  /  \  /
;; p23   p1234
;;  /  \  /
;; p3  p234
;;  \  /
;;  p34
;;  /
;; p4

;; The next few functions will discuss "rows" and "columns" of the tableau. That
;; refers to the rows and columns of this representation;
;;
;; p0 p01 p012 p0123 p01234
;; p1 p12 p123 p1234 .
;; p2 p23 p234 .     .
;; p3 p34 .    .     .
;; p4 .   .    .     .
;; .  .   .    .     .
;; .  .   .    .     .
;; .  .   .    .     .
;;
;; The first column here is the initial set of points. Each entry in each
;; successive column is generated through some operation between the entry to
;; its left, and the entry one left and one up.
;;
;; Look again at Neville's algorithm:
;;
;; $$P(x) = [(x - x_r) P_l(x) - (x - x_l) P_r(x)] / [x_l - x_r]$$
;;
;; $l$ refers to the entry in the same row, previous column, while $r$ is one
;; row higher, previous column.
;;
;; If each cell in the above tableau tracked:
;;
;; - the value of P(x) for the cell
;; - $x_l$, the x value of the leftmost point incorporated so far
;; - $x_r$, the right point
;;
;; we could build up Neville's rule incrementally. Let's attempt to build a
;; function of this signature:

(comment
  (defn neville-incremental*
    "Takes a potentially lazy sequence of `points` and a point `x` and generates a
  lazy sequence of approximations of P(x).

  entry N in the returned sequence is the estimate using a polynomial generated
  from the first N points of the input sequence."
    [points x]
    ,,,))
;;
;; First, write a function to process each initial point into a vector that
;; contains each of those required elements:

(defn- neville-prepare
  "Processes each point of the form [x, (f x)] into:

  $$[x_l, x_r, p]$$

  where $p$ is the polynomial that spans all points from $l$ to $r$. The
  recursion starts with $p = f(x)$.
  "
  [[x fx]]
  [x x fx])

;; Next, a function that generates the next entry, given l and r:

(defn- neville-combine-fn
  "Given some value $x$, returns a function that combines $l$ and $r$ entries in
  the tableau, arranged like this:

  l -- return
     /
    /
   /
  r

  generates the `return` entry of the form

  $$[x_l, x_r, p]$$."
  [x]
  (fn [[xl _ pl] [_ xr pr]]
    (let [plr (g// (g/+ (g/* (g/- x xr) pl)
                        (g/* (g/- xl x) pr))
                   (g/- xl xr))]
      [xl xr plr])))

;; We can use higher-order functions to turn this function into a NEW function
;; that can transform an entire column:

(defn- neville-next-column
  "This function takes some point $x$, and returns a new function that takes some
  column in the tableau and generates the next column."
  [x]
  (fn [prev-column]
    (map (neville-combine-fn x)
         prev-column
         (rest prev-column))))

;; `neville-tableau` will generate the entire tableau:

(defn- neville-tableau [points x]
  (->> (map neville-prepare points)
       (iterate (neville-next-column x))
       (take-while seq)))

;; Really, we're only interested in the first row:
;;
;; p0 p01 p012 p0123 p01234
;;
;; So define a function to grab that:

(defn- first-terms [tableau]
  (map first tableau))

;; the final piece we need is a function that will extract the estimate from our
;; row of $[x_l, x_r, p]$ vectors:

(defn- neville-present [row]
  (map (fn [[_ _ p]] p) row))

;; Putting it all together:

(defn neville-incremental*
  "Takes a potentially lazy sequence of `points` and a point `x` and generates a
  lazy sequence of approximations of P(x).

  entry N in the returned sequence is the estimate using a polynomial generated
  from the first N points of the input sequence."
  [points x]
  (neville-present
   (first-terms
    (neville-tableau points x))))

;; How do we know this works? We can prove it by using generic arithmetic to
;; compare the full symbolic lagrange polynomial to each entry in the successive
;; approximation.

(comment
  (defn- lagrange-incremental
    "Generates a sequence of estimates of `x` to polynomials fitted to `points`;
  each entry uses one more point, just like `neville-incremental*`."
    [points x]
    (let [n (count points)]
      (map (fn [i]
             (lagrange (take i points) x))
           (range 1 (inc n)))))

  ;; Every point is the same!
  (let [points [['x_1 'y_1] ['x_2 'y_2] ['x_3 'y_3] ['x_4 'y_4]]
        diffs  (map (fn [neville lagrange]
                      (g/simplify
                       (g/- neville lagrange)))
                    (neville-incremental* points 'x)
                    (lagrange-incremental points 'x))]
    (every? zero? diffs))
  ;; => true
  )

;; ## Generic Tableau Processing
;;
;; The above pattern, of processing tableau entries, is general enough that we
;; can abstract it out into a higher order function that takes a `prepare`,
;; `merge` and `present` function.
;;
;; This is necessarily more abstract! But we'll specialize it shortly, and
;; rebuild `neville-incremental` into its final form.
;;
;; I'm keeping `points` in the argument vector for now, vs returning a new
;; function; if you want to do this yourself, curry the function with `(partial
;; tableau-fn prepare merge present)`.

(defn tableau-fn
  "Returns the first row of a Newton-style approximation tableau, given:

  - `prepare`: a fn that processes each element of the supplied `points` into
  the state necessary to calculate future tableau entries.

  - `merge`: a fn of `l`and `r` the tableau entries:

  l -- return
     /
    /
   /
  r

  the inputs are of the same form returned by `prepare`. `merge` should return a
  new structure of the same form.

  - `present`: a function of the entire first (lazy) row of the tableau. Use
  `present` to drop extra aggregation information used in the course of
  generating tableau entries.

  - `points`: the (potentially lazy) sequence of points used to generate the
  first column of the tableau.
  "
  [prepare merge present points]
  (let [next-col (fn [previous-col]
                   (map merge
                        previous-col
                        (rest previous-col)))
        tableau (->> (map prepare points)
                     (iterate next-col)
                     (take-while seq))]
    (present
     (map first tableau))))

(defn- neville-merge
  "Returns a tableau merge function. Identical to `neville-combine-fn` but uses
  native operations instead of generic operations."
  [x]
  (fn [[xl _ pl] [_ xr pr]]
    (let [p (/ (+ (* (- x xr) pl)
                  (* (- xl x) pr))
               (- xl xr))]
      [xl xr p])))

(defn neville
  "Takes:

  - a (potentially lazy) sequence of `points` of the form `[x (f x)]` and
  - a point `x` to interpolate

  and generates a lazy sequence of approximations of P(x). Each entry in the
  return sequence incorporates one more point from `points` into the P(x)
  estimate.

  Said another way: the Nth in the returned sequence is the estimate using a
  polynomial generated from the first N points of the input sequence.

  This function generates each estimate using Neville's algorithm:

  $$P(x) = [(x - x_r) P_l(x) - (x - x_l) P_r(x)] / [x_l - x_r]$$

  References:

  - Press's Numerical Recipes (p103), chapter 3: http://phys.uri.edu/nigh/NumRec/bookfpdf/f3-1.pdf
  - Wikipedia: https://en.wikipedia.org/wiki/Neville%27s_algorithm
  "
  [points x]
  (tableau-fn neville-prepare
              (neville-merge x)
              neville-present
              points))

;; ## Modified Neville
;;
;; Press's Numerical Recipes, chapter 3 (p103) (
;; http://phys.uri.edu/nigh/NumRec/bookfpdf/f3-1.pdf ) describes a modified
;; version of Neville's algorithm that is slightly more efficient than the
;; version above.
;;
;; Allan Macleod, in "A comparison of algorithms for polynomial interpolation",
;; discusses this variation under the name "Modified Neville".

;; By generating the /delta/ from each previous estimate in the tableau,
;; Modified Neville is able to swap one of the multiplications above for an
;; addition.
;;
;; To make this work, instead of tracking the previous $p$ estimate, we track
;; two quantities:
;;
;; - $C_{abc}$ is the delta between $P_{abc}$ and $P_{ab}$, ie, $P_l$.
;; - $D_{abc}$ is the delta between $P_{abc}$ and $P_{bc}$, ie, $P_r$.
;;
;; We can recover the estimates generated by the original Neville's algorithm by
;; summing C values across the first tableau row.
;;
;; Equation 3.1.5 in Numerical recipes gives us the equations we need:
;;
;; $$
;;   C_{abc} = [(x_a - x)(C_{bc} - D_{ab})] / [x_a - x_c] &\
;;           = [(x_l - x)(C_r - D_l)] / [x_l - x_r]
;; $$
;;
;; $$
;;   D_{abc} = [(x_c - x)(C_{bc} - D_{ab})] / [x_a - x_c] &\
;;           = [(x_r - x)(C_r - D_l)] / [x_l - x_r]
;; $$
;;
;; These equations describe a `merge` function for a tableau processing scheme,
;; with state == `[x_l, x_r, C, D]`.
;;
;; Let's implement each method, and then combine them into final form. The
;; following methods use the prefix `mn` for "Modified Neville".

(defn- mn-prepare
  "Processes an initial point [x (f x)] into the required state:

  [x_l, x_r, C, D]

  The recursion starts with $C = D = f(x)$."
  [[x fx]]
  [x x fx fx])

(defn- mn-merge
  "Implements the recursion rules described above to generate x_l, x_r, C and D
  for a tableau node, given the usual left and left-up tableau entries."
  [x]
  (fn [[xl _ _ dl] [_ xr cr _]]
    (let [diff   (- cr dl)
          den    (- xl xr)
          factor (/ diff den)
          c      (* factor (- xl x))
          d      (* factor (- xr x))]
      [xl xr c d])))

(defn mn-present
  "Returns a (lazy) sequence of estimates by successively adding C values from the
  first entry of each tableau column. Each C value is the delta from the
  previous estimate."
  [row]
  (ua/scanning-sum
   (map (fn [[_ _ c _]] c) row)))

;; `tableau-fn` allows us to assemble these pieces into a final function that
;; has an interface identical to `neville` above. The implementation is more
;; obfuscated but slightly more efficient.

(defn modified-neville
  "Similar to `neville` (the interface is identical) but slightly more efficient.
  Internally this builds up its estimates by tracking the delta from the
  previous estimate.

  This non-obvious change lets us swap an addition in for a multiplication,
  making the algorithm slightly more efficient.

  See the `neville` docstring for usage information, and info about the required
  structure of the arguments.

  References:

  - \"A comparison of algorithms for polynomial interpolation\", A. Macleod,
    https://www.sciencedirect.com/science/article/pii/0771050X82900511
  - Press's Numerical Recipes (p103), chapter 3: http://phys.uri.edu/nigh/NumRec/bookfpdf/f3-1.pdf
  "
  [points x]
  (tableau-fn mn-prepare
              (mn-merge x)
              mn-present
              points))

;; ## Folds and Tableaus by Row
;;
;; The advantage of the method described above, where we generate an entire
;; tableau and lazily pull the first entry off of each column, is that we can
;; pass a lazy sequence in as `points` and get a lazy sequence of successive
;; estimates back. If we don't pull from the result sequence, no computation
;; will occur.
;;
;; One problem with that structure is that we have to have our sequence of
;; points available when we call a function like `neville`. What if we want to
;; pause, save the current estimate and pick up later where we left off?
;;
;; Look at the tableau again:
;;
;; p0 p01 p012 p0123 p01234
;; p1 p12 p123 p1234 .
;; p2 p23 p234 .     .
;; p3 p34 .    .     .
;; p4 .   .    .     .
;; .  .   .    .     .
;; .  .   .    .     .
;; .  .   .    .     .
;;
;; If you stare at this for a while, you might notice that it should be possible
;; to use the `merge` and `present` functions we already have to build the
;; tableau one /row/ at a time, given ONLY the previous row:
;;
;; (f [p1 p12 p123 p1234] [x0 fx0]) => [p0 p01 p012 p0123 p01234]
;;
;; Here's something close, using our previous `merge` and `prepare` definitions:

(defn- generate-new-row* [prepare merge]
  (fn [prev-row point]
    ;; the new point, once it's prepared, is the first entry in the new row.
    ;; From there, we can treat the previous row as a sequence of "r" values.
    (reduce merge (prepare point) prev-row)))

;; there's a problem here. `reduce` only returns the FINAL value of the
;; aggregation:
;;
;;   (let [f (generate-new-row* prepare present)]
;;     (f [p1 p12 p123 p1234] [x0 fx0]))
;;   ;; => p01234
;;
;; We want the entire new row! Lucky for us, Clojure has a version of `reduce`,
;; called `reductions`, that returns each intermediate aggregation result:

(defn- generate-new-row [prepare merge]
  (fn [prev-row point]
    (reductions merge (prepare point) prev-row)))

;;   (let [f (generate-new-row prepare present)]
;;     (f [p1 p12 p123 p1234] [x0 fx0]))
;;   ;; => [p0 p01 p012 p0123 p01234]
;;
;; Quick aside here, as we've stumbled across a familiar pattern. The discussion
;; above suggests the idea of a "fold" from functional programming:
;; https://en.wikipedia.org/wiki/Fold_(higher-order_function)
;;
;; A fold consists of:
;;
;; - `init`, an initial piece of state called an "accumulator"
;;
;; - a binary `merge` function that combines ("folds") a new element `x` into
;;   the accumulator and returns a value of the same shape / type as `init`.
;;
;; - a `present` function that transforms the accumulator into a final value.
;;
;; In Clojure, you perform a fold on a sequence with the `reduce` function:
;;
;;     (reduce merge init xs)
;;
;; For example:
;;
;;     (reduce + 0.0 (range 10))
;;     ;; => 45.0
;;
;; Our `generate-new-row` function from above is exactly the `merge` function of
;; a fold. The accumulator is the latest tableau row:

(defn- generate-new-row [prepare merge]
  (fn [prev-row point]
    (reductions merge (prepare point) prev-row)))

;; `init`    == [], the initial empty row.
;; `present` == the same present function as before (`neville-present` or
;;   `mn-present`)
;;
;; Now that we've identified this new pattern, redefine `generate-new-row` with
;; a new name:

(defn tableau-fold-fn
  "Transforms the supplied `prepare` and `merge` functions into a new function
  that can merge a new point into a tableau row (generating the next tableau
  row).

  More detail on the arguments:

  - `prepare`: a fn that processes each element of the supplied `points` into
  the state necessary to calculate future tableau entries.

  - `merge`: a fn of `l`and `r` the tableau entries:

  l -- return
     /
    /
   /
  r

  the inputs are of the same form returned by `prepare`. `merge` should return a
  new structure of the same form."
  [prepare merge]
  (fn [prev-row point]
    (reductions merge (prepare point) prev-row)))

;; Next, we can use this to generate specialized fold functions for our two
;; incremental algorithms above - `neville` and `modified-neville`:

(defn- neville-fold-fn
  "Returns a function that accepts:

  - `previous-row`: previous row of an interpolation tableau
  - a new point of the form `[x (f x)]`

  and returns the next row of the tableau using the algorithm described in
  `neville`."
  [x]
  (tableau-fold-fn neville-prepare
                   (neville-merge x)))

(defn- modified-neville-fold-fn
  "Returns a function that accepts:

  - `previous-row`: previous row of an interpolation tableau
  - a new point of the form `[x (f x)]`

  and returns the next row of the tableau using the algorithm described in
  `modified-neville`."
  [x]
  (tableau-fold-fn mn-prepare
                   (mn-merge x)))

;; This final function brings back in the notion of `present`. It returns a
;; function that consumes an entire sequence of points, and then passes the
;; final row into the exact `present-fn` we used above:

(defn tableau-fold
  "Returns a function that accepts a sequence of points and processes them into a
  tableau by generating successive rows, one at a time.

  The final row is passed into `present-fn`, which generates the final return
  value.

  This is NOT appropriate for lazy sequences! Fully consumes the input."
  [fold-fn present-fn]
  (fn [points]
    (present-fn
     (reduce fold-fn [] points))))

;; Note that these folds process points in the OPPOSITE order as the column-wise
;; tableau functions! Because you build up one row at a time, each new point is
;; PRE-pended to the interpolations in the previous row.
;;
;; The advantage is that you can save the current row, and then come back and
;; absorb further points later.
;;
;; The disadvantage is that if you `present` p123, you'll see successive
;; estimates for [p1, p12, p123]... but if you then prepend 0, you'll see
;; estimates for [p0, p01, p012, p0123]. These don't share any elements, so
;; they'll be totally different.
;;
;; If you REVERSE the incoming point sequence, the final row of the fold will
;; in fact equal the row of the column-based method.
;;
;; If you want a true incremental version of the above code, reverse points! We
;; don't do this automatically in case points is an infinite sequence.
;;
;; ## Fold Utilities
;;
;; `tableau-scan` below will return a function that acts identically to the
;; non-fold, column-wise version of the interpolators. It does this by folding
;; in one point at a time, but processing EVERY intermediate value through the
;; presentation function.

(defn tableau-scan
  "Takes a folding function and a final presentation function (of accumulator type
  => return value) and returns a NEW function that:

  - accepts a sequence of incoming points
  - returns the result of calling `present` on each successive row."
  [fold-fn present-fn]
  (fn [xs]
    (->> (reductions fold-fn [] xs)
         (map present-fn)
         (rest))))

;; And finally, we specialize to our two incremental methods. TODO add
;; docstrings here.

(defn neville-fold
  "Returns a function that consumes an entire sequence `xs` of points, and returns
  a sequence of successive approximations of `x` using polynomials fitted to the
  points in reverse order.

  This function uses the `neville` algorithm internally."
  [x]
  (tableau-fold (neville-fold-fn x)
                neville-present))

(defn neville-scan
  "Returns a function that consumes an entire sequence `xs` of points, and returns
  a sequence of SEQUENCES of successive polynomial approximations of `x`; one
  for each of the supplied points.

  For a sequence a, b, c... you'll see:

  [(neville [a] x)
   (neville [b a] x)
   (neville [c b a] x)
   ...]"
  [x]
  (tableau-scan (neville-fold-fn x)
                neville-present))

(defn modified-neville-fold
  "Returns a function that consumes an entire sequence `xs` of points, and returns
  a sequence of successive approximations of `x` using polynomials fitted to the
  points in reverse order.

  This function uses the `modified-neville` algorithm internally."
  [x]
  (tableau-fold (modified-neville-fold-fn x)
                mn-present))

(defn modified-neville-scan
  "Returns a function that consumes an entire sequence `xs` of points, and returns
  a sequence of SEQUENCES of successive polynomial approximations of `x`; one
  for each of the supplied points.

  For a sequence a, b, c... you'll see:

  [(modified-neville [a] x)
   (modified-neville [b a] x)
   (modified-neville [c b a] x)
   ...]"
  [x]
  (tableau-scan (modified-neville-fold-fn x)
                mn-present))

;; Next, check out:
;;
;; - `rational.cljc` to learn how to interpolate rational functions
;; - `richardson.cljc` for a specialized implementation of polynomial
;;   interpolation, when you know something about the ratios between successive
;;   `x` elements in the point sequence.
