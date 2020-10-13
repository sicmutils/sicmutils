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

(ns sicmutils.numerical.quadrature.midpoint
  (:require [sicmutils.numerical.interpolate.richardson :as ir]
            [sicmutils.numerical.quadrature.riemann :as qr]
            [sicmutils.util :as u]
            [sicmutils.util.aggregate :as ua]
            [sicmutils.util.stream :as us]))

;; ## Midpoint Method
;;
;; This namespace builds on the ideas introduced in `riemann.cljc`.
;;
;; `riemann.cljc` described four different integration schemes ({left, right,
;; upper, lower} Riemann sums) that were each conceptually simple, but aren't
;; often used in practice, even in their "accelerated" forms.
;;
;; One reason for this is that their error terms fall off as $h, h^2, h^3$,
;; where $h$ is the width of an integration slice. Each order of sequence
;; acceleration can cancel out one of these terms at a time; but still, the
;; performance is not great.
;;
;; It turns out that by taking the /midpoint/ if each interval, instead of
;; either side, you can reduce the order of the error series to $O(h^2)$. This
;; is too good to pass up.
;;
;; Additionally, because the error terms fall off as $h^2, h^4, h^6, ...$, each
;; order of acceleration is worth quite a bit more than in the Riemann sum case.
;;
;; This namespace follows the same development as `riemann.cljc`:
;;
;; - implement a simple, easy-to-understand version of the Midpoint method
;; - make the computation more efficient
;; - write an incremental version that can reuse prior results
;; - wrap everything up behind a nice, exposed API
;;
;;
;; ## Simple Midpoint Rule
;;
;; Here's an implementation of a function that can take the midpoint of a single
;; slice:

(defn- single-midpoint [f a b]
  (let [width      (- b a)
        half-width (/ width 2.0)
        midpoint   (+ a half-width)]
    (* width (f midpoint))))

;; And a full (though inefficient) integrator using `windowed-sum`:

(defn- midpoint-sum* [f a b]
  (let [area-fn (partial single-midpoint f)]
    (qr/windowed-sum area-fn a b)))

;; Let's integrate a triangle!

#_
(= (* 0.5 10 10)
   ((midpoint-sum* identity 0.0 10.0) 10))
;; => true

;; ## Efficient Midpoint Method
;;
;; It turns out that we already had to implement an efficient version of
;; `midpoint-sum` in `riemann.cljc`; the incremental version of left and right
;; Riemann sums added the midpoints of each interval when doubling the number of
;; slices.
;;
;; We can check our implementation against `qr/midpoint-sum`:

#_
(= ((midpoint-sum* identity 0.0 100.0) 10)
   ((qr/midpoint-sum identity 0.0 100.0) 10))

;; We'll use `qr/midpoint-sum` in the upcoming functions.

;; ## Incremental Midpoint Method
;;
;; Unlike the left and right Riemann sums, the Midpoint method can't reuse
;; function evaluations when the number of slices doubles. This is because each
;; evaluation point, on a doubling, becomes the new border between slices:
;;
;; n = 1 |-------x-------|
;; n = 2 |---x---|---x---|
;;
;; If you /triple/ the number of slices from $n$ to $3n$, you can in fact reuse
;; the previous $n$ evaluations:
;;
;; n = 1 |--------x--------|
;; n = 3 |--x--|--x--|--x--|
;;
;; By scaling Sn down by a factor of 3, and adding it to a new sum that only
;; includes the new points (using the new slice width).
;;
;; BTW: The only place I found this idea mentioned is in Section 4.4 of
;; Press's ["Numerical
;; Recipes"](http://phys.uri.edu/nigh/NumRec/bookfpdf/f4-4.pdf). I haven't found
;; other references to this trick, or implementations. I'd love to hear about
;; them (via a Github issue) if you find any!
;;
;; We'll follow the interface we used for `qr/Sn->S2n` and write `Sn->S3n`. This
;; function of $f, a, b$ will return a function that performs the incremental
;; update.
;;
;; The returned function generates $S3n$ across $(a, b)$ with $n$ intervals, and
;; picking out two new points at $h \over 6$ and $5h \over 6$ of the way across
;; the old interval. These are the midpoints of the two new slices with width $h
;; \over 3$.
;;
;; Sum them all up and add them to $S_n \over 3$ to generate $S_{3n}$:

(defn- Sn->S3n [f a b]
  (let [width (- b a)]
    (fn [Sn n]
      (let [h        (/ width n)
            delta    (/ h 6)
            l-offset (+ a delta)
            r-offset (+ a (* 5 delta))
            fx (fn [i]
                 (let [ih (* i h)]
                   (+ (f (+ l-offset ih))
                      (f (+ r-offset ih)))))]
        (-> (+ Sn (* h (ua/sum fx 0 n)))
            (/ 3.0))))))

;; Now we can write `midpoint-sequence`, analogous to `qr/left-sequence`. This
;; implementation reuses all the tricks from `qr/incrementalize`; this means it
;; will be smart about using the new incremental logic any time it sees any $n$
;; multiple of 3, just as the docstring describes.

(defn midpoint-sequence
  "Returns a (lazy) sequence of successively refined estimates of the integral of
  `f` over the open interval $(a, b)$ using the Midpoint method.

  If `n` is a number, returns estimates with $n, 3n, 9n, ...$ slices,
  geometrically increasing by a factor of 3 with each estimate.

  If `n` is a sequence, the resulting sequence will hold an estimate for each
  integer number of slices in that sequence."
  ([f a b] (midpoint-sequence f a b 1))
  ([f a b n]
   (let [S      (qr/midpoint-sum f a b)
         next-S (Sn->S3n f a b)]
     (qr/incrementalize S next-S 3 n))))

;; The following example shows that for the sequence $2, 3, 4, 6, ...$ (used in
;; the Bulirsch-Stoer method!), the incrementally-augmented `midpoint-sequence`
;; only performs 253 function evaluations, vs the 315 of the non-incremental
;; `(midpoint-sum f2 0 1)` mapped across the points.

#_
(let [f (fn [x] (/ 4 (+ 1 (* x x))))
      [counter1 f1] (u/counted f)
      [counter2 f2] (u/counted f)
      n-seq (interleave
             (iterate (fn [x] (* 2 x)) 2)
             (iterate (fn [x] (* 2 x)) 3))]
  (doall (take 12 (midpoint-sequence f1 0 1 n-seq)))
  (doall (take 12 (map (midpoint-sum f2 0 1) n-seq)))
  (= [253 315]
     [@counter1 @counter2]))

;; ## Final Midpoint API
;;
;; The final version is analogous the `qr/left-integral` and friends, including
;; an option to `:accelerate?` the final sequence with Richardson extrapolation.
;;
;; I'm not sure what to call this accelerated method. Accelerating the trapezoid
;; method in this way is called "Romberg integration". Using an $n$ sequence of
;; powers of 2 and accelerating the midpoint method by a single step - taking
;; the second column (index 1) of the Richardson tableau - produces "Milne's
;; method".
;;
;; The ability to combine these methods makes it easy to produce powerful
;; methods without known names. Beware, and enjoy!
;;
;; ### Note on Richardson Extrapolation
;;
;; We noted above that the the terms of the error series for the midpoint method
;; increase as $h^2, h^4, h^6$... Because of this, we pass $p = q = 2$ into
;; `ir/richardson-sequence` below. Additionally, `integral` hardcodes the factor
;; of `3` and doesn't currently allow for a custom sequence of $n$. This
;; requires passing $t = 3$ into `ir/richardson-sequence`.
;;
;; If you want to accelerate some other geometric sequence, call
;; `ir/richardson-sequence` with some other value of `t.`
;;
;; To accelerate an arbitrary sequence of midpoint evaluations, investigate
;; `polynomial.cljc` or `rational.cljc`. The "Bulirsch-Stoer" method uses either
;; of these to extrapolate the midpoint method using a non-geometric sequence.

(defn integral
  "Returns an estimate of the integral of `f` over the open interval $(a, b)$
  using the Midpoint method with $1, 3, 9 ... 3^n$ windows for each estimate.

  Optionally accepts `opts`, a dict of optional arguments. All of these get
  passed on to `us/seq-limit` to configure convergence checking.

  `opts` entries that configure integral behavior:

  `:accelerate?`: if true, use Richardson extrapolation to accelerate
  convergence. If false, attempts to converge directly."
  ([f a b] (integral f a b {}))
  ([f a b opts]
   (let [estimates (midpoint-sequence f a b)]
     (-> (if (:accelerate? opts)
           (ir/richardson-sequence estimates 3 2 2)
           estimates)
         (us/seq-limit opts)))))
