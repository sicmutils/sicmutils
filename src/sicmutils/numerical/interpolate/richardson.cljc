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
  "Richardson interpolation is a special case of polynomial interpolation; knowing
  the ratios of successive `x` coordinates in the point sequence allows a more
  efficient calculation."
  (:require [sicmutils.numerical.interpolate.polynomial :as ip]
            [sicmutils.generic :as g]
            [sicmutils.util :as u]
            [sicmutils.util.aggregate :as ua]
            [sicmutils.util.stream :as us]
            [sicmutils.value :as v]))

;; ## Richardson Interpolation
;;
;; This approach (and much of this numerical library!) was inspired by Gerald
;; Sussman's ["Abstraction in Numerical
;; Methods"](https://dspace.mit.edu/bitstream/handle/1721.1/6060/AIM-997.pdf?sequence=2)
;; paper.
;;
;; That paper builds up to Richardson interpolation as a method of ["series
;; acceleration"](https://en.wikipedia.org/wiki/Series_acceleration). The
;; initial example concerns a series of the side lengths of an N-sided polygon
;; inscribed in a unit circle.
;;
;; The paper derives this relationship between the sidelength of an N- and
;; 2N-sided polygon:

(defn- refine-by-doubling
  "`s` is the side length of an N-sided polygon inscribed in the unit circle. The
  return value is the side length of a 2N-sided polygon."
  [s]
  (/ s (g/sqrt (+ 2 (g/sqrt (- 4 (g/square s)))))))

;; If we can increase the number of sides => infinity, we should reach a circle.
;; The "semi-perimeter" of an N-sided polygon is
;;
;; $$P_n = {n \over 2} S_n
;;
;; In code:

(defn- semi-perimeter
  "Returns the semi-perimeter length of an `n`-sided regular polygon with side
  length `side-len`."
  [n side-len]
  (* (/ n 2) side-len))

;; so as $n \to \infty$, $P_n$ should approach $\pi$, the half-perimeter of a
;; circle.
;;
;; Let's start with a square, ie, $n = 4$ and $s_4 = \sqrt{2}$. Clojure's
;; `iterate` function will let us create an infinite sequence of side lengths:

(def ^:private side-lengths
  (iterate refine-by-doubling (Math/sqrt 2)))

;; and an infinite sequence of the number of sides:

(def ^:private side-numbers
  (iterate #(* 2 %) 4))

;; Mapping a function across two sequences at once generates a new infinite
;; sequence, of semi-perimeter lengths in this case:

(def ^:private archimedean-pi-sequence
  (map semi-perimeter side-numbers side-lengths))

;; I don't have a nice way of embedding the sequence in a notebook, but the
;; following code will print the first 20 terms:

#_
(us/pprint 20 archimedean-pi-sequence)

;; Unfortunately (for Archimedes, by hand!), as the paper notes, it takes 26 iterations to converge to machine precision:

(comment
  (= (-> archimedean-pi-sequence
         (us/seq-limit {:tolerance sicmutils.value/machine-epsilon}))

     {:converged? true
      :terms-checked 26
      :result 3.1415926535897944}))

;; Enter Sussman: "Imagine poor Archimedes doing the arithmetic by hand: square
;; roots without even the benefit of our place value system! He would be
;; interested in knowing that full precision can be reached on the fifth term,
;; by forming linear combinations of the early terms that allow the limit to be
;; seized by extrapolation." (p4, Abstraction in Numerical Methods).
;;
;; Sussman does this by noting that you can also write the side length as:
;;
;; $$S_n = 2 \sin {\pi \over n}$$
;;
;; Then the taylor series expansion for $P_n$ becomes:
;;
;; $$
;;  P_n = {n \over 2} S_n \
;;      = {n \over 2} 2 \sin {\pi \over n} \
;;      = \pi + {A\ over n^2} + B \over n^4 ...
;; $$
;;
;; A couple things to note:
;;
;; - At large N, the $A \over n^2$ term dominates the truncation error.
;; - when we double $n$ by taking $P_n$, that term becomes $A \over {4 n^2}$, 4x
;;   smaller.
;;
;; The big idea is to multiply $P_{2n}$ by 4 and subtract $P_n$ (then divide by
;; 3 to cancel out the extra factor). This will erase the $A \over n^2$ term and
;; leave a /new/ sequence with $B \over n^4$ as the dominant error term.
;;
;; Now keep going and watch the error terms drain away.
;;
;; Before we write code, let's follow the paper's example and imagine instead
;; some general sequence of $R(h), R(h/t), R(h/t^2)...$ (where $t = 2$ in the
;; example above), with a power series expansion that looks like
;;
;; $$R(h) = A + B h^{p_1} + C h^{p_2}...$$
;;
;; where the exponents $p_1, p_2, ...$ are some OTHER series of error
;; growth. (In the example above, because the taylor series expanson of $n \sin
;; n$ only has even factors, the sequence was the even numbers.)
;;
;; In that case, the general way to cancel error between successive terms is:
;;
;; $${R(h/t) - t^{p_1} R(h)} = {t^{p_1} - 1} A + C_1 h^{p_2} + ...$$
;;
;; or:
;;
;; $${R(h/t) - t^{p_1} R(h)} \over {t^{p_1} - 1} = A + C_2 h^{p_2} + ...$$
;;
;; Let's write this in code:

(defn- accelerate-sequence
  "Generates a new sequence by combining each term in the input sequence `xs`
  pairwise according to the rules for richardson acceleration.

  `xs` is a sequence of evaluations of some function of $A$ with its argument
  smaller by a factor of `t` each time:

  $$A(h), A(h/t), ...$$

  `p` is the order of the dominant error term for the sequence."
  [xs t p]
  (let [t**p   (Math/pow t p)
        t**p-1 (dec t**p)]
    (map (fn [ah ah-over-t]
           (/ (- (* t**p ah-over-t) ah)
              t**p-1))
         xs
         (rest xs))))

;; If we start with the original sequence, we can implement Richardson
;; extrapolation by using Clojure's `iterate` with the `accelerate-sequence`
;; function to generate successive columns in the "Richardson Tableau". (This is
;; starting to sound familiar to the scheme for polynomial interpolation, isn't
;; it?)
;;
;; To keep things general, let's take a general sequence `ps`, defaulting to the
;; sequence of natural numbers.

(defn- make-tableau
  "Generates the 'tableau' of succesively accelerated Richardson interpolation
  columns."
  ([xs t] (make-tableau xs t (iterate inc 1)))
  ([xs t ps]
   (iterate (fn [[xs [p & ps]]]
              [(accelerate-sequence xs t p) ps])
            [xs ps])))

;; All we really care about are the FIRST terms of each sequence. These
;; approximate the sequence's final value with small and smaller error (see the
;; paper for details:)

(defn- first-terms-of-tableau
  "The extra `first` inside exists because each tableau column holds the sequence
  AND the remaining `ps` entries."
  [tableau]
  (map (comp first first) tableau))

;; Now we can put it all together into a sequence transforming function:

(defn richardson-sequence
  ([points t]
   (first-terms-of-tableau
    (make-tableau points t)))
  ([points t p q]
   (first-terms-of-tableau
    (make-tableau points t (iterate #(+ q %) p)))))

;; And the limit:

(defn richardson-limit
  ([xs t p q]
   (richardson-limit xs t p q {}))
  ([xs t p q opts]
   (let [rs (richardson-sequence xs t p q)]
     (us/seq-limit rs opts))))

;; This is polynomial interpolation, when we can make some assumptions about the
;; spacing, or ratios, between successive points in the sequence.

(defn richardson-poly-sequence
  "SLIGHTLY less efficient... but this shows that this is identical to a
  polynomial interpolation."
  ([points t] (richardson-poly-sequence points t 1 1))
  ([points t p q]
   (ip/tableau-fn (fn [fx] [p fx])
                  (fn [[p fl] [_ fr]]
                    (let [t**p (Math/pow t p)]
                      [(+ p q) (/ (- (* t**p fr) fl)
                                  (dec t**p))]))
                  (fn [row] (map second row))
                  points)))

;; TODO file these away!

;; So next they move on to something interesting, how to kill the error term...
;; and then on to "Richardson extrapolation":
;; https://en.wikipedia.org/wiki/Richardson_extrapolation

;; Another post on Richardson extrapolation:
;; https://calculus7.org/2015/12/27/richardson-extrapolation-and-midpoint-rule/
;; where... apparently it's called Milne's rule if you apply richardson to the
;; midpoint rule.
;;
;; Great... now, we ACCELERATE this, by using Richardson extrapolation:

;; Here are some benchmarks. The slowest part seems to be calculating t.

#_
(let [xs (map-indexed (fn [i fx] [(/ 1 (Math/pow 4 (+ 1 (* i 1)))) fx])
                      archimedean-pi-sequence)]
  (time (doseq [_ (range 10000)]
          (doall
           (take 8 (ip/modified-neville xs 0.0))))))
#_
(time (doseq [_ (range 10000)]
        (doall
         (take 8 (richardson-poly-sequence archimedean-pi-sequence 4 1 1)))))

#_
(time (doseq [_ (range 10000)]
        (doall
         (take 8 (richardson-limit archimedean-pi-sequence 4 1 1)))))
