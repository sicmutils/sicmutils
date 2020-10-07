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

(ns sicmutils.numerical.interpolate.polynomial
  "Woohoo, poly interpolation. First draft for now, then I can come back with the
  exposition."
  (:require [sicmutils.generic :as g]
            [sicmutils.util.aggregate :as ua]
            [sicmutils.util.stream :as us]))

(defn lagrange
  "Generates a lagrange interpolating polynomial that fits all of the supplied
  points.

  g(x) =  (f(a) * [(x-b)(x-c)...] / [(a-b)(a-c)...])
        + (f(b) * [(x-a)(x-c)...] / [(b-a)(b-c)...])
        + ...

  this is the big, awkward thing. There are better ways to program this."
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

(defn neville
  "Here's a recursive polynomial interpolation, described in Press's Numerical
  Recipes (p103), chapter 3: http://phys.uri.edu/nigh/NumRec/bookfpdf/f3-1.pdf.

  This is a better way to make the same polynomial as above, AND builds us up to
  the idea of keeping track of error as we go.

  Start by describing the unique polynomial of degree 0 going through each point
  individually. well, that's a constant... equal to the value at that point. No
  problem. Call these P_1, etc.

  P_{12}, then, is the unique FIRST order polynomial (ie, a line) going through
  points P_1 and P_2.

  For some point like $P_{abcd}$, let's call $P_{abc}$ 'P_l', and $P_{bcd}$
  'P_r'.

  Similarly, the left and rightmost points - $x_a$ and $x_b$ - will be $x_l$ and
  $x_r$.

  So Neville's rule states that:

    P(x) = [(x - x_r) P_l(x) - (x - x_l) P_r(x)] / [x_l - x_r]

  TODO go more into the recursive relationships, and why you share stuff.

  TODO This recurrence works because the two parents already agree at points
  xi+1. Go into some more detail, perhaps.
  "
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

;; You can write these out in a 'tableau':

;; p0
;; \
;; p01
;; /  \
;; p1  p012
;; \  /  \
;; p12   p0123
;; /  \  /  \
;; p2  p123   p01234
;; \  /  \  /
;; p23   p1234
;; /  \  /
;; p3  p234
;; \  /
;; p34
;; /
;; p4

;; OR, less confusingly:

;; p0 p01 p012 p0123 p01234
;; p1 p12 p123 p1234 .
;; p2 p23 p234 .     .
;; p3 p34 .    .     .
;; p4 .   .    .     .
;; .  .   .    .     .
;; .  .   .    .     .
;; .  .   .    .     .
;;
;; We can build this up row by row in slightly better functional style if we go
;; left to right, one column at a time:

(defn neville-reductions*
  "This returns a function that goes column by column doing the polynomial thing."
  [points x]
  (letfn [(prepare [[x y]] [x x y])
          (next-column [prev-column]
            (map (fn [[xl _ pl] [_ xr pr]]
                   (let [plr (g// (g/+ (g/* (g/- x xr) pl)
                                       (g/* (g/- xl x) pr))
                                  (g/- xl xr))]
                     [xl xr plr]))
                 prev-column
                 (rest prev-column)))
          (present [row]
            (map (fn [[_ _ v]] v) row))]
    (let [tableau (->> (map prepare points)
                       (iterate next-column)
                       (take-while seq))]
      (present (map first tableau)))))

;; We can abstract this out to be ANYTHING that processes a tableau:

(defn tableau-fn [prepare merge present points]
  (let [next-col (fn [previous-col]
                   (map merge
                        previous-col
                        (rest previous-col)))
        tableau (->> (map prepare points)
                     (iterate next-col)
                     (take-while seq))]
    (present
     (map first tableau))))

(defn neville-reductions [points x]
  (let [prepare (fn [[x y]] [x x y])
        merge   (fn [[xl _ pl] [_ xr pr]]
                  (let [plr (g// (g/+ (g/* (g/- x xr) pl)
                                      (g/* (g/- xl x) pr))
                                 (g/- xl xr))]
                    [xl xr plr]))
        present (fn [row] (map (fn [[_ _ v]] v) row))]
    (tableau-fn prepare merge present points)))

;; ## Modified Neville
;;
;; the harder version keeps track of our error as we go. The version from the
;; book is NOT good, but I'm going to include it here for fun.
;;
;; They have you doing this "straight line walk" through the tableau. But they
;; also say that the sum of any path you take results in the final value.
;;
;; Additionally, the logic to jump you DOWN through the rows always dumps you
;; into the 0th row by the time you hit the end. So the delta is always the `C`
;; jump.
;;
;; To keep it simple, we'll always stay with index 0.
;;
;; What are C and D? In the original picture:
;;
;; - C is the diff between the node and the one left and UP.
;; - D is the diff between the node and the one left and DOWN.

;; - So if you want to travel toward higher indices, add C.
;; - if you want to travel to lower indices, add D.
;;
;; Again, we'll always take the C step. You can always re-order the points...

(defn poly-prepare [[x fx]] [x x fx fx])

(defn poly-merge [x]
  (fn [[xl _ _ dl] [_ xr cr _]]
    (let [diff (- cr dl)
          cnum (* diff (- xl x))
          dnum (* diff (- xr x))
          den (- xl xr)]
      [xl xr (/ cnum den) (/ dnum den)])))

(defn extract-deltas
  "TODO discuss how we're using the kahan sum..."
  [row]
  (ua/scanning-sum
   (map (fn [[_ _ c _]] c) row)))

(defn incremental-by-cl
  "Similar, but keeps track of the actual DELTAS at each level.

  points is the zip of xa, ya in this method:
  http://phys.uri.edu/nigh/NumRec/bookfpdf/f3-1.pdf"
  [points x]
  (tableau-fn poly-prepare
              (poly-merge x)
              extract-deltas
              points))

;; gut checks that these work... AND note that you can subvec into them.

(defn tableau-fold
  "interpolation as a fold! Here's BETTER functional style. Instead of actually
  doing the loop, I'm going to write a function that takes one of the steps."
  [prepare merge]
  (fn [previous-row point]
    (reductions merge
                (prepare point)
                previous-row)))

;; And then the same incremental rule as before, as a fold:
(defn poly-fold [x]
  (tableau-fold poly-prepare
                (poly-merge x)))

(defn poly-scan
  "Now, here's the money. interpolation as a scan; IE, a function that can take
  reduce itself across points.

  TODO the presentation function here is super weird. We really want a running
  reduction across rows.

  TODO write sigma as a fold!
  "
  [x]
  (let [present (fn [row]
                  (ua/sum (map (fn [[_ _ c _]] c) row)))]
    (us/scan (poly-fold x)
             :init []
             :present present)))

;; BOOM!!! Now I have polynomial AND rational function interpolation on lock.
;; So, next... what is Richardson adding to the mix here?
;;
;; Nothing... it is in fact totally equivalent to the richardson interpolation,
;; with degree increasing by one each time. Just written in a functional, wild
;; style and maybe more constrained, assuming we're drilling down toward 0
;; always.
