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

(ns sicmutils.numerical.quadrature.bulirsch-stoer
  (:require [sicmutils.generic :as g]
            [sicmutils.util :as u]))

;; Note from http://phys.uri.edu/nigh/NumRec/bookfpdf/f16-3.pdf before
;; progressing to the modified midpoint method:
;;
;; "Now would be a good time to look back at the routine qsimp in §4.2, and
;; especially to compare equation (4.2.4) with equation (16.3.4) above. You will
;; see that the transition in Chapter 4 to the idea of Richardson extrapolation,
;; as embodied in Romberg integration of §4.3, is exactly analogous to the
;; transition in going from this section to the next one."

(def default-bulirsch-stoer-steps
  ;; probably binary search steps? It used to start at 2 and 3... looks like
  ;; they want to split differently now.
  ;;
  ;; TODO this was the default, apparently, suggested in their initial paper.
  ;; And it has the nice advantage that we can reuse some work in the trapezoid
  ;; method, since we double each time. If we were to TRIPLE each time then we
  ;; could definitely reuse work.
  ;;
  ;; I bet we can figure out a nice way to get going by caching results.
  (interleave
   (iterate (fn [x] (* 2 x)) 2)
   (iterate (fn [x] (* 2 x)) 3)))

(defn bulirsch-stoer-intervals
  "This takes a range, and then an INTERNAL step sequence, which, by default, is:

  a stream of powers of 2, or 3 * powers of 2. The same sequence that we're
  going to pass into the integrator. In that context, the meaning is, \"number
  of windows to consider\".

  The return value here is, weirdly, $(b - a)^2$ times the sum of 1/the SQUARES
  of that sequence. SO we get powers of 4? Is that misleading?

  TODO this is working on SQUARES because we know we have an even error
  function... that is why this is going to work at all, this particular
  method. So we want to extrapolate forward.

  \"The third idea was discussed in the section before this one, namely to use a
  method whose error function is strictly even, allowing the rational function
  or polynomial approximation to be in terms of the variable h2 instead of just
  h.\"

  They discuss this in the 'modified midpoint method' section, holy mackerel!
  http://phys.uri.edu/nigh/NumRec/bookfpdf/f16-3.pdf

  These are the `x` values that we'll use to do a rational function
  interpolation in bulirsch-stoer.
  "
  ([a b]
   (bulirsch-stoer-intervals
    a b default-bulirsch-stoer-steps))
  ([a b step-seq]
   (let [h (- b a)]
     (map (fn [n]
            (let [dx (/ h n)]
              (g/square dx)))
          step-seq))))

(comment
  ;; SO, we know now that this is weirdly equal to the squared zeno sequence.
  (let [upper 1.0]
    ;; from richardson.
    (is (= (take 10 (drop 2 (make-zeno-sequence g/square upper)))
           (take 10 (take-nth 2 (bulirsch-stoer-intervals 0 upper)))))))

(defn integrator
  [f t1 t2 allowable-error]
  (comment
    ((advance-generator
      (bulirsch-stoer-lisptran
       ;; state = #(t int) ==> dstate = #(1.0 ,(integral f t1 t))
       (fn [state dstate]
         (assoc dstate 0 1.0)
         (assoc dstate 1 (f (nth state 0))))
       2
       allowable-error))
     [t1 0.0]			;initial state
     (- t2 t1)
     (/ (- t2 t1) 2)
     (- t2 t1)
     (fn [ns dt h cont] (cont))
     (fn [ns dt sdt] (nth ns 1)))))
