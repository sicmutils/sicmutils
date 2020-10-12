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
  "Efficient impls of the midpoint method."
  (:require [sicmutils.numerical.interpolate.richardson :as ir]
            [sicmutils.numerical.quadrature.riemann :as qr]
            [sicmutils.util :as u]
            [sicmutils.util.aggregate :as ua]
            [sicmutils.util.stream :as us]))

;; ## Midpoint Rule
;;
;; Turns out it's better to evaluate at the midpoint. Here's a simple version:

(defn single-midpoint [f a b]
  (let [width      (- b a)
        half-width (/ width 2.0)
        midpoint   (+ a half-width)]
    (* width (f midpoint))))

;; And a full aggregator using `windowed-sum`:

(defn midpoint-sum* [f a b]
  (qr/windowed-sum (partial single-midpoint f)
                   a b))

#_
(ish? 500.0 ((midpoint-sum* identity 0.0 100.0) 10))

;; The `qr/midpoint-sum` implementation is a little better, so we'll use that;

#_
(= ((midpoint-sum* identity 0.0 100.0) 10)
   ((qr/midpoint-sum identity 0.0 100.0) 10))

;; TODO discuss how we can go incremental here. We can subdivide each region
;; into THREE points.
;;
;; Press mentions that technique here:
;; http://phys.uri.edu/nigh/NumRec/bookfpdf/f4-4.pdf
;;
;; And here's the implementation of the incremental adder:

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

(defn midpoint-stream
  ([f a b] (midpoint-stream f a b 1))
  ([f a b n0]
   (let [S      (qr/midpoint-sum f a b)
         next-S (Sn->S3n f a b)]
     (if (number? n0)
       (qr/power-seq S next-S 3 n0)
       (qr/incrementalize S next-S 3 n0)))))

#_
(let [f (fn [x] (/ 4 (+ 1 (* x x))))
      [counter f] (u/counted f)
      ps (interleave
          (iterate (fn [x] (* 2 x)) 2)
          (iterate (fn [x] (* 2 x)) 3))]
  (time
   (doall (take 12 (midpoint-stream f 0 1 ps))))
  @counter)

#_
(let [f (fn [x] (/ 4 (+ 1 (* x x))))
      [counter f] (u/counted f)
      ps (interleave
          (iterate (fn [x] (* 2 x)) 2)
          (iterate (fn [x] (* 2 x)) 3))]
  (time
   (doall (take 12 (map (midpoint-sum f 0 1) ps))))
  @counter)

;; Now we can augment this thing with an `accelerate?` option.

(defn integrator
  ([f a b] (integrator f a b {}))
  ([f a b opts]
   (let [xs (midpoint-stream f a b)]
     (-> (if (:accelerate? opts)
           (ir/richardson-sequence xs 3 2 2)
           xs)
         (us/seq-limit opts)))))
