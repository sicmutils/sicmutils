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

(ns sicmutils.numerical.quadrature.romberg
  (:require [sicmutils.numerical.quadrature.midpoint :as qm]
            [sicmutils.numerical.quadrature.trapezoid :as qt]
            [sicmutils.numerical.interpolate.richardson :as ir]
            [sicmutils.util.stream :as us]))

;; ## Romberg's Method
;;
;; https://en.wikipedia.org/wiki/Romberg%27s_method
;;
;; This namespace holds the scmutils-style implementation of romberg integration
;; that I had to lift out of obscurity...
;;
;; It's all described here in Sussman's paper:
;;
;; https://dspace.mit.edu/bitstream/handle/1721.1/6060/AIM-997.pdf?sequence=2

(defn open-sequence
  ([f a b] (open-sequence f a b 1))
  ([f a b n]
   {:pre [(number? n)]}
   (-> (qm/midpoint-sequence f a b n)
       (ir/richardson-sequence 3 2 2))))

(defn closed-sequence
  ([f a b] (closed-sequence f a b 1))
  ([f a b n]
   {:pre [(number? n)]}
   (-> (qt/trapezoid-sequence f a b n)
       (ir/richardson-sequence 2 2 2))))

(defn open-integral
  "TODO rewrite!

  Returns an estimate of the integral of `f` over the open interval $(a, b)$
  using Milne's rule with $1, 2, 4 ... 2^n$ windows for each estimate.

  Optionally accepts `opts`, a dict of optional arguments. All of these get
  passed on to `us/seq-limit` to configure convergence checking."
  ([f a b] (open-integral f a b {}))
  ([f a b opts]
   (-> (open-sequence f a b)
       (us/seq-limit opts))))

(defn closed-integral
  ([f a b] (closed-integral f a b {}))
  ([f a b opts]
   (-> (closed-sequence f a b)
       (us/seq-limit opts))))
