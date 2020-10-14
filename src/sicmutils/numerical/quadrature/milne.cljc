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

(ns sicmutils.numerical.quadrature.milne
  (:require [sicmutils.numerical.quadrature.midpoint :as qm]
            [sicmutils.numerical.interpolate.richardson :as ir]
            [sicmutils.util.stream :as us]))

;; ## Milne's Rule

(defn milne-sequence
  ([f a b] (milne-sequence f a b 1))
  ([f a b n]
   {:pre [(number? n)]}
   (-> (qm/midpoint-sequence f a b (us/powers 2 n))
       (ir/richardson-column 1 2 2 2))))

(defn integral
  "Returns an estimate of the integral of `f` over the open interval $(a, b)$
  using Milne's rule with $1, 2, 4 ... 2^n$ windows for each estimate.

  Optionally accepts `opts`, a dict of optional arguments. All of these get
  passed on to `us/seq-limit` to configure convergence checking."
  ([f a b] (integral f a b {}))
  ([f a b opts]
   (-> (milne-sequence f a b)
       (us/seq-limit opts))))
