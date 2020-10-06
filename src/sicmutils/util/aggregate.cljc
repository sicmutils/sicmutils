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

(ns sicmutils.util.aggregate
  "Utilities for aggregating streams.")

;; The following method comes from rational.scm, and use Kahan's summation trick
;; to add up a stream of numbers.

(defn sigma
  "Remember, this is the name of the summation symbol. We should rename this `sum`
  in this case."
  ([xs]
   (first
    (reduce (fn [[sum c] x]
              (let [y (- x c)
                    t (+ sum y)]
                [t (- (- t sum) y)]))
            [0.0 0.0]
            xs)))
  ([f low high]
   (sigma (map f (range low high)))))

;; here's a nice little test:
#_
(sigma [1.0 1e-8 -1e-8])
