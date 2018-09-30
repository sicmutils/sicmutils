;
; Copyright © 2017 Colin Smith.
; This work is based on the Scmutils system of MIT/GNU Scheme:
; Copyright © 2002 Massachusetts Institute of Technology
;
; This is free software;  you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3 of the License, or (at
; your option) any later version.
;
; This software is distributed in the hope that it will be useful, but
; WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
; General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this code; if not, see <http://www.gnu.org/licenses/>.
;

(ns sicmutils.value
  (:refer-clojure :rename {zero? core-zero?})
  (:import (clojure.lang Keyword Symbol)))


(defn within
  "Returns a function that tests whether two values are within ε of each other."
  [^double ε]
  (fn [^double x ^double y] (< (Math/abs (- x y)) ε)))

(def twopi (* 2 Math/PI))

(defn principal-value
  [cuthigh]
  (let [cutlow (- cuthigh twopi)]
    (fn [x]
      (if (and (<= cutlow x) (< x cuthigh))
        x
        (let [y (- x (* twopi (Math/floor (/ x twopi))))]
          (if (< y cuthigh)
            y
            (- y twopi)))))))
