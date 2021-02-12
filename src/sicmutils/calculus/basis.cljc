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

(ns sicmutils.calculus.basis)

(defn make-basis
  "Make a basis object out of a vector and dual basis.

  The dimensions of `vector-basis` and `dual-basis` must agree."
  [vector-basis dual-basis]
  (let [d (count (flatten vector-basis))]
    (assert (= (count (flatten dual-basis)) d))
    {:type ::basis
     :dimension d
     :vector-basis vector-basis
     :oneform-basis dual-basis}))

(defn basis->oneform-basis
  "Extract the dual basis from the given basis object `b`."
  [b]
  {:pre [(= (:type b) ::basis)]}
  (:oneform-basis b))

(defn basis->vector-basis
  "Extract the vector basis from the given basis object `b`."
  [b]
  {:pre [(= (:type b) ::basis)]}
  (:vector-basis b))
