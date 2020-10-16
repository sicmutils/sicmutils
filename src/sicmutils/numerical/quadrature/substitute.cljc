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

(ns sicmutils.numerical.quadrature.substitute
  (:require [clojure.core.match :refer [match]]
            [sicmutils.numerical.quadrature.common :as qc]))

;; Simpler version of `evaluate-improper-integral`, from Press, et al.
;; http://phys.uri.edu/nigh/NumRec/bookfpdf/f4-4.pdf
;;
;; TODO flip the integral bounds inside `opts` when we change variables... for
;; all of these!

(defn infinitize
  "The returned function definitely requires that `a` and `b` have the same sign.
  add a precondition!"
  [integrate]
  (fn [f a b opts]
    {:pre [(not
            (and (qc/infinite? a)
                 (qc/infinite? b)))]}
    (let [f' (fn [t]
               (/ (f (/ 1.0 t))
                  (* t t)))
          a' (if (qc/infinite? b) 0.0 (/ 1.0 a))
          b' (if (qc/infinite? a) 0.0 (/ 1.0 a))]
      (integrate f' a' b' opts))))

(defn- power-law-change
  "To deal with an integral that has an integrable power-law singularity at its
  lower/upper limit, one also makes a change of variable."
  [integrate gamma lower?]
  {:pre [(and (<= 0 gamma) (< gamma 1))]}
  (fn [f a b opts]
    (let [inner-pow (/ 1.0   (- 1 gamma))
          gamma-pow (/ gamma (- 1 gamma))
          a' 0
          b' (Math/pow (- b a) (- 1 gamma))
          t->t' (if lower?
                  (fn [t] (+ a (Math/pow t inner-pow)))
                  (fn [t] (- b (Math/pow t inner-pow))))
          f' #(* gamma-pow (f (t->t' %)))]
      (* inner-pow (integrate f' a' b' opts)))))

(defn power-law-lower
  "Change of variables for singularity on the lower side."
  [integrate gamma]
  (power-law-change integrate gamma true))

(defn power-law-upper
  "Change of variables for singularity on the upper side."
  [integrate gamma]
  (power-law-change integrate gamma false))

(defn inv-square-lower
  "Change of variables for singularity on the lower side."
  [integrate gamma]
  (fn [f a b opts]
    (let [f' (fn [t] (* t (f (+ a (* t t)))))]
      (* 2 (integrate f' 0 (Math/sqrt (- b a)) opts)))))

(defn inv-square-upper
  "Change of variables for singularity on the upper side."
  [integrate gamma]
  (fn [f a b opts]
    (let [f' (fn [t] (* t (f (- b (* t t)))))]
      (* 2 (integrate f' 0 (Math/sqrt (- b a)) opts)))))
