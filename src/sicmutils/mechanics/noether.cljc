;;
;; Copyright © 2022 Sam Ritchie.
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

(ns sicmutils.mechanics.noether
  (:refer-clojure :exclude [+ - * / partial time])
  (:require [sicmutils.calculus.derivative :refer [D partial]]
            [sicmutils.function :as f :refer [compose]]
            [sicmutils.generic :as g :refer [cos sin + - * /]]
            [sicmutils.numerical.minimize :as m]
            [sicmutils.numerical.quadrature :as q]
            [sicmutils.operator :as o]
            [sicmutils.polynomial :as p]
            [sicmutils.structure :as s :refer [up up?]]))

;; ## Noether Theorem Support

;; F-tilde is a parametric coordinate transformation that given parameters takes
;; a state and returns transformed coordinates. F-tilde may take an arbitrary
;; number of real-valued parameters. F-tilde applied to zeros is the coordinate
;; selector: It takes a state and returns the coordinates. The hypothesis of
;; Noether's theorem is that the Lagrangian is invariant under the
;; transformation for all values of the parameters.

;; (D (lambda parms (compose L (F->C (apply F-tilde parms))))) = 0

(defn Noether-integral [L F-tilde]
  #_
  (let [zero-parameters (make-list (car (arity F-tilde)) 0)]
    (* ((partial 2) L) (apply (D F-tilde) zero-parameters))))

;; #|
;; (define ((L-central-rectangular m V) local)
;;   (let ((q (coordinate local))
;;         (v (velocity local)))
;;     (- (* 1/2 m (square v))
;;        (V (sqrt (square q))))))

;; (define (F-tilde theta phi psi)
;;   (compose (Rx theta)
;; 	         (Ry phi)
;; 	         (Rz psi)
;; 	         coordinate))

;; (pe ((Noether-integral
;;       (L-central-rectangular 'm (literal-function 'Vr))
;;       F-tilde)
;;      (up 't
;; 	       (up 'x 'y 'z)
;; 	       (up 'vx 'vy 'vz))))
;; (down (+ (* -1 m vy z) (* m vz y))
;;       (+ (* m vx z) (* -1 m vz x))
;;       (+ (* -1 m vx y) (* m vy x)))
;; |#
