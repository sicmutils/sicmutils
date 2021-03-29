;;
;; Copyright © 2021 Sam Ritchie.
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

(ns sicmutils.sr.boost-test
  (:refer-clojure :exclude [+ - * /])
  (:require [clojure.test :refer [is deftest testing use-fixtures]]
            ;; [sicmutils.calculus.basis :as b]
            ;; [sicmutils.calculus.coordinate :refer [let-coordinates]
            ;;  #?@(:cljs [:include-macros true])]
            ;; [sicmutils.calculus.hodge-star :as hs]
            ;; [sicmutils.calculus.manifold :as m]
            ;; [sicmutils.calculus.vector-field :as vf]
            [sicmutils.generic :as g :refer [+ - * /]]
            [sicmutils.simplify :refer [hermetic-simplify-fixture]]
            [sicmutils.structure :as s :refer [up down]]
            [sicmutils.value :as v]))

(use-fixtures :each hermetic-simplify-fixture)

(def simplify
  (comp v/freeze g/simplify))


(comment
  (is (= 0 (simplify
            (- (proper-space-interval
                ((general-boost (up 'vx 'vy 'vz))
                 (make-4tuple 'ct (up 'x 'y 'z))))
               (proper-space-interval
                (make-4tuple 'ct (up 'x 'y 'z))))))))

(comment
  (let [beta (up (/ 'v↑x :c)
                 (/ 'v↑y :c)
                 (/ 'v↑z :c))]
    (is (= '(up 0 0 0 0)
           (simplify
            (- ((general-boost2 (up 1 0 0) 0) (up 'u0 'u1 'u2 'u3))
               (up 'u0 'u1 'u2 'u3))))))
  )

(comment
  ;;; Check of the relation between boosts and rotations.

  (let ((beta (up 'bx 'by 'bz))
        (xi (make-4tuple 'ct (up 'x 'y 'z)))
        (R (compose
            (rotate-x 'theta)
            (rotate-y 'phi)
            (rotate-z 'psi)))
        (R-inverse (compose
                    (rotate-z (- 'psi))
                    (rotate-y (- 'phi))
                    (rotate-x (- 'theta)))))
    (is (= '(up 0 0 0 0)
           (simplify
            (- ((general-boost beta) xi)
               ((compose (extended-rotation R-inverse)
                         (general-boost (R beta))
                         (extended-rotation R))
                xi))))))
  )
