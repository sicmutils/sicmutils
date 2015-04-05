;; Copyright (C) 2015 Colin Smith.
;; This work is based on the Scmutils system of MIT/GNU Scheme.
;;
;; This is free software;  you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or (at
;; your option) any later version.

;; This software is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this code; if not, see <http://www.gnu.org/licenses/>.

(ns math.mechanics.hamilton-test
  (:refer-clojure :exclude [+ - * / zero?])
  (:require [clojure.test :refer :all]
            [math.generic :refer :all]
            [math.function :refer :all]
            [math.numbers]
            [math.simplify :refer [pe]]
            [math.expression :refer :all]
            [math.mechanics.hamilton :refer :all]
            [math.structure :refer :all]))

(deftest section-3.1.1
  ;; To move into Hamiltonian mechanics, we must fix the fact that
  ;; our literal functions currently support only arity 1. We won't
  ;; get far without fixing that.
  ;;
  ;; OK, what we have here (using literal-function2 as a stopgap)
  ;; doesn't quite give the right answer for the part containing
  ;; the partials of V, but it's a step forward. Why did zeros
  ;; enter the expansion? Also, should we copy the existing type
  ;; notation or make up our own? I like the idea of type exemplars
  ;; here: a type is either a thing, or [thing1 :to thing2], if it's
  ;; a function between two other things.
  (is (= '(up 0
              (up (+ (* -2 (p_x t) (/ 1 (* 2 m))) ((D x) t))
                  (+ (* -2 (p_y t) (/ 1 (* 2 m))) ((D y) t)))
              (down (+ ((D p_x) t) (((partial-derivative 0) V) (x t) (y t)))
                    (+ ((D p_y) t) (((partial-derivative 1) V) (x t) (y t)))))
         (simplify (((Hamilton-equations
                      (H-rectangular
                       'm
                       (literal-function 'V [0 0] 0)))
                     (up (literal-function 'x) (literal-function 'y))
                     (down (literal-function 'p_x) (literal-function 'p_y)))
                    't)))))
