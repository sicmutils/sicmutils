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

(ns sicmutils.fdg.ch3-test
  (:refer-clojure :exclude [+ - * /  partial])
  (:require [clojure.test :refer [is deftest testing use-fixtures]]
            [sicmutils.env :as e :refer [+ - * /
                                         D simplify compose partial
                                         literal-function
                                         literal-manifold-function
                                         literal-vector-field
                                         up down
                                         sin cos square exp
                                         point chart
                                         R2-rect R2-polar]
             #?@(:cljs [:include-macros true])]
            [sicmutils.simplify :refer [hermetic-simplify-fixture]]))

(use-fixtures :each hermetic-simplify-fixture)

(deftest section-3-1
  (let [R2-rect-point ((point R2-rect) (up 'x0 'y0))
        R2->R '(-> (UP Real Real) Real)
        v (e/components->vector-field
           (up (literal-function 'b↑0 R2->R)
               (literal-function 'b↑1 R2->R))
           R2-rect)
        v2 (literal-vector-field 'b R2-rect)]
    (is (= '(+ (* (((partial 0) f-rect) (up x0 y0)) (b↑0 (up x0 y0)))
               (* (((partial 1) f-rect) (up x0 y0)) (b↑1 (up x0 y0))))
           (simplify
            ((v (literal-manifold-function 'f-rect R2-rect)) R2-rect-point))))
    (is (= '(+ (* (((partial 0) f-rect) (up x0 y0)) (b↑0 (up x0 y0)))
               (* (((partial 1) f-rect) (up x0 y0)) (b↑1 (up x0 y0))))
           (simplify
            ((v2 (literal-manifold-function 'f-rect R2-rect)) R2-rect-point))))
    (is (= '(up (b↑0 (up x0 y0)) (b↑1 (up x0 y0)))
           (simplify
            ((v (chart R2-rect)) R2-rect-point))))
    (is (= '(+ (* (((partial 0) f-rect) (up x0 y0)) (b↑0 (up x0 y0)))
               (* (((partial 1) f-rect) (up x0 y0)) (b↑1 (up x0 y0))))
           (simplify
            (((e/coordinatize v R2-rect) (literal-function 'f-rect R2->R))
             (up 'x0 'y0)))))))

(deftest section-3-2
  (e/let-coordinates
   [[x y] R2-rect
    [r theta] R2-polar]
   (let [R2-rect-point ((point R2-rect) (up 'x0 'y0))]
     (is (= '(* 2 x0) (simplify ((d:dx (square r)) R2-rect-point))))
     (is (= '(+ (* 2 x0) (* 4 y0) 3)
            (simplify
             (((+ d:dx (* 2 d:dy)) (+ (square r) (* 3 x))) R2-rect-point)))))))

(deftest section-3-3
  #?(:clj
     ;; TODO making this clj only until we have proper ratio support.
     (e/let-coordinates
      [[x y] R2-rect]
      (let [circular (- (* x d:dy) (* y d:dx))]
        (is (= '((up 1 0)
                 (up 0 t)
                 (up (* -1/2 (expt t 2)) 0)
                 (up 0 (* -1/6 (expt t 3)))
                 (up (* 1/24 (expt t 4)) 0)
                 (up 0 (* 1/120 (expt t 5))))
               (simplify
                (take 6
                      (seq
                       (((exp (* 't circular)) (chart R2-rect))
                        ((point R2-rect) (up 1 0))))))))
        (is (= '(up
                 (+
                  (* -1/720 (expt delta-t 6))
                  (* 1/24 (expt delta-t 4))
                  (* -1/2 (expt delta-t 2))
                  1)
                 (+ (* 1/120 (expt delta-t 5)) (* -1/6 (expt delta-t 3)) delta-t))
               (simplify
                ((((e/evolution 6) 'delta-t circular) (chart R2-rect))
                 ((point R2-rect) (up 1 0))))))))))

(deftest section-3-5
  (e/let-coordinates
   [[x y] R2-rect
    [r theta] R2-polar]
   (let [R2->R '(-> (UP Real Real) Real)
         omega (e/components->oneform-field
                (down (literal-function 'a_0 R2->R)
                      (literal-function 'a_1 R2->R))
                R2-rect)
         R2-rect-point ((point R2-rect) (up 'x0 'y0))
         omega2 (e/literal-oneform-field 'a R2-rect)
         circular (- (* x d:dy) (* y d:dx))]
     (is (= '(oneform-field (down a_0 a_1)) (simplify omega))) ;; fix this
     (is (= '(down (a_0 (up x0 y0)) (a_1 (up x0 y0)))
            (simplify ((omega (down d:dx d:dy)) R2-rect-point))))
     (is (= '(down (a_0 (up x0 y0)) (a_1 (up x0 y0)))
            (simplify ((omega2 (down d:dx d:dy)) R2-rect-point))))
     (is (= '(down (((partial 0) f-rect) (up x0 y0))
                   (((partial 1) f-rect) (up x0 y0)))
            (simplify
             (((e/d (literal-manifold-function 'f-rect R2-rect))
               (e/coordinate-system->vector-basis R2-rect))
              R2-rect-point))))
     (is (= '(down (/ (+ (* r (cos theta) (((partial 0) f-polar) (up r theta)))
                         (* -1N (sin theta) (((partial 1) f-polar) (up r theta))))
                      r)
                   (/ (+ (* r (sin theta) (((partial 0) f-polar) (up r theta)))
                         (* (cos theta) (((partial 1) f-polar) (up r theta))))
                      r))
            (simplify
             (((e/d (literal-manifold-function 'f-polar R2-polar))
               (e/coordinate-system->vector-basis R2-rect))
              ((point R2-polar) (up 'r 'theta))))))
     (is (= 0 ((dx d:dy) R2-rect-point)))
     (is (= 1 ((dx d:dx) R2-rect-point)))
     (is (= '(* -1 y0) (simplify ((dx circular) R2-rect-point))))
     (is (= 'x0 ((dy circular) R2-rect-point)))
     (is (= 0 (simplify ((dr circular) R2-rect-point))))
     (is (= 1 (simplify ((dtheta circular) R2-rect-point))))
     (let [f (literal-manifold-function 'f-rect R2-rect)]
       (is (= 0 (simplify (((- circular d:dtheta) f) R2-rect-point)))))
     (let [v (literal-vector-field 'b R2-rect)]
       (is (= '(+ (* (a_0 (up x0 y0)) (b↑0 (up x0 y0)))
                  (* (a_1 (up x0 y0)) (b↑1 (up x0 y0))))
              (simplify ((omega v) R2-rect-point))))))))
