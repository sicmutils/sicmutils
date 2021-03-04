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

(ns sicmutils.calculus.form-field-test
  (:refer-clojure :exclude [+ - * /])
  (:require [clojure.test :refer [is deftest testing]]
            [sicmutils.calculus.coordinate :as c
             :refer [let-coordinates]
             #?@(:cljs [:include-macros true])]
            [sicmutils.calculus.manifold :as m :refer [R2-rect R2-polar]]
            [sicmutils.calculus.form-field :as ff]
            [sicmutils.calculus.vector-field :as vf]
            [sicmutils.generic :as g :refer [+ - * /]]
            [sicmutils.structure :refer [up down]]
            [sicmutils.value :as v]))

(deftest permutation-test
  (is (thrown? #?(:clj Exception :cljs js/Error)
               (vec (ff/permutation-sequence 0))))
  (is (= '[[a]] (ff/permutation-sequence '[a])))
  (is (= '[[a b] [b a]]
         (ff/permutation-sequence '(a b))))
  (is (= [[0 1 2] [0 2 1] [2 0 1] [2 1 0] [1 2 0] [1 0 2]]
         (ff/permutation-sequence [0 1 2])))
  (is (= [[[0 1 2] 1]
          [[0 2 1] -1]
          [[2 0 1] 1]
          [[2 1 0] -1]
          [[1 2 0] 1]
          [[1 0 2] -1]]
         (map vector (ff/permutation-sequence (range 3)) (cycle [1 -1]))))
  (is (= [[0 1 2 3] [0 1 3 2] [0 3 1 2] [3 0 1 2]
          [3 0 2 1] [0 3 2 1] [0 2 3 1] [0 2 1 3]
          [2 0 1 3] [2 0 3 1] [2 3 0 1] [3 2 0 1]
          [3 2 1 0] [2 3 1 0] [2 1 3 0] [2 1 0 3]
          [1 2 0 3] [1 2 3 0] [1 3 2 0] [3 1 2 0]
          [3 1 0 2] [1 3 0 2] [1 0 3 2] [1 0 2 3]]
         (ff/permutation-sequence (range 4))))

  ;; being the test material found in the comments of manifold.scm of scmutils
  )

(deftest manifold-test
  (let-coordinates [[x y] R2-rect
                    [r theta] R2-polar]
    (let [mr ((m/point R2-rect) (up 'x0 'y0))
          mp ((m/point R2-polar) (up 'r0 'theta0))
          circular (- (* x d:dy) (* y d:dx))
          g-polar (fn [u v] (+ (* (dr u) (dr v))
                              (* (* r (dtheta u)) (* r (dtheta v)))))
          g-rect (fn [u v] (+ (* (dx u) (dx v))
                             (* (dy u) (dy v))))
          residual (- g-polar g-rect)
          vp (vf/literal-vector-field 'v R2-polar)
          vr (vf/literal-vector-field 'v R2-rect)]
      (is (= 1 (g/simplify ((circular theta) mr))))
      (is (= 0 (g/simplify ((dr circular) mr))))
      (is (= 1 (((ff/d r) d:dr) mr)))
      (is (= 1 (g/simplify ((dr d:dr) mr))))
      (is (= '(v↑0 (up (sqrt (+ (expt x0 2) (expt y0 2))) (atan y0 x0)))
             (g/simplify ((dr vp) mr))))
      (is (= '(v↑0 (up (sqrt (+ (expt x0 2) (expt y0 2))) (atan y0 x0)))
             (g/simplify (((ff/d r) vp) mr))))
      (is (= '(/ (+ (* x0 (v↑0 (up x0 y0))) (* y0 (v↑1 (up x0 y0))))
                 (sqrt (+ (expt x0 2) (expt y0 2))))
             (g/simplify ((dr vr) mr))))

      (is (= '(/ (+ (* x0 (v↑0 (up x0 y0))) (* y0 (v↑1 (up x0 y0))))
                 (sqrt (+ (expt x0 2) (expt y0 2))))
             (g/simplify (((ff/d r) vr) mr))))
      (is (v/= 0 (g/simplify ((residual vr vr) mr))))
      (is (v/= 0 (g/simplify ((residual vp vp) mr))))
      (is (v/= 0 (g/simplify ((residual vp vp) mp))))
      (is (v/= 0 (g/simplify ((residual vr vr) mp)))))))
