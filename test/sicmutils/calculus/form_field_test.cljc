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
      (is (v/= 1 (g/simplify ((circular theta) mr))))
      (is (v/= 0 (g/simplify ((dr circular) mr))))
      (is (v/= 1 (((ff/d r) d:dr) mr)))
      (is (v/= 1 (g/simplify ((dr d:dr) mr))))

      (is (v/= '(v↑0 (up (sqrt (+ (expt x0 2) (expt y0 2))) (atan y0 x0)))
               (g/simplify ((dr vp) mr))))

      (is (v/= '(v↑0 (up (sqrt (+ (expt x0 2) (expt y0 2))) (atan y0 x0)))
               (g/simplify (((ff/d r) vp) mr))))

      (is (v/= '(/ (+ (* x0 (v↑0 (up x0 y0))) (* y0 (v↑1 (up x0 y0))))
                   (sqrt (+ (expt x0 2) (expt y0 2))))
               (g/simplify ((dr vr) mr))))

      (is (v/= '(/ (+ (* x0 (v↑0 (up x0 y0))) (* y0 (v↑1 (up x0 y0))))
                   (sqrt (+ (expt x0 2) (expt y0 2))))
               (g/simplify (((ff/d r) vr) mr))))

      (is (v/= 0 (g/simplify ((residual vr vr) mr))))
      (is (v/= 0 (g/simplify ((residual vp vp) mr))))
      (is (v/= 0 (g/simplify ((residual vp vp) mp))))
      (is (v/= 0 (g/simplify ((residual vr vr) mp)))))))

(comment
  ;; tests from form-field.
  (install-coordinates R3-rect (up 'x 'y 'z))

  (def mr ((R3-rect '->point) (up 'x0 'y0 'z0)))

  (def a-oneform
    (components->oneform-field
     (down (literal-function 'ax (-> (UP* Real) Real))
	         (literal-function 'ay (-> (UP* Real) Real))
	         (literal-function 'az (-> (UP* Real) Real)))
     R3-rect))

  (def a-vector-field
    (components->vector-field
     (up (literal-function 'vx (-> (UP* Real) Real))
         (literal-function 'vy (-> (UP* Real) Real))
         (literal-function 'vz (-> (UP* Real) Real)))
     R3-rect))

  (pec ((a-oneform a-vector-field) mr))
  ;; Result:
  (+ (* (vx (up x0 y0 z0)) (ax (up x0 y0 z0)))
     (* (vy (up x0 y0 z0)) (ay (up x0 y0 z0)))
     (* (vz (up x0 y0 z0)) (az (up x0 y0 z0))))


  (pec ((oneform-field->components a-oneform R3-rect) (up 'x0 'y0 'z0)))
  ;; Result:
  (down (ax (up x0 y0 z0)) (ay (up x0 y0 z0)) (az (up x0 y0 z0)))


  (install-coordinates R3-cyl (up 'r 'theta 'zeta))

  (pec ((oneform-field->components a-oneform R3-cyl) (up 'r0 'theta0 'z0)))
  ;; Result:
  (down
   (+ (* (sin theta0) (ay (up (* r0 (cos theta0)) (* r0 (sin theta0)) z0)))
      (* (cos theta0) (ax (up (* r0 (cos theta0)) (* r0 (sin theta0)) z0))))
   (+ (* -1 r0 (sin theta0) (ax (up (* r0 (cos theta0)) (* r0 (sin theta0)) z0)))
      (* r0 (cos theta0) (ay (up (* r0 (cos theta0)) (* r0 (sin theta0)) z0))))
   (az (up (* r0 (cos theta0)) (* r0 (sin theta0)) z0)))





  (def mr ((R3-rect '->point) (up 'x0 'y0 'z0)))
  (def mp ((R3-cyl '->point) (up 'r0 'theta0 'z0)))

  ((dx d/dx) mr)
                                        ;Value 1

  ((dx d/dx) mp)
                                        ;Value 1

  (pec ((oneform-field->components dr R3-rect) (up 'x0 'y0 'z0)))
  ;; Result:
  (down (/ x0 (sqrt (+ (expt x0 2) (expt y0 2))))
        (/ y0 (sqrt (+ (expt x0 2) (expt y0 2))))
        0)


  (pec ((oneform-field->components dtheta R3-rect) (up 'x0 'y0 'z0)))
  ;; Result:
  (down (/ (* -1 y0) (+ (expt x0 2) (expt y0 2)))
        (/ x0 (+ (expt x0 2) (expt y0 2)))
        0)


  (pec (((+ (* 'w_0 dr) (* 'w_1 dtheta)) (+ (* 'V↑0 d/dx) (* 'V↑1 d/dy))) mp))
  ;; Result:
  (+ (* V↑0 w_0 (cos theta0))
     (* V↑1 w_0 (sin theta0))
     (/ (* -1 V↑0 w_1 (sin theta0)) r0)
     (/ (* V↑1 w_1 (cos theta0)) r0))


  (pec
   (((components->oneform-field (oneform-field->components
			                           (+ (* 'w_0 dr) (* 'w_1 dtheta))
			                           R3-rect)
			                          R3-rect)
     (+ (* 'V↑0 d/dx) (* 'V↑1 d/dy)))
    mp))
  ;; Result:
  (+ (* V↑0 w_0 (cos theta0))
     (* V↑1 w_0 (sin theta0))
     (/ (* -1 V↑0 w_1 (sin theta0)) r0)
     (/ (* V↑1 w_1 (cos theta0)) r0))

  (def counter-clockwise (- (* x d/dy) (* y d/dx)))

  (def outward (+ (* x d/dx) (* y d/dy)))


  (pec ((dx counter-clockwise) mr))
  ;; Result:
  (* -1 y0)


  (pec ((dx outward) mr))
  ;; Result:
  x0


  (pec ((dr counter-clockwise) mp))
  ;; Result:
  0


  (pec ((dr outward) mp))
  ;; Result:
  r0


  (pec ((dr outward) mr))
  ;; Result:
  (sqrt (+ (expt x0 2) (expt y0 2)))


  (pec (((* x dy) (+ (* 'u d/dx) (* 'v d/dy))) mr))
  ;; Result:
  (* v x0)


  (pec ((dr d/dr) ((R3-rect '->point) (up 'x↑0 'y↑0 'z↑0))))
  ;; Result:
  1


  (pec ((dr d/dtheta) ((R3-rect '->point) (up 'x↑0 'y↑0 'z↑0))))
  ;; Result:
  0


  (pec ((dtheta d/dr) ((R3-rect '->point) (up 'x↑0 'y↑0 'z↑0))))
  ;; Result:
  0


  (pec ((dtheta d/dtheta) ((R3-rect '->point) (up 'x↑0 'y↑0 'z↑0))))
  ;; Result:
  1




  )
