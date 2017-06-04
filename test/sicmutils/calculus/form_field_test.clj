(ns sicmutils.calculus.form-field-test
  (:refer-clojure :exclude [+ - * / ref zero? partial])
  (:require [clojure.test :refer :all]
            [sicmutils.calculus.form-field :refer [permutation-sequence]]
            [sicmutils.env :refer :all]))

(deftest permutation-test
  (is (thrown? Exception (vec (permutation-sequence 0))))
  (is (= '[[a]] (permutation-sequence '[a])))
  (is (= '[[a b] [b a]]
         (permutation-sequence '(a b))))
  (is (= [[0 1 2] [0 2 1] [2 0 1] [2 1 0] [1 2 0] [1 0 2]]
         (permutation-sequence [0 1 2])))
  (is (= [[[0 1 2] 1]
          [[0 2 1] -1]
          [[2 0 1] 1]
          [[2 1 0] -1]
          [[1 2 0] 1]
          [[1 0 2] -1]]
         (map vector (permutation-sequence (range 3)) (cycle [1 -1]))))
  (is (= [[0 1 2 3] [0 1 3 2] [0 3 1 2] [3 0 1 2]
          [3 0 2 1] [0 3 2 1] [0 2 3 1] [0 2 1 3]
          [2 0 1 3] [2 0 3 1] [2 3 0 1] [3 2 0 1]
          [3 2 1 0] [2 3 1 0] [2 1 3 0] [2 1 0 3]
          [1 2 0 3] [1 2 3 0] [1 3 2 0] [3 1 2 0]
          [3 1 0 2] [1 3 0 2] [1 0 3 2] [1 0 2 3]]
         (permutation-sequence (range 4))))

  ;; being the test material found in the comments of manifold.scm of scmutils
  )

(deftest manifold-test
  (let-coordinates [[x y] R2-rect
                    [r theta] R2-polar]
    (let [mr ((point R2-rect) (up 'x0 'y0))
          mp ((point R2-polar) (up 'r0 'theta0))
          circular (- (* x d:dy) (* y d:dx))
          g-polar (fn [u v] (+ (* (dr u) (dr v))
                               (* (* r (dtheta u)) (* r (dtheta v)))))
          g-rect (fn [u v] (+ (* (dx u) (dx v))
                              (* (dy u) (dy v))))
          residual (- g-polar g-rect)
          vp (literal-vector-field 'v R2-polar)
          vr (literal-vector-field 'v R2-rect)]
      (is (= 1 (simplify ((circular theta) mr))))
      (is (= 0 (simplify ((dr circular) mr))))
      (is (= 1 (((d r) d:dr) mr)))
      (is (= 1 (simplify ((dr d:dr) mr))))
      (is (= '(v↑0 (up (sqrt (+ (expt x0 2) (expt y0 2))) (atan y0 x0)))
             (simplify ((dr vp) mr))))
      (is (= '(v↑0 (up (sqrt (+ (expt x0 2) (expt y0 2))) (atan y0 x0)))
             (simplify (((d r) vp) mr))))
      (is (= '(/ (+ (* x0 (v↑0 (up x0 y0))) (* y0 (v↑1 (up x0 y0))))
                 (sqrt (+ (expt x0 2) (expt y0 2))))
             (simplify ((dr vr) mr))))

      (is (= '(/ (+ (* x0 (v↑0 (up x0 y0))) (* y0 (v↑1 (up x0 y0))))
                 (sqrt (+ (expt x0 2) (expt y0 2))))
             (simplify (((d r) vr) mr))))
      (is (= 0 (simplify ((residual vr vr) mr))))
      (is (= 0 (simplify ((residual vp vp) mr))))
      (is (= 0 (simplify ((residual vp vp) mp))))
      (is (= 0 (simplify ((residual vr vr) mp))))
      )))
