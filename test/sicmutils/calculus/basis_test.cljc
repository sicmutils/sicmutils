#_"SPDX-License-Identifier: GPL-3.0"

(ns sicmutils.calculus.basis-test
  (:require [clojure.test :refer [is deftest testing]]
            [sicmutils.abstract.function :refer [literal-function]]
            [sicmutils.calculus.basis :as b]
            [sicmutils.calculus.coordinate :as c
             #?(:clj :refer :cljs :refer-macros) [let-coordinates]]
            [sicmutils.calculus.form-field :as ff]
            [sicmutils.calculus.manifold :as m]
            [sicmutils.calculus.vector-field :as vf]
            [sicmutils.generic :as g]
            [sicmutils.structure :as s :refer [up down]]
            [sicmutils.value :as v]))

(deftest basis-tests
  (testing "vector-basis->dual"
    (let-coordinates [[theta phi] m/S2-spherical]
      (let [e0 (vf/components->vector-field
                (up (literal-function 'e0t '(-> (UP* Real 2) Real))
                    (literal-function 'e0p '(-> (UP* Real 2) Real)))
                S2-spherical)
            e1 (vf/components->vector-field
                (up (literal-function 'e1t '(-> (UP* Real 2) Real))
                    (literal-function 'e1p '(-> (UP* Real 2) Real)))
                S2-spherical)
            edual (b/vector-basis->dual (down e0 e1) S2-spherical)]
        (is (= (up (down 1 0)
                   (down 0 1))
               (g/simplify
                ((edual (down e0 e1))
                 ((m/point S2-spherical)
                  (up 'theta0 'phi0)))))))))

  (testing "Jacobian"
    (let [v   (vf/literal-vector-field 'v m/R2-rect)
          vjp (g/* (b/Jacobian
                    (b/coordinate-system->basis m/R2-polar)
                    (b/coordinate-system->basis m/R2-rect))
                   ((ff/coordinate-system->oneform-basis m/R2-rect) v))]
      (is (= '(up
               (/ (+ (* x (v↑0 (up x y))) (* y (v↑1 (up x y))))
                  (sqrt (+ (expt x 2) (expt y 2))))
               (/ (+ (* x (v↑1 (up x y))) (* -1 y (v↑0 (up x y))))
                  (+ (expt x 2) (expt y 2))))
             (v/freeze
              (g/simplify
               (vjp ((m/point m/R2-rect) (up 'x 'y)))))))))

  (testing "test ported from dgutils.scm"
    (let [chi-R2         (m/chart m/R2-rect)
          chi-inverse-R2 (m/point m/R2-rect)
          R2-basis       (b/coordinate-system->basis m/R2-rect)]
      (is (v/= '(+ (((partial 0) f) (up x0 y0))
                   (((partial 1) f) (up x0 y0)))
               (s/sumr (fn [e]
		                     ((e (comp (literal-function 'f '(-> (UP Real Real) Real))
			                             chi-R2))
		                      (chi-inverse-R2 (up 'x0 'y0))))
		                   (b/basis->vector-basis R2-basis)))))))
