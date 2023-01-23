#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.calculus.metric-test
  (:refer-clojure :exclude [+ - * /])
  (:require [clojure.test :refer [is deftest testing use-fixtures]]
            [emmy.abstract.function :as af]
            [emmy.calculus.basis :as b]
            [emmy.calculus.coordinate :refer [let-coordinates]]
            [emmy.calculus.form-field :as ff]
            [emmy.calculus.manifold :as m]
            [emmy.calculus.metric :as cm]
            [emmy.calculus.vector-field :as vf]
            [emmy.function :refer [compose]]
            [emmy.generic :as g :refer [+ *]]
            [emmy.matrix :as matrix]
            [emmy.simplify :refer [hermetic-simplify-fixture]]
            [emmy.structure :as s :refer [up]]
            [emmy.util.aggregate :as ua]
            [emmy.value :as v]))

(use-fixtures :each hermetic-simplify-fixture)

(def simplify
  (comp v/freeze g/simplify))

(deftest metric-tests
  (testing "Example: natural metric on a sphere of radius R"
    (let [two-sphere m/R2-rect]
      (let-coordinates [[theta phi] two-sphere]
        (let [g-sphere (fn [R]
                         (fn [u v]
                           (* (g/square R)
                              (+ (* (dtheta u) (dtheta v))
                                 (* (compose (g/square g/sin) theta)
                                    (dphi u)
                                    (dphi v))))))
              u (vf/literal-vector-field 'u two-sphere)
              v (vf/literal-vector-field 'v two-sphere)]
          (is (= '(+ (* (expt R 2)
                        (expt (sin theta0) 2)
                        (u↑1 (up theta0 phi0))
                        (v↑1 (up theta0 phi0)))
                     (* (expt R 2)
                        (u↑0 (up theta0 phi0))
                        (v↑0 (up theta0 phi0))))
                 (simplify
                  (((g-sphere 'R) u v)
                   ((m/point two-sphere)
                    (up 'theta0 'phi0))))))))))

  (testing "Example: Lorentz metric on R↑4"
    (let [SR m/R4-rect]
      (let-coordinates [[t x y z] SR]
        (let [g-Lorentz (fn [c]
                          (fn [u v]
                            (+ (* (dx u) (dx v))
                               (* (dy u) (dy v))
                               (* (dz u) (dz v))
                               (* -1 (g/square c)
                                  (dt u) (dt v)))))
              u (vf/literal-vector-field 'u SR)
              v (vf/literal-vector-field 'v SR)]
          (is (= '(+ (* -1 (expt c 2)
                        (u↑0 (up t0 x0 y0 z0)) (v↑0 (up t0 x0 y0 z0)))
                     (* (u↑1 (up t0 x0 y0 z0)) (v↑1 (up t0 x0 y0 z0)))
                     (* (u↑2 (up t0 x0 y0 z0)) (v↑2 (up t0 x0 y0 z0)))
                     (* (u↑3 (up t0 x0 y0 z0)) (v↑3 (up t0 x0 y0 z0))))
                 (simplify
                  (((g-Lorentz 'c) u v)
                   ((m/point SR) (up 't0 'x0 'y0 'z0))))))))))

  (testing "Example: general metric on R↑2"
    (let-coordinates [[x y] m/R2-rect]
      (let [g-R2 (fn [g_00 g_01 g_11]
                   (fn [u v]
                     (+ (* g_00 (dx u) (dx v))
                        (* g_01 (+ (* (dx u) (dy v))
                                   (* (dy u) (dx v))))
                        (* g_11 (dy u) (dy v)))))
            u (vf/literal-vector-field 'u R2-rect)
            v (vf/literal-vector-field 'v R2-rect)]
        (is (= '(+ (* a (u↑0 (up x0 y0)) (v↑0 (up x0 y0)))
                   (* b (u↑0 (up x0 y0)) (v↑1 (up x0 y0)))
                   (* b (v↑0 (up x0 y0)) (u↑1 (up x0 y0)))
                   (* c (v↑1 (up x0 y0)) (u↑1 (up x0 y0))))
               (simplify
                (((g-R2 'a 'b 'c) u v)
                 ((m/point R2-rect) (up 'x0 'y0)))))))))

  (testing "metric examples"
    (is (= '(down (down 1 0 0)
                  (down 0 (expt r 2) 0)
                  (down 0 0 (* (expt r 2) (expt (sin theta) 2))))
           (simplify
            ((cm/coordinate-system->metric-components m/R3-spherical)
             (up 'r 'theta 'phi)))))

    (is (= '(down (down 1 0 0)
                  (down 0 (expt r 2) 0)
                  (down 0 0 (* (expt r 2) (expt (sin theta) 2))))
           (simplify
            (s/mapr
             (fn [v1]
               (s/mapr
                (fn [v2]
                  (((cm/coordinate-system->metric m/R3-spherical) v1 v2)
                   ((m/point m/R3-spherical) (up 'r 'theta 'phi))))
                (vf/coordinate-system->vector-basis m/R3-spherical)))
             (vf/coordinate-system->vector-basis m/R3-spherical)))))

    (is (= '(up (up 1 0 0)
                (up 0 (/ 1 (expt r 2)) 0)
                (up 0 0 (/ 1 (* (expt r 2) (expt (sin theta) 2)))))
           (simplify
            (s/mapr
             (fn [w1]
               (s/mapr
                (fn [w2]
                  (((cm/coordinate-system->inverse-metric m/R3-spherical) w1 w2)
                   ((m/point m/R3-spherical) (up 'r 'theta 'phi))))
                (ff/coordinate-system->oneform-basis m/R3-spherical)))
             (ff/coordinate-system->oneform-basis m/R3-spherical))))))

  (testing "more"
    (let-coordinates [[x y z] m/R3-rect]
      (is (= '(+ (* (g_00 (up x0 y0 z0)) (u↑0 (up x0 y0 z0)) (v↑0 (up x0 y0 z0)))
                 (* (u↑0 (up x0 y0 z0)) (g_01 (up x0 y0 z0)) (v↑1 (up x0 y0 z0)))
                 (* (u↑0 (up x0 y0 z0)) (g_02 (up x0 y0 z0)) (v↑2 (up x0 y0 z0)))
                 (* (v↑0 (up x0 y0 z0)) (g_01 (up x0 y0 z0)) (u↑1 (up x0 y0 z0)))
                 (* (v↑0 (up x0 y0 z0)) (g_02 (up x0 y0 z0)) (u↑2 (up x0 y0 z0)))
                 (* (v↑1 (up x0 y0 z0)) (u↑1 (up x0 y0 z0)) (g_11 (up x0 y0 z0)))
                 (* (v↑1 (up x0 y0 z0)) (g_12 (up x0 y0 z0)) (u↑2 (up x0 y0 z0)))
                 (* (v↑2 (up x0 y0 z0)) (u↑1 (up x0 y0 z0)) (g_12 (up x0 y0 z0)))
                 (* (v↑2 (up x0 y0 z0)) (u↑2 (up x0 y0 z0)) (g_22 (up x0 y0 z0))))
             (simplify
              (((cm/literal-metric 'g R3-rect)
                (vf/literal-vector-field 'u R3-rect)
                (vf/literal-vector-field 'v R3-rect))
               ((m/point R3-rect) (up 'x0 'y0 'z0)))))))

    (let-coordinates [[x y] m/R2-rect]
      (let [R2-basis (b/coordinate-system->basis R2-rect)
            g-R2 (fn [g_00 g_01 g_11]
                   (fn [u v]
                     (+ (* g_00 (dx u) (dx v))
                        (* g_01 (+ (* (dx u) (dy v))
                                   (* (dy u) (dx v))))
                        (* g_11 (dy u) (dy v)))))
            omega (ff/literal-oneform-field 'omega R2-rect)
            theta (ff/literal-oneform-field 'theta R2-rect)]
        (is (= '(/ (+ (* a (omega_1 (up x0 y0)) (theta_1 (up x0 y0)))
                      (* -1 b (omega_1 (up x0 y0)) (theta_0 (up x0 y0)))
                      (* -1 b (theta_1 (up x0 y0)) (omega_0 (up x0 y0)))
                      (* c (omega_0 (up x0 y0)) (theta_0 (up x0 y0))))
                   (+ (* a c) (* -1 (expt b 2))))
               (simplify
                (((cm/invert (g-R2 'a 'b 'c) R2-basis)
                  omega theta)
                 ((m/point R2-rect) (up 'x0 'y0))))))

        (testing "test of inversion"
          (let [g (g-R2 'a 'b 'c)
                gi (cm/invert g R2-basis)
                vector-basis [d:dx d:dy]
                dual-basis [dx dy]
                m ((m/point R2-rect) (up 'x0 'y0))]
            (is (= '(matrix-by-rows
                     (up 1 0) (up 0 1))
                   (simplify
                    (matrix/generate
                     2 2
                     (fn [i k]
                       (ua/generic-sum
                        (fn [j]
                          (* ((gi (nth dual-basis i) (nth dual-basis j)) m)
                             ((g (nth vector-basis j) (nth vector-basis k)) m)))
                        0 2))))))))

        (testing "raise/lower"
          ;; Note: raise needs an extra argument -- the basis -- why?
          (is (= '(+ (* a (v↑0 (up x0 y0)) (w↑0 (up x0 y0)))
                     (* b (v↑0 (up x0 y0)) (w↑1 (up x0 y0)))
                     (* b (v↑1 (up x0 y0)) (w↑0 (up x0 y0)))
                     (* c (v↑1 (up x0 y0)) (w↑1 (up x0 y0))))
                 (simplify
                  ((((cm/lower (g-R2 'a 'b 'c))
                     (vf/literal-vector-field 'v R2-rect))
                    (vf/literal-vector-field 'w R2-rect))
                   ((m/point R2-rect) (up 'x0 'y0))))))

          (is (= '(+ (* (v↑0 (up x0 y0)) (((partial 0) w) (up x0 y0)))
                     (* (v↑1 (up x0 y0)) (((partial 1) w) (up x0 y0))))
                 (simplify
                  ((((cm/raise (g-R2 'a 'b 'c) R2-basis)
                     ((cm/lower (g-R2 'a 'b 'c)) (vf/literal-vector-field 'v R2-rect)))
                    (compose (af/literal-function 'w '(-> (UP Real Real) Real))
	                           (m/chart R2-rect)))
                   ((m/point R2-rect) (up 'x0 'y0))))))

          (is (= '(+ (* (v↑0 (up x0 y0)) (((partial 0) w) (up x0 y0)))
                     (* (v↑1 (up x0 y0)) (((partial 1) w) (up x0 y0))))
                 (simplify
                  ((((cm/sharpen (g-R2 'a 'b 'c) R2-basis ((m/point R2-rect) (up 'x0 'y0)))
                     ((cm/lower (g-R2 'a 'b 'c)) (vf/literal-vector-field 'v R2-rect)))
                    (compose (af/literal-function 'w '(-> (UP Real Real) Real))
	                           (m/chart R2-rect)))
                   ((m/point R2-rect) (up 'x0 'y0)))))
              "NOTE could this be right? The sum components are in an up-tuple
               in the metric.scm example."))))))
