#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.fdg.ch6-test
  (:refer-clojure :exclude [+ - * / zero? partial])
  (:require [clojure.test :refer [is deftest testing use-fixtures]]
            [emmy.env :as e :refer [- zero?
                                    d compose
                                    literal-function
                                    up
                                    point chart
                                    R1-rect R2-rect R3-rect
                                    S2-spherical
                                    let-coordinates]]
            [emmy.simplify :refer [hermetic-simplify-fixture]]))

(use-fixtures :each hermetic-simplify-fixture)

(def simplify
  (comp e/freeze e/simplify))

(deftest section-6-3
  (testing "walking on a sphere, p75"
    (let-coordinates [[theta phi] S2-spherical
                      t           R1-rect]
      (let [S2-basis (e/coordinate-system->basis S2-spherical)
            mu (compose (point S2-spherical)
                        (up (literal-function 'theta)
                            (literal-function 'phi))
                        (chart R1-rect))
            S2-basis-over-mu (e/basis->basis-over-map mu S2-basis)
            h (e/literal-manifold-function 'h-spherical S2-spherical)]
        (is (= '(down
                 (((partial 0) h-spherical) (up (theta t0) (phi t0)))
                 (((partial 1) h-spherical) (up (theta t0) (phi t0))))
               (simplify
                (((e/basis->vector-basis S2-basis-over-mu) h)
                 ((point R1-rect) 't0))))
            "page 75")

        (is (= '(up (down 1 0)
                    (down 0 1))
               (simplify
                (((e/basis->oneform-basis S2-basis-over-mu)
                  (e/basis->vector-basis S2-basis-over-mu))
                 ((point R1-rect) 't0))))
            "page 76: We can check that the dual basis over the map does the
             correct thing")

        (is (= '(up ((D theta) t0) ((D phi) t0))
               (simplify
                (((e/basis->oneform-basis S2-basis-over-mu)
                  ((e/differential mu) d:dt))
                 ((point R1-rect) 't0))))
            "coordinate velocities on a sphere")))))

(deftest section-6-4
  (let [mu (e/literal-manifold-map 'MU R2-rect R3-rect)
        f (e/literal-manifold-function 'f-rect R3-rect)
        X (e/literal-vector-field 'X-rect R2-rect)]
    (is (zero?
         (simplify
          (((- ((e/pullback mu) (d f)) (d ((e/pullback mu) f))) X)
           ((point R2-rect) (up 'x0 'y0)))))
        "page 81")

    (let [theta (e/literal-oneform-field 'THETA R3-rect)
          Y (e/literal-vector-field 'Y-rect R2-rect)]
      (is (zero?
           (simplify
            (((- ((e/pullback mu) (d theta))
                 (d ((e/pullback mu) theta)))
              X Y)
             ((point R2-rect) (up 'x0 'y0)))))
          "page 81"))))
