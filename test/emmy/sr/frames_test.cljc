#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.sr.frames-test
  (:refer-clojure :exclude [+ - * /])
  (:require [clojure.test :refer [is deftest testing use-fixtures]]
            [emmy.calculus.frame :as cf]
            [emmy.calculus.manifold :as m]
            [emmy.generic :as g :refer [+ - * /]]
            [emmy.simplify :refer [hermetic-simplify-fixture]]
            [emmy.sr.frames :as sf]
            [emmy.structure :as s :refer [up]]
            [emmy.value :as v]))

(use-fixtures :each hermetic-simplify-fixture)

(def simplify
  (comp v/freeze g/simplify))

(deftest sr-frames-tests
  (testing "Velocity addition formula"
    (let [A (sf/make-SR-frame 'A
                              sf/the-ether
                              (up 1 0 0)
                              (/ 'va 'C)
                              (sf/make-SR-coordinates sf/the-ether (up 0 0 0 0)))
          B (sf/make-SR-frame 'B
                              A
                              (up 1 0 0)
                              (/ 'vb 'C)
                              (sf/make-SR-coordinates A (up 0 0 0 0)))
          foo ((m/chart sf/the-ether)
               ((m/point B)
                (sf/make-SR-coordinates B (up (* 'C 'tau) 0 0 0))))]
      (is (= 0 (simplify
                (- (/ (+ 'va 'vb)
                      (+ 1 (* (/ 'va 'C) (/ 'vb 'C))))
                   (/ (get foo 1)
                      (/ (get foo 0) 'C))))))))

  (testing "Simple test of reversibility"
    (let [A (sf/make-SR-frame 'A sf/the-ether
                              (up 1 0 0)
                              'va:c
                              (sf/make-SR-coordinates sf/the-ether
                                                      (up 'cta 'xa 'ya 'za)))]
      (is (= '(up ct x y z)
             (simplify
              ((m/chart A)
               ((m/point A)
                (sf/make-SR-coordinates A (up 'ct 'x 'y 'z))))))))

    (testing "The ether coordinates of the origin of A relative to 'the ether'"
      (let [A (sf/make-SR-frame 'A sf/the-ether
                                (up 1 0 0)
                                'va:c
                                (sf/make-SR-coordinates sf/the-ether
                                                        (up 'cta 'xa 'ya 'za)))
            origin-A (sf/coordinate-origin A)
            B
            (sf/make-SR-frame 'B A (up 1 0 0) 'vba:c
                              (sf/make-SR-coordinates A
                                                      (up 'ctba 'xba 'yba 'zba)))]
        (is (= 'the-ether
               (cf/frame-name
                (cf/frame-owner origin-A))))

        (is (= '(up ct x y z)
               (simplify
                ((m/chart B)
                 ((m/point B)
                  (sf/make-SR-coordinates B (up 'ct 'x 'y 'z)))))))))

    (testing "poincare formula"
      (let [A (sf/make-SR-frame 'A sf/the-ether (up 1 0 0)
                                'va:c
                                (sf/make-SR-coordinates
                                 sf/the-ether
                                 (up 'cta 'xa 'ya 'za)))
            B (sf/make-SR-frame 'B A (up 1 0 0)
                                'vba:c
                                (sf/make-SR-coordinates
                                 A
                                 (up 'ctba 'xba 'yba 'zba)))

            ;; The ether coordinates of the origin of B relative to "the ether"
            ;; is
            origin-B ((m/chart sf/the-ether)
                      ((m/point A)
                       (sf/coordinate-origin B)))]
        (is (= '(up (/ (+ (* cta (sqrt (+ (* -1 (expt va:c 2)) 1)))
                          (* va:c xba)
                          ctba)
                       (sqrt (+ (* -1 (expt va:c 2)) 1)))
                    (/ (+ (* ctba va:c)
                          (* xa (sqrt (+ (* -1 (expt va:c 2)) 1)))
                          xba)
                       (sqrt (+ (* -1 (expt va:c 2)) 1)))
                    (+ ya yba)
                    (+ za zba))
               (simplify origin-B)))))))
