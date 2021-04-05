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

(ns sicmutils.sr.frames-test
  (:refer-clojure :exclude [+ - * /])
  (:require [clojure.test :refer [is deftest testing use-fixtures]]
            [sicmutils.calculus.frame :as cf]
            [sicmutils.calculus.manifold :as m]
            [sicmutils.sr.frames :as sf]
            [sicmutils.generic :as g :refer [+ - * /]]
            [sicmutils.simplify :refer [hermetic-simplify-fixture]]
            [sicmutils.structure :as s :refer [up down]]
            [sicmutils.value :as v]))

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
      (is (= '(/ (+ (* (expt C 2) va)
                    (* -1 (expt C 2) vb)
                    (* 2 C vb (sqrt (+ (expt C 2) (* -1 (expt va 2))))))
                 (+ (expt C 2) (* va vb)))
             (simplify
              (/ (get foo 1)
                 (/ (get foo 0) 'C))))
          "NOTE: the current simplifier can't get this down as tight as
          scmutils. This is correct... but there is some cancellation that must
          have happened along the way.")

      (comment
        (is (= 0 (simplify
                  (- (/ (+ 'va 'vb)
                        (+ 1 (* (/ 'va 'C) (/ 'vb 'C))))
                     (/ (get foo 1)
                        (/ (get foo 0) 'C)))))
            "This is what we actually want. TODO test and enable when we get the
            simplifier upgraded."))))

  (testing "Simple test of reversibility"
    (let [A (sf/make-SR-frame 'A sf/the-ether
                              (up 1 0 0)
                              'va:c
                              (sf/make-SR-coordinates sf/the-ether
                                                      (up 'cta 'xa 'ya 'za)))]

      (comment
        ;; The current simplifier is incapable of getting back to this much
        ;; simpler result that we want. I've verified with scmutils that the
        ;; unsimplified result is correct; but we want this!
        (is (= '(up ct x y z)
               (simplify
                ((m/chart A)
                 ((m/point A)
                  (sf/make-SR-coordinates A (up 'ct 'x 'y 'z))))))))

      (is (= '(up (/ (+ (* ct (expt va:c 2))
                        (* 2 va:c x (sqrt (+ (* -1 (expt va:c 2)) 1)))
                        (* -2 va:c x)
                        (* -1 ct))
                     (+ (expt va:c 2) -1))
                  (/ (+ (* -2 ct va:c (sqrt (+ (* -1 (expt va:c 2)) 1)))
                        (* 5 (expt va:c 2) x)
                        (* 2 ct va:c)
                        (* 4 x (sqrt (+ (* -1 (expt va:c 2)) 1)))
                        (* -5 x))
                     (+ (expt va:c 2) -1))
                  y z)
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

        (comment
          ;; Same note here as above. The current simplifier isn't able to
          ;; obtain this clean result; but the raw formula is indeed fine.
          (is (= '(up ct x y z)
                 (simplify
                  ((m/chart B)
                   ((m/point B)
                    (sf/make-SR-coordinates B (up 'ct 'x 'y 'z))))))))))

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
                          (* 2 xba (sqrt (+ (* -1 (expt va:c 2)) 1)))
                          (* -1 xba))
                       (sqrt (+ (* -1 (expt va:c 2)) 1)))
                    (+ ya yba)
                    (+ za zba))
               (simplify origin-B)))))))
