#_"SPDX-License-Identifier: GPL-3.0"

(ns sicmutils.sr.boost-test
  (:refer-clojure :exclude [+ - * /])
  (:require [clojure.test :refer [is deftest testing use-fixtures]]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]
             #?@(:cljs [:include-macros true])]
            [sicmutils.function :as f]
            [sicmutils.generators :as sg]
            [sicmutils.generic :as g :refer [-]]
            [sicmutils.mechanics.rotation :as mr]
            [sicmutils.polynomial.gcd :as pg]
            [sicmutils.simplify :refer [hermetic-simplify-fixture]]
            [sicmutils.sr.boost :as sb]
            [sicmutils.structure :as s :refer [up]]
            [sicmutils.value :as v]))

(use-fixtures :each hermetic-simplify-fixture)

(def simplify
  (comp v/freeze g/simplify))

(deftest boost-tests
  (is (= 0 (simplify
            (- (sb/proper-space-interval
                ((sb/general-boost (up 'vx 'vy 'vz))
                 (sb/make-four-tuple 'ct (up 'x 'y 'z))))
               (sb/proper-space-interval
                (sb/make-four-tuple 'ct (up 'x 'y 'z)))))))

  (checking "a 0-velocity boost in any direction does nothing" 100
            [[vx vy vz] (gen/vector sg/real 3)]
            (let [tuple (up 'u0 'u1 'u2 'u3)]
              (is (= (up 0 0 0 0)
                     (g/simplify
                      (- ((sb/general-boost2 (up vx vy vz) 0) tuple)
                         tuple))))))

  (comment
    ;; TODO: enable once GCD can handle expressions like this. The binding below
    ;; is not sufficient.
    (testing "Check of the relation between boosts and rotations."
      (let [beta (up 'bx 'by 'bz)
            xi (sb/make-four-tuple 'ct (up 'x 'y 'z))
            R (f/compose
               (mr/rotate-x 'theta)
               (mr/rotate-y 'phi)
               (mr/rotate-z 'psi))
            R-inverse (f/compose
                       (mr/rotate-z (- 'psi))
                       (mr/rotate-y (- 'phi))
                       (mr/rotate-x (- 'theta)))]
        (binding [pg/*poly-gcd-time-limit* [100 :seconds]]
          (is (= '(up 0 0 0 0)
                 (simplify
                  (- ((sb/general-boost beta) xi)
                     ((f/compose (sb/extended-rotation R-inverse)
                                 (sb/general-boost (R beta))
                                 (sb/extended-rotation R))
                      xi))))))))))
