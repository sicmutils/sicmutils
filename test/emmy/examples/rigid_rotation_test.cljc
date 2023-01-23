#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.examples.rigid-rotation-test
  (:refer-clojure :exclude [+ - * /])
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [emmy.env :as e :refer [up]]
            [emmy.examples.rigid-rotation :as rigid-rotation]
            [emmy.mechanics.rigid :as rigid]
            [emmy.polynomial.gcd :as pg]
            [emmy.simplify :refer [hermetic-simplify-fixture]]))

(use-fixtures :each hermetic-simplify-fixture)

(deftest ^:long smoke
  (binding [pg/*poly-gcd-time-limit* #?(:clj  [2 :seconds]
                                        :cljs [15 :seconds])]
    ;; this test is :long because of the simplification. Remove this
    ;; tag when we fix that.
    (is (= 11 (count (rigid-rotation/evolver 1 0.1 1 1.2 2 0.1 0.1 0.1 1 1 1))))

    ;; Admittedly, I haven't checked this yet.
    (let [expected '(up
                     1
                     (up θdot φdot ψdot)
                     (up
                      (/ (+ (* (expt A 2) (expt φdot 2) (expt (sin ψ) 2) (sin θ) (cos θ))
                            (* -1 A C (expt φdot 2) (expt (sin ψ) 2) (sin θ) (cos θ))
                            (* (expt B 2) (expt φdot 2) (sin θ) (cos θ) (expt (cos ψ) 2))
                            (* -1 B C (expt φdot 2) (sin θ) (cos θ) (expt (cos ψ) 2))
                            (* (expt A 2) θdot φdot (sin ψ) (cos θ) (cos ψ))
                            (* (expt A 2) φdot ψdot (expt (sin ψ) 2) (sin θ))
                            (* -1 A C θdot φdot (sin ψ) (cos θ) (cos ψ))
                            (* -1 A C φdot ψdot (expt (sin ψ) 2) (sin θ))
                            (* -1 (expt B 2) θdot φdot (sin ψ) (cos θ) (cos ψ))
                            (* (expt B 2) φdot ψdot (sin θ) (expt (cos ψ) 2))
                            (* B C θdot φdot (sin ψ) (cos θ) (cos ψ))
                            (* -1 B C φdot ψdot (sin θ) (expt (cos ψ) 2))
                            (* (expt A 2) θdot ψdot (sin ψ) (cos ψ))
                            (* -1 A C θdot ψdot (sin ψ) (cos ψ))
                            (* -1 (expt B 2) θdot ψdot (sin ψ) (cos ψ))
                            (* B C θdot ψdot (sin ψ) (cos ψ))
                            (* -1 A B φdot ψdot (sin θ)))
                         (* A B))
                      (/ (+ (* -1 (expt A 2) (expt φdot 2) (sin ψ) (sin θ) (cos θ) (cos ψ))
                            (* A C (expt φdot 2) (sin ψ) (sin θ) (cos θ) (cos ψ))
                            (* (expt B 2) (expt φdot 2) (sin ψ) (sin θ) (cos θ) (cos ψ))
                            (* -1 B C (expt φdot 2) (sin ψ) (sin θ) (cos θ) (cos ψ))
                            (* -1 (expt A 2) θdot φdot (cos θ) (expt (cos ψ) 2))
                            (* -1 (expt A 2) φdot ψdot (sin ψ) (sin θ) (cos ψ))
                            (* A C θdot φdot (cos θ) (expt (cos ψ) 2))
                            (* A C φdot ψdot (sin ψ) (sin θ) (cos ψ))
                            (* -1 (expt B 2) θdot φdot (expt (sin ψ) 2) (cos θ))
                            (* (expt B 2) φdot ψdot (sin ψ) (sin θ) (cos ψ))
                            (* B C θdot φdot (expt (sin ψ) 2) (cos θ))
                            (* -1 B C φdot ψdot (sin ψ) (sin θ) (cos ψ))
                            (* -1 (expt A 2) θdot ψdot (expt (cos ψ) 2))
                            (* A C θdot ψdot (expt (cos ψ) 2))
                            (* -1 (expt B 2) θdot ψdot (expt (sin ψ) 2))
                            (* B C θdot ψdot (expt (sin ψ) 2))
                            (* -1 A B θdot φdot (cos θ))
                            (* A B θdot ψdot))
                         (* A B (sin θ)))
                      (/ (+ (* (expt A 2) B (expt φdot 2) (sin ψ) (expt (sin θ) 3) (cos ψ))
                            (* (expt A 2) C (expt φdot 2) (sin ψ) (sin θ) (expt (cos θ) 2) (cos ψ))
                            (* -1 A (expt B 2) (expt φdot 2) (sin ψ) (expt (sin θ) 3) (cos ψ))
                            (* -1 A (expt C 2) (expt φdot 2) (sin ψ) (sin θ) (expt (cos θ) 2) (cos ψ))
                            (* -1 (expt B 2) C (expt φdot 2) (sin ψ) (sin θ) (expt (cos θ) 2) (cos ψ))
                            (* B (expt C 2) (expt φdot 2) (sin ψ) (sin θ) (expt (cos θ) 2) (cos ψ))
                            (* 2 (expt A 2) B θdot φdot (expt (sin θ) 2) (expt (cos ψ) 2))
                            (* (expt A 2) C θdot φdot (expt (cos θ) 2) (expt (cos ψ) 2))
                            (* (expt A 2) C φdot ψdot (sin ψ) (sin θ) (cos θ) (cos ψ))
                            (* -2 A (expt B 2) θdot φdot (expt (sin θ) 2) (expt (cos ψ) 2))
                            (* -1 A (expt C 2) θdot φdot (expt (cos θ) 2) (expt (cos ψ) 2))
                            (* -1 A (expt C 2) φdot ψdot (sin ψ) (sin θ) (cos θ) (cos ψ))
                            (* (expt B 2) C θdot φdot (expt (sin ψ) 2) (expt (cos θ) 2))
                            (* -1 (expt B 2) C φdot ψdot (sin ψ) (sin θ) (cos θ) (cos ψ))
                            (* -1 B (expt C 2) θdot φdot (expt (sin ψ) 2) (expt (cos θ) 2))
                            (* B (expt C 2) φdot ψdot (sin ψ) (sin θ) (cos θ) (cos ψ))
                            (* -1 (expt A 2) B (expt θdot 2) (sin ψ) (sin θ) (cos ψ))
                            (* (expt A 2) C θdot ψdot (cos θ) (expt (cos ψ) 2))
                            (* A (expt B 2) (expt θdot 2) (sin ψ) (sin θ) (cos ψ))
                            (* -1 A (expt C 2) θdot ψdot (cos θ) (expt (cos ψ) 2))
                            (* (expt B 2) C θdot ψdot (expt (sin ψ) 2) (cos θ))
                            (* -1 B (expt C 2) θdot ψdot (expt (sin ψ) 2) (cos θ))
                            (* -1 (expt A 2) B θdot φdot (expt (sin θ) 2))
                            (* A (expt B 2) θdot φdot (expt (sin θ) 2))
                            (* -1 A B C θdot ψdot (cos θ))
                            (* A B C θdot φdot))
                         (* A B C (sin θ)))))]
      (is (= expected
             (e/freeze
              (e/simplify ((rigid/rigid-sysder 'A 'B 'C)
                           (up 't
                               (up 'θ 'φ 'ψ)
                               (up 'θdot 'φdot 'ψdot))))))))))
