;
; Copyright (C) 2016 Colin Smith.
; This work is based on the Scmutils system of MIT/GNU Scheme.
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

(ns sicmutils.examples.rigid-rotation-test
  (:refer-clojure :exclude [+ - * / zero? partial ref])
  (:require [clojure.test :refer :all :exclude [function?]]
            [sicmutils.env :refer :all]
            [sicmutils.examples.rigid-rotation :as rigid-rotation]
            [sicmutils.mechanics.rigid :as rigid]
            [sicmutils.simplify :refer [hermetic-simplify-fixture]]))

(use-fixtures :once hermetic-simplify-fixture)

(deftest ^:long smoke
  ;; this test is :long because of the simplification. Remove this
  ;; tag when we fix that.
  (is (rigid-rotation/evolver 1 0.1 1 1.2 2 0.1 0.1 0.1 1 1 1))
  ;; Admittedly, I haven't checked this yet.
  (is (= '(up 1
              (up θdot φdot ψdot)
              (up (/ (+ (* (expt (sin ψ) 2) (cos θ) (sin θ) (expt A 2) (expt φdot 2))
                        (* -1 (expt (sin ψ) 2) (cos θ) (sin θ) A C (expt φdot 2))
                        (* (cos θ) (sin θ) (expt (cos ψ) 2) (expt B 2) (expt φdot 2))
                        (* -1 (cos θ) (sin θ) (expt (cos ψ) 2) B C (expt φdot 2))
                        (* (expt (sin ψ) 2) (sin θ) (expt A 2) φdot ψdot)
                        (* -1 (expt (sin ψ) 2) (sin θ) A C φdot ψdot)
                        (* (sin ψ) (cos θ) (cos ψ) (expt A 2) θdot φdot)
                        (* -1 (sin ψ) (cos θ) (cos ψ) A C θdot φdot)
                        (* -1 (sin ψ) (cos θ) (cos ψ) (expt B 2) θdot φdot)
                        (* (sin ψ) (cos θ) (cos ψ) B C θdot φdot)
                        (* (sin θ) (expt (cos ψ) 2) (expt B 2) φdot ψdot)
                        (* -1 (sin θ) (expt (cos ψ) 2) B C φdot ψdot)
                        (* (sin ψ) (cos ψ) (expt A 2) θdot ψdot)
                        (* -1 (sin ψ) (cos ψ) A C θdot ψdot)
                        (* -1 (sin ψ) (cos ψ) (expt B 2) θdot ψdot)
                        (* (sin ψ) (cos ψ) B C θdot ψdot)
                        (* -1 (sin θ) A B φdot ψdot))
                     (* A B))
                  (/ (+ (* -1 (sin ψ) (cos θ) (sin θ) (cos ψ) (expt A 2) (expt φdot 2))
                        (* (sin ψ) (cos θ) (sin θ) (cos ψ) A C (expt φdot 2))
                        (* (sin ψ) (cos θ) (sin θ) (cos ψ) (expt B 2) (expt φdot 2))
                        (* -1 (sin ψ) (cos θ) (sin θ) (cos ψ) B C (expt φdot 2))
                        (* -1 (expt (sin ψ) 2) (cos θ) (expt B 2) θdot φdot)
                        (* (expt (sin ψ) 2) (cos θ) B C θdot φdot)
                        (* -1 (sin ψ) (sin θ) (cos ψ) (expt A 2) φdot ψdot)
                        (* (sin ψ) (sin θ) (cos ψ) A C φdot ψdot)
                        (* (sin ψ) (sin θ) (cos ψ) (expt B 2) φdot ψdot)
                        (* -1 (sin ψ) (sin θ) (cos ψ) B C φdot ψdot)
                        (* -1 (cos θ) (expt (cos ψ) 2) (expt A 2) θdot φdot)
                        (* (cos θ) (expt (cos ψ) 2) A C θdot φdot)
                        (* -1 (expt (sin ψ) 2) (expt B 2) θdot ψdot)
                        (* (expt (sin ψ) 2) B C θdot ψdot)
                        (* -1 (expt (cos ψ) 2) (expt A 2) θdot ψdot)
                        (* (expt (cos ψ) 2) A C θdot ψdot)
                        (* -1 (cos θ) A B θdot φdot)
                        (* A B θdot ψdot))
                     (* (sin θ) A B))
                  (/ (+ (* (sin ψ) (expt (cos θ) 2) (sin θ) (cos ψ) (expt A 2) C (expt φdot 2))
                        (* -1 (sin ψ) (expt (cos θ) 2) (sin θ) (cos ψ) A (expt C 2) (expt φdot 2))
                        (* -1 (sin ψ) (expt (cos θ) 2) (sin θ) (cos ψ) (expt B 2) C (expt φdot 2))
                        (* (sin ψ) (expt (cos θ) 2) (sin θ) (cos ψ) B (expt C 2) (expt φdot 2))
                        (* (sin ψ) (expt (sin θ) 3) (cos ψ) (expt A 2) B (expt φdot 2))
                        (* -1 (sin ψ) (expt (sin θ) 3) (cos ψ) A (expt B 2) (expt φdot 2))
                        (* (expt (sin ψ) 2) (expt (cos θ) 2) (expt B 2) C θdot φdot)
                        (* -1 (expt (sin ψ) 2) (expt (cos θ) 2) B (expt C 2) θdot φdot)
                        (* (sin ψ) (cos θ) (sin θ) (cos ψ) (expt A 2) C φdot ψdot)
                        (* -1 (sin ψ) (cos θ) (sin θ) (cos ψ) A (expt C 2) φdot ψdot)
                        (* -1 (sin ψ) (cos θ) (sin θ) (cos ψ) (expt B 2) C φdot ψdot)
                        (* (sin ψ) (cos θ) (sin θ) (cos ψ) B (expt C 2) φdot ψdot)
                        (* (expt (cos θ) 2) (expt (cos ψ) 2) (expt A 2) C θdot φdot)
                        (* -1 (expt (cos θ) 2) (expt (cos ψ) 2) A (expt C 2) θdot φdot)
                        (* 2 (expt (sin θ) 2) (expt (cos ψ) 2) (expt A 2) B θdot φdot)
                        (* -2 (expt (sin θ) 2) (expt (cos ψ) 2) A (expt B 2) θdot φdot)
                        (* (expt (sin ψ) 2) (cos θ) (expt B 2) C θdot ψdot)
                        (* -1 (expt (sin ψ) 2) (cos θ) B (expt C 2) θdot ψdot)
                        (* -1 (sin ψ) (sin θ) (cos ψ) (expt A 2) B (expt θdot 2))
                        (* (sin ψ) (sin θ) (cos ψ) A (expt B 2) (expt θdot 2))
                        (* (cos θ) (expt (cos ψ) 2) (expt A 2) C θdot ψdot)
                        (* -1 (cos θ) (expt (cos ψ) 2) A (expt C 2) θdot ψdot)
                        (* -1 (expt (sin θ) 2) (expt A 2) B θdot φdot)
                        (* (expt (sin θ) 2) A (expt B 2) θdot φdot)
                        (* -1 (cos θ) A B C θdot ψdot)
                        (* A B C θdot φdot))
                     (* (sin θ) A B C))))

         (simplify ((rigid/rigid-sysder 'A 'B 'C)
                    (up 't
                        (up 'θ 'φ 'ψ)
                        (up 'θdot 'φdot 'ψdot)))))))
