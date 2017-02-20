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
  (:require [clojure.test :refer :all]
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
              (up (/ (+ (* (expt A 2) (expt φdot 2) (expt (sin ψ) 2) (cos θ) (sin θ)) (* -1 A C (expt φdot 2) (expt (sin ψ) 2) (cos θ) (sin θ)) (* (expt B 2) (expt φdot 2) (cos θ) (sin θ) (expt (cos ψ) 2)) (* -1 B C (expt φdot 2) (cos θ) (sin θ) (expt (cos ψ) 2)) (* (expt A 2) θdot φdot (sin ψ) (cos θ) (cos ψ)) (* (expt A 2) φdot ψdot (expt (sin ψ) 2) (sin θ)) (* -1 A C θdot φdot (sin ψ) (cos θ) (cos ψ)) (* -1 A C φdot ψdot (expt (sin ψ) 2) (sin θ)) (* -1 (expt B 2) θdot φdot (sin ψ) (cos θ) (cos ψ)) (* (expt B 2) φdot ψdot (sin θ) (expt (cos ψ) 2)) (* B C θdot φdot (sin ψ) (cos θ) (cos ψ)) (* -1 B C φdot ψdot (sin θ) (expt (cos ψ) 2)) (* (expt A 2) θdot ψdot (sin ψ) (cos ψ)) (* -1 A C θdot ψdot (sin ψ) (cos ψ)) (* -1 (expt B 2) θdot ψdot (sin ψ) (cos ψ)) (* B C θdot ψdot (sin ψ) (cos ψ)) (* -1 A B φdot ψdot (sin θ)))
                     (* A B))
                  (/ (+ (* -1 (expt A 2) (expt φdot 2) (sin ψ) (cos θ) (sin θ) (cos ψ)) (* A C (expt φdot 2) (sin ψ) (cos θ) (sin θ) (cos ψ)) (* (expt B 2) (expt φdot 2) (sin ψ) (cos θ) (sin θ) (cos ψ)) (* -1 B C (expt φdot 2) (sin ψ) (cos θ) (sin θ) (cos ψ)) (* -1 (expt A 2) θdot φdot (cos θ) (expt (cos ψ) 2)) (* -1 (expt A 2) φdot ψdot (sin ψ) (sin θ) (cos ψ)) (* A C θdot φdot (cos θ) (expt (cos ψ) 2)) (* A C φdot ψdot (sin ψ) (sin θ) (cos ψ)) (* -1 (expt B 2) θdot φdot (expt (sin ψ) 2) (cos θ)) (* (expt B 2) φdot ψdot (sin ψ) (sin θ) (cos ψ)) (* B C θdot φdot (expt (sin ψ) 2) (cos θ)) (* -1 B C φdot ψdot (sin ψ) (sin θ) (cos ψ)) (* -1 (expt A 2) θdot ψdot (expt (cos ψ) 2)) (* A C θdot ψdot (expt (cos ψ) 2)) (* -1 (expt B 2) θdot ψdot (expt (sin ψ) 2)) (* B C θdot ψdot (expt (sin ψ) 2)) (* -1 A B θdot φdot (cos θ)) (* A B θdot ψdot))
                     (* A B (sin θ)))
                  (/ (+ (* (expt A 2) B (expt φdot 2) (sin ψ) (expt (sin θ) 3) (cos ψ)) (* (expt A 2) C (expt φdot 2) (sin ψ) (expt (cos θ) 2) (sin θ) (cos ψ)) (* -1 A (expt B 2) (expt φdot 2) (sin ψ) (expt (sin θ) 3) (cos ψ)) (* -1 A (expt C 2) (expt φdot 2) (sin ψ) (expt (cos θ) 2) (sin θ) (cos ψ)) (* -1 (expt B 2) C (expt φdot 2) (sin ψ) (expt (cos θ) 2) (sin θ) (cos ψ)) (* B (expt C 2) (expt φdot 2) (sin ψ) (expt (cos θ) 2) (sin θ) (cos ψ)) (* 2 (expt A 2) B θdot φdot (expt (sin θ) 2) (expt (cos ψ) 2)) (* (expt A 2) C θdot φdot (expt (cos θ) 2) (expt (cos ψ) 2)) (* (expt A 2) C φdot ψdot (sin ψ) (cos θ) (sin θ) (cos ψ)) (* -2 A (expt B 2) θdot φdot (expt (sin θ) 2) (expt (cos ψ) 2)) (* -1 A (expt C 2) θdot φdot (expt (cos θ) 2) (expt (cos ψ) 2)) (* -1 A (expt C 2) φdot ψdot (sin ψ) (cos θ) (sin θ) (cos ψ)) (* (expt B 2) C θdot φdot (expt (sin ψ) 2) (expt (cos θ) 2)) (* -1 (expt B 2) C φdot ψdot (sin ψ) (cos θ) (sin θ) (cos ψ)) (* -1 B (expt C 2) θdot φdot (expt (sin ψ) 2) (expt (cos θ) 2)) (* B (expt C 2) φdot ψdot (sin ψ) (cos θ) (sin θ) (cos ψ)) (* -1 (expt A 2) B (expt θdot 2) (sin ψ) (sin θ) (cos ψ)) (* (expt A 2) C θdot ψdot (cos θ) (expt (cos ψ) 2)) (* A (expt B 2) (expt θdot 2) (sin ψ) (sin θ) (cos ψ)) (* -1 A (expt C 2) θdot ψdot (cos θ) (expt (cos ψ) 2)) (* (expt B 2) C θdot ψdot (expt (sin ψ) 2) (cos θ)) (* -1 B (expt C 2) θdot ψdot (expt (sin ψ) 2) (cos θ)) (* -1 (expt A 2) B θdot φdot (expt (sin θ) 2)) (* A (expt B 2) θdot φdot (expt (sin θ) 2)) (* -1 A B C θdot ψdot (cos θ)) (* A B C θdot φdot))
                     (* A B C (sin θ)))))
         (simplify ((rigid/rigid-sysder 'A 'B 'C)
                    (up 't
                        (up 'θ 'φ 'ψ)
                        (up 'θdot 'φdot 'ψdot)))))))
