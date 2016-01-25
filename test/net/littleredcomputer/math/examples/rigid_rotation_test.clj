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

(ns net.littleredcomputer.math.examples.rigid-rotation-test
  (:refer-clojure :exclude [+ - * / zero? partial])
  (:require [clojure.test :refer :all]
            [net.littleredcomputer.math.env :refer :all]
            [net.littleredcomputer.math.examples.rigid-rotation :as rigid-rotation]
            [net.littleredcomputer.math.mechanics.rigid :as rigid]))

(deftest ^:long smoke
  ;; this test is :long because of the simplification. Remove this
  ;; tag when we fix that.
  (is (rigid-rotation/evolver 1 0.1 1 1.2 2 0.1 0.1 0.1 1 1 1))
  ;; Admittedly, I haven't checked this yet.
  (is (= '(up 1
              (up θdot φdot ψdot)
              (up
               (/
                (+
                 (* (cos θ) (sin θ) (expt (cos ψ) 2) (expt B 2) (expt φdot 2))
                 (* -1N (cos θ) (sin θ) (expt (cos ψ) 2) B C (expt φdot 2))
                 (* (cos θ) (sin θ) (expt (sin ψ) 2) (expt A 2) (expt φdot 2))
                 (* -1N (cos θ) (sin θ) (expt (sin ψ) 2) A C (expt φdot 2))
                 (* (cos θ) (cos ψ) (sin ψ) (expt A 2) θdot φdot)
                 (* -1N (cos θ) (cos ψ) (sin ψ) A C θdot φdot)
                 (* -1N (cos θ) (cos ψ) (sin ψ) (expt B 2) θdot φdot)
                 (* (cos θ) (cos ψ) (sin ψ) B C θdot φdot)
                 (* (sin θ) (expt (cos ψ) 2) (expt B 2) φdot ψdot)
                 (* -1N (sin θ) (expt (cos ψ) 2) B C φdot ψdot)
                 (* (sin θ) (expt (sin ψ) 2) (expt A 2) φdot ψdot)
                 (* -1N (sin θ) (expt (sin ψ) 2) A C φdot ψdot)
                 (* (cos ψ) (sin ψ) (expt A 2) θdot ψdot)
                 (* -1N (cos ψ) (sin ψ) A C θdot ψdot)
                 (* -1N (cos ψ) (sin ψ) (expt B 2) θdot ψdot)
                 (* (cos ψ) (sin ψ) B C θdot ψdot)
                 (* -1N (sin θ) A B φdot ψdot))
                (* A B))
               (/
                (+
                 (* -1N (cos θ) (sin ψ) (cos ψ) (sin θ) (expt A 2) (expt φdot 2))
                 (* (cos θ) (sin ψ) (cos ψ) (sin θ) A C (expt φdot 2))
                 (* (cos θ) (sin ψ) (cos ψ) (sin θ) (expt B 2) (expt φdot 2))
                 (* -1N (cos θ) (sin ψ) (cos ψ) (sin θ) B C (expt φdot 2))
                 (* -1N (cos θ) (expt (sin ψ) 2) (expt B 2) θdot φdot)
                 (* (cos θ) (expt (sin ψ) 2) B C θdot φdot)
                 (* -1N (cos θ) (expt (cos ψ) 2) (expt A 2) θdot φdot)
                 (* (cos θ) (expt (cos ψ) 2) A C θdot φdot)
                 (* -1N (sin ψ) (cos ψ) (sin θ) (expt A 2) φdot ψdot)
                 (* (sin ψ) (cos ψ) (sin θ) A C φdot ψdot)
                 (* (sin ψ) (cos ψ) (sin θ) (expt B 2) φdot ψdot)
                 (* -1N (sin ψ) (cos ψ) (sin θ) B C φdot ψdot)
                 (* -1N (expt (sin ψ) 2) (expt B 2) θdot ψdot)
                 (* (expt (sin ψ) 2) B C θdot ψdot)
                 (* -1N (expt (cos ψ) 2) (expt A 2) θdot ψdot)
                 (* (expt (cos ψ) 2) A C θdot ψdot)
                 (* -1N (cos θ) A B θdot φdot)
                 (* A B θdot ψdot))
                (* (sin θ) A B))
               (/
                (+
                 (* (cos ψ) (sin ψ) (expt (sin θ) 3) (expt A 2) B (expt φdot 2))
                 (*
                  -1N
                  (cos ψ)
                  (sin ψ)
                  (expt (sin θ) 3)
                  A
                  (expt B 2)
                  (expt φdot 2))
                 (*
                  (cos ψ)
                  (sin ψ)
                  (sin θ)
                  (expt (cos θ) 2)
                  (expt A 2)
                  C
                  (expt φdot 2))
                 (*
                  -1N
                  (cos ψ)
                  (sin ψ)
                  (sin θ)
                  (expt (cos θ) 2)
                  A
                  (expt C 2)
                  (expt φdot 2))
                 (*
                  -1N
                  (cos ψ)
                  (sin ψ)
                  (sin θ)
                  (expt (cos θ) 2)
                  (expt B 2)
                  C
                  (expt φdot 2))
                 (*
                  (cos ψ)
                  (sin ψ)
                  (sin θ)
                  (expt (cos θ) 2)
                  B
                  (expt C 2)
                  (expt φdot 2))
                 (* 2N (expt (cos ψ) 2) (expt (sin θ) 2) (expt A 2) B θdot φdot)
                 (* -2 (expt (cos ψ) 2) (expt (sin θ) 2) A (expt B 2) θdot φdot)
                 (* (expt (cos ψ) 2) (expt (cos θ) 2) (expt A 2) C θdot φdot)
                 (* -1N (expt (cos ψ) 2) (expt (cos θ) 2) A (expt C 2) θdot φdot)
                 (* (cos ψ) (sin ψ) (sin θ) (cos θ) (expt A 2) C φdot ψdot)
                 (* -1N (cos ψ) (sin ψ) (sin θ) (cos θ) A (expt C 2) φdot ψdot)
                 (* -1N (cos ψ) (sin ψ) (sin θ) (cos θ) (expt B 2) C φdot ψdot)
                 (* (cos ψ) (sin ψ) (sin θ) (cos θ) B (expt C 2) φdot ψdot)
                 (* (expt (sin ψ) 2) (expt (cos θ) 2) (expt B 2) C θdot φdot)
                 (* -1N (expt (sin ψ) 2) (expt (cos θ) 2) B (expt C 2) θdot φdot)
                 (* (expt (cos ψ) 2) (cos θ) (expt A 2) C θdot ψdot)
                 (* -1N (expt (cos ψ) 2) (cos θ) A (expt C 2) θdot ψdot)
                 (* -1N (cos ψ) (sin ψ) (sin θ) (expt A 2) B (expt θdot 2))
                 (* (cos ψ) (sin ψ) (sin θ) A (expt B 2) (expt θdot 2))
                 (* (expt (sin ψ) 2) (cos θ) (expt B 2) C θdot ψdot)
                 (* -1N (expt (sin ψ) 2) (cos θ) B (expt C 2) θdot ψdot)
                 (* -1N (expt (sin θ) 2) (expt A 2) B θdot φdot)
                 (* (expt (sin θ) 2) A (expt B 2) θdot φdot)
                 (* -1N (cos θ) A B C θdot ψdot)
                 (* A B C θdot φdot))
                (* (sin θ) A B C))))
         (simplify ((rigid/rigid-sysder 'A 'B 'C)
                    (up 't
                        (up 'θ 'φ 'ψ)
                        (up 'θdot 'φdot 'ψdot)))))))
