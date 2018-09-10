;
; Copyright © 2017 Colin Smith.
; This work is based on the Scmutils system of MIT/GNU Scheme:
; Copyright © 2002 Massachusetts Institute of Technology
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

(ns sicmutils.analyzer-test
  (:require [clojure.test :refer :all]
            [sicmutils
             [analyze :as a]
             [polynomial :as poly]]
            [sicmutils.mechanics.lagrange :refer :all]))

(deftest analyzer-test
  (let [new-analyzer (fn [] (a/make-analyzer (poly/->PolynomialAnalyzer) (a/monotonic-symbol-generator "k%08d")))
        A #((new-analyzer) %)]
    (is (= '(+ x 1) (A '(+ 1 x))))
    (is (= '(+ x 1) (A '[+ 1 x])))
    (is (= 'x (A '(* 1/2 (+ x x)))))
    (is (= '(* y (sin y) (cos (+ (expt (sin y) 4) (* 2 (sin y)) 1)))
           (A '(* y (sin y) (cos (+ 1 (sin y) (sin y) (expt (sin y) 4)))))))
    (is (= '(+ (* -1 m (expt ((D phi) t) 2) (r t)) (* m (((expt D 2) r) t)) ((D U) (r t)))
           (A '(- (* 1/2 m (+ (((expt D 2) r) t) (((expt D 2) r) t)))
                    (+ (* 1/2 m (+ (* ((D phi) t) ((D phi) t) (r t))
                                         (* ((D phi) t) ((D phi) t) (r t))))
                         (* -1 ((D U) (r t))))))))
    (is (= '(+ (expt cos 2) (expt sin 2)) (A '(+ (expt cos 2) (expt sin 2)))))
    (is (= '(+ (expt cos 2) (expt sin 2)) (A '(+ (expt sin 2) (expt cos 2)))))))
