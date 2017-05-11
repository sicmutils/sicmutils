;
; Copyright (C) 2017 Colin Smith.
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

(ns sicmutils.fdg.ch3-test
  (:refer-clojure :exclude [+ - * / zero? ref partial])
  (:require [clojure.test :refer :all]
            [sicmutils.env :refer :all]
            [sicmutils.simplify :refer [hermetic-simplify-fixture]]))

(use-fixtures :once hermetic-simplify-fixture)

(deftest section-3-1
  (let [R2-rect-point ((point R2-rect) (up 'x0 'y0))
        R2->R '(-> (UP Real Real) Real)
        v (components->vector-field
           (up (literal-function 'b↑0 R2->R)
               (literal-function 'b↑1 R2->R))
           R2-rect)
        v2 (literal-vector-field 'b R2-rect)]
    (is (= '(+ (* (((∂ 0) f-rect) (up x0 y0)) (b↑0 (up x0 y0)))
               (* (((∂ 1) f-rect) (up x0 y0)) (b↑1 (up x0 y0))))
           (simplify
            ((v (literal-manifold-function 'f-rect R2-rect)) R2-rect-point))))
    (is (= '(+ (* (((∂ 0) f-rect) (up x0 y0)) (b↑0 (up x0 y0)))
               (* (((∂ 1) f-rect) (up x0 y0)) (b↑1 (up x0 y0))))
           (simplify
            ((v2 (literal-manifold-function 'f-rect R2-rect)) R2-rect-point))))
    (is (= '(up (b↑0 (up x0 y0)) (b↑1 (up x0 y0)))
           (simplify
            ((v (chart R2-rect)) R2-rect-point))))
    (is (= '(+ (* (((∂ 0) f-rect) (up x0 y0)) (b↑0 (up x0 y0)))
               (* (((∂ 1) f-rect) (up x0 y0)) (b↑1 (up x0 y0))))
           (simplify
            (((coordinatize v R2-rect) (literal-function 'f-rect R2->R))
             (up 'x0 'y0)))))))
