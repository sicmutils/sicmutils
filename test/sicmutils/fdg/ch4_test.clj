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

(ns sicmutils.fdg.ch4-test
  (:refer-clojure :exclude [+ - * / zero? ref partial])
  (:require [clojure.test :refer :all]
            [sicmutils.env :refer :all]
            [sicmutils.calculus.manifold :as m]
            [sicmutils.simplify :refer [hermetic-simplify-fixture]]))

(use-fixtures :once hermetic-simplify-fixture)

(deftest section-4-1
  (let-coordinates [[x y] R2-rect]
    (let [e0 (+ (* (literal-manifold-function 'e0x R2-rect) d:dx)
                (* (literal-manifold-function 'e0y R2-rect) d:dy))
          e1 (+ (* (literal-manifold-function 'e1x R2-rect) d:dx)
                (* (literal-manifold-function 'e1y R2-rect) d:dy))
          F (literal-manifold-function 'F R2-rect)
          e-vector-basis (down e0 e1)
          e-dual-basis (vector-basis->dual e-vector-basis R2-polar)
          R2-rect-chi-inverse (point R2-rect)
          v (R2-rect-chi-inverse (up 'X 'Y))]
      ;; e0, 1 are vector fields. They act on manifold functions to
      ;; produce directional derivatives... which are manifold functions.

      (is (= '(+
               (* (e0x (up X Y)) (((∂ 0) F) (up X Y)))
               (* (e0y (up X Y)) (((∂ 1) F) (up X Y))))
             (simplify ((e0 F) v))))
      (is (= '(+
               (* (((∂ 0) F) (up X Y)) (e1x (up X Y)))
               (* (((∂ 1) F) (up X Y)) (e1y (up X Y))))
             (simplify ((e1 F) v))))
      (is (= '(down
               (+
                (* (e0x (up X Y)) (((∂ 0) F) (up X Y)))
                (* (e0y (up X Y)) (((∂ 1) F) (up X Y))))
               (+
                (* (((∂ 0) F) (up X Y)) (e1x (up X Y)))
                (* (((∂ 1) F) (up X Y)) (e1y (up X Y)))))
             (simplify ((e-vector-basis F) v))))
      (is (= '(up (down 1 0) (down 0 1)) (simplify ((e-dual-basis e-vector-basis) v)))))))
