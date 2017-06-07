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

(ns sicmutils.fdg.ch7-test
  (:refer-clojure :exclude [+ - * / zero? ref partial])
  (:require [clojure.test :refer :all]
            [sicmutils.env :refer :all]
            [sicmutils.value :as v]
            [sicmutils.simplify :refer [hermetic-simplify-fixture]]))

(use-fixtures :once hermetic-simplify-fixture)

(defn ^:private F->directional-derivative
  [F]
  (fn [v]
    (fn [u]
      (fn [f]
        (fn [m]
          (let [g (fn [delta]
                    (- ((u f) m) (((((F v) delta) u) f) m)))]
            ((D g) 0)))))))

(defn ^:private F-Lie
  [phi]
  (fn [v]
    (fn [delta]
      (pushforward-vector ((phi v) delta) ((phi v) (- delta))))))

(defn phi
  [coordinate-system order]
  (fn [v]
    (fn [delta]
      (fn [m]
        ((point coordinate-system)
         (series:sum (((exp (* delta v)) (chart coordinate-system)) m)
                     order))))))

(defn ^:private Lie-directional
  [coordinate-system order]
  (let [Phi (phi coordinate-system order)]
    (F->directional-derivative (F-Lie Phi))))

(deftest section-7-1
  (let [v (literal-vector-field 'v-rect R3-rect)
        w (literal-vector-field 'w-rect R3-rect)
        f (literal-manifold-function 'f-rect R3-rect)]
    (is (= 0
           (simplify ((- ((((Lie-directional R3-rect 2) v) w) f)
                         ((commutator v w) f))
                      ((point R3-rect) (up 'x0 'y0 'z0)))))))
  (let [a (literal-manifold-function 'alpha R3-rect)
        b (literal-manifold-function 'beta R3-rect)
        c (literal-manifold-function 'gamma R3-rect)]
    (let-coordinates [[x y z] R3-rect]
      (let [theta (+ (* a dx) (* b dy) (* c dz))
            omega (+ (* a (wedge dy dz))
                     (* b (wedge dz dx))
                     (* c (wedge dx dy)))
            X (literal-vector-field 'X-rect R3-rect)
            Y (literal-vector-field 'Y-rect R3-rect)
            Z (literal-vector-field 'Z-rect R3-rect)
            V (literal-vector-field 'V-rect R3-rect)
            R3-rect-point ((point R3-rect) (up 'x0 'y0 'z0))]
        (is (= :sicmutils.calculus.vector-field/vector-field (v/kind V)))
        (is (= ::v/function (v/kind (Lie-derivative V))))
        (is (= :sicmutils.calculus.form-field/form-field (v/kind theta)))
        #_(is (= 'a
               (simplify (((d ((Lie-derivative V) theta))
                           X Y)
                          R3-rect-point))))
        #_(is (= 'a
               (simplify ((((Lie-derivative V) (d theta))
                           X Y)
                          R3-rect-point))))
        ;; if you look at the LH and RH of the subtraction, you will observe that
        ;; this is nontrivial :)
        (is (= 0
               (simplify (((- ((Lie-derivative V) (d theta))
                              (d ((Lie-derivative V) theta)))
                           X Y)
                          R3-rect-point))))))))
