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
  (:refer-clojure :exclude [+ - * /  ref partial])
  (:require [clojure.test :refer [is deftest testing use-fixtures]]
            [sicmutils.env :as e :refer [+ - * /
                                         D d simplify partial
                                         up down exp
                                         point chart wedge
                                         R2-rect R2-polar R3-rect]
             #?@(:cljs [:include-macros true])]
            [sicmutils.value :as v]
            [sicmutils.simplify :refer [hermetic-simplify-fixture]]))

(use-fixtures :once hermetic-simplify-fixture)

;; (defn ^:private F->directional-derivative
;;   [F]
;;   (fn [v]
;;     (fn [u]
;;       (fn [f]
;;         (fn [m]
;;           (let [g (fn [delta]
;;                     (- ((u f) m) (((((F v) delta) u) f) m)))]
;;             ((D g) 0)))))))

;; (defn ^:private F-Lie
;;   [phi]
;;   (fn [v]
;;     (fn [delta]
;;       (e/pushforward-vector ((phi v) delta) ((phi v) (- delta))))))

;; (defn phi
;;   [coordinate-system order]
;;   (fn [v]
;;     (fn [delta]
;;       (fn [m]
;;         ((point coordinate-system)
;;          (e/series:sum (((exp (* delta v)) (chart coordinate-system)) m)
;;                        order))))))

;; (defn ^:private Lie-directional
;;   [coordinate-system order]
;;   (let [Phi (phi coordinate-system order)]
;;     (F->directional-derivative (F-Lie Phi))))

;; (deftest section-7-1
;;   (let [v (e/literal-vector-field 'v-rect R3-rect)
;;         w (e/literal-vector-field 'w-rect R3-rect)
;;         f (e/literal-manifold-function 'f-rect R3-rect)]
;;     (is (= 0
;;            (simplify ((- ((((Lie-directional R3-rect 2) v) w) f)
;;                          ((e/commutator v w) f))
;;                       ((point R3-rect) (up 'x0 'y0 'z0)))))))
;;   (let [a (e/literal-manifold-function 'alpha R3-rect)
;;         b (e/literal-manifold-function 'beta R3-rect)
;;         c (e/literal-manifold-function 'gamma R3-rect)]
;;     (e/let-coordinates
;;      [[x y z] R3-rect]
;;      (let [theta (+ (* a dx) (* b dy) (* c dz))
;;            omega (+ (* a (wedge dy dz))
;;                     (* b (wedge dz dx))
;;                     (* c (wedge dx dy)))
;;            X (e/literal-vector-field 'X-rect R3-rect)
;;            Y (e/literal-vector-field 'Y-rect R3-rect)
;;            Z (e/literal-vector-field 'Z-rect R3-rect)
;;            V (e/literal-vector-field 'V-rect R3-rect)
;;            R3-rect-point ((point R3-rect) (up 'x0 'y0 'z0))]
;;        (is (= :sicmutils.calculus.vector-field/vector-field (v/kind V)))
;;        (is (= :sicmutils.operator/operator (v/kind (e/Lie-derivative V))))
;;        (is (= :sicmutils.calculus.form-field/form-field (v/kind theta)))
;;        (is (= 1 (:rank (:context theta))))
;;        (is (= 2 (:rank (:context omega))))
;;        #_(is (= 'a
;;                 (simplify (((d ((e/Lie-derivative V) theta))
;;                             X Y)
;;                            R3-rect-point))))
;;        #_(is (= 'a
;;                 (simplify ((((e/Lie-derivative V) (d theta))
;;                             X Y)
;;                            R3-rect-point))))
;;        ;; if you look at the LH and RH of the subtraction, you will observe that
;;        ;; this is nontrivial :)
;;        (is (zero?
;;             (simplify (((- ((e/Lie-derivative V) (d theta))
;;                            (d ((e/Lie-derivative V) theta)))
;;                         X Y)
;;                        R3-rect-point))))
;;        (is (zero?
;;             (simplify (((- ((e/Lie-derivative V) (d omega))
;;                            (d ((e/Lie-derivative V) omega)))
;;                         X Y Z)
;;                        R3-rect-point)
;;                       )))
;;        (is (zero?
;;             (simplify ((((- (e/commutator (e/Lie-derivative X) (e/Lie-derivative Y))
;;                             (e/Lie-derivative (e/commutator X Y)))
;;                          theta)
;;                         Z)
;;                        R3-rect-point))))
;;        (is (zero?
;;             (simplify ((((- (e/commutator (e/Lie-derivative X) (e/Lie-derivative Y))
;;                             (e/Lie-derivative (e/commutator X Y)))
;;                          omega)
;;                         Z V)
;;                        R3-rect-point))))))))

;; (comment
;;   (deftest section-7-1b
;;     (e/let-coordinates
;;      [[x y z] R3-rect]
;;      (let [Jz (- (* x d:dy) (* y d:dx))]
;;        ;; Seems like there may be a misprint
;;        #_(is (= 'a (simplify
;;                     (take 5
;;                           (seq
;;                            ((((exp (* 'a (e/Lie-derivative Jz))) d:dy)
;;                              (e/literal-manifold-function 'f-rect R3-rect))
;;                             ((point R3-rect) (up 1 0 0)))))))))))

;;   (deftest section-7-1c
;;     (e/let-coordinates
;;      [[x y z] R3-rect]
;;      (let [X (e/literal-vector-field 'X-rect R3-rect)
;;            Y (e/literal-vector-field 'Y-rect R3-rect)
;;            Z (e/literal-vector-field 'Z-rect R3-rect)
;;            a (e/literal-manifold-function 'alpha R3-rect)
;;            b (e/literal-manifold-function 'beta R3-rect)
;;            c (e/literal-manifold-function 'gamma R3-rect)
;;            omega (+ (* a (wedge dx dy))
;;                     (* b (wedge dy dz))
;;                     (* c (wedge dz dx)))
;;            L1 (fn [X]
;;                 (fn [omega]
;;                   (+ ((e/interior-product X) (d omega))
;;                      (d ((e/interior-product X) omega)))))]
;;        (is (= 0 (simplify
;;                  ((- (((e/Lie-derivative X) omega) Y Z)
;;                      (((L1 X) omega) Y Z))
;;                   ((point R3-rect) (up 'x0 'y0 'z0)))))))))

;;   (defn ^:private F-parallel
;;     [omega phi coordinate-system]
;;     (fn [v]
;;       (fn [delta]
;;         (fn [u]
;;           (fn [f]
;;             (fn [m]
;;               (let [basis (e/coordinate-system->basis coordinate-system)
;;                     etilde (e/basis->oneform-basis basis)
;;                     e (e/basis->vector-basis basis)
;;                     m0 (((phi v) (- delta)) m)
;;                     Aij (+ (v/one-like ((omega v) m0))
;;                            (* delta (- ((omega v) m0))))
;;                     ui ((etilde u) m0)]
;;                 (* ((e f) m)
;;                    (* Aij ui)))))))))

;;   (deftest section-7-2
;;     (e/let-coordinates
;;      [[x y] R2-rect
;;       [r theta] R2-polar]
;;      (let [R2-rect-basis (e/coordinate-system->basis R2-rect)
;;            R2-polar-basis (e/coordinate-system->basis R2-polar)
;;            R2-rect-Christoffel (e/make-Christoffel
;;                                 (let [zero (fn [m] 0)]
;;                                   (down (down (up zero zero)
;;                                               (up zero zero))
;;                                         (down (up zero zero)
;;                                               (up zero zero))))
;;                                 R2-rect-basis)
;;            R2-rect-Cartan (e/Christoffel->Cartan R2-rect-Christoffel)
;;            R2-polar-Cartan (e/Cartan-transform R2-rect-Cartan R2-polar-basis)
;;            circular (- (* x d:dy) (* y d:dx))
;;            f (e/literal-manifold-function 'f-rect R2-rect)
;;            R2-rect-point ((point R2-rect) (up 'x0 'y0))]

;;        (is (= '(((partial 1) f-rect) (up x0 y0))
;;               (simplify
;;                (((((e/covariant-derivative R2-rect-Cartan) d:dx)
;;                   circular)
;;                  f)
;;                 R2-rect-point))))
;;        (is (= '(((partial 1) f-rect) (up x0 y0))
;;               (simplify
;;                ((d:dy f) R2-rect-point))))
;;        (is (= '(((partial 1) f-rect) (up x0 y0))
;;               (simplify
;;                (((((e/covariant-derivative R2-polar-Cartan) d:dx) circular) f)
;;                 R2-rect-point))))
;;        (let [V (e/literal-vector-field 'V-rect R2-rect)
;;              W (e/literal-vector-field 'W-rect R2-rect)]
;;          ;; this one runs out of time. The culprit may be the
;;          ;; in-rule call to simplifies-to-zero in trig-cleanup.
;;          ;; (or else it may just be the exponential size of the
;;          ;; search for repeated segment...) Ultimately it works,
;;          ;; but boy. This expression warrants further study for
;;          ;; optimization's sake
;;          #_(is (= 0
;;                   (simplify
;;                    (((((- (covariant-derivative R2-rect-Cartan)
;;                           (covariant-derivative R2-polar-Cartan))
;;                        V)
;;                       W)
;;                      f)
;;                     R2-rect-point)))))))))
