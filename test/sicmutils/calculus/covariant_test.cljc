;;
;; Copyright © 2021 Sam Ritchie.
;; This work is based on the Scmutils system of MIT/GNU Scheme:
;; Copyright © 2002 Massachusetts Institute of Technology
;;
;; This is free software;  you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or (at
;; your option) any later version.
;;
;; This software is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this code; if not, see <http://www.gnu.org/licenses/>.
;;

(ns sicmutils.calculus.covariant-test
  (:refer-clojure :exclude [+ - * /  partial])
  (:require [clojure.test :refer [is deftest testing]]
            #_[sicmutils.abstract.function :as af]
            [sicmutils.calculus.basis :as b]
            [sicmutils.calculus.coordinate :as c
             :refer [let-coordinates]
             #?@(:cljs [:include-macros true])]
            [sicmutils.calculus.covariant :as cov]
            [sicmutils.calculus.form-field :as ff]
            [sicmutils.calculus.manifold :as man
             :refer [R1-rect R2-rect R3-rect R3-cyl S2-spherical]]
            [sicmutils.calculus.map :as cm]
            [sicmutils.calculus.vector-field :as vf]
            [sicmutils.expression :as x]
            [sicmutils.function :as f]
            [sicmutils.generic :as g :refer [+ - * /]]
            [sicmutils.structure :as s :refer [up down]]
            ;; [sicmutils.value :as v]
            ))

(def simplify
  (comp v/freeze g/simplify))

(deftest lie-derivative-tests
  (testing "Lie derivative."
    (let-coordinates [[x y z]        R3-rect
                      [r theta zeta] R3-cyl]
      (let [R3-rect-point ((man/point R3-rect) (up 'x0 'y0 'z0))
            R3-cyl-point ((man/point R3-cyl) (up 'r0 'theta0 'zeta0))

            w (ff/literal-oneform-field 'w R3-rect)
            u (ff/literal-oneform-field 'u R3-rect)
            v (ff/literal-oneform-field 'v R3-rect)

            X (vf/literal-vector-field 'X R3-rect)
            Y (vf/literal-vector-field 'Y R3-rect)
            Z (vf/literal-vector-field 'Z R3-rect)
            W (vf/literal-vector-field 'W R3-rect)

            f (man/literal-scalar-field 'f R3-rect)
            present (fn [expr]
                      (-> (simplify expr)
                          (x/substitute '(up x0 y0 z0) 'p)))]
        (is (= '(+ (* (w_0 p) (Y↑0 p) (((partial 0) X↑0) p))
                   (* (w_0 p) (Y↑1 p) (((partial 1) X↑0) p))
                   (* (w_0 p) (Y↑2 p) (((partial 2) X↑0) p))
                   (* (X↑0 p) (Y↑0 p) (((partial 0) w_0) p))
                   (* (X↑0 p) (Y↑1 p) (((partial 0) w_1) p))
                   (* (X↑0 p) (Y↑2 p) (((partial 0) w_2) p))
                   (* (w_1 p) (Y↑0 p) (((partial 0) X↑1) p))
                   (* (w_1 p) (Y↑1 p) (((partial 1) X↑1) p))
                   (* (w_1 p) (Y↑2 p) (((partial 2) X↑1) p))
                   (* (X↑1 p) (Y↑0 p) (((partial 1) w_0) p))
                   (* (X↑1 p) (Y↑1 p) (((partial 1) w_1) p))
                   (* (X↑1 p) (Y↑2 p) (((partial 1) w_2) p))
                   (* (w_2 p) (Y↑0 p) (((partial 0) X↑2) p))
                   (* (w_2 p) (Y↑1 p) (((partial 1) X↑2) p))
                   (* (w_2 p) (Y↑2 p) (((partial 2) X↑2) p))
                   (* (X↑2 p) (Y↑0 p) (((partial 2) w_0) p))
                   (* (X↑2 p) (Y↑1 p) (((partial 2) w_1) p))
                   (* (X↑2 p) (Y↑2 p) (((partial 2) w_2) p)))
               (present
                ((((g/Lie-derivative X) w) Y) R3-rect-point))))

        (is (= 0 (simplify
                  ((- ((ff/d ((g/Lie-derivative X) f)) Y)
                      (((g/Lie-derivative X) (ff/d f)) Y) )
                   R3-rect-point))))

        (is (= 0 (simplify
                  ((- ((ff/d ((g/Lie-derivative X) w)) Y Z)
                      (((g/Lie-derivative X) (ff/d w)) Y Z))
                   ((man/point R3-rect)
                    (up 'x↑0 'y↑0 'z↑0))))))))

    (let-coordinates [[x y] R2-rect]
      (let [R2-rect-point ((man/point R2-rect) (up 'x0 'y0))
            X (vf/literal-vector-field 'X R2-rect)
            Y (vf/literal-vector-field 'Y R2-rect)

            f (man/literal-scalar-field 'f R2-rect)
            present (fn [expr]
                      (-> (simplify expr)
                          (x/substitute '(up x0 y0) 'p)))]

        (is (= '(+ (* (X↑0 p) (((partial 0) f) p) (((partial 0) Y↑0) p))
                   (* (X↑0 p) (((partial 1) f) p) (((partial 0) Y↑1) p))
                   (* -1 (Y↑0 p) (((partial 0) f) p) (((partial 0) X↑0) p))
                   (* -1 (Y↑0 p) (((partial 1) f) p) (((partial 0) X↑1) p))
                   (* -1 (((partial 0) f) p) (Y↑1 p) (((partial 1) X↑0) p))
                   (* (((partial 0) f) p) (X↑1 p) (((partial 1) Y↑0) p))
                   (* -1 (Y↑1 p) (((partial 1) f) p) (((partial 1) X↑1) p))
                   (* (((partial 1) f) p) (X↑1 p) (((partial 1) Y↑1) p)))
               (present
                ((((g/Lie-derivative X) Y) f) R2-rect-point))))

        ;; Let phi_t(x) be the integral curve of V from x for interval t

        ;; L_V Y (f) (x) = lim_t->0 ( Y(f) (phi_t (x)) - (d phi_t)(Y)(f)(x))/t
        ;; = D (lambda (t)
        ;;             ( Y(f) (phi_t (x)) - (d phi_t)(Y)(f)(x)))
        ;; (t=0)
        ;; so let g(t) = ( Y(f) (phi_t (x)) - (d phi_t)(Y)(f)(x))
        ;; = ( Y(f) (phi_t (x)) - Y(f circ phi_t)(x))

        ;; we only need linear terms in phi_t(x)


        ;; Perhaps

        ;; phi_t(x) = (I + t v(I))(x)

        ;; Is this correct? No! Cannot add to a manifold point.

        ;; g(t) = ( Y(f) ((I + t v(I))(x)) - Y(f circ (I + t v(I)))(x))
        (letfn [(Lie-test [V]
                  (fn [Y]
                    (fn [f]
                      (fn [x]
                        (letfn [(g [t]
                                  (- ((Y f) ((+ identity (* t (V identity))) x))
                                     ((Y (f/compose f (+ identity (* t (V identity))))) x)))]
                          ((D g) 0))))))]
          (comment
            ;; TODO I think this is broken in Lie.scm; it only works because
            ;; zero-like on a point returns 0.
            (is (= 0 (- ((((Lie-test X) Y) f) R2-rect-point)
                        ((((g/Lie-derivative X) Y) f) R2-rect-point))))))))

    (testing "Lie derivative satisfies extended Leibnitz rule"
      (let [V (vf/literal-vector-field 'V R2-rect)
            Y (vf/literal-vector-field 'Y R2-rect)

            q_0 (up 'q_x 'q_y)
            m ((man/point R2-rect) q_0)
            f (man/literal-manifold-function 'f R2-rect)

            e_0 (vf/literal-vector-field 'e_0 R2-rect)
            e_1 (vf/literal-vector-field 'e_1 R2-rect)

            vector-basis (down e_0 e_1)
            oneform-basis (b/vector-basis->dual (down e_0 e_1) R2-rect)
            basis (b/make-basis vector-basis oneform-basis)

            Y↑i (oneform-basis Y)]
        (is (= 0 (simplify
                  ((- (((g/Lie-derivative V) Y) f)
                      (+ (* (s/mapr (g/Lie-derivative V) Y↑i) (vector-basis f))
                         (* Y↑i ((s/mapr (g/Lie-derivative V) vector-basis) f))))
                   m))))))

    (testing "Computation of Lie derivatives by difference quotient."
      (let [X (vf/literal-vector-field 'X R2-rect)
            Y (vf/literal-vector-field 'Y R2-rect)

            q_0 (up 'q_x 'q_y)

            m_0 ((man/point R2-rect) q_0)

            q (fn [coords]
                (fn [t]
                  (+ coords
                     (* t ((X (man/chart R2-rect))
                           ((man/point R2-rect) coords))))))

            gamma (fn [initial-point]
                    (f/compose
                     (man/point R2-rect)
                     (q ((man/chart R2-rect) initial-point))))

            phi↑X (fn [t]
                    (fn [point]
                      ((gamma point) t)))
            f (man/literal-manifold-function 'f R2-rect)
            result-via-Lie ((((g/Lie-derivative X) Y) f) m_0)
            present (fn [expr]
                      (-> (simplify expr)
                          (x/substitute '(up q_x q_y) 'p)))
            ]
        (is (= '(+ (* -1 (Y↑0 p) (((partial 0) f) p) (((partial 0) X↑0) p))
                   (* -1 (Y↑0 p) (((partial 1) f) p) (((partial 0) X↑1) p))
                   (* (((partial 0) f) p) (((partial 0) Y↑0) p) (X↑0 p))
                   (* -1 (((partial 0) f) p) (Y↑1 p) (((partial 1) X↑0) p))
                   (* (((partial 0) f) p) (((partial 1) Y↑0) p) (X↑1 p))
                   (* -1 (Y↑1 p) (((partial 1) f) p) (((partial 1) X↑1) p))
                   (* (((partial 1) f) p) (((partial 0) Y↑1) p) (X↑0 p))
                   (* (((partial 1) f) p) (((partial 1) Y↑1) p) (X↑1 p)))
               (present
                ((D (fn [t]
                      (- ((Y f) ((phi↑X t) m_0))
                         ((Y (f/compose f (phi↑X t))) m_0))))
                 0))))

        (is (= 0 (simplify
                  (- result-via-Lie
                     ((D (fn [t]
                           (- ((Y f) ((phi↑X t) m_0))
                              ((Y (f/compose f (phi↑X t))) m_0))))
                      0)))
               ))

        (is (= 0 (simplify
                  (- result-via-Lie
                     ((D (fn [t]
                           (- ((Y f) ((phi↑X t) m_0))
                              ((((cm/pushforward-vector (phi↑X t) (phi↑X (- t)))
                                 Y)
                                f)
                               ((phi↑X t) m_0)))))
                      0)))))

        (is (= 0 (simplify
                  (- result-via-Lie
                     ((D (fn [t]
                           ((((cm/pushforward-vector (phi↑X (- t)) (phi↑X t))
                              Y)
                             f)
                            m_0)))
                      0))))))

      (let [m ((man/point R2-rect) (up 'x 'y))
            V (vf/literal-vector-field 'V R2-rect)
            Y (vf/literal-vector-field 'Y R2-rect)
            f (man/literal-manifold-function 'f R2-rect)

            e_0 (vf/literal-vector-field 'e_0 R2-rect)
            e_1 (vf/literal-vector-field 'e_1 R2-rect)

            vector-basis (down e_0 e_1)
            oneform-basis (b/vector-basis->dual (down e_0 e_1) R2-rect)
            [e↑0 e↑1] oneform-basis

            basis (b/make-basis vector-basis oneform-basis)

            Delta↑i_j (fn [v]
                        (oneform-basis
                         (s/mapr (g/Lie-derivative v) vector-basis)))]
        (is (= 0 (simplify
                  ((- (((g/Lie-derivative V) Y) f)
                      (* (vector-basis f)
                         (+ (V (oneform-basis Y))
                            (* (oneform-basis Y) (Delta↑i_j V)))))
                   m))))

        (is (= 0 (simplify
                  ((- (* (oneform-basis Y)
                         ((s/mapr (g/Lie-derivative V) vector-basis) f))
                      (* (oneform-basis Y)
                         (Delta↑i_j V)
                         (vector-basis f)))
                   m))))

        ;; even simpler:
        (is (= (down 0 0)
               (g/simplify
                ((- (* ((s/mapr (g/Lie-derivative V) vector-basis) f))
                    (* (Delta↑i_j V) (vector-basis f)))
                 m))))))))

(deftest interior-product-tests
  (testing "Claim: L_x omega = i_x d omega + d i_x omega (Cartan Homotopy Formula)"
    (let-coordinates [[x y z] R3-rect]
      (let [R3-rect-point ((man/point R3-rect) (up 'x0 'y0 'z0))

            X (vf/literal-vector-field 'X R3-rect)
            Y (vf/literal-vector-field 'Y R3-rect)
            Z (vf/literal-vector-field 'Z R3-rect)
            W (vf/literal-vector-field 'W R3-rect)

            alpha (man/literal-manifold-function 'alpha R3-rect)
            beta  (man/literal-manifold-function 'beta R3-rect)
            gamma (man/literal-manifold-function 'gamma R3-rect)
            omega
            (+ (* alpha (ff/wedge dx dy))
               (* beta (ff/wedge dy dz))
               (* gamma (ff/wedge dz dx)))

            L1 (fn [X]
                 (fn [omega]
                   (+ ((cov/interior-product X) (ff/d omega))
                      (ff/d ((cov/interior-product X) omega)))))]
        (is (= 0 (simplify
                  ((- (((g/Lie-derivative X) omega) Y Z)
	                    (((L1 X) omega) Y Z))
                   R3-rect-point))))

        (let [omega (ff/literal-oneform-field 'omega R3-rect)]
          (is (= 0 (simplify
                    ((- (((g/Lie-derivative X) omega) Y)
	                      (((L1 X) omega) Y))
	                   R3-rect-point)))))

        (let [omega (* alpha (ff/wedge dx dy dz))]
          (is (= 0 (simplify
                    ((- (((g/Lie-derivative X) omega) Y Z W)
	                      (((L1 X) omega) Y Z W))
	                   R3-rect-point)))))))))
