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
  (:require [clojure.test :refer [is deftest testing use-fixtures]]
            [sicmutils.calculus.basis :as b]
            [sicmutils.calculus.connection :as conn]
            [sicmutils.calculus.coordinate :as c
             :refer [let-coordinates]
             #?@(:cljs [:include-macros true])]
            [sicmutils.calculus.covariant :as cov]
            [sicmutils.calculus.derivative :refer [D]]
            [sicmutils.calculus.form-field :as ff]
            [sicmutils.calculus.indexed :as ci]
            [sicmutils.calculus.manifold :as man
             :refer [R2-polar R2-rect R3-rect R3-cyl R4-rect
                     point chart]]
            [sicmutils.calculus.map :as cm]
            [sicmutils.calculus.metric :refer [S2-metric]]
            [sicmutils.calculus.vector-field :as vf]
            [sicmutils.expression :as x]
            [sicmutils.function :as f]
            [sicmutils.generic :as g :refer [+ - * /]]
            [sicmutils.simplify :refer [hermetic-simplify-fixture]]
            [sicmutils.structure :as s :refer [up down]]
            [sicmutils.value :as v]))

(use-fixtures :each hermetic-simplify-fixture)

(def simplify
  (comp v/freeze g/simplify))

(deftest Lie-derivative-tests
  (testing "Lie derivative."
    (let-coordinates [[x y z]        R3-rect
                      [r theta zeta] R3-cyl]
      (let [R3-rect-point ((point R3-rect) (up 'x0 'y0 'z0))
            R3-cyl-point ((point R3-cyl) (up 'r0 'theta0 'zeta0))

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


        (is (= '(+ (* (Y↑0 p) (((partial 0) w_0) p) (X↑0 p))
                   (* (Y↑0 p) (w_0 p) (((partial 0) X↑0) p))
                   (* (Y↑0 p) (w_1 p) (((partial 0) X↑1) p))
                   (* (Y↑0 p) (w_2 p) (((partial 0) X↑2) p))
                   (* (Y↑0 p) (((partial 1) w_0) p) (X↑1 p))
                   (* (Y↑0 p) (((partial 2) w_0) p) (X↑2 p))
                   (* (X↑0 p) (Y↑1 p) (((partial 0) w_1) p))
                   (* (X↑0 p) (Y↑2 p) (((partial 0) w_2) p))
                   (* (w_0 p) (Y↑1 p) (((partial 1) X↑0) p))
                   (* (w_0 p) (Y↑2 p) (((partial 2) X↑0) p))
                   (* (w_1 p) (Y↑1 p) (((partial 1) X↑1) p))
                   (* (w_1 p) (Y↑2 p) (((partial 2) X↑1) p))
                   (* (w_2 p) (Y↑1 p) (((partial 1) X↑2) p))
                   (* (w_2 p) (Y↑2 p) (((partial 2) X↑2) p))
                   (* (X↑1 p) (Y↑1 p) (((partial 1) w_1) p))
                   (* (X↑1 p) (Y↑2 p) (((partial 1) w_2) p))
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
                   ((point R3-rect)
                    (up 'x↑0 'y↑0 'z↑0))))))))

    (let-coordinates [[x y] R2-rect]
      (let [R2-rect-point ((point R2-rect) (up 'x0 'y0))
            X (vf/literal-vector-field 'X R2-rect)
            Y (vf/literal-vector-field 'Y R2-rect)

            f (man/literal-scalar-field 'f R2-rect)
            present (fn [expr]
                      (-> (simplify expr)
                          (x/substitute '(up x0 y0) 'p)))]

        (is (= '(+ (* -1 (Y↑0 p) (((partial 0) f) p) (((partial 0) X↑0) p))
                   (* -1 (Y↑0 p) (((partial 1) f) p) (((partial 0) X↑1) p))
                   (* (((partial 0) f) p) (X↑1 p) (((partial 1) Y↑0) p))
                   (* (((partial 0) f) p) (((partial 0) Y↑0) p) (X↑0 p))
                   (* -1 (((partial 0) f) p) (Y↑1 p) (((partial 1) X↑0) p))
                   (* (X↑1 p) (((partial 1) f) p) (((partial 1) Y↑1) p))
                   (* (((partial 1) f) p) (X↑0 p) (((partial 0) Y↑1) p))
                   (* -1 (((partial 1) f) p) (Y↑1 p) (((partial 1) X↑1) p)))
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
            m ((point R2-rect) q_0)
            f (man/literal-manifold-function 'f R2-rect)

            e_0 (vf/literal-vector-field 'e_0 R2-rect)
            e_1 (vf/literal-vector-field 'e_1 R2-rect)

            vector-basis (down e_0 e_1)
            oneform-basis (b/vector-basis->dual (down e_0 e_1) R2-rect)
            basis (b/make-basis vector-basis oneform-basis)

            Yi (oneform-basis Y)]
        (is (= 0 (simplify
                  ((- (((g/Lie-derivative V) Y) f)
                      (+ (* (s/mapr (g/Lie-derivative V) Yi) (vector-basis f))
                         (* Yi ((s/mapr (g/Lie-derivative V) vector-basis) f))))
                   m))))))

    (testing "Computation of Lie derivatives by difference quotient."
      (let [X (vf/literal-vector-field 'X R2-rect)
            Y (vf/literal-vector-field 'Y R2-rect)

            q_0 (up 'q_x 'q_y)

            m_0 ((point R2-rect) q_0)

            q (fn [coords]
                (fn [t]
                  (+ coords
                     (* t ((X (chart R2-rect))
                           ((point R2-rect) coords))))))

            gamma (fn [initial-point]
                    (f/compose
                     (point R2-rect)
                     (q ((chart R2-rect) initial-point))))

            phiX (fn [t]
                   (fn [point]
                     ((gamma point) t)))
            f (man/literal-manifold-function 'f R2-rect)
            result-via-Lie ((((g/Lie-derivative X) Y) f) m_0)
            present (fn [expr]
                      (-> (simplify expr)
                          (x/substitute '(up q_x q_y) 'p)))]
        (is (= '(+ (* -1 (Y↑0 p) (((partial 0) f) p) (((partial 0) X↑0) p))
                   (* -1 (Y↑0 p) (((partial 1) f) p) (((partial 0) X↑1) p))
                   (* (((partial 0) f) p) (((partial 0) Y↑0) p) (X↑0 p))
                   (* -1 (((partial 0) f) p) (Y↑1 p) (((partial 1) X↑0) p))
                   (* (((partial 0) f) p) (((partial 1) Y↑0) p) (X↑1 p))
                   (* (((partial 1) f) p) (X↑0 p) (((partial 0) Y↑1) p))
                   (* -1 (((partial 1) f) p) (Y↑1 p) (((partial 1) X↑1) p))
                   (* (((partial 1) f) p) (X↑1 p) (((partial 1) Y↑1) p)))
               (present
                ((D (fn [t]
                      (- ((Y f) ((phiX t) m_0))
                         ((Y (f/compose f (phiX t))) m_0))))
                 0))))

        (is (= 0 (simplify
                  (- result-via-Lie
                     ((D (fn [t]
                           (- ((Y f) ((phiX t) m_0))
                              ((Y (f/compose f (phiX t))) m_0))))
                      0)))
               ))

        (is (= 0 (simplify
                  (- result-via-Lie
                     ((D (fn [t]
                           (- ((Y f) ((phiX t) m_0))
                              ((((cm/pushforward-vector (phiX t) (phiX (- t)))
                                 Y)
                                f)
                               ((phiX t) m_0)))))
                      0)))))

        (is (= 0 (simplify
                  (- result-via-Lie
                     ((D (fn [t]
                           ((((cm/pushforward-vector (phiX (- t)) (phiX t))
                              Y)
                             f)
                            m_0)))
                      0))))))

      (let [m ((point R2-rect) (up 'x 'y))
            V (vf/literal-vector-field 'V R2-rect)
            Y (vf/literal-vector-field 'Y R2-rect)
            f (man/literal-manifold-function 'f R2-rect)

            e_0 (vf/literal-vector-field 'e_0 R2-rect)
            e_1 (vf/literal-vector-field 'e_1 R2-rect)

            vector-basis (down e_0 e_1)
            oneform-basis (b/vector-basis->dual (down e_0 e_1) R2-rect)
            basis (b/make-basis vector-basis oneform-basis)

            Deltai_j (fn [v]
                       (oneform-basis
                        (s/mapr (g/Lie-derivative v) vector-basis)))]
        (is (= 0 (simplify
                  ((- (((g/Lie-derivative V) Y) f)
                      (* (vector-basis f)
                         (+ (V (oneform-basis Y))
                            (* (oneform-basis Y) (Deltai_j V)))))
                   m))))

        (is (= 0 (simplify
                  ((- (* (oneform-basis Y)
                         ((s/mapr (g/Lie-derivative V) vector-basis) f))
                      (* (oneform-basis Y)
                         (Deltai_j V)
                         (vector-basis f)))
                   m))))

        ;; even simpler:
        (is (= (down 0 0)
               (g/simplify
                ((- (* ((s/mapr (g/Lie-derivative V) vector-basis) f))
                    (* (Deltai_j V) (vector-basis f)))
                 m))))))))

(deftest interior-product-tests
  (testing "Claim: L_x omega = i_x d omega + d i_x omega (Cartan Homotopy Formula)"
    (let-coordinates [[x y z] R3-rect]
      (let [R3-rect-point ((point R3-rect) (up 'x0 'y0 'z0))

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


(deftest new-tests
  ;; Structured objects, such as tensors, take vector fields and oneform
  ;; fields as arguments.
  ;;
  ;; oneform fields can act as (0,1) tensor fields if arguments are declared:
  (let [omega (-> (ff/literal-oneform-field 'omega R4-rect)
                  (ci/with-argument-types [::vf/vector-field]))
        m (man/typical-point R4-rect)
        X (vf/literal-vector-field 'X R4-rect)
        Tomega (-> omega
                   (ci/typed->indexed (b/coordinate-system->basis R4-rect))
                   (ci/indexed->typed (b/coordinate-system->basis R4-rect)))
        V (vf/literal-vector-field 'V R4-rect)
        C (conn/literal-Cartan 'G R4-rect)]
    (is (zero?
         (simplify
          (- (((((cov/covariant-derivative C) X) omega) V) m)
             (((((cov/covariant-derivative C) X) Tomega) V) m))))))

  ;; So to test the operation on a vector field we must construct a
  ;; (1,0) tensor field that behaves like a vector field, but acts on
  ;; oneform fields rather than manifold functions.
  (let [basis (b/coordinate-system->basis R4-rect)
        V (vf/literal-vector-field 'V R4-rect)
        TV (-> (fn [oneform] (oneform V))
               (ci/with-argument-types [::ff/oneform-field]))
        m (man/typical-point R4-rect)
        X (vf/literal-vector-field 'X R4-rect)
        omega (ff/literal-oneform-field 'omega R4-rect)
        C (conn/literal-Cartan 'G R4-rect)]
    (is (zero?
         (simplify
          (- ((omega V) m) ((TV omega) m)))))

    ;; So TV is the tensor field that acts as the vector field V.
    (is (zero?
         (simplify
          (- ((omega (((cov/covariant-derivative C) X) V)) m)
             (((((cov/covariant-derivative C) X) TV) omega) m))))))

  (let [g (ci/with-argument-types
            S2-metric [::vf/vector-field
                       ::vf/vector-field])
        G (conn/metric->Christoffel-2
           g
           (b/coordinate-system->basis man/S2-spherical))
        C (cov/Christoffel->Cartan G)
        V (vf/literal-vector-field 'V man/S2-spherical)
        X (vf/literal-vector-field 'X man/S2-spherical)
        Y (vf/literal-vector-field 'Y man/S2-spherical)
        m ((point man/S2-spherical) (up 'theta 'phi))]

    (is (v/zero?
         (simplify
          (((((cov/covariant-derivative C) V) g) X Y) m)))))

  (testing "Fun with Christoffel symbols."
    (let-coordinates [[x y] R2-rect]
      (let [R2-rect-basis (b/coordinate-system->basis R2-rect)
            R2-rect-point ((point R2-rect) (up 'x0 'y0))
            Gijk (fn [i j k]
                   (man/literal-manifold-function
                    (symbol
                     (str "G↑" i "_" j k))
                    R2-rect))
            G (down (down (up (Gijk 0 0 0)
                              (Gijk 1 0 0))
                          (up (Gijk 0 1 0)
                              (Gijk 1 1 0)))
                    (down (up (Gijk 0 0 1)
                              (Gijk 1 0 1))
                          (up (Gijk 0 1 1)
                              (Gijk 1 1 1))))
            present (fn [expr]
                      (-> (simplify expr)
                          (x/substitute '(up x0 y0) 'p)))]
        (is (= '(down (down (up (G↑0_00 p) (G↑1_00 p))
                            (up (G↑0_10 p) (G↑1_10 p)))
                      (down (up (G↑0_01 p) (G↑1_01 p))
                            (up (G↑0_11 p) (G↑1_11 p))))
               (v/freeze
                (present
                 (G R2-rect-point)))))

        (let [CG (cov/make-Christoffel G R2-rect-basis)
              CF (cov/Christoffel->Cartan CG)]
          (is (= '(down
                   (up (+ (* (G↑0_00 p) (X↑0 p)) (* (G↑0_01 p) (X↑1 p)))
                       (+ (* (G↑1_00 p) (X↑0 p)) (* (G↑1_01 p) (X↑1 p))))
                   (up (+ (* (G↑0_10 p) (X↑0 p)) (* (G↑0_11 p) (X↑1 p)))
                       (+ (* (G↑1_10 p) (X↑0 p)) (* (G↑1_11 p) (X↑1 p)))))
                 (v/freeze
                  (present
                   (((cov/Cartan->forms CF) (vf/literal-vector-field 'X R2-rect))
                    R2-rect-point)))))

          (is (= '(down
                   (down (up (G↑0_00 p) (G↑1_00 p))
                         (up (G↑0_10 p) (G↑1_10 p)))
                   (down (up (G↑0_01 p) (G↑1_01 p))
                         (up (G↑0_11 p) (G↑1_11 p))))
                 (v/freeze
                  (present
                   ((cov/Christoffel->symbols
                     (cov/Cartan->Christoffel (cov/Christoffel->Cartan CG)))
                    R2-rect-point)))))

          ;; Transformation of Cartan to polar leaves covariant derivative
          ;; invariant.
          #_
          (is (zero?
               (simplify
                (((((- (cov/covariant-derivative CF)
                       (cov/covariant-derivative
                        (cov/Cartan-transform
                         ;; TODO we need to get this working!!
                         CF (R2-polar 'coordinate-basis))))
                    (vf/literal-vector-field 'A R2-rect))
                   (vf/literal-vector-field 'B R2-polar))
                  (man/literal-scalar-field 'f R2-polar))
                 R2-rect-point)))))))))

(deftest more-new-tests
  (testing "Example from the text:"
    (let-coordinates [[x y] R2-rect
                      [r theta] R2-polar]
      (let [v (vf/literal-vector-field 'v R2-rect)
            w (vf/literal-vector-field 'w R2-rect)
            f (man/literal-manifold-function 'f R2-rect)
            R2-rect-point ((point R2-rect) (up 'x0 'y0))
            R2-rect-basis (b/coordinate-system->basis R2-rect)
            R2-polar-basis (b/coordinate-system->basis R2-polar)
            R2-rect-Christoffel
            (cov/make-Christoffel
             (let [zero man/zero-manifold-function]
               (down (down (up zero zero)
                           (up zero zero))
                     (down (up zero zero)
                           (up zero zero))))
             R2-rect-basis)
            R2-rect-Cartan (cov/Christoffel->Cartan R2-rect-Christoffel)
            R2-polar-Christoffel (cov/make-Christoffel
                                  (let [zero man/zero-manifold-function]
                                    (down (down (up zero zero)
                                                (up zero (/ 1 r)))
                                          (down (up zero (/ 1 r))
                                                (up (* -1 r) zero))))
                                  R2-polar-basis)
            R2-polar-Cartan (cov/Christoffel->Cartan R2-polar-Christoffel)]
        (is (zero?
             (simplify
              (((((- (cov/covariant-derivative R2-rect-Cartan)
                     (cov/covariant-derivative R2-polar-Cartan))
                  v)
                 w)
                f)
               (man/typical-point R2-rect)))))

        (is (zero?
             (simplify
              (((((- (cov/covariant-derivative R2-polar-Cartan)
                     (cov/covariant-derivative
                      (cov/Cartan-transform R2-polar-Cartan R2-rect-basis)))
                  v)
                 w)
                f)
               R2-rect-point)))))))

  #_
  (comment
    (define X (vf/literal-vector-field 'X R2-rect))

    (define V (vf/literal-vector-field 'V R2-rect))

    (pec (((((cov/covariant-derivative CF) X) V)
           (man/literal-manifold-function 'F R2-rect))
          R2-rect-point))
    ;; Result:
    (+ (* G↑0_00 V↑0 ((partial 0) F) X↑0)
       (* G↑1_00 V↑0 ((partial 1) F) X↑0)
       (* G↑0_10 ((partial 0) F) V↑1 X↑0)
       (* G↑1_10 ((partial 1) F) V↑1 X↑0)
       (* G↑0_01 V↑0 ((partial 0) F) X↑1)
       (* G↑1_01 V↑0 ((partial 1) F) X↑1)
       (* G↑0_11 ((partial 0) F) V↑1 X↑1)
       (* G↑1_11 ((partial 1) F) V↑1 X↑1)
       (* ((partial 0) F) ((partial 0) V↑0) X↑0)
       (* ((partial 0) F) ((partial 1) V↑0) X↑1)
       (* ((partial 1) F) ((partial 0) V↑1) X↑0)
       (* ((partial 1) F) ((partial 1) V↑1) X↑1))


    (define-coordinates (up x y) R2-rect)
    (define rect-basis (b/coordinate-system->basis R2-rect))

    (define-coordinates (up r theta) R2-polar)
    (define polar-basis (b/coordinate-system->basis R2-polar))

    (define rect-chi (R2-rect '->coords))
    (define rect-chi-inverse (R2-rect '->point))
    (define polar-chi (R2-polar '->coords))
    (define polar-chi-inverse (R2-polar '->point))
    (define m2 (rect-chi-inverse (up 'x0 'y0)))

    (define rect-Christoffel
      (cov/make-Christoffel
       (let ((zero (fn (m) 0)))
         (down (down (up zero zero)
                     (up zero zero))
               (down (up zero zero)
                     (up zero zero))))
       rect-basis))

    (define polar-Christoffel
      (cov/make-Christoffel
       (let ((zero (fn (m) 0)))
         (down (down (up zero zero)
                     (up zero (/ 1 r)))
               (down (up zero (/ 1 r))
                     (up (* -1 r) zero))))
       polar-basis))

    (define rect-Cartan
      (cov/Christoffel->Cartan rect-Christoffel))

    (define polar-Cartan
      (cov/Christoffel->Cartan polar-Christoffel))

    (define J (- (* x d/dy) (* y d/dx)))

    (define f (man/literal-scalar-field 'f R2-rect))

    (pec
     (((((cov/covariant-derivative rect-Cartan)
         d/dx)
        J)
       f)
      m2))
    ;; Result:
    ((partial 1) f)

    ;; Note: arg-suppressor is in force from above.

    (pec
     (((((cov/covariant-derivative polar-Cartan)
         d/dx)
        J)
       f)
      m2))
    ;; Result:
    ((partial 1) f)



    ;; More generally, can show independence here

    (define v (vf/literal-vector-field 'v R2-rect))
    (define w (vf/literal-vector-field 'w R2-rect))

    (pec
     (((((- (cov/covariant-derivative rect-Cartan)
            (cov/covariant-derivative polar-Cartan))
         v)
        w)
       f)
      m2))
    ;; Result:
    0


    (define v (vf/literal-vector-field 'v R2-polar))
    (define w (vf/literal-vector-field 'w R2-polar))

    (pec
     (((((- (cov/covariant-derivative rect-Cartan)
            (cov/covariant-derivative polar-Cartan))
         v)
        w)
       f)
      m2))
    ;; Result:
    0


    (testing "after cartan-over-map"

      (define M (make-manifold S↑2-type 2 3))
      (define spherical
        (coordinate-system-at 'spherical 'north-pole M))
      (define-coordinates (up theta phi) spherical)
      (define-coordinates t the-real-line)
      (define spherical-basis (b/coordinate-system->basis spherical))

      (define G-S2-1
        (cov/make-Christoffel
         (let ((zero  (fn (point) 0)))
           (down (down (up zero zero)
                       (up zero (/ 1 (tan theta))))
                 (down (up zero (/ 1 (tan theta)))
                       (up (- (* (sin theta) (cos theta))) zero))))
         spherical-basis))


      (define gamma:N->M
        (compose (spherical '->point)
                 (up (literal-function 'alpha)
                     (literal-function 'beta))
                 (the-real-line '->coords)))

      (define basis-over-gamma
        (basis->basis-over-map gamma:N->M spherical-basis))

      (define w
        (basis-components->vector-field
         (up (compose (literal-function 'w0)
                      (the-real-line '->coords))
             (compose (literal-function 'w1)
                      (the-real-line '->coords)))
         (basis->vector-basis basis-over-gamma)))

      (define sphere-Cartan (cov/Christoffel->Cartan G-S2-1))

      (pec
       (s:map/r
        (fn (omega)
          ((omega
            (((cov/covariant-derivative sphere-Cartan gamma:N->M)
              d/dt)
             w))
           ((the-real-line '->point) 'tau)))
        (basis->oneform-basis basis-over-gamma)))
      ;; Result:
      (up
       (+ (* -1 (sin (alpha tau)) ((D beta) tau) (w1 tau) (cos (alpha tau)))
          ((D w0) tau))
       (+ (/ (* (w0 tau) ((D beta) tau) (cos (alpha tau))) (sin (alpha tau)))
          (/ (* (w1 tau) ((D alpha) tau) (cos (alpha tau))) (sin (alpha tau)))
          ((D w1) tau))))))

#_
(deftest geodesic-equations
  (testing "geodesic-equations"
    (pec
     (s:map/r
      (fn (omega)
        ((omega
          (((cov/covariant-derivative sphere-Cartan gamma:N->M)
            d/dt)
           ((differential gamma:N->M) d/dt)))
         ((the-real-line '->point) 't)))
      (basis->oneform-basis basis-over-gamma)))

    ;; Result:
    (up
     (+ (* -1 (sin (alpha t)) (expt ((D beta) t) 2) (cos (alpha t)))
        (((expt D 2) alpha) t))
     (+ (/ (* 2 ((D beta) t) (cos (alpha t)) ((D alpha) t)) (sin (alpha t)))
        (((expt D 2) beta) t)))


    ;; Geodesic equation

    (define-coordinates (up x y) R2-rect)

    (define (Gijk i j k)
      (man/literal-manifold-function
       (string->symbol
        (string-append "G↑"
                       (number->string i)
                       "_"
                       (number->string j)
                       (number->string k)))
       R2-rect))

    (define G
      (down (down (up (Gijk 0 0 0)
                      (Gijk 1 0 0))
                  (up (Gijk 0 1 0)
                      (Gijk 1 1 0)))
            (down (up (Gijk 0 0 1)
                      (Gijk 1 0 1))
                  (up (Gijk 0 1 1)
                      (Gijk 1 1 1)))))

    (define CG
      (cov/make-Christoffel G (b/coordinate-system->basis R2-rect)))

    (define gamma:N->M
      (compose (R2-rect '->point)
               (up (literal-function 'alpha)
                   (literal-function 'beta))
               (the-real-line '->coords)))

    (define basis-over-gamma
      (basis->basis-over-map gamma:N->M
                             (b/coordinate-system->basis R2-rect)))

    (define u
      (basis-components->vector-field
       (up (compose (literal-function 'u0)
                    (the-real-line '->coords))
           (compose (literal-function 'u1)
                    (the-real-line '->coords)))
       (basis->vector-basis basis-over-gamma)))


    (pec
     (s:map/r
      (fn (omega)
        ((omega
          (((cov/covariant-derivative (cov/Christoffel->Cartan CG) gamma:N->M)
            d/dt)
           u))
         ((the-real-line '->point) 't)))
      (basis->oneform-basis basis-over-gamma)))
    ;; Result:
    (up
     (+ (* ((D alpha) t) (u0 t) (G↑0_00 (up (alpha t) (beta t))))
        (* ((D alpha) t) (u1 t) (G↑0_10 (up (alpha t) (beta t))))
        (* ((D beta) t) (u0 t) (G↑0_01 (up (alpha t) (beta t))))
        (* ((D beta) t) (u1 t) (G↑0_11 (up (alpha t) (beta t))))
        ((D u0) t))
     (+ (* ((D alpha) t) (u0 t) (G↑1_00 (up (alpha t) (beta t))))
        (* ((D alpha) t) (u1 t) (G↑1_10 (up (alpha t) (beta t))))
        (* ((D beta) t) (u0 t) (G↑1_01 (up (alpha t) (beta t))))
        (* ((D beta) t) (u1 t) (G↑1_11 (up (alpha t) (beta t))))
        ((D u1) t)))


    (pec
     (s:map/r
      (fn (omega)
        ((omega
          (((cov/covariant-derivative (cov/Christoffel->Cartan CG) gamma:N->M)
            d/dt)
           ((differential gamma:N->M) d/dt)))
         ((the-real-line '->point) 't)))
      (basis->oneform-basis basis-over-gamma)))
    ;; Result:
    (up
     (+ (* (expt ((D alpha) t) 2) (G↑0_00 (up (alpha t) (beta t))))
        (* ((D alpha) t) ((D beta) t) (G↑0_01 (up (alpha t) (beta t))))
        (* ((D alpha) t) ((D beta) t) (G↑0_10 (up (alpha t) (beta t))))
        (* (expt ((D beta) t) 2) (G↑0_11 (up (alpha t) (beta t))))
        (((expt D 2) alpha) t))
     (+ (* (expt ((D alpha) t) 2) (G↑1_00 (up (alpha t) (beta t))))
        (* ((D alpha) t) ((D beta) t) (G↑1_01 (up (alpha t) (beta t))))
        (* ((D alpha) t) ((D beta) t) (G↑1_10 (up (alpha t) (beta t))))
        (* (expt ((D beta) t) 2) (G↑1_11 (up (alpha t) (beta t))))
        (((expt D 2) beta) t)))


    ;; Geodesic Equations = Lagrange Equations
    ;;
    ;; Here I restrict everything to the unit sphere.
    ;; The coordinates on the unit sphere

    (define-coordinates t R1-rect)
    (define-coordinates (up theta phi) S2-spherical)

    (define two-sphere-basis (b/coordinate-system->basis S2-spherical))

    ;; The Christoffel symbols (for r=1) (p.341 MTW) are:

    (define G-S2-1
      (cov/make-Christoffel
       (let ((zero  (fn (point) 0)))
         (down (down (up zero zero)
                     (up zero (/ 1 (tan theta))))
               (down (up zero (/ 1 (tan theta)))
                     (up (- (* (sin theta) (cos theta))) zero))))
       two-sphere-basis))

    (pec (let ((mu:N->M (compose (S2-spherical '->point)
                                 (up (literal-function 'mu-theta)
                                     (literal-function 'mu-phi))
                                 (R1-rect '->coords)))
               (Cartan (cov/Christoffel->Cartan G-S2-1)))
           (s:map/r
            (fn (w)
              ((w
                (((cov/covariant-derivative Cartan mu:N->M) d/dt)
                 ((differential mu:N->M) d/dt)))
               ((R1-rect '->point) 'tau)))
            (basis->oneform-basis
             (basis->basis-over-map mu:N->M
                                    (Cartan->basis Cartan))))))
    ;; Result:
    (up (+ (* -1
              (expt ((D mu-phi) tau) 2)
              (cos (mu-theta tau))
              (sin (mu-theta tau)))
           (((expt D 2) mu-theta) tau))
        (+ (/ (* 2 ((D mu-phi) tau)
                 (cos (mu-theta tau))
                 ((D mu-theta) tau))
              (sin (mu-theta tau)))
           (((expt D 2) mu-phi) tau)))

    ;; We can get the geodesic equations as ordinary Lagrange
    ;; equations of a free particle constrained to the surface
    ;; of the sphere:

    (define ((Lfree m) s)
      (let ((t (time s))
            (q (coordinate s))
            (v (velocity s)))
        (* 1/2 m (square v))))


    ;; F is really the embedding map, from the coordinates on the sphere
    ;; to the 3-space coordinates in the embedding manifold.

    ;; This hides the assumption that the R3 manifold is the same one as
    ;; the embedding manifold.

    (define F
      (compose (R3-rect '->coords)
               (S2-spherical '->point)
               coordinate))

    ;; Actually (9 June 2009--GJS) this no longer works, because R3-rect
    ;; does not accept an S2-spherical point as in the same manifold.

    ;; Fixed by explicit transfer of a point -- see manifold.scm



    (define F
      (compose (R3-rect '->coords)
               (transfer-point S2-spherical R3-rect)
               (S2-spherical '->point)
               coordinate))

    (define Lsphere
      (compose (Lfree 1) (F->C F)))

    (pec (((Lagrange-equations Lsphere)
           (up (literal-function 'theta)
               (literal-function 'phi)))
          't))
    ;; Result:
    (down
     (+ (((expt D 2) theta) t)
        (* -1 (cos (theta t)) (sin (theta t)) (expt ((D phi) t) 2)))
     (+ (* (expt (sin (theta t)) 2) (((expt D 2) phi) t))
        (* 2 (cos (theta t)) (sin (theta t)) ((D phi) t) ((D theta) t))))



    ;; Note these are DOWN while the geodesic equations are UP.  This is
    ;; due to the fact that the geodesic equations are raised by the
    ;; metric, which is diagonal, here R=1, and cancels an instance
    ;; of(expt (sin theta) 2).

    ;; Also see p.345 MTW for computing Christoffel symbols from Lagrange
    ;; equations.


    ;; Exercise on computation of Christoffel symbols.

    (install-coordinates R3-rect (up 'x 'y 'z))
    (define R3-rect-point ((R3-rect '->point) (up 'x0 'y0 'z0)))

    (install-coordinates R3-cyl (up 'r 'theta 'zeta))
    (define R3-cyl-point ((R3-cyl '->point) (up 'r0 'theta0 'z0)))

    (define mpr (R3-rect '->coords))

    (pec (((* d/dr d/dr) mpr) R3-rect-point))
    ;; Result:
    (up 0 0 0)

    ;; So \Gamma↑r_{rr} = 0, \Gamma↑\theta_{rr} = 0

    (pec (((* d/dtheta d/dr) mpr) R3-rect-point))
    ;; Result:
    (up (/ (* -1 y0) (sqrt (+ (expt x0 2) (expt y0 2))))
        (/ x0 (sqrt (+ (expt x0 2) (expt y0 2))))
        0)

    ;; by hand = -sint d/dx + cost d/dy = 1/r d/dtheta
    ;; Indeed.

    (pec (((* d/dtheta d/dr) mpr) R3-cyl-point))
    ;; Result:
    (up (* -1 (sin theta0)) (cos theta0) 0)

    ;; So \Gamma↑r_{r\theta} = 0, \Gamma↑\theta_{r\theta} = 1/r

    (pec (((* d/dr d/dtheta) mpr) R3-rect-point))
    ;; Result:
    (up (/ (* -1 y0) (sqrt (+ (expt x0 2) (expt y0 2))))
        (/ x0 (sqrt (+ (expt x0 2) (expt y0 2))))
        0)

    ;; by hand = -sint d/dx + cost d/dy = 1/r d/dtheta

    (pec (((* d/dr d/dtheta) mpr) R3-cyl-point))
    ;; Result:
    (up (* -1 (sin theta0)) (cos theta0) 0)

    ;; So \Gammar_{\theta r} = 0, \Gamma\theta_{\theta r} = 1/r

    (pec (((* d/dtheta d/dtheta) mpr) R3-rect-point))
    ;; Result:
    (up (* -1 x0) (* -1 y0) 0)

    ;; by hand = -r cost d/dx - r sint d/dy = -r d/dr

    (pec (((* d/dtheta d/dtheta) mpr) R3-cyl-point))
    ;; Result:
    (up (* -1 r0 (cos theta0)) (* -1 r0 (sin theta0)) 0)

    ;; So \Gammar_{\theta \theta} = -r, \Gamma\theta_{\theta \theta} = 0

    ;; These are correct Christoffel symbols...


    ;; Computation of Covariant derivatives by difference quotient.
    ;; CD below is parallel in definition to the Lie Derivative.
    ;; Does not seem to depend on a derivative of basis vectors, in fact
    ;; the derivative of the basis vectors is multiplied by zero in the
    ;; product rule output.
    (define (Gijk i j k)
      (man/literal-manifold-function
       (string->symbol
        (string-append "G↑"
                       (number->string i)
                       "_"
                       (number->string j)
                       (number->string k)))
       R2-rect))

    (define G
      (down (down (up (Gijk 0 0 0)
                      (Gijk 1 0 0))
                  (up (Gijk 0 1 0)
                      (Gijk 1 1 0)))
            (down (up (Gijk 0 0 1)
                      (Gijk 1 0 1))
                  (up (Gijk 0 1 1)
                      (Gijk 1 1 1)))))

    (define X (vf/literal-vector-field 'X R2-rect))

    (define Y (vf/literal-vector-field 'Y R2-rect))

    (define q_0 (up 'q_x 'q_y))

    (define m_0
      ((R2-rect '->point) q_0))

    (define F (man/literal-manifold-function 'F R2-rect))


    (define (((((CD CF chart) v) u) F) m)

      (define (Sigma state) (ref state 0))
      (define (U state) (ref state 1))
      (define (sigma-u sigma u) (up sigma u))

      (define chi (chart '->coords))
      (define chi↑-1 (chart '->point))

      ;; ((gamma m) delta) is the point on gamma advanced by delta.

      (define ((gamma m) delta)
        (chi↑-1 (+ (chi m) (* delta ((v chi) m)))))

      (let ((basis (Cartan->basis CF)))
        (let ((vector-basis (basis->vector-basis basis))
              (oneform-basis (basis->oneform-basis basis)))
          (let ((u↑i (oneform-basis u)))
            (let ((initial-state
                   (sigma-u (chi m) (u↑i m))))

              (define (bs state)
                (let ((sigma (Sigma state)))
                  (let ((m_0 (chi↑-1 sigma)))
                    (up ((v chi) m_0)
                        (* -1
                           (((cov/Cartan->forms CF) v) m_0)
                           (u↑i m_0))))))

              (define (vs fs)
                (* (D fs) bs))

              ;; First-order approximation to A

              (define (Au delta)
                (+ (U initial-state)
                   (* delta ((vs U) initial-state))))

              (define (g delta)
                (let ((advanced-m ((gamma m) delta)))
                  (* (- (u↑i advanced-m) (Au delta))
                     ((vector-basis F) advanced-m))))

              ((D g) 0))))))

    ;; A bit simpler, but lacking in motivation?

    (define (((((CD CF chart) v) u) F) m)

      (define (Sigma state) (ref state 0))
      (define (U state) (ref state 1))
      (define (sigma-u sigma u) (up sigma u))

      (define chi (chart '->coords))
      (define chi↑-1 (chart '->point))

      ;; ((gamma m) delta) is the point on gamma advanced by delta.

      (define ((gamma m) delta)
        (chi↑-1 (+ (chi m) (* delta ((v chi) m)))))

      (let ((basis (Cartan->basis CF)))
        (let ((vector-basis (basis->vector-basis basis))
              (oneform-basis (basis->oneform-basis basis)))
          (let ((u↑i (oneform-basis u)))
            (let ((initial-state
                   (sigma-u (chi m) (u↑i m))))

              ;; First-order approximation to A

              (define (Au delta)
                (- (u↑i m)
                   (* delta
                      (((cov/Cartan->forms CF) v) m)
                      (u↑i m))))

              (define (g delta)
                (let ((advanced-m ((gamma m) delta)))
                  (* (- (u↑i advanced-m) (Au delta))
                     ((vector-basis F) advanced-m))))

              ((D g) 0))))))

    (let ((CF (cov/Christoffel->Cartan
               (cov/make-Christoffel G
                                     (b/coordinate-system->basis R2-rect)))))
      (pe (- (((((CD CF R2-rect) X) Y) F) m_0)
             (((((cov/covariant-derivative CF) X) Y) F) m_0))))
    0

    (let ((CF (cov/Christoffel->Cartan
               (cov/make-Christoffel G
                                     (b/coordinate-system->basis R2-polar)))))
      (pe (- (((((CD CF R2-rect) X) Y) F) m_0)
             (((((cov/covariant-derivative CF) X) Y) F) m_0))))
    0

    (let ((CF (cov/Christoffel->Cartan
               (cov/make-Christoffel G
                                     (b/coordinate-system->basis R2-rect)))))
      (pe (- (((((CD CF R2-polar) X) Y) F) m_0)
             (((((cov/covariant-derivative CF) X) Y) F) m_0))))
    0

    (let ((CF (cov/Christoffel->Cartan
               (cov/make-Christoffel G
                                     (b/coordinate-system->basis R2-polar)))))
      (pe (- (((((CD CF R2-polar) X) Y) F) m_0)
             (((((cov/covariant-derivative CF) X) Y) F) m_0))))
    0
    ;; Too slow...


    ;; Testing on forms.

    (define (Gijk i j k)
      (man/literal-manifold-function
       (string->symbol
        (string-append "G↑"
                       (number->string i)
                       "_"
                       (number->string j)
                       (number->string k)))
       R2-rect))

    (define G
      (down (down (up (Gijk 0 0 0)
                      (Gijk 1 0 0))
                  (up (Gijk 0 1 0)
                      (Gijk 1 1 0)))
            (down (up (Gijk 0 0 1)
                      (Gijk 1 0 1))
                  (up (Gijk 0 1 1)
                      (Gijk 1 1 1)))))

    (define X (vf/literal-vector-field 'X R2-rect))

    (define Y (vf/literal-vector-field 'Y R2-rect))

    (define omega (ff/literal-oneform-field 'omega R2-rect))

    (define q_0 (up 'q_x 'q_y))

    (define m_0
      ((R2-rect '->point) q_0))

    (define F (man/literal-manifold-function 'F R2-rect))

    (let* ((CF (cov/Christoffel->Cartan
                (cov/make-Christoffel G
                                      (b/coordinate-system->basis R2-rect))))
           (D_x ((cov/covariant-derivative CF) X)))
      (pe (- (+ (((D_x omega) Y) m_0)
                ((omega (D_x Y)) m_0))
             ((D_x (omega Y)) m_0))))
    0


    (define tau (ff/literal-oneform-field 'tau R2-rect))

    (define Z (vf/literal-vector-field 'Z R2-rect))

    (let* ((CF (cov/Christoffel->Cartan
                (cov/make-Christoffel G
                                      (b/coordinate-system->basis R2-rect))))
           (D_x ((cov/covariant-derivative CF) X)))
      (pe (- (((D_x (wedge omega tau)) Y Z) m_0)
             (+ (((wedge omega (D_x tau)) Y Z) m_0)
                (((wedge (D_x omega) tau) Y Z) m_0)))))
    0

    (let* ((CF (cov/Christoffel->Cartan
                (cov/make-Christoffel G
                                      (b/coordinate-system->basis R2-polar))))
           (D_x ((cov/covariant-derivative CF) X)))
      (pe (- (((D_x (wedge omega tau)) Y Z) m_0)
             (+ (((wedge omega (D_x tau)) Y Z) m_0)
                (((wedge (D_x omega) tau) Y Z) m_0)))))
    0


    ;; Next, tests, for the actual functions:

    (((geodesic-equation the-real-line R2-rect (conn/literal-Cartan 'G R2-rect))
      (literal-manifold-map 'gamma the-real-line R2-rect))
     ((point the-real-line) 't))

    (up
     (+ (* (expt ((D gamma0) t) 2) (G_00↑0 (up (gamma0 t) (gamma1 t))))
        (* ((D gamma0) t) ((D gamma1) t) (G_10↑0 (up (gamma0 t) (gamma1 t))))
        (* ((D gamma0) t) ((D gamma1) t) (G_01↑0 (up (gamma0 t) (gamma1 t))))
        (* (expt ((D gamma1) t) 2) (G_11↑0 (up (gamma0 t) (gamma1 t))))
        (((expt D 2) gamma0) t))
     (+ (* (expt ((D gamma0) t) 2) (G_00↑1 (up (gamma0 t) (gamma1 t))))
        (* ((D gamma0) t) ((D gamma1) t) (G_10↑1 (up (gamma0 t) (gamma1 t))))
        (* ((D gamma0) t) ((D gamma1) t) (G_01↑1 (up (gamma0 t) (gamma1 t))))
        (* (expt ((D gamma1) t) 2) (G_11↑1 (up (gamma0 t) (gamma1 t))))
        (((expt D 2) gamma1) t)))



    (let ((C (conn/literal-Cartan 'G R2-rect)))
      (- (((geodesic-equation the-real-line R2-rect C)
           (literal-manifold-map 'gamma the-real-line R2-rect))
          ((point the-real-line) 't))
         (((geodesic-equation the-real-line R2-rect (symmetrize-Cartan C))
           (literal-manifold-map 'gamma the-real-line R2-rect))
          ((point the-real-line) 't))))

    (up 0 0)))

#_(deftest parallel-transport-equation
    (define M (make-manifold S↑2-type 2 3))
    (define S2-spherical
      (coordinate-system-at 'spherical 'north-pole M))
    (define-coordinates (up theta phi) S2-spherical)
    (define S2-basis
      (b/coordinate-system->basis S2-spherical))

    (define G-S2-1
      (cov/make-Christoffel
       (let ((zero  (fn (point) 0)))
         (down (down (up zero zero)
                     (up zero (/ 1 (tan theta))))
               (down (up zero (/ 1 (tan theta)))
                     (up (- (* (sin theta)
                               (cos theta)))
                         zero))))
       S2-basis))

    (define gamma
      (compose (point S2-spherical)
               (up (literal-function 'alpha)
                   (literal-function 'beta))
               (chart the-real-line)))


    (define basis-over-gamma
      (basis->basis-over-map gamma S2-basis))


    (define u
      (basis-components->vector-field
       (up (compose (literal-function 'u↑0)
                    (chart the-real-line))
           (compose (literal-function 'u↑1)
                    (chart the-real-line)))
       (basis->vector-basis basis-over-gamma)))

    (define sphere-Cartan
      (cov/Christoffel->Cartan G-S2-1))

    ((((parallel-transport-equation
        the-real-line S2-spherical sphere-Cartan)
       gamma)
      u)
     ((point the-real-line) 't))


    (up
     (+ (* -1 (sin (alpha t)) ((D beta) t) (u↑1 t) (cos (alpha t))) ((D u↑0) t))
     (+ (/ (* (u↑0 t) ((D beta) t) (cos (alpha t))) (sin (alpha t)))
        (/ (* (u↑1 t) ((D alpha) t) (cos (alpha t))) (sin (alpha t)))
        ((D u↑1) t))))
