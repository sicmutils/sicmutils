#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.calculus.covariant-test
  (:refer-clojure :exclude [+ - * /  partial])
  (:require [clojure.test :refer [is deftest testing use-fixtures]]
            [emmy.abstract.function :as af]
            [emmy.calculus.basis :as b]
            [emmy.calculus.connection :as conn]
            [emmy.calculus.coordinate :as c :refer [let-coordinates]]
            [emmy.calculus.covariant :as cov]
            [emmy.calculus.derivative :refer [D]]
            [emmy.calculus.form-field :as ff]
            [emmy.calculus.indexed :as ci]
            [emmy.calculus.manifold :as man
             :refer [R2-polar R2-rect R3-rect R3-cyl R4-rect
                     S2-spherical the-real-line
                     point chart]]
            [emmy.calculus.map :as cm]
            [emmy.calculus.metric :refer [S2-metric]]
            [emmy.calculus.vector-field :as vf]
            [emmy.expression :as x]
            [emmy.function :as f :refer [compose]]
            [emmy.generic :as g :refer [+ - * / sin cos tan]]
            [emmy.mechanics.lagrange :as ml]
            [emmy.polynomial.gcd :as pg]
            [emmy.simplify :refer [hermetic-simplify-fixture]]
            [emmy.structure :as s :refer [up down]]
            [emmy.value :as v]))

(use-fixtures :each hermetic-simplify-fixture)

(def simplify
  (comp v/freeze g/simplify))

(deftest Lie-derivative-tests
  (testing "Lie derivative."
    (let-coordinates [[x y z]        R3-rect
                      [r theta zeta] R3-cyl]
      (let [R3-rect-point ((point R3-rect) (up 'x0 'y0 'z0))

            w (ff/literal-oneform-field 'w R3-rect)

            X (vf/literal-vector-field 'X R3-rect)
            Y (vf/literal-vector-field 'Y R3-rect)
            Z (vf/literal-vector-field 'Z R3-rect)

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
        (let [I (chart R2-rect)]
          (letfn [(Lie-test [V]
                    (fn [Y]
                      (fn [f]
                        (fn [x]
                          (letfn [(g [t]
                                    (- ((compose (Y f) (point R2-rect))
                                        ((+ I (* t (V I))) x))
                                       ((Y (compose f (point R2-rect)
                                                    (+ I (* t (V I)))))
                                        x)))]
                            ((D g) 0))))))]
            (is (zero?
                 (simplify
                  (- ((((Lie-test X) Y) f) R2-rect-point)
                     ((((g/Lie-derivative X) Y) f) R2-rect-point)))))))))

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
                    (compose
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
                         ((Y (compose f (phiX t))) m_0))))
                 0))))

        (is (= 0 (simplify
                  (- result-via-Lie
                     ((D (fn [t]
                           (- ((Y f) ((phiX t) m_0))
                              ((Y (compose f (phiX t))) m_0))))
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

(defn Gijk [i j k]
  (man/literal-manifold-function
   (symbol
    (str "G↑" i "_" j k))
   R2-rect))

(def G
  (down (down (up (Gijk 0 0 0)
                  (Gijk 1 0 0))
              (up (Gijk 0 1 0)
                  (Gijk 1 1 0)))
        (down (up (Gijk 0 0 1)
                  (Gijk 1 0 1))
              (up (Gijk 0 1 1)
                  (Gijk 1 1 1)))))

(def R2-rect-point ((point R2-rect) (up 'x0 'y0)))
(def R2-rect-basis (b/coordinate-system->basis R2-rect))
(def R2-polar-basis (b/coordinate-system->basis R2-polar))

(defn present
  "Simplifies and returns an expression with all generic R2-rect points replaced
  by `'p` for a tighter presentation."
  [expr]
  (-> (simplify expr)
      (x/substitute '(up x0 y0) 'p)))

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
  (let [V (vf/literal-vector-field 'V R4-rect)
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
           (b/coordinate-system->basis S2-spherical))
        C (cov/Christoffel->Cartan G)
        V (vf/literal-vector-field 'V S2-spherical)
        X (vf/literal-vector-field 'X S2-spherical)
        Y (vf/literal-vector-field 'Y S2-spherical)
        m ((point S2-spherical) (up 'theta 'phi))]

    (is (v/zero?
         (simplify
          (((((cov/covariant-derivative C) V) g) X Y) m)))))

  (testing "Fun with Christoffel symbols."
    (let-coordinates [[x y] R2-rect]
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
        (is (zero?
             (simplify
              (((((- (cov/covariant-derivative CF)
                     (cov/covariant-derivative
                      (cov/Cartan-transform
                       CF (b/coordinate-system->basis R2-polar))))
                  (vf/literal-vector-field 'A R2-rect))
                 (vf/literal-vector-field 'B R2-polar))
                (man/literal-scalar-field 'f R2-polar))
               R2-rect-point))))))))

(deftest more-new-tests
  (testing "Example from the text:"
    (let-coordinates [[x y] R2-rect
                      [r theta] R2-polar]
      (let [v (vf/literal-vector-field 'v R2-rect)
            w (vf/literal-vector-field 'w R2-rect)
            f (man/literal-manifold-function 'f R2-rect)
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

  (let [X (vf/literal-vector-field 'X R2-rect)
        V (vf/literal-vector-field 'V R2-rect)
        CG (cov/make-Christoffel G R2-rect-basis)
        CF (cov/Christoffel->Cartan CG)]
    (is (= '(+ (* (((partial 0) F) p) (X↑0 p) (G↑0_00 p) (V↑0 p))
               (* (((partial 0) F) p) (X↑0 p) (G↑0_10 p) (V↑1 p))
               (* (((partial 0) F) p) (V↑0 p) (X↑1 p) (G↑0_01 p))
               (* (((partial 0) F) p) (V↑1 p) (X↑1 p) (G↑0_11 p))
               (* (X↑0 p) (V↑0 p) (((partial 1) F) p) (G↑1_00 p))
               (* (X↑0 p) (V↑1 p) (((partial 1) F) p) (G↑1_10 p))
               (* (V↑0 p) (X↑1 p) (((partial 1) F) p) (G↑1_01 p))
               (* (V↑1 p) (X↑1 p) (((partial 1) F) p) (G↑1_11 p))
               (* (((partial 0) F) p) (X↑0 p) (((partial 0) V↑0) p))
               (* (((partial 0) F) p) (X↑1 p) (((partial 1) V↑0) p))
               (* (X↑0 p) (((partial 1) F) p) (((partial 0) V↑1) p))
               (* (X↑1 p) (((partial 1) F) p) (((partial 1) V↑1) p)))
           (v/freeze
            (present
             (((((cov/covariant-derivative CF) X) V)
               (man/literal-manifold-function 'F R2-rect))
              R2-rect-point))))))

  (let-coordinates [[x y]     R2-rect
                    [r theta] R2-polar]
    (let [rect-chi-inverse (point R2-rect)
          m2 (rect-chi-inverse (up 'x0 'y0))
          zero man/zero-manifold-function
          rect-Christoffel (cov/make-Christoffel
                            (down (down (up zero zero)
                                        (up zero zero))
                                  (down (up zero zero)
                                        (up zero zero)))
                            R2-rect-basis)
          polar-Christoffel (cov/make-Christoffel
                             (down (down (up zero zero)
                                         (up zero (/ 1 r)))
                                   (down (up zero (/ 1 r))
                                         (up (* -1 r) zero)))
                             R2-polar-basis)
          rect-Cartan (cov/Christoffel->Cartan rect-Christoffel)
          polar-Cartan (cov/Christoffel->Cartan polar-Christoffel)
          J (- (* x d:dy) (* y d:dx))
          f (man/literal-scalar-field 'f R2-rect)
          ]
      (is (= '(((partial 1) f) (up x0 y0))
             (v/freeze
              (simplify
               (((((cov/covariant-derivative rect-Cartan)
                   d:dx)
                  J)
                 f)
                m2)))))

      (is (= '(((partial 1) f) (up x0 y0))
             (v/freeze
              (simplify
               (((((cov/covariant-derivative polar-Cartan)
                   d:dx)
                  J)
                 f)
                m2)))))

      ;; More generally, can show independence here
      (let [v (vf/literal-vector-field 'v R2-rect)
            w (vf/literal-vector-field 'w R2-rect)]
        (is (zero?
             (simplify
              (((((- (cov/covariant-derivative rect-Cartan)
                     (cov/covariant-derivative polar-Cartan))
                  v)
                 w)
                f)
               m2)))))

      (let [v (vf/literal-vector-field 'v R2-polar)
            w (vf/literal-vector-field 'w R2-polar)]
        (is (zero?
             (simplify
              (((((- (cov/covariant-derivative rect-Cartan)
                     (cov/covariant-derivative polar-Cartan))
                  v)
                 w)
                f)
               m2))))))))

(deftest tests-after-Cartan->Cartan-over-map
  (let-coordinates [[theta phi] S2-spherical
                    t           the-real-line]
    (let [spherical-basis (b/coordinate-system->basis S2-spherical)
          G-S2-1 (cov/make-Christoffel
                  (let [zero man/zero-manifold-function]
                    (down (down (up zero zero)
                                (up zero (/ 1 (tan theta))))
                          (down (up zero (/ 1 (tan theta)))
                                (up (- (* (sin theta) (cos theta))) zero))))
                  spherical-basis)
          gamma:N->M (compose (point S2-spherical)
                                (up (af/literal-function 'alpha)
                                    (af/literal-function 'beta))
                                (chart the-real-line))
          basis-over-gamma (cm/basis->basis-over-map gamma:N->M spherical-basis)
          w (vf/basis-components->vector-field
             (up (compose (af/literal-function 'w0)
                            (chart the-real-line))
                 (compose (af/literal-function 'w1)
                            (chart the-real-line)))
             (b/basis->vector-basis basis-over-gamma))
          sphere-Cartan (cov/Christoffel->Cartan G-S2-1)]
      (is (= '(up (+ (* -1 (sin (alpha tau)) (cos (alpha tau)) ((D beta) tau) (w1 tau))
                     ((D w0) tau))
                  (/ (+ (* (cos (alpha tau)) ((D beta) tau) (w0 tau))
                        (* (cos (alpha tau)) (w1 tau) ((D alpha) tau))
                        (* (sin (alpha tau)) ((D w1) tau)))
                     (sin (alpha tau))))
             (v/freeze
              (simplify
               (s/mapr
                (fn [omega]
                  ((omega
                    (((cov/covariant-derivative sphere-Cartan gamma:N->M)
                      d:dt)
                     w))
                   ((point the-real-line) 'tau)))
                (b/basis->oneform-basis basis-over-gamma))))))

      (testing "geodesic equation"
        (is (= '(up (+ (* -1 (sin (alpha t)) (cos (alpha t)) (expt ((D beta) t) 2))
                       (((expt D 2) alpha) t))
                    (/ (+ (* 2 (cos (alpha t)) ((D beta) t) ((D alpha) t))
                          (* (sin (alpha t)) (((expt D 2) beta) t)))
                       (sin (alpha t))))
               (v/freeze
                (simplify
                 (s/mapr
                  (fn [omega]
                    ((omega
                      (((cov/covariant-derivative sphere-Cartan gamma:N->M)
                        d:dt)
                       ((cm/differential gamma:N->M) d:dt)))
                     ((point the-real-line) 't)))
                  (b/basis->oneform-basis basis-over-gamma))))))))))

(deftest geodesic-equation-tests-a
  (testing "geodesic equations"
    (let-coordinates [[x y] R2-rect
                      t the-real-line]
      (let [CG (cov/make-Christoffel G (b/coordinate-system->basis R2-rect))

            gamma:N->M (compose
                        (point R2-rect)
                        (up (af/literal-function 'alpha)
                            (af/literal-function 'beta))
                        (chart the-real-line))
            basis-over-gamma (cm/basis->basis-over-map
                              gamma:N->M
                              (b/coordinate-system->basis R2-rect))
            u (vf/basis-components->vector-field
               (up (compose (af/literal-function 'u0)
                            (chart the-real-line))
                   (compose (af/literal-function 'u1)
                            (chart the-real-line)))
               (b/basis->vector-basis basis-over-gamma))]
        (is (= '(up (+ (* (G↑0_00 (up (alpha t) (beta t))) ((D alpha) t) (u0 t))
                       (* ((D alpha) t) (G↑0_10 (up (alpha t) (beta t))) (u1 t))
                       (* (u0 t) (G↑0_01 (up (alpha t) (beta t))) ((D beta) t))
                       (* (u1 t) ((D beta) t) (G↑0_11 (up (alpha t) (beta t))))
                       ((D u0) t))
                    (+ (* ((D alpha) t) (u0 t) (G↑1_00 (up (alpha t) (beta t))))
                       (* ((D alpha) t) (u1 t) (G↑1_10 (up (alpha t) (beta t))))
                       (* (u0 t) ((D beta) t) (G↑1_01 (up (alpha t) (beta t))))
                       (* (u1 t) ((D beta) t) (G↑1_11 (up (alpha t) (beta t))))
                       ((D u1) t)))
               (v/freeze
                (simplify
                 (s/mapr
                  (fn [omega]
                    ((omega
                      (((cov/covariant-derivative (cov/Christoffel->Cartan CG) gamma:N->M)
                        d:dt)
                       u))
                     ((point the-real-line) 't)))
                  (b/basis->oneform-basis basis-over-gamma))))))

        (is (= '(up (+ (* (G↑0_00 (up (alpha t) (beta t))) (expt ((D alpha) t) 2))
                       (* ((D alpha) t) (G↑0_10 (up (alpha t) (beta t))) ((D beta) t))
                       (* ((D alpha) t) (G↑0_01 (up (alpha t) (beta t))) ((D beta) t))
                       (* (expt ((D beta) t) 2) (G↑0_11 (up (alpha t) (beta t))))
                       (((expt D 2) alpha) t))
                    (+ (* (expt ((D alpha) t) 2) (G↑1_00 (up (alpha t) (beta t))))
                       (* ((D alpha) t) ((D beta) t) (G↑1_10 (up (alpha t) (beta t))))
                       (* ((D alpha) t) ((D beta) t) (G↑1_01 (up (alpha t) (beta t))))
                       (* (expt ((D beta) t) 2) (G↑1_11 (up (alpha t) (beta t))))
                       (((expt D 2) beta) t)))
               (v/freeze
                (simplify
                 (s/mapr
                  (fn [omega]
                    ((omega
                      (((cov/covariant-derivative (cov/Christoffel->Cartan CG) gamma:N->M)
                        d:dt)
                       ((cm/differential gamma:N->M) d:dt)))
                     ((point the-real-line) 't)))
                  (b/basis->oneform-basis basis-over-gamma)))))))))

  (testing "Geodesic Equations = Lagrange Equations"
    ;; Here I restrict everything to the unit sphere.
    ;; The coordinates on the unit sphere
    (let-coordinates [t           man/R1-rect
                      [theta phi] S2-spherical]
      (let [two-sphere-basis (b/coordinate-system->basis S2-spherical)
            ;; The Christoffel symbols (for r=1) (p.341 MTW) are:
            G-S2-1
            (cov/make-Christoffel
             (let [zero man/zero-manifold-function]
               (down (down (up zero zero)
                           (up zero (/ 1 (tan theta))))
                     (down (up zero (/ 1 (tan theta)))
                           (up (- (* (sin theta) (cos theta))) zero))))
             two-sphere-basis)
            mu:N->M (compose (point S2-spherical)
                             (up (af/literal-function 'mu-theta)
                                 (af/literal-function 'mu-phi))
                             (chart R1-rect))
            Cartan (cov/Christoffel->Cartan G-S2-1)]
        (is (= '(up (+ (* -1
                          (sin (mu-theta tau))
                          (cos (mu-theta tau))
                          (expt ((D mu-phi) tau) 2))
                       (((expt D 2) mu-theta) tau))
                    (/ (+ (* 2 (cos (mu-theta tau))
                             ((D mu-phi) tau)
                             ((D mu-theta) tau))
                          (* (sin (mu-theta tau))
                             (((expt D 2) mu-phi) tau)))
                       (sin (mu-theta tau))))
               (v/freeze
                (simplify
                 (s/mapr
                  (fn [w]
                    ((w
                      (((cov/covariant-derivative Cartan mu:N->M) d:dt)
                       ((cm/differential mu:N->M) d:dt)))
                     ((point R1-rect) 'tau)))
                  (b/basis->oneform-basis
                   (cm/basis->basis-over-map
                    mu:N->M
                    (cov/Cartan->basis Cartan))))))))))
    (let [
          ;; We can get the geodesic equations as ordinary Lagrange
          ;; equations of a free particle constrained to the surface
          ;; of the sphere:
          Lfree (fn [m]
                  (fn [[_ _ v]]
                    (* (/ 1 2) m (g/square v))))
          ;; F is really the embedding map, from the coordinates on the sphere
          ;; to the 3-space coordinates in the embedding manifold.

          ;; This hides the assumption that the R3 manifold is the same one as
          ;; the embedding manifold.

          ;; Comment on an older version from scmutils: Actually (9 June 2009--GJS)
          ;; this no longer works, because R3-rect does not accept an S2-spherical
          ;; point as in the same manifold. Fixed by explicit transfer of a point --
          ;; see manifold.scm
          F (compose
             (chart R3-rect)
             (man/transfer-point S2-spherical R3-rect)
             (point S2-spherical)
             ml/coordinate)
          Lsphere (compose (Lfree 1) (ml/F->C F))]
      ;; Note these are DOWN while the geodesic equations are UP.  This is
      ;; due to the fact that the geodesic equations are raised by the
      ;; metric, which is diagonal, here R=1, and cancels an instance
      ;; of(expt (sin theta) 2).

      ;; Also see p.345 MTW for computing Christoffel symbols from Lagrange
      ;; equations.
      (is (= '(down
               (+ (* -1 (cos (theta t)) (sin (theta t)) (expt ((D phi) t) 2))
                  (((expt D 2) theta) t))
               (+ (* 2 (cos (theta t)) ((D theta) t) (sin (theta t)) ((D phi) t))
                  (* (expt (sin (theta t)) 2) (((expt D 2) phi) t))))
             (v/freeze
              (simplify
               (((ml/Lagrange-equations Lsphere)
                 (up (af/literal-function 'theta)
                     (af/literal-function 'phi)))
                't))))))

    (testing "Exercise on computation of Christoffel symbols."
      (let-coordinates [[x y z]        R3-rect
                        [r theta zeta] man/R3-cyl]
        (let [R3-rect-point ((point R3-rect) (up 'x0 'y0 'z0))
              R3-cyl-point ((point R3-cyl) (up 'r0 'theta0 'z0))
              mpr (chart R3-rect)]
          (is (= '(up 0 0 0)
                 (v/freeze
                  (simplify
                   (((* d:dr d:dr) mpr) R3-rect-point)))))
          ;; So \Gamma↑r_{rr} = 0, \Gamma↑\theta_{rr} = 0
          (is (= '(up (/ (* -1 y0) (sqrt (+ (expt x0 2) (expt y0 2))))
                      (/ x0 (sqrt (+ (expt x0 2) (expt y0 2))))
                      0)
                 (v/freeze
                  (simplify
                   (((* d:dtheta d:dr) mpr) R3-rect-point)))))

          ;; by hand = -sint d:dx + cost d:dy = 1/r d:dtheta
          ;; Indeed.
          (is (= '(up (* -1 (sin theta0)) (cos theta0) 0)
                 (v/freeze
                  (simplify
                   (((* d:dtheta d:dr) mpr) R3-cyl-point)))))

          ;; So \Gamma↑r_{r\theta} = 0, \Gamma↑\theta_{r\theta} = 1/r

          (is  (= '(up (/ (* -1 y0) (sqrt (+ (expt x0 2) (expt y0 2))))
                       (/ x0 (sqrt (+ (expt x0 2) (expt y0 2))))
                       0)
                  (v/freeze
                   (simplify
                    (((* d:dr d:dtheta) mpr) R3-rect-point)))))

          ;; by hand = -sint d:dx + cost d:dy = 1/r d:dtheta
          (is  (= '(up (* -1 (sin theta0)) (cos theta0) 0)
                  (v/freeze
                   (simplify
                    (((* d:dr d:dtheta) mpr) R3-cyl-point)))))

          ;; So \Gammar_{\theta r} = 0, \Gamma\theta_{\theta r} = 1/r
          (is (= '(up (* -1 x0) (* -1 y0) 0)
                 (simplify
                  (((* d:dtheta d:dtheta) mpr) R3-rect-point))))

          ;; by hand = -r cost d:dx - r sint d:dy = -r d:dr

          (is (= '(up (* -1 r0 (cos theta0)) (* -1 r0 (sin theta0)) 0)
                 (v/freeze
                  (simplify
                   (((* d:dtheta d:dtheta) mpr) R3-cyl-point)))))
          ;; So \Gammar_{\theta \theta} = -r, \Gamma\theta_{\theta \theta} = 0
          ;;
          ;; These are correct Christoffel symbols...
          )))))

(defn CD
  "Computation of Covariant derivatives by difference quotient. [[CD]] is parallel
  in definition to the Lie Derivative. Does not seem to depend on a derivative
  of basis vectors, in fact the derivative of the basis vectors is multiplied by
  zero in the product rule output."
  [CF chart]
  (let [chi (man/chart chart)
        chi-inv (man/point chart)
        basis (cov/Cartan->basis CF)
        vector-basis  (b/basis->vector-basis basis)
        oneform-basis (b/basis->oneform-basis basis)]
    (letfn [(Sigma [state] (nth state 0))
            (U [state] (nth state 1))
            (sigma-u [sigma u] (up sigma u))]
      (fn [v]
        ;; ((gamma m) delta) is the point on gamma advanced by delta.
        (letfn [(gamma [m]
                  (fn [delta]
                    (chi-inv (+ (chi m) (* delta ((v chi) m))))))]
          (fn [u]
            (let [u-i (oneform-basis u)]
              (fn [F]
                (fn [m]
                  (let [initial-state (sigma-u (chi m) (u-i m))]
                    (letfn [(bs [state]
                              (let [sigma (Sigma state)
                                    m_0   (chi-inv sigma)]
                                (up ((v chi) m_0)
                                    (* -1
                                       (((cov/Cartan->forms CF) v) m_0)
                                       (u-i m_0)))))
                            (vs [fs]
                              (* (D fs) bs))

                            ;; First-order approximation to A
                            (Au [delta]
                              (+ (U initial-state)
                                 (* delta ((vs U) initial-state))))

                            (g [delta]
                              (let [advanced-m ((gamma m) delta)]
                                (* (- (u-i advanced-m) (Au delta))
                                   ((vector-basis F) advanced-m))))]
                      ((D g) 0))))))))))))

(deftest geodesic-equation-tests-b
  (let [X (vf/literal-vector-field 'X R2-rect)
        Y (vf/literal-vector-field 'Y R2-rect)
        F (man/literal-manifold-function 'F R2-rect)
        q_0 (up 'q_x 'q_y)
        m_0 ((point R2-rect) q_0)
        CF-rect (cov/Christoffel->Cartan
                 (cov/make-Christoffel G (b/coordinate-system->basis R2-rect)))
        CF-polar (cov/Christoffel->Cartan
                  (cov/make-Christoffel G (b/coordinate-system->basis R2-polar)))]
    (is (zero?
         (simplify
          (- (((((CD CF-rect R2-rect) X) Y) F) m_0)
             (((((cov/covariant-derivative CF-rect) X) Y) F) m_0)))))

    (is (zero?
         (simplify
          (- (((((CD CF-polar R2-rect) X) Y) F) m_0)
             (((((cov/covariant-derivative CF-polar) X) Y) F) m_0)))))

    (is (zero?
         (simplify
          (- (((((CD CF-rect R2-polar) X) Y) F) m_0)
             (((((cov/covariant-derivative CF-rect) X) Y) F) m_0)))))

    ;; TODO: Too slow... it works if we bump the timeout, but this is not fast.
    #_
    (binding [pg/*poly-gcd-time-limit* [5 :seconds]]
      (is (zero?
           (simplify
            (- (((((CD CF-polar R2-polar) X) Y) F) m_0)
               (((((cov/covariant-derivative CF-polar) X) Y) F) m_0))))))

    (testing "Testing on forms."
      (let [omega (ff/literal-oneform-field 'omega R2-rect)
            tau (ff/literal-oneform-field 'tau R2-rect)
            Z (vf/literal-vector-field 'Z R2-rect)
            D_x-rect ((cov/covariant-derivative CF-rect) X)
            D_x-polar ((cov/covariant-derivative CF-polar) X)]
        (is (zero?
             (simplify
              (- (+ (((D_x-rect omega) Y) m_0)
                    ((omega (D_x-rect Y)) m_0))
                 ((D_x-rect (omega Y)) m_0)))))

        (is (zero?
             (simplify
              (- (((D_x-rect (ff/wedge omega tau)) Y Z) m_0)
                 (+ (((ff/wedge omega (D_x-rect tau)) Y Z) m_0)
                    (((ff/wedge (D_x-rect omega) tau) Y Z) m_0))))))

        (testing "TODO: investigate if there is some setting that will prevent
        GCD from running without me having to add this manually. The test passes
        without the super slow GCD."
          (binding [pg/*poly-gcd-time-limit* [0 :seconds]]
            (is (zero?
                 (simplify
                  (- (((D_x-polar (ff/wedge omega tau)) Y Z) m_0)
                     (+ (((ff/wedge omega (D_x-polar tau)) Y Z) m_0)
                        (((ff/wedge (D_x-polar omega) tau) Y Z) m_0)))))))))))

  ;; Next, tests, for the actual functions:
  (is (= '(up
           (+ (* (G_00↑0 (up (gamma↑0 t) (gamma↑1 t))) (expt ((D gamma↑0) t) 2))
              (* ((D gamma↑0) t) (G_10↑0 (up (gamma↑0 t) (gamma↑1 t))) ((D gamma↑1) t))
              (* ((D gamma↑0) t) ((D gamma↑1) t) (G_01↑0 (up (gamma↑0 t) (gamma↑1 t))))
              (* (expt ((D gamma↑1) t) 2) (G_11↑0 (up (gamma↑0 t) (gamma↑1 t))))
              (((expt D 2) gamma↑0) t))
           (+ (* (expt ((D gamma↑0) t) 2) (G_00↑1 (up (gamma↑0 t) (gamma↑1 t))))
              (* ((D gamma↑0) t) ((D gamma↑1) t) (G_10↑1 (up (gamma↑0 t) (gamma↑1 t))))
              (* ((D gamma↑0) t) ((D gamma↑1) t) (G_01↑1 (up (gamma↑0 t) (gamma↑1 t))))
              (* (expt ((D gamma↑1) t) 2) (G_11↑1 (up (gamma↑0 t) (gamma↑1 t))))
              (((expt D 2) gamma↑1) t)))
         (v/freeze
          (simplify
           (((cov/geodesic-equation the-real-line R2-rect (conn/literal-Cartan 'G R2-rect))
             (cm/literal-manifold-map 'gamma the-real-line R2-rect))
            ((point the-real-line) 't))))))

  (let [C (conn/literal-Cartan 'G R2-rect)]
    (is (= '(up 0 0)
           (v/freeze
            (simplify
             (- (((cov/geodesic-equation the-real-line R2-rect C)
                  (cm/literal-manifold-map 'gamma the-real-line R2-rect))
                 ((point the-real-line) 't))
                (((cov/geodesic-equation the-real-line R2-rect (cov/symmetrize-Cartan C))
                  (cm/literal-manifold-map 'gamma the-real-line R2-rect))
                 ((point the-real-line) 't)))))))))

(deftest parallel-transport-tests
  (let-coordinates [[theta phi] S2-spherical]
    (let [S2-basis (b/coordinate-system->basis S2-spherical)
          G-S2-1 (cov/make-Christoffel
                  (let [zero man/zero-manifold-function]
                    (down (down (up zero zero)
                                (up zero (/ 1 (tan theta))))
                          (down (up zero (/ 1 (tan theta)))
                                (up (- (* (sin theta)
                                          (cos theta)))
                                    zero))))
                  S2-basis)
          gamma (compose (point S2-spherical)
                         (up (af/literal-function 'alpha)
                             (af/literal-function 'beta))
                         (chart the-real-line))
          basis-over-gamma (cm/basis->basis-over-map gamma S2-basis)
          u (vf/basis-components->vector-field
             (up (compose (af/literal-function 'u↑0)
                          (chart the-real-line))
                 (compose (af/literal-function 'u↑1)
                          (chart the-real-line)))
             (b/basis->vector-basis basis-over-gamma))
          sphere-Cartan (cov/Christoffel->Cartan G-S2-1)]

      (is (= '(up
               (+ (* -1 (sin (alpha t)) (cos (alpha t)) ((D beta) t) (u↑1 t)) ((D u↑0) t))
               (/ (+ (* (cos (alpha t)) ((D beta) t) (u↑0 t))
                     (* (cos (alpha t)) (u↑1 t) ((D alpha) t))
                     (* (sin (alpha t)) ((D u↑1) t)))
                  (sin (alpha t))))
             (v/freeze
              (simplify
               ((((cov/parallel-transport-equation
                   the-real-line S2-spherical sphere-Cartan)
                  gamma)
                 u)
                ((point the-real-line) 't)))))))))
