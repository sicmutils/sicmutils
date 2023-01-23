#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.calculus.vector-field-test
  (:refer-clojure :exclude [+ - * / partial])
  (:require [clojure.test :refer [is deftest testing use-fixtures]]
            [same :refer [ish?]]
            [emmy.abstract.function :as af]
            [emmy.calculus.basis :as b]
            [emmy.calculus.coordinate :refer [let-coordinates]]
            [emmy.calculus.manifold :as m :refer [R2-rect R3-cyl R3-rect]]
            [emmy.calculus.vector-field :as vf]
            [emmy.generic :as g :refer [cos sin + - * /]]
            [emmy.operator :as o]
            [emmy.simplify :refer [hermetic-simplify-fixture]]
            [emmy.structure :refer [up down]]
            [emmy.value :as v]))

(use-fixtures :each hermetic-simplify-fixture)

(def simplify
  (comp v/freeze g/simplify))

(deftest vector-field-tests
  (testing "v/zero-like, v/one-like, v/identity-like"
    (let [vf (vf/literal-vector-field 'b R2-rect)]
      (is (v/zero? (v/zero-like vf)))
      (is (vf/vector-field? (v/zero-like vf)))
      (is (= 'vf:zero (v/freeze
                       (v/zero-like vf))))

      (testing "the returned identity keeps its context and `::vf/vector-field`
       status."
        (is (vf/vector-field? (v/one-like vf)))
        (is (vf/vector-field? (v/identity-like vf))))))

  (testing "with-coordinate-prototype"
    (let [A R2-rect
          B (m/with-coordinate-prototype R2-rect '[X Y])]
      (is (= '[d:dx0 d:dx1] (map o/name (vf/coordinate-system->vector-basis A))))
      (is (= '[d:dX d:dY] (map o/name (vf/coordinate-system->vector-basis B))))))

  (testing "literal"
    (let [f (m/literal-manifold-function 'f-rect R2-rect)
          v (vf/literal-vector-field 'b R2-rect)
          R2-rect-chi-inverse (m/point R2-rect)
          p (R2-rect-chi-inverse (up 'x0 'y0))]
      (is (= '(+ (* (((partial 0) f-rect) (up x0 y0)) (b↑0 (up x0 y0)))
                 (* (((partial 1) f-rect) (up x0 y0)) (b↑1 (up x0 y0))))
             (simplify ((v f) p))))
      (is (= '(up (b↑0 (up x0 y0)) (b↑1 (up x0 y0)))
             (simplify ((v (m/chart R2-rect)) p))))
      (is (= ::vf/vector-field (v/kind v)))))

  (testing "vf->components"
    (let-coordinates [[x y] R2-rect]
      (let [circular (- (* x d:dy) (* y d:dx))
            f (vf/literal-vector-field 'f R2-rect)]
        (is (vf/vector-field? circular))
        (is (vf/vector-field? f))

        (is (= '(up (- y0) x0)
               (v/freeze
                ((vf/vector-field->components circular R2-rect)
                 (up 'x0 'y0)))))

        (is (= '(up (f↑0 (up x0 y0))
                    (f↑1 (up x0 y0)))
               (v/freeze
                ((vf/vector-field->components f R2-rect)
                 (up 'x0 'y0))))))))

  (testing "coordinate-basis-vector-field"
    (let-coordinates [[x y] R2-rect]
      (let [f (vf/coordinate-basis-vector-field R2-rect 'f)
            fx (vf/coordinate-basis-vector-field R2-rect 'fx 0)
            fy (vf/coordinate-basis-vector-field R2-rect 'fy 1)]
        (testing "every returned fn is a vector field."
          (is (vf/vector-field? f))
          (is (vf/vector-field? fx))
          (is (vf/vector-field? fy)))

        (is (= (down 'y0 'x0)
               ((f (* x y))
                ((m/point R2-rect)
                 (up 'x0 'y0))))
            "Given a manifold function, the vector field returns a structure of
            partial derivatives for each component of the coordinate system.")

        (is (= ((vf/vector-field->components f R2-rect)
                (up 'x0 'y0))
               ((down (vf/vector-field->components fx R2-rect)
                      (vf/vector-field->components fy R2-rect))
                (up 'x0 'y0)))
            "Passing no indices generates the full Jacobian, ie, a down built
            out of the partials."))))

  (testing "vf->components, 1D"
    (let-coordinates [t m/R1-rect]
      (let [f (vf/literal-vector-field 'f R1-rect)]
        (is (= (up '(f↑0 t))
               ((vf/vector-field->components f R1-rect) 't))
            "a literal vector field with a 1d input takes a non-structural
            argument."))))

  (testing "coordinate-system->vector-basis and friends"
    (let-coordinates [[x y] R2-rect]
      (let [vb       (vf/coordinate-system->vector-basis R2-rect)
            bvf      (vf/coordinate-basis-vector-field R2-rect 'f)
            R2-point ((m/point R2-rect) (up 'x0 'y0))
            f (* x y)]
        (is (= (down 'y0 'x0)
               ((vb f) R2-point)
               ((bvf f) R2-point))
            "multiple ways to generate a vector basis with respect to some
            coordinate system.")

        (is (= '(+ (* x d:dx)
                   (* y d:dy))
               (v/freeze
                (vf/basis-components->vector-field (up x y) vb))))

        (let [vf (vf/basis-components->vector-field (up x y) vb)
              dual (b/vector-basis->dual vb R2-rect)]
          (is (= ((up x y) R2-point)
                 ((vf/vector-field->basis-components vf dual)
                  R2-point))
              "vector-field->basis-components completes the round-trip of basis
        components through basis-components->vector-field")))))

  (testing "coordinatize"
    (is (= '(+ (* (((partial 0) f) (up x0 y0 z0)) (v↑0 (up x0 y0 z0)))
               (* (((partial 1) f) (up x0 y0 z0)) (v↑1 (up x0 y0 z0)))
               (* (((partial 2) f) (up x0 y0 z0)) (v↑2 (up x0 y0 z0))))
           (simplify
            (((vf/coordinatize (vf/literal-vector-field 'v R3-rect) R3-rect)
              (af/literal-function 'f (up 1 2 3) 1))
             (up 'x0 'y0 'z0)))))

    (let-coordinates [[x y] R3-rect]
      (let [circular (- (* x d:dy)
                        (* y d:dx))]
        (is (= '((up 1 0 0)
                 (up 0 a 0)
                 (up (* (/ -1 2) (expt a 2)) 0 0)
                 (up 0 (* (/ -1 6) (expt a 3)) 0)
                 (up (* (/ 1 24) (expt a 4)) 0 0)
                 (up 0 (* (/ 1 120) (expt a 5)) 0))
               (simplify
                (take 6 (((g/exp (vf/coordinatize (* 'a circular) R3-rect))
                          identity)
                         (up 1 0 0)))))
            "The coordinate version of the `circular` vector field can be
              exponentiated.")))

    (testing "exponential, evolution test"
      (let-coordinates [[x y] R2-rect]
        (let [circular (- (* x d:dy) (* y d:dx))]
          (is (= '(up (+ (* (/ -1 720) (expt a 6))
                         (* (/ 1 24) (expt a 4))
                         (* (/ -1 2) (expt a 2))
                         1)
                      (+ (* (/ 1 120) (expt a 5))
                         (* (/ -1 6) (expt a 3))
                         a))
                 (simplify
                  ((((vf/evolution 6) 'a circular) (m/chart R2-rect))
                   ((m/point R2-rect) (up 1 0)))))
              "We can use the coordinatized vector field to build an evolution
              along an integral curve.")))))

  (testing "gjs-examples, ported from vector-fields.scm"
    (let-coordinates [[x y z] R3-rect]
      (is (= '(+ (* -1 a b (cos a) (cos b)) (* -2 a (cos a) (sin b)))
             (simplify
              (((* (g/expt d:dy 2) x y d:dx) (* (sin x) (cos y)))
               ((m/point R3-rect) (up 'a 'b 'c))))))

      (let [counter-clockwise (- (* x d:dy) (* y d:dx))
            outward (+ (* x d:dx) (* y d:dy))
            mr ((m/point R3-rect) (up 'x0 'y0 'z0))]
        (is (= 0 (simplify ((counter-clockwise
                             (g/sqrt (+ (g/square x)
                                        (g/square y)))) mr))))
        (is (= '(+ (expt x0 2) (* -1 (expt y0 2)))
               (simplify ((counter-clockwise (* x y)) mr))))
        (is (= '(* 2 x0 y0) (simplify ((outward (* x y)) mr))))))

    (testing "From McQuistan: Scalar and Vector Fields, pp. 103-106"
      (let-coordinates [[r theta zeta] R3-cyl
                        [x y z] R3-rect]
        (let [A (+ (* 'A_r d:dr)
                   (* 'A_theta d:dtheta)
                   (* 'A_z d:dzeta))
              p ((m/point R3-rect) (up 'x 'y 'z))]
          ;; TODO: simplification isn't all it could be here (?)
          (is (= '(up (/ (+ (* -1N A_theta y (sqrt (+ (expt x 2) (expt y 2))))
                            (* A_r x))
                         (sqrt (+ (expt x 2) (expt y 2))))
                      (/ (+ (* A_theta x (sqrt (+ (expt x 2) (expt y 2))))
                            (* A_r y))
                         (sqrt (+ (expt x 2) (expt y 2))))
                      A_z)
                 (simplify
                  ((vf/vector-field->components A R3-rect) (up 'x 'y 'z)))))


          ;; This disagrees with McQuistan.  Explanation follows.

          (is (= '(up (* -1 y) x 0)
                 (simplify
                  ((d:dtheta (up x y z)) p))))
          ;; has length (sqrt (+ (expt x 2) (expt y 2)))

          (is (= '(up (/ x (sqrt (+ (expt x 2) (expt y 2))))
                      (/ y (sqrt (+ (expt x 2) (expt y 2))))
                      0)
                 (simplify
                  ((d:dr (up x y z)) p))))
          ;; has length 1

          (is (= '(up 0 0 1)
                 (simplify
                  ((d:dzeta (up x y z)) p))))

          (is (= '(up 0 0 1)
                 (simplify
                  ((d:dz (up x y z)) p))))

          ;; "so introduce..."
          (let [e-theta (* (/ 1 r) d:dtheta)
                e-r d:dr
                e-z d:dzeta
                A (+ (* 'A_r e-r)
                     (* 'A_theta e-theta)
                     (* 'A_z e-z))]
            (is (= '(up (/ (+ (* A_r x) (* -1 A_theta y))
                           (sqrt (+ (expt x 2) (expt y 2))))
                        (/ (+ (* A_r y) (* A_theta x))
                           (sqrt (+ (expt x 2) (expt y 2))))
                        A_z)
                   (simplify
                    ((vf/vector-field->components A R3-rect) (up 'x 'y 'z))))
                "Now agrees with McQuistan.")))

        (is (= (up 0 1 0)
               ((vf/vector-field->components d:dy R3-rect)
                (up 'x0 'y0 'z0))))

        (is (= (up 0 1 0)
               ((vf/vector-field->components d:dy R3-rect)
                (up 'r0 'theta0 'z0))))

        (is (ish? (up 1 0 0)
                  ((vf/vector-field->components d:dy R3-cyl)
                   (up 1 (/ Math/PI 2) 0))))

        (is (= (up 0 1 0)
               ((vf/vector-field->components d:dy R3-cyl)
                (up 1 0 0))))

        (is (= '(up (sin theta0) (/ (cos theta0) r0)0)
               (simplify
                ((vf/vector-field->components d:dy R3-cyl)
                 (up 'r0 'theta0 'z)))))))))
