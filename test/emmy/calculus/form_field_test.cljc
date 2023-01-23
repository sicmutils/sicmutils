#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.calculus.form-field-test
  (:refer-clojure :exclude [+ - * /])
  (:require [clojure.test :refer [is deftest testing use-fixtures]]
            [emmy.abstract.function :as af]
            [emmy.calculus.coordinate :as c :refer [let-coordinates]]
            [emmy.calculus.form-field :as ff]
            [emmy.calculus.manifold :as m :refer [R2-rect R2-polar]]
            [emmy.calculus.vector-field :as vf]
            [emmy.expression :as x]
            [emmy.generic :as g :refer [+ - *]]
            [emmy.simplify :refer [hermetic-simplify-fixture]]
            [emmy.structure :refer [up down]]
            [emmy.value :as v]))

(use-fixtures :each hermetic-simplify-fixture)

(def simplify
  (comp v/freeze g/simplify))

(deftest manifold-tests
  (testing "first block of test material from manifold.scm"
    (let-coordinates [[x y]     R2-rect
                      [r theta] R2-polar]
      (let [mr ((m/point R2-rect) (up 'x0 'y0))
            mp ((m/point R2-polar) (up 'r0 'theta0))
            circular (- (* x d:dy) (* y d:dx))
            g-polar (fn [u v]
                      (+ (* (dr u) (dr v))
                         (* (* r (dtheta u)) (* r (dtheta v)))))
            g-rect (fn [u v]
                     (+ (* (dx u) (dx v))
                        (* (dy u) (dy v))))
            residual (- g-polar g-rect)
            vp (vf/literal-vector-field 'v R2-polar)
            vr (vf/literal-vector-field 'v R2-rect)]
        (is (= '(+ (* 3 x0) (* -2 y0))
               (simplify
                ((circular (+ (* 2 x) (* 3 y))) mr))))

        (is (= 1 (simplify ((circular theta) mr))))
        (is (= 0 (simplify ((dr circular) mr))))
        (is (= 1 (((ff/d r) d:dr) mr)))
        (is (= 1 ((dr d:dr) mr)))

        (is (= '(v↑0 (up (sqrt (+ (expt x0 2) (expt y0 2))) (atan y0 x0)))
               (simplify
                ((dr vp) mr))))

        (is (= '(/ (+ (* x0 (v↑0 (up x0 y0))) (* y0 (v↑1 (up x0 y0))))
                   (sqrt (+ (expt x0 2) (expt y0 2))))
               (simplify
                ((dr vr) mr))))

        (is (= '(v↑0 (up (sqrt (+ (expt x0 2) (expt y0 2))) (atan y0 x0)))
               (simplify
                (((ff/d r) vp) mr))))

        (is (= '(/ (+ (* x0 (v↑0 (up x0 y0))) (* y0 (v↑1 (up x0 y0))))
                   (sqrt (+ (expt x0 2) (expt y0 2))))
               (simplify
                (((ff/d r) vr) mr))))

        (is (= '(/ (+ (* x0 (v↑0 (up x0 y0))) (* y0 (v↑1 (up x0 y0))))
                   (sqrt (+ (expt x0 2) (expt y0 2))))
               (simplify (((ff/d r) vr) mr))))

        (is (= 0 (simplify ((residual vr vr) mr))))
        (is (= 0 (simplify ((residual vp vp) mr))))
        (is (= 0 (simplify ((residual vp vp) mp))))
        (is (= 0 (simplify ((residual vr vr) mp))))))))

(deftest form-field-tests
  (testing "v/zero-like"
    (let [oneform-field (ff/literal-oneform-field 'b R2-rect)]
      (is (v/zero? (v/zero-like oneform-field)))
      (is (ff/form-field?
           (v/zero-like oneform-field)))
      (is (= 'ff:zero (v/freeze
                       (v/zero-like oneform-field))))))

  (testing "oneform-field->components"
    (let-coordinates [[x y] R2-rect]
      (let [f (ff/literal-oneform-field 'f R2-rect)]
        (is (ff/oneform-field? f))
        (is (ff/form-field? f))
        (is (ff/nform-field? f 1))

        (is (= '(down (f_0 (up x0 y0))
                      (f_1 (up x0 y0)))
               (v/freeze
                ((ff/oneform-field->components f R2-rect)
                 (up 'x0 'y0))))
            "retrieve the components"))))

  (testing "to and from oneform-field bases"
    (let-coordinates [[x y] R2-rect]
      (let [ofb (ff/coordinate-system->oneform-basis R2-rect)
            off (ff/coordinate-basis-oneform-field R2-rect 'f)
            vf  (vf/literal-vector-field 'vf R2-rect)
            R2-point ((m/point R2-rect) (up 'x0 'y0))
            components (down d:dx d:dy)
            f (* x y)]
        (is (= ((off vf) R2-point)
               ((ofb vf) R2-point))
            "two ways to create a oneform-basis.")

        (is (= '(+ (* x0 y0 (((partial 0) vf↑0) (up x0 y0)))
                   (* x0 y0 (((partial 1) vf↑1) (up x0 y0)))
                   (* x0 (vf↑1 (up x0 y0)))
                   (* y0 (vf↑0 (up x0 y0))))
               (simplify
                ((((ff/basis-components->oneform-field components ofb)
                   (vf/literal-vector-field 'vf R2-rect))
                  f)
                 R2-point)))))))

  (testing "coordinate-system->oneform-basis and friends"
    (let-coordinates [[x y] R2-rect]
      (let [vb  (vf/coordinate-system->vector-basis R2-rect)
            ofb (ff/coordinate-system->oneform-basis R2-rect)
            R2-point ((m/point R2-rect) (up 'x0 'y0))
            components (down d:dx d:dy)
            rt-components (-> components
                              (ff/basis-components->oneform-field ofb)
                              (ff/oneform-field->basis-components vb))
            f (* x y)]
        (is (= (down 'y0 'x0)
               ((components f) R2-point))
            "confirming that components take directional derivatives. ")

        (is (= ((components f) R2-point)
               ((rt-components f) R2-point))
            "oneform-field->basis-components completes the round-trip of basis
            components through basis-components->oneform-field"))))

  (testing "tests from form-fields.scm"
    (let-coordinates [[x y z]        m/R3-rect
                      [r theta zeta] m/R3-cyl]
      (let [mr ((m/point R3-rect) (up 'x0 'y0 'z0))
            a-oneform      (ff/components->oneform-field
                            (down (af/literal-function 'ax '(-> (UP* Real 3) Real))
                                  (af/literal-function 'ay '(-> (UP* Real 3) Real))
                                  (af/literal-function 'az '(-> (UP* Real 3) Real)))
                            R3-rect)
            a-vector-field (vf/components->vector-field
                            (up (af/literal-function 'vx '(-> (UP* Real 3) Real))
                                (af/literal-function 'vy '(-> (UP* Real 3) Real))
                                (af/literal-function 'vz '(-> (UP* Real 3) Real)))
                            R3-rect)]
        (is (= '(+ (* (ax (up x0 y0 z0)) (vx (up x0 y0 z0)))
                   (* (ay (up x0 y0 z0)) (vy (up x0 y0 z0)))
                   (* (az (up x0 y0 z0)) (vz (up x0 y0 z0))))
               (simplify
                ((a-oneform a-vector-field) mr))))

        (is (= '(down (ax (up x0 y0 z0))
                      (ay (up x0 y0 z0))
                      (az (up x0 y0 z0)))
               (simplify
                ((ff/oneform-field->components a-oneform R3-rect)
                 (up 'x0 'y0 'z0)))))

        (is (= '(down (+ (* (cos theta0)
                            (ax (up (* r0 (cos theta0))
                                    (* r0 (sin theta0))
                                    z0)))
                         (* (sin theta0)
                            (ay (up (* r0 (cos theta0))
                                    (* r0 (sin theta0))
                                    z0))))
                      (+ (* r0 (cos theta0)
                            (ay (up (* r0 (cos theta0))
                                    (* r0 (sin theta0))
                                    z0)))
                         (* -1 r0 (sin theta0)
                            (ax (up (* r0 (cos theta0))
                                    (* r0 (sin theta0))
                                    z0))))
                      (az (up (* r0 (cos theta0))
                              (* r0 (sin theta0))
                              z0)))
               (simplify
                ((ff/oneform-field->components a-oneform R3-cyl)
                 (up 'r0 'theta0 'z0))))))

      (let [mr ((m/point R3-rect) (up 'x0 'y0 'z0))
            mp ((m/point R3-cyl) (up 'r0 'theta0 'z0))]
        (is (= 1 ((dx d:dx) mr)))
        (is (= 1 ((dx d:dx) mp)))

        (is (= '(down (/ x0 (sqrt (+ (expt x0 2) (expt y0 2))))
                      (/ y0 (sqrt (+ (expt x0 2) (expt y0 2))))
                      0)
               (simplify
                ((ff/oneform-field->components dr R3-rect)
                 (up 'x0 'y0 'z0)))))

        (is (= '(down (/ (* -1 y0) (+ (expt x0 2) (expt y0 2)))
                      (/ x0 (+ (expt x0 2) (expt y0 2)))
                      0)
               (simplify
                ((ff/oneform-field->components dtheta R3-rect)
                 (up 'x0 'y0 'z0)))))
        (is (= '(/ (+ (* V↑0 r0 w_0 (cos theta0))
                      (* V↑1 r0 w_0 (sin theta0))
                      (* -1 V↑0 w_1 (sin theta0))
                      (* V↑1 w_1 (cos theta0)))
                   r0)
               (simplify
                (((+ (* 'w_0 dr) (* 'w_1 dtheta))
                  (+ (* 'V↑0 d:dx) (* 'V↑1 d:dy)))
                 mp))))

        (is (= '(/ (+ (* V↑0 r0 w_0 (cos theta0))
                      (* V↑1 r0 w_0 (sin theta0))
                      (* -1 V↑0 w_1 (sin theta0))
                      (* V↑1 w_1 (cos theta0)))
                   r0)
               (simplify
                (((-> (+ (* 'w_0 dr) (* 'w_1 dtheta))
                      (ff/oneform-field->components R3-rect)
                      (ff/components->oneform-field R3-rect))
                  (+ (* 'V↑0 d:dx) (* 'V↑1 d:dy)))
                 mp))))

        (let [counter-clockwise (- (* x d:dy) (* y d:dx))
              outward (+ (* x d:dx) (* y d:dy))]
          (is (= (- 'y0)
                 ((dx counter-clockwise) mr)))

          (is (= 'x0 ((dx outward) mr)))

          (is (zero?
               (simplify
                ((dr counter-clockwise) mp))))

          (is (= 'r0 (simplify
                      ((dr outward) mp))))

          (is (= '(sqrt (+ (expt x0 2) (expt y0 2)))
                 (simplify
                  ((dr outward) mr))))

          (is (= '(* v x0)
                 (simplify
                  (((* x dy) (+ (* 'u d:dx) (* 'v d:dy)))
                   mr))))))

      (is (= 1 ((dr d:dr)
                ((m/point R3-rect) (up 'x↑0 'y↑0 'z↑0)))))

      (is (= 0 ((dr d:dtheta)
                ((m/point R3-rect) (up 'x↑0 'y↑0 'z↑0)))))

      (is (= 0 ((dtheta d:dr)
                ((m/point R3-rect) (up 'x↑0 'y↑0 'z↑0)))))

      (is (= 1 ((dtheta d:dtheta)
                ((m/point R3-rect) (up 'x↑0 'y↑0 'z↑0))))))))

(deftest wedge-tests
  (let-coordinates [[x y z] m/R3-rect]
    (let [R3-point ((m/point R3-rect) (up 'x0 'y0 'z0))

          w (ff/literal-oneform-field 'w R3-rect)
          u (ff/literal-oneform-field 'u R3-rect)
          v (ff/literal-oneform-field 'v R3-rect)

          X (vf/literal-vector-field 'X R3-rect)
          Y (vf/literal-vector-field 'Y R3-rect)
          Z (vf/literal-vector-field 'Z R3-rect)]

      (testing "wedge tests"
        (is (= '(+ (* (w_0 (up x0 y0 z0)) (X↑0 (up x0 y0 z0)))
                   (* (w_1 (up x0 y0 z0)) (X↑1 (up x0 y0 z0)))
                   (* (w_2 (up x0 y0 z0)) (X↑2 (up x0 y0 z0))))
               (v/freeze ((w X) R3-point))))

        ;; A few theorems

        (is (= 0 (simplify
                  (((- (ff/wedge (ff/wedge w u) v)
                       (ff/wedge w (ff/wedge u v))) X Y Z)
                   R3-point))))

        (is (= 0 (simplify
                  (((- (ff/wedge (+ w u) v)
                       (+ (ff/wedge w v) (ff/wedge u v)))
                    X Y)
                   R3-point))))

        (is (= 0 (simplify
                  (((- (ff/wedge u v) (* u v)) X Y)
                   R3-point)))
            "a product of forms is their wedge!")

        (is (= 0 (simplify
                  (((- (ff/wedge u v)
                       (ff/alt-wedge u v)) X Y)
                   R3-point)))
            "alt-wedge matches wedge")

        (let [dx:dy (ff/wedge dx dy)]
          (is (= 1 ((dx:dy d:dx d:dy) R3-point)))
          (is (= 0 ((dx:dy d:dx d:dx) R3-point)))
          (is (= -1 ((dx:dy d:dy d:dx) R3-point))))))))

(deftest exterior-derivative-tests
  (testing "tests from exterior-derivative.scm"
    (let-coordinates [[x y z]        m/R3-rect
                      [r theta zeta] m/R3-cyl]
      (let [R3-rect-point ((m/point R3-rect) (up 'x0 'y0 'z0))
            R3-cyl-point  ((m/point R3-cyl) (up 'r0 'theta0 'zeta0))

            w (ff/literal-oneform-field 'w R3-rect)

            X (vf/literal-vector-field 'X R3-rect)
            Y (vf/literal-vector-field 'Y R3-rect)
            Z (vf/literal-vector-field 'Z R3-rect)
            W (vf/literal-vector-field 'W R3-rect)]

        (is (= '(+ (* (((partial 0) f) (up x0 y0 z0)) (X↑0 (up x0 y0 z0)))
                   (* (((partial 1) f) (up x0 y0 z0)) (X↑1 (up x0 y0 z0)))
                   (* (((partial 2) f) (up x0 y0 z0)) (X↑2 (up x0 y0 z0))))
               (simplify
                (((ff/d (m/literal-scalar-field 'f R3-rect)) X)
                 R3-rect-point))))

        (is (= 0 (simplify
                  ((((g/square ff/d) (m/literal-scalar-field 'f R3-rect)) X Y)
                   R3-cyl-point))))

        (is (= '(+ (* (X↑0 p) (Y↑1 p) (((partial 0) w_1) p))
                   (* -1 (X↑0 p) (Y↑1 p) (((partial 1) w_0) p))
                   (* (X↑0 p) (Y↑2 p) (((partial 0) w_2) p))
                   (* -1 (X↑0 p) (Y↑2 p) (((partial 2) w_0) p))
                   (* -1 (X↑1 p) (((partial 0) w_1) p) (Y↑0 p))
                   (* (X↑1 p) (((partial 1) w_0) p) (Y↑0 p))
                   (* (X↑1 p) (Y↑2 p) (((partial 1) w_2) p))
                   (* -1 (X↑1 p) (Y↑2 p) (((partial 2) w_1) p))
                   (* -1 (X↑2 p) (Y↑1 p) (((partial 1) w_2) p))
                   (* (X↑2 p) (Y↑1 p) (((partial 2) w_1) p))
                   (* -1 (X↑2 p) (((partial 0) w_2) p) (Y↑0 p))
                   (* (X↑2 p) (((partial 2) w_0) p) (Y↑0 p)))
               (-> (((ff/d w) X Y) R3-rect-point)
                   (simplify)
                   (x/substitute '(up x0 y0 z0) 'p))))

        (let [omega (+ (* (m/literal-scalar-field 'omega_0 R3-rect)
	                        (ff/wedge dx dy))
                       (* (m/literal-scalar-field 'omega_1 R3-rect)
	                        (ff/wedge dy dz))
                       (* (m/literal-scalar-field 'omega_2 R3-rect)
	                        (ff/wedge dz dx)))]
          (is (= '(+ (* (X↑0 p) (Y↑1 p) (Z↑2 p) (((partial 0) omega_1) p))
                     (* (X↑0 p) (Y↑1 p) (Z↑2 p) (((partial 1) omega_2) p))
                     (* (X↑0 p) (Y↑1 p) (Z↑2 p) (((partial 2) omega_0) p))
                     (* -1 (X↑0 p) (Y↑2 p) (((partial 0) omega_1) p) (Z↑1 p))
                     (* -1 (X↑0 p) (Y↑2 p) (((partial 1) omega_2) p) (Z↑1 p))
                     (* -1 (X↑0 p) (Y↑2 p) (((partial 2) omega_0) p) (Z↑1 p))
                     (* (X↑1 p) (Y↑2 p) (((partial 0) omega_1) p) (Z↑0 p))
                     (* (X↑1 p) (Y↑2 p) (((partial 1) omega_2) p) (Z↑0 p))
                     (* (X↑1 p) (Y↑2 p) (((partial 2) omega_0) p) (Z↑0 p))
                     (* -1 (X↑1 p) (Y↑0 p) (Z↑2 p) (((partial 0) omega_1) p))
                     (* -1 (X↑1 p) (Y↑0 p) (Z↑2 p) (((partial 1) omega_2) p))
                     (* -1 (X↑1 p) (Y↑0 p) (Z↑2 p) (((partial 2) omega_0) p))
                     (* -1 (X↑2 p) (Y↑1 p) (((partial 0) omega_1) p) (Z↑0 p))
                     (* -1 (X↑2 p) (Y↑1 p) (((partial 1) omega_2) p) (Z↑0 p))
                     (* -1 (X↑2 p) (Y↑1 p) (((partial 2) omega_0) p) (Z↑0 p))
                     (* (X↑2 p) (Y↑0 p) (((partial 0) omega_1) p) (Z↑1 p))
                     (* (X↑2 p) (Y↑0 p) (((partial 1) omega_2) p) (Z↑1 p))
                     (* (X↑2 p) (Y↑0 p) (((partial 2) omega_0) p) (Z↑1 p)))
                 (-> (((ff/d omega) X Y Z) R3-rect-point)
                     (simplify)
                     (x/substitute '(up x0 y0 z0) 'p))))

          (is (= 0 (((ff/d (ff/d omega)) X Y Z W) R3-rect-point))))))))
