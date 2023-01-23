#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.calculus.hodge-star-test
  (:refer-clojure :exclude [+ - * /])
  (:require [clojure.test :refer [is deftest testing use-fixtures]]
            [emmy.calculus.basis :as b]
            [emmy.calculus.coordinate :refer [let-coordinates]]
            [emmy.calculus.covariant :as cov]
            [emmy.calculus.form-field :as ff]
            [emmy.calculus.hodge-star :as hs]
            [emmy.calculus.manifold :as m]
            [emmy.calculus.metric :as cm]
            [emmy.calculus.vector-field :as vf]
            [emmy.expression :as x]
            [emmy.function :refer [compose]]
            [emmy.generic :as g :refer [+ - * /]]
            [emmy.simplify :refer [hermetic-simplify-fixture]]
            [emmy.structure :as s :refer [up down]]
            [emmy.value :as v]))

(use-fixtures :each hermetic-simplify-fixture)

(def simplify
  (comp v/freeze g/simplify))

(deftest gram-schmidt-tests
  (testing "Orthonormalizing with respect to the Lorentz metric in 2 dimensions."
    (let-coordinates [[t x] m/R2-rect]
      (let [R2-point ((m/point R2-rect) (up 't0 'x0))
            R2-basis (b/coordinate-system->basis R2-rect)
            L2-metric (fn [c]
                        (fn [u v]
                          (+ (* -1 c c (dt u) (dt v))
                             (* 1 (dx u) (dx v)))))
            L2-vector-basis (hs/Gram-Schmidt (b/basis->vector-basis R2-basis)
                                             (L2-metric 'c))]
        (is (= '[(/ (((partial 0) f) (up t0 x0)) c)
                 (((partial 1) f) (up t0 x0))]
               (map (fn [v]
                      (simplify
                       ((v (m/literal-manifold-function 'f R2-rect))
                        R2-point)))
                    L2-vector-basis)))

        (is (= '[(* c (v↑0 (up t0 x0)))
                 (v↑1 (up t0 x0))]
               (map (fn [omega]
                      (simplify
                       ((omega (vf/literal-vector-field 'v R2-rect))
                        R2-point)))
                    (b/vector-basis->dual L2-vector-basis R2-rect)))))))

  (testing "4-dimensional Lorentz metric."
    (let [SR m/R4-rect]
      (let-coordinates [[t x y z] SR]
        (let [g-Lorentz (fn [c]
                          (fn [u v]
                            (+ (* (dx u) (dx v))
                               (* (dy u) (dy v))
                               (* (dz u) (dz v))
                               (* -1 (g/square c) (dt u) (dt v)))))
              SR-basis (b/coordinate-system->basis SR)
              an-event ((m/point SR) (up 't0 'x0 'y0 'z0))
              SR-V (b/basis->vector-basis SR-basis)
              SR-V1 (flatten (hs/Gram-Schmidt SR-V (g-Lorentz 'c)))]
          (testing "SR-V1 is orthogonal"
            (doseq [v1 SR-V1
                    v2 (rest (drop-while #(not= % v1) SR-V1))]
              (is (zero?
                   (simplify
                    (((g-Lorentz 'c) v1 v2) an-event))))))

          (testing "SR-V1 is normal"
            (is (= [-1 1 1 1]
                   (map (fn [v]
                          (simplify
                           (((g-Lorentz 'c) v v) an-event)))
                        SR-V1))))

          (is (= '[(up (/ 1 c) 0 0 0)
                   (up 0 1 0 0)
                   (up 0 0 1 0)
                   (up 0 0 0 1)]
                 (map (fn [v]
                        (simplify
                         ((v (m/chart SR))
                          an-event)))
                      SR-V1)))))))

  (let-coordinates [[x y z] m/R3-rect]
    (let [R3-point ((m/point R3-rect) (up 'x0 'y0 'z0))
          R3-basis (b/coordinate-system->basis R3-rect)
          g3-maker (fn [a b c d e f]
                     (fn [v1 v2]
                       (+ (* a (dx v1) (dx v2))
                          (* b (dx v1) (dy v2))
                          (* c (dx v1) (dz v2))
                          (* b (dx v2) (dy v1))
                          (* d (dy v1) (dy v2))
                          (* e (dy v1) (dz v2))
                          (* c (dx v2) (dz v1))
                          (* e (dy v2) (dz v1))
                          (* f (dz v1) (dz v2)))))
          g3 (g3-maker 'a 'b 'c 'd 'e 'f)
          results (map (fn [v]
                         ((v (m/chart R3-rect))
                          R3-point))
                       (flatten
                        (hs/Gram-Schmidt
                         (b/basis->vector-basis R3-basis)
                         g3)))]
      (is (= '(up (/ 1 (sqrt a)) 0 0)
             (simplify
              (first results))))

      (is (= '(up (/ (* -1 b)
                     (sqrt (+ (* (expt a 2) d) (* -1 a (expt b 2)))))
                  (/ a (sqrt (+ (* (expt a 2) d) (* -1 a (expt b 2)))))
                  0)
             (simplify
              (second results)))))))

(deftest hodge-star-tests
  (let-coordinates [[x y] m/R2-rect]
    (let [E2-metric (fn [v1 v2]
                      (+ (* (dx v1) (dx v2))
                         (* (dy v1) (dy v2))))
          omega (ff/wedge dx dy)
          E2-star (hs/Hodge-star E2-metric
                                 (b/coordinate-system->basis
                                  R2-rect))]
      (is (= 1 (simplify
                ((E2-star omega)
                 ((m/point R2-rect) (up 'x 'y))))))

      (testing "What is a rank 0 form?"
        (is (= '(V↑1 (up x y))
               (v/freeze
                (((E2-star dx)
                  (vf/literal-vector-field 'V R2-rect))
                 ((m/point R2-rect) (up 'x 'y))))))

        (is (= '(* -1 (V↑0 (up x y)))
               (simplify
                (((E2-star dy)
                  (vf/literal-vector-field 'V R2-rect))
                 ((m/point R2-rect) (up 'x 'y)))))))

      (is (= '(+ (* (V↑0 (up x y)) (W↑1 (up x y)))
                 (* -1 (W↑0 (up x y)) (V↑1 (up x y))))
             (simplify
              (((E2-star (fn [_pt] 1))
                (vf/literal-vector-field 'V R2-rect)
                (vf/literal-vector-field 'W R2-rect))
               ((m/point R2-rect) (up 'x 'y)))))))))

(deftest three-d-tests
  (testing "First, some simple tests on 3-dimensional Euclidean space."
    (let-coordinates [[x y z] m/R3-rect]
      (let [R3-point ((m/point R3-rect) (up 'x0 'y0 'z0))
            E3-metric (fn [v1 v2]
                        (+ (* (dx v1) (dx v2))
                           (* (dy v1) (dy v2))
                           (* (dz v1) (dz v2))))
            E3-star (hs/Hodge-star E3-metric R3-rect)

            ;; omega = alpha*dx + beta*dy + gamma*dz
            omega (+ (* (m/literal-scalar-field 'alpha R3-rect) dx)
                     (* (m/literal-scalar-field 'beta R3-rect) dy)
                     (* (m/literal-scalar-field 'gamma R3-rect) dz))


            present (fn [expr]
                      (-> (simplify expr)
                          (x/substitute '(up x0 y0 z0) 'p)))]
        (is (= 0 (simplify
                  (((- (E3-star (fn [_pt] 1))
                       (ff/wedge dx dy dz))
                    (vf/literal-vector-field 'u R3-rect)
                    (vf/literal-vector-field 'v R3-rect)
                    (vf/literal-vector-field 'w R3-rect))
                   R3-point))))

        (is (= 0 (simplify
                  (((- (E3-star dx)
                       (ff/wedge dy dz))
                    (vf/literal-vector-field 'u R3-rect)
                    (vf/literal-vector-field 'v R3-rect))
                   R3-point))))

        (is (= 0 (simplify
                  (((+ (E3-star (ff/wedge dx dz)) dy)
                    (vf/literal-vector-field 'u R3-rect))
                   R3-point))))

        (is (= 0 (simplify
                  ((- (E3-star (ff/wedge dx dy dz)) 1)
                   R3-point))))

        (is (= '(+ (* -1 (u↑1 p) (f p) (w↑2 p) (v↑0 p))
                   (* (u↑1 p) (f p) (v↑2 p) (w↑0 p))
                   (* (f p) (w↑2 p) (u↑0 p) (v↑1 p))
                   (* (f p) (v↑0 p) (w↑1 p) (u↑2 p))
                   (* -1 (f p) (v↑2 p) (u↑0 p) (w↑1 p))
                   (* -1 (f p) (w↑0 p) (v↑1 p) (u↑2 p)))
               (present
                (((E3-star (m/literal-scalar-field 'f R3-rect))
                  (vf/literal-vector-field 'u R3-rect)
                  (vf/literal-vector-field 'v R3-rect)
                  (vf/literal-vector-field 'w R3-rect))
                 R3-point))))

        (is (= '(+ (* (u↑1 p) (gamma p))
                   (* (u↑0 p) (beta p))
                   (* (u↑2 p) (alpha p)))
               (present
                (((E3-star
                   (+ (* (m/literal-scalar-field 'alpha R3-rect)
                         (ff/wedge dx dy))
                      (* (m/literal-scalar-field 'beta R3-rect)
                         (ff/wedge dy dz))
                      (* (m/literal-scalar-field 'gamma R3-rect)
                         (ff/wedge dz dx))))
                  (vf/literal-vector-field 'u R3-rect))
                 R3-point))))

        (is  (= '(alpha p)
                (present
                 ((E3-star
                   (* (m/literal-scalar-field 'alpha R3-rect)
                      (ff/wedge dx dy dz)))
                  R3-point))))

        (is (= '(+ (* -1 (u↑1 p) (v↑0 p) (gamma p))
                   (* (u↑1 p) (v↑2 p) (alpha p))
                   (* (v↑0 p) (u↑2 p) (beta p))
                   (* -1 (v↑2 p) (u↑0 p) (beta p))
                   (* (u↑0 p) (v↑1 p) (gamma p))
                   (* -1 (v↑1 p) (u↑2 p) (alpha p)))
               (present
                (((E3-star omega)
                  (vf/literal-vector-field 'u R3-rect)
                  (vf/literal-vector-field 'v R3-rect))
                 R3-point))))

        (is (= '(+ (* -1 (u↑1 p) (((partial 0) gamma) p))
                   (* (u↑1 p) (((partial 2) alpha) p))
                   (* (u↑0 p) (((partial 1) gamma) p))
                   (* -1 (u↑0 p) (((partial 2) beta) p))
                   (* (u↑2 p) (((partial 0) beta) p))
                   (* -1 (u↑2 p) (((partial 1) alpha) p)))
               (present
                (((E3-star (ff/d omega))
                  (vf/literal-vector-field 'u R3-rect))
                 R3-point))))

        (is (= '(+ (* -1 (u↑1 p) (w↑2 p) (v↑0 p) (((partial 0) alpha) p))
                   (* -1 (u↑1 p) (w↑2 p) (v↑0 p) (((partial 1) beta) p))
                   (* -1 (u↑1 p) (w↑2 p) (v↑0 p) (((partial 2) gamma) p))
                   (* (u↑1 p) (v↑2 p) (w↑0 p) (((partial 0) alpha) p))
                   (* (u↑1 p) (v↑2 p) (w↑0 p) (((partial 1) beta) p))
                   (* (u↑1 p) (v↑2 p) (w↑0 p) (((partial 2) gamma) p))
                   (* (w↑2 p) (u↑0 p) (v↑1 p) (((partial 0) alpha) p))
                   (* (w↑2 p) (u↑0 p) (v↑1 p) (((partial 1) beta) p))
                   (* (w↑2 p) (u↑0 p) (v↑1 p) (((partial 2) gamma) p))
                   (* (v↑0 p) (w↑1 p) (u↑2 p) (((partial 0) alpha) p))
                   (* (v↑0 p) (w↑1 p) (u↑2 p) (((partial 1) beta) p))
                   (* (v↑0 p) (w↑1 p) (u↑2 p) (((partial 2) gamma) p))
                   (* -1 (v↑2 p) (u↑0 p) (w↑1 p) (((partial 0) alpha) p))
                   (* -1 (v↑2 p) (u↑0 p) (w↑1 p) (((partial 1) beta) p))
                   (* -1 (v↑2 p) (u↑0 p) (w↑1 p) (((partial 2) gamma) p))
                   (* -1 (w↑0 p) (v↑1 p) (u↑2 p) (((partial 0) alpha) p))
                   (* -1 (w↑0 p) (v↑1 p) (u↑2 p) (((partial 1) beta) p))
                   (* -1 (w↑0 p) (v↑1 p) (u↑2 p) (((partial 2) gamma) p)))
               (present
                (((ff/d (E3-star omega))
                  (vf/literal-vector-field 'u R3-rect)
                  (vf/literal-vector-field 'v R3-rect)
                  (vf/literal-vector-field 'w R3-rect))
                 R3-point)))
            "Indeed, *d is the curl operator.")

        (is (= '(+ (((partial 0) alpha) p)
                   (((partial 1) beta) p)
                   (((partial 2) gamma) p))
               (present
                ((E3-star (ff/d (E3-star omega)))
                 R3-point))))))))

(deftest two-plus-one-minkowski-tests
  (testing "Now for a 2+1 Minkowski space with c=1."
    (let-coordinates [[t x y] m/R3-rect]
      (let [R3-point ((m/point R3-rect) (up 't0 'x0 'y0))
            L3-metric (fn [u v]
                        (+ (* -1 (dt u) (dt v))
                           (* (dx u) (dx v))
                           (* (dy u) (dy v))))
            L3-star (hs/Hodge-star L3-metric R3-rect)]
        (is (= -1 ((L3-metric d:dt d:dt) R3-point)))

        (is (= 0 (simplify
                  (((- (L3-star (fn [_m] 1))
                       (ff/wedge dx dy dt))
                    (vf/literal-vector-field 'U R3-rect)
                    (vf/literal-vector-field 'V R3-rect)
                    (vf/literal-vector-field 'W R3-rect))
                   R3-point))))

        (is (= 0 (simplify
                  (((- (L3-star dx)
                       (ff/wedge dy dt))
                    (vf/literal-vector-field 'U R3-rect)
                    (vf/literal-vector-field 'V R3-rect))
                   R3-point))))

        (is (= 0 (simplify
                  (((- (L3-star dy)
                       (ff/wedge dt dx))
                    (vf/literal-vector-field 'U R3-rect)
                    (vf/literal-vector-field 'V R3-rect))
                   R3-point))))

        (is (= 0 (simplify
                  (((- (L3-star dt)
                       (ff/wedge dy dx))
                    (vf/literal-vector-field 'U R3-rect)
                    (vf/literal-vector-field 'V R3-rect))
                   R3-point))))

        (is (= 0 (simplify
                  (((- (L3-star (ff/wedge dx dy)) dt)
                    (vf/literal-vector-field 'U R3-rect))
                   R3-point))))

        (is (= 0 (simplify
                  (((+ (L3-star (ff/wedge dy dt)) dx)
                    (vf/literal-vector-field 'U R3-rect))
                   R3-point))))

        (is (= 0 (simplify
                  (((+ (L3-star (ff/wedge dt dx)) dy)
                    (vf/literal-vector-field 'U R3-rect))
                   R3-point))))

        (is (= 0 (simplify
                  ((+ (L3-star (ff/wedge dx dy dt)) 1)
                   R3-point))))))))

(deftest one-plus-one-minkowski-tests
  (testing "Now for a 1-1 Minkowski space with c"
    (let-coordinates [[t x] m/R2-rect]
      (let [R2-point ((m/point R2-rect) (up 't0 'x0))
            R2-basis (b/coordinate-system->basis R2-rect)
            c 'c
            L2-metric (fn [u v]
                        (+ (* -1 c c (dt u) (dt v))
                           (* 1 (dx u) (dx v))))]
        (let [L2-Hodge-star (hs/Hodge-star L2-metric R2-rect)]
          (is (= '(+ (* (u↑0 (up t0 x0)) (v↑1 (up t0 x0)))
                     (* -1 (v↑0 (up t0 x0)) (u↑1 (up t0 x0))))
                 (simplify
                  (((L2-Hodge-star (fn [_x] 1))
                    (vf/literal-vector-field 'u R2-rect)
                    (vf/literal-vector-field 'v R2-rect))
                   R2-point)))
              "Wrong! Must generally orthonormalize."))

        (let [L2-Hodge-star (hs/Hodge-star L2-metric R2-rect
                                           :orthonormalize? true)]
          (is (= '(+ (* c (u↑0 (up t0 x0)) (v↑1 (up t0 x0)))
                     (* -1 c (v↑0 (up t0 x0)) (u↑1 (up t0 x0))))
                 (simplify
                  (((L2-Hodge-star (fn [_x] 1))
                    (vf/literal-vector-field 'u R2-rect)
                    (vf/literal-vector-field 'v R2-rect))
                   R2-point)))
              "'cdt↑dx(u v)"))

        (testing "Accelerate by explicitly passing in an explicitly
    constructed orthonormal constant basis."
          (let [L2-basis (hs/orthonormalize R2-basis L2-metric R2-rect)
                L2-vector-basis  (b/basis->vector-basis L2-basis)
                L2-oneform-basis (b/vector-basis->dual
                                  L2-vector-basis R2-rect)]
            (is (= (up (down 1 0)
                       (down 0 1))
                   (g/simplify
                    ((L2-oneform-basis L2-vector-basis) R2-point))))

            (is (= '[(/ (((partial 0) f) (up t0 x0)) c)
                     (((partial 1) f) (up t0 x0))]
                   (simplify
                    (map (fn [v]
                           ((v (m/literal-manifold-function 'f R2-rect))
                            R2-point))
                         L2-vector-basis))))

            (is (= '[(* c (v↑0 (up t0 x0)))
                     (v↑1 (up t0 x0))]
                   (simplify
                    (map (fn [omega]
                           ((omega (vf/literal-vector-field 'v R2-rect))
                            R2-point))
                         L2-oneform-basis))))))

        (testing "now make a constant basis..."
          (let [L2-constant-vector-basis (down (* (/ 1 c) d:dt) d:dx)
                L2-constant-oneform-basis (up (* c dt) dx)
                L2-constant-basis (b/make-basis
                                   L2-constant-vector-basis
                                   L2-constant-oneform-basis)
                L2-Hodge-star (hs/Hodge-star L2-metric L2-constant-basis)]
            (is (= '(+ (* c (u↑0 (up t0 x0)) (v↑1 (up t0 x0)))
                       (* -1 c (v↑0 (up t0 x0)) (u↑1 (up t0 x0))))
                   (simplify
                    (((L2-Hodge-star (fn [_x] 1))
                      (vf/literal-vector-field 'u R2-rect)
                      (vf/literal-vector-field 'v R2-rect))
                     R2-point)))
                "As desired.")

            (is (= '(* -1 (u↑1 (up t0 x0)) (alpha (up t0 x0)))
                   (simplify
                    (((L2-Hodge-star
                       (* (m/literal-manifold-function 'alpha R2-rect)
                          (* c dt)))
                      (vf/literal-vector-field 'u R2-rect))
                     R2-point)))
                "-alpha dx(u)")

            (is (= '(* -1 c (u↑0 (up t0 x0)) (alpha (up t0 x0)))
                   (simplify
                    (((L2-Hodge-star
                       (* (m/literal-manifold-function 'alpha R2-rect)
                          dx))
                      (vf/literal-vector-field 'u R2-rect))
                     R2-point)))
                "-alpha c dt(u)")

            (is (= '(* -1 (alpha (up t0 x0)))
                   (simplify
                    ((L2-Hodge-star
                      (* (m/literal-manifold-function 'alpha R2-rect)
                         (ff/wedge (* c dt) dx)))
                     R2-point)))))))))

  (testing "next example"
    (let-coordinates [[x y] m/R2-rect]
      (let [R2-point ((m/point R2-rect) (up 'x0 'y0))
            g-R2 (fn [g_00 g_01 g_11]
                   (fn [u v]
                     (+ (* g_00 (dx u) (dx v))
                        (* g_01 (+ (* (dx u) (dy v))
                                   (* (dy u) (dx v))))
                        (* g_11 (dy u) (dy v)))))
            R2-metric (g-R2 'a 'b 'c)
            ;; Hodge-star must Orthonormalize here
            R2-star (hs/Hodge-star R2-metric R2-rect
                                   :orthonormalize? true)]
        (is (= '(sqrt (+ (* a c)
                         (* -1 (expt b 2))))
               (simplify
                (((R2-star (fn [_x] 1)) d:dx d:dy)
                 R2-point))))

        (is (= '(/ b (sqrt (+ (* a c) (* -1 (expt b 2)))))
               (simplify
                (((R2-star dx) d:dx) R2-point))))

        (is (= '(/ c (sqrt (+ (* a c) (* -1 (expt b 2)))))
               (simplify
                (((R2-star dx) d:dy) R2-point))))

        (is (= '(/ (* -1 a) (sqrt (+ (* a c) (* -1 (expt b 2)))))
               (simplify
                (((R2-star dy) d:dx) R2-point))))

        (is (= '(/ (* -1 b) (sqrt (+ (* a c) (* -1 (expt b 2)))))
               (simplify
                (((R2-star dy) d:dy) R2-point))))

        (is (= '(/ 1 (sqrt (+ (* a c) (* -1 (expt b 2)))))
               (simplify
                ((R2-star (ff/wedge dx dy)) R2-point))))))))

(deftest lorentz-metric-tests
  (testing "Example: Lorentz metric on R↑4"
    (let [SR m/R4-rect]
      (let-coordinates [(up t x y z) SR]
        (let [c 'c
              SR-point ((m/point SR) (up 't0 'x0 'y0 'z0))
              SR-constant-vector-basis (down (* (/ 1 c) d:dt) d:dx d:dy d:dz)
              SR-constant-oneform-basis (up (* c dt) dx dy dz)
              SR-constant-basis (b/make-basis
                                 SR-constant-vector-basis
                                 SR-constant-oneform-basis)
              g-Lorentz (fn [u v]
                          (+ (* (dx u) (dx v))
                             (* (dy u) (dy v))
                             (* (dz u) (dz v))
                             (* -1 (g/square c) (dt u) (dt v))))
              SR-star (hs/Hodge-star g-Lorentz SR-constant-basis)
              u (+ (* (m/literal-manifold-function 'ut SR) (/ 1 c) d:dt)
                   (* (m/literal-manifold-function 'ux SR) d:dx)
                   (* (m/literal-manifold-function 'uy SR) d:dy)
                   (* (m/literal-manifold-function 'uz SR) d:dz))

              v (+ (* (m/literal-manifold-function 'vt SR) (/ 1 c) d:dt)
                   (* (m/literal-manifold-function 'vx SR) d:dx)
                   (* (m/literal-manifold-function 'vy SR) d:dy)
                   (* (m/literal-manifold-function 'vz SR) d:dz))]

          (is (= 0 (simplify
                    (((- (SR-star (ff/wedge dy dz))
                         (ff/wedge (* c dt) dx))
                      u v)
                     SR-point))))

          (is (= 0 (simplify
                    (((- (SR-star (ff/wedge dz dx))
                         (ff/wedge (* c dt) dy))
                      u v)
                     SR-point))))

          (is (= 0 (simplify
                    (((- (SR-star (ff/wedge dx dy))
                         (ff/wedge (* c dt) dz))
                      u v)
                     SR-point))))

          ;; Other rotations of variables are all similar.
          )))))

(defn ip
  "Claim: this is the interior product in a metric space."
  [metric basis]
  (fn [X]
    (fn [alpha]
      (let [k    (ff/get-rank alpha)
            n    (b/basis->dimension basis)
            dual (hs/Hodge-star metric basis)
            sign (if (even? (* k (- n k))) 1 -1)]
        (* sign (dual (ff/wedge (dual alpha)
                                ((cm/lower metric) X))))))))

(deftest interior-metric-tests
  (testing "interior product in a metric space"
    (let-coordinates [(up x y z) m/R3-rect]
      (let [R3-basis (b/coordinate-system->basis R3-rect)
            R3-point ((m/point R3-rect) (up 'x0 'y0 'z0))

            u (vf/literal-vector-field 'u R3-rect)
            v (vf/literal-vector-field 'v R3-rect)
            w (vf/literal-vector-field 'w R3-rect)

            E3-metric (fn [v1 v2]
                        (+ (* (dx v1) (dx v2))
                           (* (dy v1) (dy v2))
                           (* (dz v1) (dz v2))))
            omega
            (+ (* (m/literal-manifold-function 'alpha R3-rect)
                  (ff/wedge dx dy))
               (* (m/literal-manifold-function 'beta  R3-rect)
                  (ff/wedge dy dz))
               (* (m/literal-manifold-function 'gamma R3-rect)
                  (ff/wedge dz dx)))
            theta (* (m/literal-scalar-field 'delta R3-rect)
                     (ff/wedge dx dy dz))]
        (is (= 0 (simplify
                  (- (((((ip E3-metric R3-basis) u) omega) v) R3-point)
                     ((((cov/interior-product u) omega) v) R3-point)))))

        (is (= 0 (simplify
                  (- (((((ip E3-metric R3-basis) u) theta) v w) R3-point)
                     ((((cov/interior-product u) theta) v w) R3-point)))))))))

(deftest electrodynamics-tests
  (testing "Electrodynamics"
    (let [SR m/R4-rect]
      (let-coordinates [[t x y z] SR]
        (let [c 'c
              an-event ((m/point SR) (up 't0 'x0 'y0 'z0))
              g-Lorentz (fn [u v]
                          (+ (* (dx u) (dx v))
                             (* (dy u) (dy v))
                             (* (dz u) (dz v))
                             (* -1 (g/square c) (dt u) (dt v))))
              L4-constant-vector-basis (down (* (/ 1 c) d:dt) d:dx d:dy d:dz)
              L4-constant-oneform-basis (up (* c dt) dx dy dz)
              L4-constant-basis (b/make-basis
                                 L4-constant-vector-basis
                                 L4-constant-oneform-basis)
              SR-star (hs/Hodge-star g-Lorentz L4-constant-basis)]

          (is (= '(Bx (up t0 x0 y0 z0))
                 (simplify
                  (((SR-star
                     (* (m/literal-manifold-function 'Bx SR)
                        (ff/wedge dy dz)))
                    (* (/ 1 c) d:dt)
                    d:dx)
                   an-event))))

          (testing "Fields E, B.  From MTW p.108"
            (let [Faraday (fn [Ex Ey Ez Bx By Bz]
                            (+ (* Ex c (ff/wedge dx dt))
                               (* Ey c (ff/wedge dy dt))
                               (* Ez c (ff/wedge dz dt))
                               (* Bx (ff/wedge dy dz))
                               (* By (ff/wedge dz dx))
                               (* Bz (ff/wedge dx dy))))

                  Maxwell (fn [Ex Ey Ez Bx By Bz]
                            (+ (* -1 Bx c (ff/wedge dx dt))
                               (* -1 By c (ff/wedge dy dt))
                               (* -1 Bz c (ff/wedge dz dt))
                               (* Ex (ff/wedge dy dz))
                               (* Ey (ff/wedge dz dx))
                               (* Ez (ff/wedge dx dy))))]
              (is (= 0 (simplify
                        (((- (SR-star (Faraday 'Ex 'Ey 'Ez 'Bx 'By 'Bz))
                             (Maxwell 'Ex 'Ey 'Ez 'Bx 'By 'Bz))
                          (vf/literal-vector-field 'u SR)
                          (vf/literal-vector-field 'v SR))
                         an-event))))

              (is (= 0 (simplify
                        (((+ ((compose SR-star SR-star)
                              (Faraday 'Ex 'Ey 'Ez 'Bx 'By 'Bz))
                             (Faraday 'Ex 'Ey 'Ez 'Bx 'By 'Bz))
                          (vf/literal-vector-field 'u SR)
                          (vf/literal-vector-field 'v SR))
                         an-event)))
                  "**F + F = 0")

              (testing "Defining the 4-current density J."
                ;; Charge density is a manifold function.  Current density is a
                ;; vector field having only spatial components.
                (let [J (fn [charge-density Jx Jy Jz]
                          (- (* (/ 1 c) (+ (* Jx dx) (* Jy dy) (* Jz dz)))
                             (* charge-density c dt)))
                      rho (m/literal-manifold-function 'rho SR)
                      four-current (J rho
                                      (m/literal-manifold-function 'Ix SR)
                                      (m/literal-manifold-function 'Iy SR)
                                      (m/literal-manifold-function 'Iz SR))]
                  (comment
                    ;; This will generate the charge conservation equations...
                    ;; but they are too ugly to include, as per GJS's comment.
                    (simplify
                     (((ff/d (SR-star four-current))
                       (vf/literal-vector-field 'a SR)
                       (vf/literal-vector-field 'b SR)
                       (vf/literal-vector-field 'c SR)
                       (vf/literal-vector-field 'd SR))
                      an-event)))

                  (is (= '(rho (up t0 x0 y0 z0))
                         (simplify
                          (((SR-star four-current) d:dx d:dy d:dz) an-event))))


                  (is (= '(/ (* -1 (Ix (up t0 x0 y0 z0))) c)
                         (simplify
                          (((SR-star four-current)
                            (* (/ 1 c) d:dt) d:dy d:dz)
                           an-event))))

                  (is (= '(/ (* -1 (Iy (up t0 x0 y0 z0))) c)
                         (simplify
                          (((SR-star four-current)
                            (* (/ 1 c) d:dt) d:dz d:dx)
                           an-event))))

                  (is (= '(/ (* -1 (Iz (up t0 x0 y0 z0))) c)
                         (simplify
                          (((SR-star four-current)
                            (* (/ 1 c) d:dt) d:dx d:dy)
                           an-event))))

                  (testing "Maxwell's equations in the form language are:
            dF=0, d(*F)=fourpi *J"
                    (let [F (Faraday
                             (m/literal-manifold-function 'Ex SR)
                             (m/literal-manifold-function 'Ey SR)
                             (m/literal-manifold-function 'Ez SR)
                             (m/literal-manifold-function 'Bx SR)
                             (m/literal-manifold-function 'By SR)
                             (m/literal-manifold-function 'Bz SR))]
                      (is (= '(+ (((partial 1) Bx) (up t0 x0 y0 z0))
                                 (((partial 2) By) (up t0 x0 y0 z0))
                                 (((partial 3) Bz) (up t0 x0 y0 z0)))
                             (simplify
                              (((ff/d F) d:dx d:dy d:dz) an-event)))
                          "div B = 0")

                      (is (= '(/ (+ (* c (((partial 2) Ez) (up t0 x0 y0 z0)))
                                    (* -1 c (((partial 3) Ey) (up t0 x0 y0 z0)))
                                    (((partial 0) Bx) (up t0 x0 y0 z0)))
                                 c)
                             (simplify
                              (((ff/d F) (* (/ 1 c) d:dt) d:dy d:dz) an-event)))
                          "curl E = -1/c dB/dt")

                      (is (= '(/ (+ (* c (((partial 3) Ex) (up t0 x0 y0 z0)))
                                    (* -1 c (((partial 1) Ez) (up t0 x0 y0 z0)))
                                    (((partial 0) By) (up t0 x0 y0 z0)))
                                 c)
                             (simplify
                              (((ff/d F) (* (/ 1 c) d:dt) d:dz d:dx) an-event))))

                      (is (= '(/ (+ (* c (((partial 1) Ey) (up t0 x0 y0 z0)))
                                    (* -1 c (((partial 2) Ex) (up t0 x0 y0 z0)))
                                    (((partial 0) Bz) (up t0 x0 y0 z0)))
                                 c)
                             (simplify
                              (((ff/d F) (* (/ 1 c) d:dt) d:dx d:dy) an-event))))

                      (is (= '(+ (* -1 fourpi (rho (up t0 x0 y0 z0)))
                                 (((partial 1) Ex) (up t0 x0 y0 z0))
                                 (((partial 2) Ey) (up t0 x0 y0 z0))
                                 (((partial 3) Ez) (up t0 x0 y0 z0)))
                             (simplify
                              (((- (ff/d (SR-star F)) (* 'fourpi (SR-star four-current)))
                                d:dx d:dy d:dz)
                               an-event)))
                          "div E = fourpi rho")

                      (is (= '(/ (+ (* -1 c (((partial 2) Bz) (up t0 x0 y0 z0)))
                                    (* c (((partial 3) By) (up t0 x0 y0 z0)))
                                    (* fourpi (Ix (up t0 x0 y0 z0)))
                                    (((partial 0) Ex) (up t0 x0 y0 z0)))
                                 c)
                             (simplify
                              (((- (ff/d (SR-star F)) (* 'fourpi (SR-star four-current)))
                                (* (/ 1 'c) d:dt) d:dy d:dz)
                               an-event)))
                          "curl B = 1/c dE/dt + fourpi I")

                      (is (= '(/ (+ (* -1 c (((partial 3) Bx) (up t0 x0 y0 z0)))
                                    (* c (((partial 1) Bz) (up t0 x0 y0 z0)))
                                    (* fourpi (Iy (up t0 x0 y0 z0)))
                                    (((partial 0) Ey) (up t0 x0 y0 z0)))
                                 c)
                             (simplify
                              (((- (ff/d (SR-star F)) (* 'fourpi (SR-star four-current)))
                                (* (/ 1 c) d:dt) d:dz d:dx)
                               an-event))))

                      (is (= '(/ (+ (* -1 c (((partial 1) By) (up t0 x0 y0 z0)))
                                    (* c (((partial 2) Bx) (up t0 x0 y0 z0)))
                                    (* fourpi (Iz (up t0 x0 y0 z0)))
                                    (((partial 0) Ez) (up t0 x0 y0 z0)))
                                 c)
                             (simplify
                              (((- (ff/d (SR-star F)) (* 'fourpi (SR-star four-current)))
                                (* (/ 1 c) d:dt) d:dx d:dy)
                               an-event)))))))))))))))
