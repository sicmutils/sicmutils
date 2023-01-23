#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.mechanics.hamilton-test
  (:refer-clojure :exclude [+ - * / partial])
  (:require [clojure.test :refer [is deftest testing use-fixtures]]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]]
            [emmy.abstract.function :as f]
            [emmy.calculus.derivative :refer [D]]
            [emmy.generators :as sg]
            [emmy.generic :as g :refer [+ - * /]]
            [emmy.matrix :as m]
            [emmy.mechanics.hamilton :as H]
            [emmy.mechanics.lagrange :as L]
            [emmy.operator :as o]
            [emmy.simplify :refer [hermetic-simplify-fixture]]
            [emmy.structure :as s :refer [component up down]]
            [emmy.value :as v]))

(use-fixtures :each hermetic-simplify-fixture)

(def simplify
  (comp v/freeze g/simplify))

(deftest basic-tests
  (checking "basic accessors and state creation" 100
            [t sg/symbol
             q sg/symbol
             p sg/symbol]
            (let [state (H/->H-state t q p)]
              (is (H/H-state? state)
                  "constructs to a proper H-state.")

              (is (H/compatible-H-state? (g/transpose state))
                  "transposing inner and outer gets you something compatible for
                  contraction.")

              (is (= [q p] (H/state->qp state))
                  "state->qp works")

              (is (= t (L/time state))
                  "time lookup works correctly")

              (is (= q (L/coordinate state))
                  "coordinate lookup works correctly")

              (doseq [->p [H/momentum H/state->p H/momenta H/P]]
                (is (= p (->p state))
                    "momentum lookup works correctly"))))

  (testing "Hamiltonian type sig"
    (let [state (H/literal-Hamiltonian-state 4)]
      (is (= (list 'f (v/freeze state))
             (v/freeze
              ((f/literal-function 'f (H/Hamiltonian 4))
               state)))
          "Applying the state passes the type check.")))

  (testing "H-state?"
    (is (H/H-state?
         (H/literal-Hamiltonian-state 2))
        "literal state works")

    (let [L-state (L/->L-state 't (up 'x) (up 'vx))]
      (is (not (H/H-state? L-state))
          "up in momentum slot fails.")

      (is (H/H-state?
           ((H/L-state->H-state (L/L-free-particle 'm))
            L-state))
          "up in momentum slot fails.")))

  (testing "round trip H-state, L-state"
    (let [H-state (H/->H-state 't (up 'x) (down 'p_x))
          L (L/L-free-particle 'm)
          H (H/Lagrangian->Hamiltonian L)]
      (is (H/H-state? H-state)
          "confirming!")

      (is (not
           (H/H-state?
            ((H/H-state->L-state H) H-state)))
          "L-state is not an H-state")

      (is (= H-state
             (g/simplify
              ((H/L-state->H-state L)
               ((H/H-state->L-state H) H-state))))
          "round-tripping")))

  (testing "qp->H-state-path"
    (f/with-literal-functions [q p]
      (is (= (up 't '(q t) '(p t))
             ((H/qp->H-state-path q p) 't))
          "qp->H-state-path works, given two functions of t"))))

(deftest matrix<->state-tests
  (let [s  (up 't (up 'x 'y) (down 'p_x 'p_y))
        m  (m/by-cols ['t 'x 'y 'p_x 'p_y])
        s2 (H/literal-Hamiltonian-state 2)]
    (is (= s
           (-> (H/H-state->matrix s)
               (H/matrix->H-state s2)))
        "round tripping from H-state to matrix and back")

    (is (= m
           (-> (H/matrix->H-state m s2)
               (H/H-state->matrix)))
        "round tripping from matrix to H-state and back")))

(deftest hamiltonian-tests
  (testing "H-rectangular"
    (is (= '(up 0
                (up (/ (+ (* m ((D x) t)) (* -1 (p_x t))) m)
                    (/ (+ (* m ((D y) t)) (* -1 (p_y t))) m))
                (down (+ ((D p_x) t) (((partial 0) V) (x t) (y t)))
                      (+ ((D p_y) t) (((partial 1) V) (x t) (y t)))))
           (simplify
            (f/with-literal-functions [x y p_x p_y [V [0 0] 0]]
              (((H/Hamilton-equations
                 (H/H-rectangular 'm V))
                (L/coordinate-tuple x y)
                (L/momentum-tuple p_x p_y))
               't)))))))

(deftest legendre-xform-tests
  (testing "Legendre-transform"
    ;; To move further into Hamiltonian mechanics, we will need
    ;; literal functions mapping structures to structures.
    (f/with-literal-functions [x y p_x p_y [V [1 2] 3]]
      (is (= '(V x y)
             (simplify (V 'x 'y))))

      (is (= '(up 0 (up (/ (+ (* m ((D x) t)) (* -1 (p_x t))) m)
                        (/ (+ (* m ((D y) t)) (* -1 (p_y t))) m))
                  (down (+ ((D p_x) t) (((partial 0) V) (x t) (y t)))
                        (+ ((D p_y) t) (((partial 1) V) (x t) (y t)))))
             (simplify
              (((H/Hamilton-equations
                 (H/H-rectangular
                  'm V))
                (up x y)
                (down p_x p_y))
               't))))
      (is (= '(/ (* (/ 1 4) (expt y 2)) c)
             (simplify
              ((H/Legendre-transform (fn [x] (* 'c x x))) 'y))))

      (is (= '(* (/ 1 4) (expt p 2))
             (v/freeze
              (simplify
               ((H/Legendre-transform g/square) 'p)))))

      (is (= '(+ (* (/ 1 2) m (expt v_x 2))
                 (* (/ 1 2) m (expt v_y 2))
                 (* -1 (V x y)))
             (v/freeze
              (simplify
               ((L/L-rectangular 'm V) (up 't (up 'x 'y) (up 'v_x 'v_y)))))))

      (is (= '(/ (+ (* m (V x y))
                    (* (/ 1 2) (expt p_x 2))
                    (* (/ 1 2) (expt p_y 2)))
                 m)
             (simplify
              ((H/Lagrangian->Hamiltonian
                (L/L-rectangular 'm V))
               (up 't (up 'x 'y) (down 'p_x 'p_y))))))

      (is (= '(+ (* (/ 1 2) m (expt v_x 2))
                 (* (/ 1 2) m (expt v_y 2))
                 (* -1 (V x y)))
             (simplify
              ((H/Hamiltonian->Lagrangian
                (H/Lagrangian->Hamiltonian
                 (L/L-rectangular 'm V)))
               (up 't (up 'x 'y) (up 'v_x 'v_y)))))
          "round tripping a Lagrangian to Hamiltonian and back"))))

(deftest L<->H-tests
  (is (= '(/ (+ (* m (V x y))
                (* (/ 1 2) (expt p_x 2))
                (* (/ 1 2) (expt p_y 2)))
             m)
         (f/with-literal-functions [[V [0 0] 0]]
           (simplify
            ((H/Lagrangian->Hamiltonian
              (L/L-rectangular 'm V))
             (H/->H-state 't
                          (L/coordinate-tuple 'x 'y)
                          (L/momentum-tuple 'p_x 'p_y)))))))


  (is (= '(/ (+ (* m (expt r 2) (V r))
                (* (/ 1 2) (expt p_r 2) (expt r 2))
                (* (/ 1 2) (expt p_phi 2)))
             (* m (expt r 2)))
         (simplify
          ((H/Lagrangian->Hamiltonian
            (L/L-central-polar 'm (f/literal-function 'V)))
           (H/->H-state 't
                        (L/coordinate-tuple 'r 'phi)
                        (L/momentum-tuple 'p_r 'p_phi))))))

  (is (= '(up 0
              (up (/ (+ (* m ((D r) t)) (* -1 (p_r t)))
                     m)
                  (/ (+ (* m (expt (r t) 2) ((D phi) t)) (* -1 (p_phi t)))
                     (* m (expt (r t) 2))))
              (down (/ (+ (* m (expt (r t) 3) ((D p_r) t))
                          (* m (expt (r t) 3) ((D V) (r t)))
                          (* -1 (expt (p_phi t) 2)))
                       (* m (expt (r t) 3)))
                    ((D p_phi) t)))
         (f/with-literal-functions [r phi p_r p_phi V]
           (simplify
            (((H/Hamilton-equations
               (H/Lagrangian->Hamiltonian
                (L/L-central-polar 'm V)))
              (L/coordinate-tuple r phi)
              (L/momentum-tuple p_r p_phi))
             't)))))

  ;; If we substitute a Coulomb potential in for V we get the equations for
  ;; satellite motion around a spherical primary.
  (is (= '(up 0
              (up (/ (+ (* m ((D r) t)) (* -1 (p_r t)))
                     m)
                  (/ (+ (* m (expt (r t) 2)
                           ((D phi) t))
                        (* -1 (p_phi t)))
                     (* m (expt (r t) 2))))
              (down (/ (+ (* m (expt (r t) 3) ((D p_r) t))
                          (* GM (expt m 2) (r t))
                          (* -1 (expt (p_phi t) 2)))
                       (* m (expt (r t) 3)))
                    ((D p_phi) t)))
         (f/with-literal-functions [r phi p_r p_phi]
           (simplify
            (((H/Hamilton-equations
               (H/Lagrangian->Hamiltonian
                (L/L-central-polar 'm
                                   (fn [r] (- (/ (* 'GM 'm) r))))))
              (L/coordinate-tuple r phi)
              (L/momentum-tuple p_r p_phi))
             't)))))

  (is (= '(up 0
              (up (/ (+ (* m ((D x_1) t)) (* -1 (p_1 t))) m)
                  (/ (+ (* m ((D x_2) t)) (* -1 (p_2 t))) m))
              (down (+ (* k (x_1 t)) ((D p_1) t))
                    (+ (* k (x_2 t)) ((D p_2) t))))
         (f/with-literal-functions [x_1 x_2 p_1 p_2]
           (simplify
            (((H/Hamilton-equations
               (H/Lagrangian->Hamiltonian (L/L-harmonic 'm 'k)))
              (L/coordinate-tuple x_1 x_2)
              (L/momentum-tuple p_1 p_2))
             't)))))

  ;; Continuing with our coupled harmonic oscillators we obtain the Hamiltonian:
  (is (= '(/ (+ (* c m_1 m_2 x_1 x_2)
                (* (/ 1 2) k_1 m_1 m_2 (expt x_1 2))
                (* (/ 1 2) k_2 m_1 m_2 (expt x_2 2))
                (* (/ 1 2) m_1 (expt p_2 2))
                (* (/ 1 2) m_2 (expt p_1 2)))
             (* m_1 m_2))
         (simplify
          ((H/Lagrangian->Hamiltonian
            (L/L-coupled-harmonic
             (down (down 'm_1 0)
                   (down 0 'm_2))
             (down (down 'k_1 'c)
                   (down 'c 'k_2))))
           (H/->H-state 't
                        (L/coordinate-tuple 'x_1 'x_2)
                        (L/momentum-tuple 'p_1 'p_2))))))


  (is (= '(up 0
              (up (/ (+ (* m_1 ((D x_1) t)) (* -1 (p_1 t))) m_1)
                  (/ (+ (* m_2 ((D x_2) t)) (* -1 (p_2 t))) m_2))
              (down (+ (* c (x_2 t)) (* k_1 (x_1 t)) ((D p_1) t))
                    (+ (* c (x_1 t)) (* k_2 (x_2 t)) ((D p_2) t))))
         (f/with-literal-functions [x_1 x_2 p_1 p_2]
           (simplify
            (((H/Hamilton-equations
               (H/Lagrangian->Hamiltonian
                (L/L-coupled-harmonic (down (down 'm_1 0)
                                            (down 0 'm_2))
                                      (down (down 'k_1 'c)
                                            (down 'c 'k_2)))))
              (L/coordinate-tuple x_1 x_2)
              (L/momentum-tuple p_1 p_2))
             't)))))

  ;; Continuation of demonstration of bundled coordinates.
  (is (= '(up 0
              (up
               (up (/ (+ (* m_1 ((D x_1) t)) (* -1 (p_x_1 t))) m_1)
                   (/ (+ (* m_1 ((D y_1) t)) (* -1 (p_y_1 t))) m_1))
               (up (/ (+ (* m_2 ((D x_2) t)) (* -1 (p_x_2 t))) m_2)
                   (/ (+ (* m_2 ((D y_2) t)) (* -1 (p_y_2 t))) m_2)))
              (down
               (down
                (+ ((D p_x_1) t)
                   (((partial 0 0) V) (up (x_1 t) (y_1 t)) (up (x_2 t) (y_2 t))))
                (+ ((D p_y_1) t)
                   (((partial 0 1) V) (up (x_1 t) (y_1 t)) (up (x_2 t) (y_2 t)))))
               (down
                (+ ((D p_x_2) t)
                   (((partial 1 0) V) (up (x_1 t) (y_1 t)) (up (x_2 t) (y_2 t))))
                (+ ((D p_y_2) t)
                   (((partial 1 1) V) (up (x_1 t) (y_1 t)) (up (x_2 t) (y_2 t)))))))

         (f/with-literal-functions [x_1 y_1 x_2 y_2
                                    p_x_1 p_y_1 p_x_2 p_y_2
                                    [V [[0 0] [0 0]] 0]]
           (simplify
            (((H/Hamilton-equations
               (H/Lagrangian->Hamiltonian
                (L/L-two-particle 'm_1 'm_2 V)))
              (L/coordinate-tuple
               (L/coordinate-tuple x_1 y_1)
               (L/coordinate-tuple x_2 y_2))
              (L/momentum-tuple
               (L/momentum-tuple p_x_1 p_y_1)
               (L/momentum-tuple p_x_2 p_y_2)))
             't))))))

(deftest section-3-3-tests
  (testing "From 3.3 -- Phase-space reduction"
    (is (= '(/ (+ (* A C gMR (expt (sin theta) 2) (cos theta))
                  (* (/ 1 2) A (expt p_psi 2) (expt (sin theta) 2))
                  (* (/ 1 2) C (expt p_psi 2) (expt (cos theta) 2))
                  (* (/ 1 2) C (expt p_theta 2) (expt (sin theta) 2))
                  (* -1N C p_phi p_psi (cos theta))
                  (* (/ 1 2) C (expt p_phi 2)))
               (* A C (expt (sin theta) 2)))
           (simplify
            ((H/Lagrangian->Hamiltonian (L/L-axisymmetric-top 'A 'C 'gMR))
             (H/->H-state 't
                          ['theta 'phi 'psi]
                          ['p_theta 'p_phi 'p_psi])))))

    (is (= '(up 0
                (up (/ (+ (* A ((D theta) t)) (* -1 (p_theta t))) A)
                    (/ (+
                        (* A (expt (sin (theta t)) 2) ((D phi) t))
                        (* (cos (theta t)) (p_psi t))
                        (* -1 (p_phi t)))
                       (* A (expt (sin (theta t)) 2)))
                    (/ (+
                        (* A C (expt (sin (theta t)) 2) ((D psi) t))
                        (* -1 A (expt (sin (theta t)) 2) (p_psi t))
                        (* -1 C (expt (cos (theta t)) 2) (p_psi t))
                        (* C (cos (theta t)) (p_phi t)))
                       (* A C (expt (sin (theta t)) 2))))
                (down
                 (/ (+
                     (* -1 A gMR (expt (cos (theta t)) 4))
                     (* A (expt (sin (theta t)) 3) ((D p_theta) t))
                     (* 2 A gMR (expt (cos (theta t)) 2))
                     (* (expt (cos (theta t)) 2) (p_psi t) (p_phi t))
                     (* -1 (cos (theta t)) (expt (p_psi t) 2))
                     (* -1 (cos (theta t)) (expt (p_phi t) 2))
                     (* -1 A gMR)
                     (* (p_psi t) (p_phi t)))
                    (* A (expt (sin (theta t)) 3)))
                 ((D p_phi) t)
                 ((D p_psi) t)))
           (f/with-literal-functions [theta phi psi p_theta p_phi p_psi]
             (simplify
              (((H/Hamilton-equations
                 (H/Lagrangian->Hamiltonian
                  (L/L-axisymmetric-top 'A 'C 'gMR)))
                (L/coordinate-tuple theta phi psi)
                (L/momentum-tuple p_theta p_phi p_psi))
               't))))
        "'That got ugly', says GJS.")))

(deftest poisson-tests
  (let [a-state (H/->H-state 't
                             (L/coordinate-tuple 'x 'y 'z)
                             (L/momentum-tuple 'p_x 'p_y 'p_z))]
    (is (= '(up (down 1 0 0)
                (down 0 1 0)
                (down 0 0 1))
           (simplify
            ((H/Poisson-bracket
              (up (comp (component 0) L/coordinate)
                  (comp (component 1) L/coordinate)
                  (comp (component 2) L/coordinate))
              (down (comp (component 0) H/momentum)
                    (comp (component 1) H/momentum)
                    (comp (component 2) H/momentum)))
             a-state)))))

  (f/with-literal-functions [[FF (up 0 (up 1 2) (down 3 4)) 5]
                             [GG (up 0 (up 1 2) (down 3 4)) 5]
                             [HH (up 0 (up 1 2) (down 3 4)) 5]]
    (is (= '(FF (up t (up x y) (down pa pb)))
           (simplify
            (FF (up 't (up 'x 'y) (down 'pa 'pb))))))

    (is (= '(down
             (((partial 0) FF) (up t (up x y) (down pa pb)))
             (down
              (((partial 1 0) FF) (up t (up x y) (down pa pb)))
              (((partial 1 1) FF) (up t (up x y) (down pa pb))))
             (up
              (((partial 2 0) FF) (up t (up x y) (down pa pb)))
              (((partial 2 1) FF) (up t (up x y) (down pa pb)))))
           (simplify
            ((D FF) (up 't (up 'x 'y) (down 'pa 'pb))))))

    (is (= '(+
             (*
              -1
              (((partial 2 0) FF) (up t (up x y) (down p_x p_y)))
              (((partial 1 0) GG) (up t (up x y) (down p_x p_y))))
             (*
              -1
              (((partial 2 1) FF) (up t (up x y) (down p_x p_y)))
              (((partial 1 1) GG) (up t (up x y) (down p_x p_y))))
             (*
              (((partial 1 0) FF) (up t (up x y) (down p_x p_y)))
              (((partial 2 0) GG) (up t (up x y) (down p_x p_y))))
             (*
              (((partial 1 1) FF) (up t (up x y) (down p_x p_y)))
              (((partial 2 1) GG) (up t (up x y) (down p_x p_y)))))
           (simplify
            ((* (D FF)
                (H/Poisson-bracket identity identity)
                (D GG))
             (up 't (up 'x 'y) (down 'p_x 'p_y))))))

    (testing "Jacobi identity"
      (is (zero?
           (simplify
            ((+ (H/Poisson-bracket FF (H/Poisson-bracket GG HH))
                (H/Poisson-bracket GG (H/Poisson-bracket HH FF))
                (H/Poisson-bracket HH (H/Poisson-bracket FF GG)))
             (up 't (up 'x 'y) (down 'p_x 'p_y))))))))


  (let [Sx (comp (s/component 0) L/coordinate)
        Sy (comp (s/component 1) L/coordinate)
        Sz (comp (s/component 2) L/coordinate)

        Spx (comp (s/component 0) H/momentum)
        Spy (comp (s/component 1) H/momentum)
        Spz (comp (s/component 2) H/momentum)

        ;; L = [Lx, Ly, Lz]
        ;; where Li are components of angular momentum
        Lx (- (* Sy Spz) (* Spy Sz))
        Ly (- (* Sz Spx) (* Spz Sx))
        Lz (- (* Sx Spy) (* Spx Sy))
        L  (down Lx Ly Lz)
        three-state (H/->H-state
                     't
                     (L/coordinate-tuple 'x 'y 'z)
                     (L/momentum-tuple 'p_x 'p_y 'p_z))]
    (is (= '(down 0
                  (+ (* -1 p_x y) (* p_y x))
                  (+ (* -1 p_x z) (* p_z x)))
           (simplify
            ((H/Poisson-bracket Lx L) three-state))))

    ;; Poisson brackets are compositional with canonical transformations
    ;; (see point-transformations.scm for F->CT and time-varying.scm for
    ;; C-rotating, repeated here.)
    (letfn [(rotating [n]
              (fn [[t [x y z]]]
                (L/coordinate-tuple
                 (+ (* (g/cos (* n t)) x) (* (g/sin (* n t)) y))
                 (- (* (g/cos (* n t)) y) (* (g/sin (* n t)) x))
                 z)))
            (C-rotating [n]
              (H/F->CT (rotating n)))
            (K [n]
              (fn [[_ [x y] [px py]]]
                (* n (- (* x py) (* y px)))))]
      (is (zero?
           (simplify
            ((- (comp (H/Poisson-bracket Lx Ly) (C-rotating 'n))
                (H/Poisson-bracket
                 (comp Lx (C-rotating 'n))
                 (comp Ly (C-rotating 'n))))
             three-state))))

      (is (= (up 0 (up 0 0 0) (down 0 0 0))
             (g/simplify
              ((H/canonical-K?
                (C-rotating 'n) (K 'n)) three-state))))

      ;; or getting K directly from F
      (is (= '(up 0 (up 0 0 0) (down 0 0 0))
             (simplify
              ((H/canonical-K?
                (C-rotating 'n)
                (H/F->K (rotating 'n)))
               three-state))))

      (is (zero?
           (simplify
            ((- (H/F->K (rotating 'n))
                (K 'n))
             three-state))))

      ;; not all K's work:
      (letfn [(bad-K [n]
                (fn [s]
                  (- ((K n) s))))]
        (is (= '(up
                 0
                 (up (+ (* 2 n x (sin (* n t))) (* -2 n y (cos (* n t))))
                     (+ (* 2 n x (cos (* n t))) (* 2 n y (sin (* n t))))
                     0)
                 (down (+ (* 2 n p_x (sin (* n t))) (* -2 n p_y (cos (* n t))))
                       (+ (* 2 n p_x (cos (* n t))) (* 2 n p_y (sin (* n t))))
                       0))
               (simplify
                ((H/canonical-K?
                  (C-rotating 'n)
                  (bad-K 'n))

                 three-state))))))

    ;; Poisson brackets in terms of J
    ;; Guaranteed to work only for scalar valued functions
    (letfn [(PB [f g]
              (fn [s]
                (* ((D f) s) (H/J-func ((D g) s)))))]
      (is (zero?
           (simplify
            ((- (H/Poisson-bracket Lx Ly) Lz)
             three-state))))

      (is (zero?
           (simplify
            ((- (PB Lx Ly) Lz) three-state)))))

    (letfn [(PB [f g]
              (fn [s]
                (let [J (H/linear-function->multiplier H/J-func ((D g) s))]
                  (* ((D f) s) (* J ((D g) s))))))
            (H-harmonic [m k]
              (H/Lagrangian->Hamiltonian (L/L-harmonic m k)))]
      (is (zero?
           (simplify
            (- ((H/Poisson-bracket (H/H-harmonic 'm 'k)
                                   (comp (component 0) L/coordinate))
                three-state)
               ((PB (H-harmonic 'm 'k)
                    (comp (s/component 0) L/coordinate))
                three-state)))))

      (is (= (up 0 0 0)
             (g/simplify
              (- ((H/Poisson-bracket (H-harmonic 'm 'k) L/coordinate)
                  three-state)
                 ((PB (H/H-harmonic 'm 'k) L/coordinate)
                  three-state)))))

      (is (= '(down (* -1 k x) (* -1 k y) (* -1 k z))
             (simplify
              ((PB H/momentum (H-harmonic 'm 'k))
               three-state))))

      (is (= '(up (/ p_x m) (/ p_y m) (/ p_z m))
             (simplify
              ((PB L/coordinate (H-harmonic 'm 'k))
               three-state)))))))

(deftest Lie-derivative-tests
  (let [F (f/literal-function 'F (H/Hamiltonian 2))
        G (f/literal-function 'G (H/Hamiltonian 2))
        H (f/literal-function 'H (H/Hamiltonian 2))
        L_F (H/Lie-derivative F)
        L_G (H/Lie-derivative G)]
    (is  (zero?
          (simplify
           (((+ (o/commutator L_F L_G)
                (H/Lie-derivative (H/Poisson-bracket F G)))
             H)
            (up 't (up 'x 'y) (down 'px 'py))))))))

(deftest iterated-map-test
  (let [fail (constantly false)
        M (fn [x y cont fail]
            (if (> x 10)
              (fail)
              (cont (inc x) (dec y))))]
    (is (= [6 95]
           ((H/iterated-map M 5) 1 100 vector fail)))

    (is (= [10 91]
           ((H/iterated-map M 9) 1 100 vector fail)))

    (is (false?
         ((H/iterated-map M 20) 1 100 vector fail)))))

(deftest canonical-tests
  (testing "two-particle-center-of-mass"
    (let [b-state
          (up 't
              (L/coordinate-tuple
               (L/coordinate-tuple 'x_1 'y_1)
               (L/coordinate-tuple 'x_2 'y_2))
              (L/momentum-tuple
               (L/momentum-tuple 'p_x_1 'p_y_1)
               (L/momentum-tuple 'p_x_2 'p_y_2)))]
      (is (= (up 0 (up (up 0 0) (up 0 0)) (down (down 0 0) (down 0 0)))
             (g/simplify
              (- ((H/F->CT (H/two-particle-center-of-mass 'm0 'm1)) b-state)
                 ((H/two-particle-center-of-mass-canonical 'm0 'm1) b-state)))))

      (is (= (up 0 (up (up 0 0) (up 0 0)) (down (down 0 0) (down 0 0)))
             (g/simplify
              ((H/time-independent-canonical?
                (H/two-particle-center-of-mass-canonical 'm1 'm2))
               b-state))))

      (is (= '(up
               (up 0 (up (up 0 0) (up 0 0)) (down (down 0 0) (down 0 0)))
               (up
                (up (up 0 (up (up 0 0) (up 0 0)) (down (down 0 0) (down 0 0)))
                    (up 0 (up (up 0 0) (up 0 0)) (down (down 0 0) (down 0 0))))
                (up (up 0 (up (up 0 0) (up 0 0)) (down (down 0 0) (down 0 0)))
                    (up 0 (up (up 0 0) (up 0 0)) (down (down 0 0) (down 0 0)))))
               (down
                (down (up 0 (up (up 0 0) (up 0 0)) (down (down 0 0) (down 0 0)))
                      (up 0 (up (up 0 0) (up 0 0)) (down (down 0 0) (down 0 0))))
                (down (up 0 (up (up 0 0) (up 0 0)) (down (down 0 0) (down 0 0)))
                      (up 0 (up (up 0 0) (up 0 0)) (down (down 0 0) (down 0 0))))))
             (simplify
              ((H/canonical-transform?
                (H/two-particle-center-of-mass-canonical 'm1 'm2))
               b-state))))))

  (testing "multiplicative-transpose"
    (let [T (fn [v]
              (* (down (up 'a 'c) (up 'b 'd)) v))]
      (is (= '(up (+ (* a x) (* b y)) (+ (* c x) (* d y)))
             (simplify
              (T (up 'x 'y)))))

      (is (= '(+ (* a p_x v_x)
                 (* b p_x v_y)
                 (* c p_y v_x)
                 (* d p_y v_y))
             (simplify
              (* (* (down 'p_x 'p_y) ((D T) (up 'x 'y)))
                 (up 'v_x 'v_y)))))

      (is (= '(+ (* a p_x v_x)
                 (* b p_x v_y)
                 (* c p_y v_x)
                 (* d p_y v_y))
             (simplify
              (* (down 'p_x 'p_y)
                 (* ((D T) (up 'x 'y)) (up 'v_x 'v_y))))))

      (is (= '(+ (* a p_x v_x)
                 (* b p_x v_y)
                 (* c p_y v_x)
                 (* d p_y v_y))
             (simplify
              (* (* ((H/multiplicative-transpose
                      (down 'p_x 'p_y)) ((D T) (up 'x 'y)))
                    (down 'p_x 'p_y))
                 (up 'v_x 'v_y)))))

      ;; But strangely enough...
      (is (= '(+ (* a p_x v_x)
                 (* b p_x v_y)
                 (* c p_y v_x)
                 (* d p_y v_y))
             (simplify
              (* (* (down 'p_x 'p_y)
                    ((H/multiplicative-transpose
                      (down 'p_x 'p_y)) ((D T) (up 'x 'y))))
                 (up 'v_x 'v_y))))))

    (is (= (up 0 (up 0 0) (down 0 0))
           (g/simplify
            ((H/time-independent-canonical? (H/F->CT L/p->r))
             (up 't
                 (L/coordinate-tuple 'r 'phi)
                 (L/momentum-tuple 'p_r 'p_phi))))))

    ;; but not all transforms are canonical:
    (testing "non-canonical-transform"
      (with-redefs [gensym (let [i (atom 0)]
                             (fn
                               ([] (clojure.core/gensym))
                               ([x] (symbol (str x (swap! i inc))))))]
        (letfn [(a-non-canonical-transform [[t theta p]]
                  (let [x (* p (g/sin theta))
                        p_x (* p (g/cos theta))]
                    (up t x p_x)))]
          (is (= '(up 0
                      (+ (* -1 p x3) x3)
                      (+ (* p x2) (* -1 x2)))
                 (simplify
                  ((H/time-independent-canonical? a-non-canonical-transform)
                   (up 't 'theta 'p))))
              "The genysym redef is ugly but makes this test repeatable, at
              least.")

          (is (= '(matrix-by-rows
                   (up 0 0 0)
                   (up 0 0 (+ (* -1 p) 1))
                   (up 0 (+ p -1) 0))
                 (simplify
                  ((H/symplectic? a-non-canonical-transform)
                   (up 't 'theta 'p)))))

          (is (= '(matrix-by-rows
                   (up 0 (+ (* -1 p) 1))
                   (up (+ p -1) 0))
                 (simplify
                  ((H/symplectic-transform? a-non-canonical-transform)
                   (up 't 'theta 'p))))))))))

(deftest symplectic-tests
  (testing "unit"
    (is (= (m/by-rows [0 0 0 0 0 0 1 0 0 0 0 0]
                      [0 0 0 0 0 0 0 1 0 0 0 0]
                      [0 0 0 0 0 0 0 0 1 0 0 0]
                      [0 0 0 0 0 0 0 0 0 1 0 0]
                      [0 0 0 0 0 0 0 0 0 0 1 0]
                      [0 0 0 0 0 0 0 0 0 0 0 1]
                      [-1 0 0 0 0 0 0 0 0 0 0 0]
                      [0 -1 0 0 0 0 0 0 0 0 0 0]
                      [0 0 -1 0 0 0 0 0 0 0 0 0]
                      [0 0 0 -1 0 0 0 0 0 0 0 0]
                      [0 0 0 0 -1 0 0 0 0 0 0 0]
                      [0 0 0 0 0 -1 0 0 0 0 0 0])
           (H/symplectic-unit 6))))

  (testing "symplectic?"
    (is (= '(matrix-by-rows
             (up 0 0 0 0 0)
             (up 0 0 0 0 0)
             (up 0 0 0 0 0)
             (up 0 0 0 0 0)
             (up 0 0 0 0 0))
           (simplify
            ((H/symplectic? (H/F->CT L/p->r))
             (up 't
                 (up 'r 'phi)
                 (down 'p_r 'p_phi)))))))

  (testing "symplectic-transform?"
    ;; For example, point transforms are canonical
    (is (= '(matrix-by-rows
             (up 0 0 0 0)
             (up 0 0 0 0)
             (up 0 0 0 0)
             (up 0 0 0 0))
           (simplify
            ((H/symplectic-transform? (H/F->CT L/p->r))
             (up 't
                 (up 'r 'theta)
                 (down 'p_r 'p_theta))))))))

(deftest Lie-transform-tests
  ;; The general solution for a trajectory is:
  ;;
  ;;  q(t,q0,p0) = A(q0,p0) cos (sqrt(k/m)*t + phi(q0,p0))
  ;;
  ;;  where A(q0,p0) = sqrt(2/k)*sqrt(p0^2/(2*m) + (k/2)*q0^2)
  ;;                 = sqrt((2/k)*E0)
  ;;
  ;;  and   phi(q0,p0) = - atan((1/sqrt(k*m))*(p0/q0))
  ;;
  ;; Thus, with initial conditions q0, p0
  ;;   we should get q(t) = q0*cos(sqrt(k/m)*t)+p0*sin(sqrt(k/m)*t)
  ;;
  ;; We can expand this as a Lie series:
  (is (= '(x_0
           (/ (* dt p_0) m)
           (/ (* (/ -1 2) (expt dt 2) k x_0) m)
           (/ (* (/ -1 6) (expt dt 3) k p_0) (expt m 2))
           (/ (* (/ 1 24) (expt dt 4) (expt k 2) x_0) (expt m 2))
           (/ (* (/ 1 120) (expt dt 5) (expt k 2) p_0) (expt m 3)))
         (simplify
          (take 6 (((H/Lie-transform (H/H-harmonic 'm 'k) 'dt)
                    L/state->q)
                   (H/->H-state 0 'x_0 'p_0))))))

  (is (= '(p_0
           (* -1 dt k x_0)
           (/ (* (/ -1 2) (expt dt 2) k p_0) m)
           (/ (* (/ 1 6) (expt dt 3) (expt k 2) x_0) m)
           (/ (* (/ 1 24) (expt dt 4) (expt k 2) p_0) (expt m 2))
           (/ (* (/ -1 120) (expt dt 5) (expt k 3) x_0) (expt m 2)))
         (simplify
          (take 6 (((H/Lie-transform (H/H-harmonic 'm 'k) 'dt)
                    H/momentum)
                   (H/->H-state 0 'x_0 'p_0))))))

  (is (= '((/ (+ (* (/ 1 2) k m (expt x_0 2))
                 (* (/ 1 2) (expt p_0 2)))
              m)
           0 0 0 0 0)
         (simplify
          (take 6 (((H/Lie-transform (H/H-harmonic 'm 'k) 'dt)
                    (H/H-harmonic 'm 'k))
                   (H/->H-state 0 'x_0 'p_0))))))

  (is (= '((up r_0 phi_0)
           (up (/ (* dt p_r_0) m) (/ (* dt p_phi_0) (* m (expt r_0 2))))
           (up (/ (+ (* (/ -1 2) (expt dt 2) m (expt r_0 3) ((D U) r_0))
                     (* (/ 1 2) (expt dt 2) (expt p_phi_0 2)))
                  (* (expt m 2) (expt r_0 3)))
               (/ (* -1 (expt dt 2) p_phi_0 p_r_0) (* (expt m 2) (expt r_0 3))))
           (up
            (/ (+ (* (/ -1 6) (expt dt 3) m p_r_0 (expt r_0 4) (((expt D 2) U) r_0))
                  (* (/ -1 2) (expt dt 3) (expt p_phi_0 2) p_r_0))
               (* (expt m 3) (expt r_0 4)))
            (/ (+ (* (/ 1 3) (expt dt 3) m p_phi_0 (expt r_0 3) ((D U) r_0))
                  (* (expt dt 3) p_phi_0 (expt p_r_0 2) (expt r_0 2))
                  (* (/ -1 3) (expt dt 3) (expt p_phi_0 3)))
               (* (expt m 3) (expt r_0 6)))))
         (simplify
          (take 4 (((H/Lie-transform
                     (H/H-central-polar 'm (f/literal-function 'U))
                     'dt)
                    L/state->q)
                   (H/->H-state
                    0
                    (L/coordinate-tuple 'r_0 'phi_0)
                    (L/momentum-tuple 'p_r_0 'p_phi_0)))))))

  ;; Note from GJS: I left this one that uses the Lagrangian because it appears
  ;; to be used for timings
  (is (= '((up r_0 phi_0)
           (up (/ (* dt p_r_0) m)
               (/ (* dt p_phi_0)
                  (* m (expt r_0 2)))))
         (simplify
          (take 2
                (((H/Lie-transform
                   (H/Lagrangian->Hamiltonian
                    (L/L-central-polar 'm (fn [r] (- (/ 'GM r)))))
                   'dt)
                  L/state->q)
                 (H/->H-state 0
                              (L/coordinate-tuple 'r_0 'phi_0)
                              (L/momentum-tuple 'p_r_0 'p_phi_0))))))
      "Wow, this is very slow after the first two terms! We are still faster
      than the original but I absolutely don't buy the timings in
      Lie-transform.scm."))

(deftest more-tests
  (is (= '(/ (+ (* m (expt r 2) (V r))
                (* (/ 1 2) (expt p_r 2) (expt r 2))
                (* (/ 1 2) (expt p_phi 2)))
             (* m (expt r 2)))
         (simplify
          ((comp (H/H-central 'm (f/literal-function 'V))
                 (H/F->CT L/p->r))
           (H/->H-state
            't
            (L/coordinate-tuple 'r 'phi)
            (L/momentum-tuple 'p_r 'p_phi))))))

  (testing "polar-canonical-tests"
    (is (= (up 't 'x 'p)
           (g/simplify
            ((comp (H/polar-canonical-inverse 'alpha)
                   (H/polar-canonical 'alpha))
             (up 't 'x 'p)))))

    (is (= '(matrix-by-rows (up 0 0) (up 0 0))
           (simplify
            ((H/symplectic-transform? (H/polar-canonical 'alpha))
             (up 't 'a 'I))))
        "It is clearly canonical.")

    (is (= '(up t x p)
           (simplify
            ((comp (H/polar-canonical-inverse 'alpha)
                   (H/polar-canonical 'alpha))
             (up 't 'x 'p)))))

    (is (= '(matrix-by-rows
             (up 0 0 0)
             (up 0 0 0)
             (up 0 0 0))
           (simplify
            ((H/symplectic? (H/polar-canonical 'alpha))
             (up 't 'a 'I)))))

    (is (= (up 0 0 0)
           (g/simplify
            ((H/time-independent-canonical? (H/polar-canonical 'alpha))
             (up 't 'a 'I)))))

    (let [Cmix (fn [[t [q0 q1] [p0 p1]]]
                 (up t
                     (L/coordinate-tuple q0 (- p1))
                     (L/momentum-tuple p0 q1)))
          Cmix2 (fn [[t q p]]
                  (up t
                      (s/opposite p)
                      (- (s/opposite q))))
          a-state (up 't
                      (L/coordinate-tuple 'x 'y)
                      (L/momentum-tuple 'p_x 'p_y))]
      (is (= (up 0 (up 0 0) (down 0 0))
             (g/simplify
              ((H/time-independent-canonical? Cmix)
               a-state))))

      (is (= '(matrix-by-rows
               (up 0 0 0 0 0)
               (up 0 0 0 0 0)
               (up 0 0 0 0 0)
               (up 0 0 0 0 0)
               (up 0 0 0 0 0))
             (simplify
              ((H/symplectic? Cmix) a-state))))

      (is (= (up 0 (up 0 0) (down 0 0))
             (g/simplify
              ((H/time-independent-canonical? Cmix2)
               a-state))))

      (is (= '(up (up 0 (up 0 0) (down 0 0))
                  (up (up 0 (up 0 0) (down 0 0))
                      (up 0 (up 0 0) (down 0 0)))
                  (down (up 0 (up 0 0) (down 0 0))
                        (up 0 (up 0 0) (down 0 0))))
             (simplify
              ((H/canonical-transform? Cmix2)
               a-state)))))))
