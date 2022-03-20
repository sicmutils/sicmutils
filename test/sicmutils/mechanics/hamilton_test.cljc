#_"SPDX-License-Identifier: GPL-3.0"

(ns sicmutils.mechanics.hamilton-test
  (:refer-clojure :exclude [+ - * / partial])
  (:require [clojure.test :refer [is deftest testing use-fixtures]]
            [sicmutils.abstract.function :as f #?@(:cljs [:include-macros true])]
            [sicmutils.calculus.derivative :refer [D]]
            [sicmutils.generic :as g :refer [+ - * /]]
            [sicmutils.matrix :as m]
            [sicmutils.mechanics.hamilton :as H]
            [sicmutils.mechanics.lagrange :as L]
            [sicmutils.operator :as o]
            [sicmutils.simplify :refer [hermetic-simplify-fixture]]
            [sicmutils.structure :as s :refer [component up down]]
            [sicmutils.value :as v]))

(use-fixtures :each hermetic-simplify-fixture)

(def simplify
  (comp v/freeze g/simplify))

(deftest poisson
  (let [a-state (H/->H-state 't
                             (L/coordinate-tuple 'x 'y 'z)
                             (H/momentum-tuple 'p_x 'p_y 'p_z))]
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
             a-state))))
    (f/with-literal-functions
      [[FF (up 0 (up 1 2) (down 3 4)) 5]
       [GG (up 0 (up 1 2) (down 3 4)) 5]
       [HH (up 0 (up 1 2) (down 3 4)) 5]]
      (is (= '(FF (up t (up x y) (down pa pb)))
             (simplify (FF (up 't (up 'x 'y) (down 'pa 'pb))))))
      (is (= '(down
               (((partial 0) FF) (up t (up x y) (down pa pb)))
               (down
                (((partial 1 0) FF) (up t (up x y) (down pa pb)))
                (((partial 1 1) FF) (up t (up x y) (down pa pb))))
               (up
                (((partial 2 0) FF) (up t (up x y) (down pa pb)))
                (((partial 2 1) FF) (up t (up x y) (down pa pb)))))
             (simplify ((D FF) (up 't (up 'x 'y) (down 'pa 'pb))))))
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
        (is (= 0 (simplify ((+ (H/Poisson-bracket FF (H/Poisson-bracket GG HH))
                                 (H/Poisson-bracket GG (H/Poisson-bracket HH FF))
                                 (H/Poisson-bracket HH (H/Poisson-bracket FF GG)))
                              (up 't (up 'x 'y) (down 'p_x 'p_y))))))))))

(deftest section-3-1-1
  ;; To move further into Hamiltonian mechanics, we will need
  ;; literal functions mapping structures to structures.

  (f/with-literal-functions [x y p_x p_y [V [1 2] 3]]
    (is (= '(V x y) (simplify (V 'x 'y))))
    (is (= '(up 0 (up (/ (+ (* m ((D x) t)) (* -1 (p_x t))) m) (/ (+ (* m ((D y) t)) (* -1 (p_y t))) m))
                (down (+ ((D p_x) t) (((partial 0) V) (x t) (y t))) (+ ((D p_y) t) (((partial 1) V) (x t) (y t)))))
           (simplify (((H/Hamilton-equations
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
            (simplify ((H/Legendre-transform g/square) 'p)))))

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
           (simplify ((H/Lagrangian->Hamiltonian
                       (L/L-rectangular 'm V))
                      (up 't (up 'x 'y) (down 'p_x 'p_y))))))))

(deftest gjs-tests
  (is (= '(up 0
              (up (/ (+ (* m ((D x) t)) (* -1 (p_x t))) m)
                  (/ (+ (* m ((D y) t)) (* -1 (p_y t))) m))
              (down (+ ((D p_x) t) (((partial 0) V) (x t) (y t)))
                    (+ ((D p_y) t) (((partial 1) V) (x t) (y t)))))

         (f/with-literal-functions [x y p_x p_y [V [0 1] 2]]
           (simplify (((H/Hamilton-equations
                        (H/H-rectangular
                         'm V))
                       (L/coordinate-tuple x y)
                       (H/momentum-tuple p_x p_y))
                      't)))))

  (is (= '(/ (+ (* m (expt r 2) (V r))
                (* (/ 1 2) (expt p_r 2) (expt r 2))
                (* (/ 1 2) (expt p_phi 2)))
             (* m (expt r 2)))
         (simplify
          ((H/Lagrangian->Hamiltonian
            (L/L-central-polar 'm (f/literal-function 'V)))
           (H/->H-state 't
                        (L/coordinate-tuple 'r 'phi)
                        (H/momentum-tuple 'p_r 'p_phi))))))
  (is (= '(up 0
              (up (/ (+ (* m ((D r) t)) (* -1 (p_r t))) m)
                  (/ (+ (* m (expt (r t) 2) ((D phi) t)) (* -1 (p_phi t))) (* m (expt (r t) 2))))
              (down (/ (+ (* m (expt (r t) 3) ((D p_r) t)) (* m (expt (r t) 3) ((D V) (r t))) (* -1 (expt (p_phi t) 2))) (* m (expt (r t) 3)))
                    ((D p_phi) t)))
         (f/with-literal-functions [r phi p_r p_phi V]
           (simplify
            (((H/Hamilton-equations
               (H/Lagrangian->Hamiltonian
                (L/L-central-polar 'm V)))
              (L/coordinate-tuple r phi)
              (H/momentum-tuple p_r p_phi))
             't)))))
  (is (= '(up 0
              (up (/ (+ (* m ((D r) t)) (* -1 (p_r t))) m)
                  (/ (+ (* m (expt (r t) 2) ((D phi) t)) (* -1 (p_phi t))) (* m (expt (r t) 2))))
              (down (/ (+ (* m (expt (r t) 3) ((D p_r) t)) (* GM (expt m 2) (r t)) (* -1 (expt (p_phi t) 2))) (* m (expt (r t) 3)))
                    ((D p_phi) t)))
         (f/with-literal-functions [r phi p_r p_phi]
           (simplify
            (((H/Hamilton-equations
               (H/Lagrangian->Hamiltonian
                (L/L-central-polar 'm
                                   (fn [r] (- (/ (* 'GM 'm) r))))))
              (L/coordinate-tuple r phi)
              (H/momentum-tuple p_r p_phi))
             't)))))
  (let [F (f/literal-function 'F (H/Hamiltonian 2))
        G (f/literal-function 'G (H/Hamiltonian 2))
        H (f/literal-function 'G (H/Hamiltonian 2))
        L_F (g/Lie-derivative F)
        L_G (g/Lie-derivative G)]
    (is (= 0 (simplify (((+ (o/commutator L_F L_G)
                            (g/Lie-derivative (H/Poisson-bracket F G)))
                         H)
                        (up 't (up 'x 'y) (down 'px 'py))))))))

(deftest symplectic
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
           (H/symplectic-unit 6)))))

(deftest iterated-map-test
  (let [fail (constantly false)
        M (fn [x y cont fail] (if (> x 10) (fail) (cont (inc x) (dec y))))]
    (is (= '(6 95) ((H/iterated-map M 5) 1 100 list fail)))
    (is (= '(10 91) ((H/iterated-map M 9) 1 100 list fail)))
    (is (= false ((H/iterated-map M 20) 1 100 list fail )))))
