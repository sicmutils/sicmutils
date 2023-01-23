#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.mechanics.routhian-test
  (:refer-clojure :exclude [+ - * / partial])
  (:require [clojure.test :refer [is deftest testing use-fixtures]]
            [emmy.abstract.function :as f]
            [emmy.generic :as g :refer [+ - * / square]]
            [emmy.mechanics.routhian :as rn]
            [emmy.simplify :refer [hermetic-simplify-fixture]]
            [emmy.structure :as s :refer [up down]]
            [emmy.value :as v]))

(use-fixtures :each hermetic-simplify-fixture)

(def simplify
  (comp v/freeze g/simplify))

(defn Lag [mx kx my ky]
  (fn [[_ [x y] [vx vy]]]
    (- (+ (* (/ 1 2) mx (square vx))
          (* (/ 1 2) my (square vy)))
       (+ (* (/ 1 2) kx (square x))
          (* (/ 1 2) ky (square y))
          (* x y y)))))

(defn Lag2 [m k]
  (fn [[_ [x y] [vx vy]]]
    (- (+ (* (/ 1 2) m (square vx))
          (* (/ 1 2) m (square vy)))
       (+ (* (/ 1 2) k (square x))
          (* (/ 1 2) k (square y))))))

(deftest routhian-tests
  (let [L (Lag 'mx 'kx 'my 'ky)
        R (rn/Lagrangian->Routhian L)
        state (up 't (up 'x 'y) (up 'vx 'py))]
    (is (= state
           (g/simplify
            ((rn/Routhian-state->Lagrangian-state R)
             ((rn/Lagrangian-state->Routhian-state L)
              state))))
        "state round-trips through the back and forth functions."))

  (is (= '(/ (+ (* (/ 1 2) kx my (expt x 2))
                (* (/ 1 2) ky my (expt y 2))
                (* (/ -1 2) mx my (expt vx 2))
                (* my x (expt y 2))
                (* (/ 1 2) (expt py 2)))
             my)
         (simplify
          ((rn/Lagrangian->Routhian
            (Lag 'mx 'kx 'my 'ky))
           (up 't (up 'x 'y) (up 'vx 'py))))))

  (is (= '(up
           (+ (* -1 kx (x t))
              (* -1 mx (((expt D 2) x) t))
              (* -1 (expt (y t) 2)))
           (up 0
               (/ (+ (* my ((D y) t)) (* -1 (py t))) my)
               (+ (* ky (y t)) (* 2 (x t) (y t)) ((D py) t))))
         (f/with-literal-functions [x y py]
           (simplify
            (((rn/Routh-equations
               (rn/Lagrangian->Routhian (Lag 'mx 'kx 'my 'ky)))
              x y py)
             't)))))

  (is (= '(up
           (down
            (+ (* -1 k (x0 t)) (* -1 m (((expt D 2) x0) t)))
            (+ (* -1 k (x1 t)) (* -1 m (((expt D 2) x1) t))))
           (up
            0
            (up
             (/ (+ (* m ((D y0) t)) (* -1 (py0 t))) m)
             (/ (+ (* m ((D y1) t)) (* -1 (py1 t))) m))
            (down (+ (* k (y0 t)) ((D py0) t))
                  (+ (* k (y1 t)) ((D py1) t)))))
         (f/with-literal-functions [x0 x1 y0 y1 py0 py1]
           (simplify
            (((rn/Routh-equations
               (rn/Lagrangian->Routhian (Lag2 'm 'k)))
              (up x0 x1)
              (up y0 y1)
              (down py0 py1))
             't)))))

  (testing "Routhian->acceleration"
    (is (= '(up (/ (* -1 k x0) m) (/ (* -1 k x1) m))
           (simplify
            ((rn/Routhian->acceleration
              (rn/Lagrangian->Routhian (Lag2 'm 'k)))
             (up 't
                 (up (up 'x0 'x1) (up 'y0 'y1))
                 (up (up 'vx0 'vx1) (down 'py0 'py1))))))))

  (testing "Routhian->state-derivative"
    (is (= '(up
             1
             (up (up vx0 vx1)
                 (up (/ py0 m) (/ py1 m)))
             (up (up (/ (* -1 k x0) m) (/ (* -1 k x1) m))
                 (down (* -1 k y0) (* -1 k y1))))
           (simplify
            ((rn/Routhian->state-derivative
              (rn/Lagrangian->Routhian (Lag2 'm 'k)))
             (up 't
                 (up (up 'x0 'x1) (up 'y0 'y1))
                 (up (up 'vx0 'vx1) (down 'py0 'py1)))))))

    (testing "a test of the dissipation function"
      ;; in Lagrangian variables...
      (letfn [(diss2 [delta0 delta1]
                (fn [[_ _ [vx vy]]]
                  (+ (* (/ 1 2) delta0 (square vx))
                     (* (/ 1 2) delta1 (square vy)))))]
        (is (= '(up 1
                    (up (up vx0 vx1) (up (/ py0 m) (/ py1 m)))
                    (up
                     (up (/ (+ (* -1 delta0 vx0) (* -1 k x0)) m)
                         (/ (+ (* -1 delta0 vx1) (* -1 k x1)) m))
                     (down (/ (+ (* -1 k m y0) (* -1 delta1 py0)) m)
                           (/ (+ (* -1 k m y1) (* -1 delta1 py1)) m))))
               (simplify
                ((rn/Routhian->state-derivative
                  (rn/Lagrangian->Routhian (Lag2 'm 'k))
                  (diss2 'delta0 'delta1))
                 (up 't
                     (up (up 'x0 'x1) (up 'y0 'y1))
                     (up (up 'vx0 'vx1) (down 'py0 'py1)))))))))))

(deftest two-D-particles-test
  (testing "Two 2-dimensional particles"
    (letfn [(L [m1 m2 V]
              (fn [[_ [xy1 xy2] [v1 v2]]]
                (- (+ (* (/ 1 2) m1 (square v1))
                      (* (/ 1 2) m2 (square v2)))
                   (V xy1 xy2))))]
      (is (= '(/ (+ (* (/ -1 2) m1 m2 (expt v1x 2))
                    (* (/ -1 2) m1 m2 (expt v1y 2))
                    (* m2 (V (up x1 y1) (up x2 y2)))
                    (* (/ 1 2) (expt p2x 2))
                    (* (/ 1 2) (expt p2y 2)))
                 m2)
             (f/with-literal-functions [[V [[0 0] [0 0]] 0]]
               (simplify
                ((rn/Lagrangian->Routhian
                  (L 'm1 'm2 V))
                 (up 't
                     (up (up 'x1 'y1) (up 'x2 'y2))
                     (up (up 'v1x 'v1y) (down 'p2x 'p2y))))))))

      (is (= '(up
               (down
                (+ (* -1 m1 (((expt D 2) x1) t))
                   (* -1 (((partial 0 0) V)
                          (up (x1 t) (y1 t)) (up (x2 t) (y2 t)))))
                (+ (* -1 m1 (((expt D 2) y1) t))
                   (* -1 (((partial 0 1) V)
                          (up (x1 t) (y1 t)) (up (x2 t) (y2 t))))))
               (up 0
                   (up (/ (+ (* m2 ((D x2) t)) (* -1 (p2x t))) m2)
                       (/ (+ (* m2 ((D y2) t)) (* -1 (p2y t))) m2))
                   (down (+ ((D p2x) t)
                            (((partial 1 0) V)
                             (up (x1 t) (y1 t)) (up (x2 t) (y2 t))))
                         (+ ((D p2y) t)
                            (((partial 1 1) V)
                             (up (x1 t) (y1 t)) (up (x2 t) (y2 t)))))))
             (f/with-literal-functions [x1 y1 x2 y2 p2x p2y
                                        [V [[0 0] [0 0]] 0]]
               (simplify
                (((rn/Routh-equations
                   (rn/Lagrangian->Routhian (L 'm1 'm2 V)))
                  (up x1 y1)
                  (up x2 y2)
                  (down p2x p2y))
                 't))))))))
