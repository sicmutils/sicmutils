;;
;; Copyright © 2017 Colin Smith.
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

(ns sicmutils.mechanics.lagrange
  (:refer-clojure :exclude [+ - * / partial time])
  (:require [sicmutils.calculus.derivative :refer [D partial]]
            [sicmutils.function :as f :refer [compose]]
            [sicmutils.generic :as g :refer [cos sin + - * /]]
            [sicmutils.numerical.minimize :as m]
            [sicmutils.numerical.quadrature :as q]
            [sicmutils.operator :as o]
            [sicmutils.polynomial :as p]
            [sicmutils.structure :as s :refer [up up?]]))

;; ## Variational Mechanics

(def coordinate-tuple up)
(def velocity-tuple up)
(def acceleration-tuple up)

;; TODO delete from hamilton.cljc
(def momentum-tuple up)

;; Lagrangian mechanics requires a configuration space Q, and a function
;;   L:RxQxQ' --> R
;;
;; Mechanical systems have state at each instant. The state is the information
;; required, along with the equations of motion, to determine the future of the
;; system.
;;
;; At every instant a system has a kinematic state, which has the time, the
;; configuration, and the rate of change of the configuration. Lagrangian
;; mechanics is formulated in terms of the kinematic state.
;;
;; Kinematic states and their derivatives are represented as Scheme
;; vectors, with components time, configuration, and derivatives.

(defn ->L-state
  "Constructs a Lagrangian state, also knows as a local tuple"
  [t q qdot & derivs]
  (apply up t q qdot derivs))

(def ->local ->L-state)
(def ->state ->L-state)

;; ### Local Tuple Selectors

(defn time
  "Extract the time slot from a state tuple.

  See [[coordinate]] for more detail."
  [local]
  {:pre [(up? local) (> (count local) 0)]}
  (nth local 0))

(defn coordinate
  "A convenience function on local tuples. A local tuple describes
  the state of a system at a particular time:

  ```
  [t, q, D q, D^2 q]
  ```

  representing time, position, velocity (and optionally acceleration etc.)

  [[coordinate]] returns the `q` element, which is expected to be a mapping from
  time to a structure of coordinates."
  [local]
  {:pre [(up? local) (> (count local) 1)]}
  (nth local 1))

(defn velocity
  "Returns the velocity element of a local tuple (by convention, the third
  element).

  See [[coordinate]] for more detail."
  [local]
  {:pre [(up? local) (> (count local) 2)]}
  (nth local 2))

(defn acceleration
  "Returns the acceleration element of a local tuple (by convention, the fourth
  element).

  See [[coordinate]] for more detail."
  [local]
  {:pre [(up? local) (> (count local) 3)]}
  (nth local 3))

(defn state->n-dof [state]
  {:pre [(s/structure? state)]}
  (let [q (nth state 1)]
    (if (up? q)
      (count q)
      1)))

;; Selector aliases:

(def state->t time)
(def state->q coordinate)
(def state->qdot velocity)
(def state->qddot acceleration)

(def coordinates coordinate)
(def velocities velocity)
(def accelerations acceleration)

(def Q coordinate)
(def Qdot velocity)
(def Qdotdot acceleration)

;; TODO suggest mod to GJS
(defn literal-Lagrangian-state [n-dof]
  (up (gensym 't)
      (s/literal-up (gensym 'x) n-dof)
      (s/literal-up (gensym 'v) n-dof)))

;; ## Chapter 1
;;
;; Paths in the configuration manifold are functions that give a configuration
;; for each time. From such a path we can construct a path in the kinematic
;; state space. If such a path is described in terms of generalized coordinates,
;; we have

(comment
  (defn path->state-path* [q]
    (fn [t]
      (->local t
               (q t)
               ((D q) t)))))

(defn Gamma
  "Gamma takes a path function (from time to coordinates) to a state
  function (from time to local tuple)."
  ([q]
   (let [Dq (D q)]
     (-> (fn [t]
           (up t (q t) (Dq t)))
         (f/with-arity [:exactly 1]))))
  ([q n]
   {:pre [(> n 1)]}
   (let [Dqs   (take (dec n) (iterate D q))
         local (into (up identity) Dqs)]
     (-> (fn [t] (local t))
         (f/with-arity [:exactly 1])))))

(def path->state-path Gamma)

;; TODO ask GJS about this comment:
;;
;; Can we do it this way?

#_
(defn path->state [q]
  (->local identity q (D q)))

;; No... We don't know number of degrees of freedom when we build state vector.

;; ### Lagrangians
;;
;; A Lagrangian is an example of an L-function.
;; An L-function takes  a scalar argument and 2 vector arguments
;; (t, q, q-dot).  An L-function produces a scalar result.

(defn make-Lagrangian [kinetic-energy potential-energy]
  (- kinetic-energy potential-energy))

;; The following are the functions that are defined in the SICM
;; book, but NOT in MIT Scmutils.  Marked here for possible future
;; relocation. BUT they are great!
;; ---------------------------------------------------------------

(defn L-free-particle
  "The lagrangian of a free particle of mass m. The Lagrangian
  returned is a function of the local tuple. Since the particle
  is free, there is no potential energy, so the Lagrangian is
  just the kinetic energy."
  [mass]
  (fn [[_ _ v]]
    (* (/ 1 2) mass (g/square v))))

;; TODO tests:
;; (show-expression
;;  ((L-free-particle 'm)
;;   (->local 't
;;            (coordinate-tuple 'x 'y 'z)
;;            (velocity-tuple 'xdot 'ydot 'zdot))))
;; (+ (* 1/2 m (expt xdot 2))
;;    (* 1/2 m (expt ydot 2))
;;    (* 1/2 m (expt zdot 2)))

;; (show-expression
;;  ((compose
;;    (L-free-particle 'm)
;;    (Gamma (coordinate-tuple (literal-function 'x)
;;                             (literal-function 'y)
;;                             (literal-function 'z))))
;;   't))
;; (+ (* 1/2 (expt ((D x) t) 2) m)
;;    (* 1/2 (expt ((D y) t) 2) m)
;;    (* 1/2 (expt ((D z) t) 2) m))

(defn L-rectangular
  "Lagrangian for a point mass on with the potential energy V(x, y)"
  [m V]
  (fn [[_ [q0 q1] qdot]]
    (- (* (/ 1 2) m (g/square qdot))
       (V q0 q1))))

(defn L-harmonic
  "The Lagrangian of a simple harmonic oscillator (mass-spring
  system). m is the mass and k is the spring constant used in
  Hooke's law. The resulting Lagrangian is a function of the
  local tuple of the system."
  [m k]
  (fn [[_ q v]]
    (- (* (/ 1 2) m (g/square v)) (* (/ 1 2) k (g/square q)))))

;; (show-expression
;;  (((Lagrange-equations (L-harmonic 'm 'k))
;;    (literal-function 'x))
;;   't))
;; (+ (* k (x t)) (* m (((expt D 2) x) t)))

;; (show-expression
;;  (((Lagrange-equations (L-harmonic 'm 'k))
;;    (lambda (t) (* 'a (cos (+ (* 'omega t) 'phi)))))
;;   't))
;; (+ (* a k (cos (+ (* omega t) phi)))
;;    (* -1 a m (expt omega 2) (cos (+ (* omega t) phi))))

(defn L-uniform-acceleration
  "The Lagrangian of an object experiencing uniform acceleration
  in the negative y direction, i.e. the acceleration due to gravity"
  [m g]
  (fn [[_ [_ y] v]]
    (- (* (/ 1 2) m (g/square v)) (* m g y))))

;; (show-expression
;;  (((Lagrange-equations
;;     (L-uniform-acceleration 'm 'g))
;;    (coordinate-tuple (literal-function 'x)
;;                     (literal-function 'y)))
;;   't))
;; (down (* m (((expt D 2) x) t))
;;       (+ (* g m) (* m (((expt D 2) y) t))))

(defn L-central-rectangular [m U]
  (fn [[_ q v]]
    (- (* (/ 1 2) m (g/square v))
       (U (g/abs q)))))

;; (show-expression
;;  (((Lagrange-equations
;;     (L-central-rectangular 'm (literal-function 'V)))
;;    (coordinate-tuple (literal-function 'x) (literal-function 'y)))
;;   't))
;; (down
;;  (+ (* m (((expt D 2) x) t))
;;     (/ (* ((D V) (sqrt (+ (expt (x t) 2) (expt (y t) 2)))) (x t))
;;        (sqrt (+ (expt (x t) 2) (expt (y t) 2)))))
;;  (+ (* m (((expt D 2) y) t))
;;     (/ (* ((D V) (sqrt (+ (expt (x t) 2) (expt (y t) 2)))) (y t))
;;        (sqrt (+ (expt (x t) 2) (expt (y t) 2))))))


;; Consider planar motion in a central force field, with an arbitrary potential,
;; U, depending only on the radius. The generalized coordinates are polar.

(defn L-central-polar [m U]
  (fn [[_ [r] [rdot φdot]]]
    (- (* (/ 1 2) m
          (+ (g/square rdot)
             (g/square (* r φdot))))
       (U r))))

;; (show-expression
;;  (((Lagrange-equations
;;     (L-central-polar 'm (literal-function 'V)))
;;    (coordinate-tuple (literal-function 'r)
;;                     (literal-function 'phi)))
;;   't))
;; (down
;;  (+ (* -1 m (r t) (expt ((D phi) t) 2))
;;     (* m (((expt D 2) r) t))
;;     ((D V) (r t)))
;;  (+ (* 2 m ((D r) t) (r t) ((D phi) t))
;;     (* m (((expt D 2) phi) t) (expt (r t) 2))))

(defn L-Kepler-polar [GM m]
  (fn [[_ [r] [rdot phidot]]]
    (+ (* (/ 1 2) m
          (+ (g/square rdot)
             (g/square (* r phidot))) )
       (/ (* GM m) r))))

;;; Coupled harmonic oscillators.

(defn L-coupled-harmonic [m k]
  (fn [_ q qdot]
    (- (* (/ 1 2) qdot m qdot)
       (* (/ 1 2) q k q))))

;; (show-expression
;;   (((Lagrange-equations
;;      (L-coupled-harmonic (down (down 'm_1 0) (down 0 'm_2))
;;                         (down (down 'k_1 'c) (down 'c 'k_2))))
;;     (coordinate-tuple (literal-function 'x)
;;                      (literal-function 'y)))
;;    't))
;; (down (+ (* c (y t)) (* k_1 (x t)) (* m_1 (((expt D 2) x) t)))
;;       (+ (* c (x t)) (* k_2 (y t)) (* m_2 (((expt D 2) y) t))))


;; Pendulum of mass m2 and length b, hanging from a support of mass m1 that is
;; free to move horizontally (from Groesberg, Advanced Mechanics, p. 72)

(defn L-sliding-pend [m1 m2 b g]
  (fn [[_ [_ theta] [xdot thetadot]]]
    (let [rel-pend-vel (* b thetadot (velocity-tuple (cos theta) (sin theta)))
          pend-vel (+ rel-pend-vel (velocity-tuple xdot 0))
          Tpend (* (/ 1 2) m2 (g/square pend-vel))
          Tsupport (* (/ 1 2) m1 (g/square xdot))
          V (- (* m2 g b (cos theta)))]
      (+ Tpend Tsupport (- V)))))

;; (show-expression
;;  (((Lagrange-equations (L-sliding-pend 'm_1 'm_2 'b 'g))
;;    (coordinate-tuple (literal-function 'x)
;;                     (literal-function 'theta)))
;;   't))
;; (down
;;  (+ (* -1 b m_2 (sin (theta t)) (expt ((D theta) t) 2))
;;     (* b m_2 (((expt D 2) theta) t) (cos (theta t)))
;;     (* m_1 (((expt D 2) x) t))
;;     (* m_2 (((expt D 2) x) t)))
;;  (+ (* (expt b 2) m_2 (((expt D 2) theta) t))
;;     (* b g m_2 (sin (theta t)))
;;     (* b m_2 (((expt D 2) x) t) (cos (theta t)))))


;;; Nicer treatment
(defn F-sliding-pend [l]
  (fn [_ [x theta]]
    (up (up x 0)
        (up (+ x (* l (sin theta)))
            (* -1 l (cos theta))))))

(defn two-free [m1 m2 g]
  (fn [[_ [[_ h1] [_ h2]] [v1 v2]]]
    (- (+ (* 1/2 m1 (g/square v1))
          (* 1/2 m2 (g/square v2)))
       (+ (* m1 g h1)
          (* m2 g h2)))))

(declare F->C)

(defn L-sliding-pend* [m1 m2 l g]
  (compose (two-free m1 m2 g)
           (F->C (F-sliding-pend l))))

;; (show-expression
;;  (((Lagrange-equations
;;     (L-sliding-pend* 'm_1 'm_2 'b 'g))
;;    (up (literal-function 'x)
;;        (literal-function 'theta)))
;;   't))
;; (down
;;  (+ (* -1 b m_2 (sin (theta t)) (expt ((D theta) t) 2))
;;     (* b m_2 (((expt D 2) theta) t) (cos (theta t)))
;;     (* m_1 (((expt D 2) x) t))
;;     (* m_2 (((expt D 2) x) t)))
;;  (+ (* (expt b 2) m_2 (((expt D 2) theta) t))
;;     (* b g m_2 (sin (theta t)))
;;     (* b m_2 (cos (theta t)) (((expt D 2) x) t))))

;;; Consider a simple pendulum with Rayleigh dissipation:

(defn L-pendulum [g m l]
  (fn [[_ theta thetadot]]
    (+ (* (/ 1 2) m (g/square (* l thetadot)))
       (* g m l (cos theta)))))

(defn Rayleigh-dissipation [k]
  (fn [[_ _ qdot]]
    (* qdot k qdot)))

;; (show-expression
;;  (((Lagrange-equations (L-pendulum 'g 'm 'l)
;;                       (Rayleigh-dissipation 'k))
;;    (literal-function 'theta))
;;   't))
;; (+ (* 2 k ((D theta) t))
;;    (* g l m (sin (theta t)))
;;    (* (expt l 2) m (((expt D 2) theta) t)))

;;; Can group coordinates.  Procedures don't care.

(defn L-two-particle [m1 m2]
  (fn [[_ [x1 x2] [v1 v2]]]
    ;; TODO fix up sig!
    (let [V (f/literal-function 'V #_(-> (X (^ Real 2) (^ Real 2)) Real))]
      (- (+ (* (/ 1 2) m1 (g/square v1))
            (* (/ 1 2) m2 (g/square v2)))
         (V x1 x2)))))

;; (show-expression
;;  (((Lagrange-equations (L-two-particle 'm_1 'm_2))
;;    (coordinate-tuple
;;     (coordinate-tuple (literal-function 'x_1) (literal-function 'y_1))
;;     (coordinate-tuple (literal-function 'x_2) (literal-function 'y_2))))
;;   't))
;; (down
;;  (down
;;   (+ (* m_1 (((expt D 2) x_1) t))
;;      (((partial 0 0) V) (up (x_1 t) (y_1 t)) (up (x_2 t) (y_2 t))))
;;   (+ (* m_1 (((expt D 2) y_1) t))
;;      (((partial 0 1) V) (up (x_1 t) (y_1 t)) (up (x_2 t) (y_2 t)))))
;;  (down
;;   (+ (* m_2 (((expt D 2) x_2) t))
;;      (((partial 1 0) V) (up (x_1 t) (y_1 t)) (up (x_2 t) (y_2 t))))
;;   (+ (* m_2 (((expt D 2) y_2) t))
;;      (((partial 1 1) V) (up (x_1 t) (y_1 t)) (up (x_2 t) (y_2 t))))))

;; ---- end of functions undefined in Scmutils --------

;; Given a Lagrangian, we can obtain Lagrange's equations of motion.

(defn Lagrange-equations
  ([Lagrangian]
   (fn [q]
     (let [state-path (Gamma q)]
       (- (D (compose ((partial 2) Lagrangian) state-path))
          (compose ((partial 1) Lagrangian) state-path)))))
  ([Lagrangian dissipation-function]
   (fn [q]
     (let [state-path (Gamma q)]
       (- (D (compose ((partial 2) Lagrangian) state-path))
          (compose ((partial 1) Lagrangian) state-path)
          (- (compose ((partial 2) dissipation-function) state-path)))))))

;; (define (test-path t)
;;   (coordinate-tuple (+ (* 'a t) 'a0)
;;                    (+ (* 'b t) 'b0)
;;                    (+ (* 'c t) 'c0)))

;; (print-expression
;;  (((Lagrange-equations (L-free-particle 'm))
;;    test-path)
;;   't))
;; (down 0 0 0)

;; (show-expression
;;  (((Lagrange-equations (L-free-particle 'm))
;;    (literal-function 'x))
;;   't))
;; (* m (((expt D 2) x) t))

;; TODO verify that the new one is good!
#_
(defn Lagrangian->acceleration-old [L]
  (let [P ((partial 2) L)
        F ((partial 1) L)]
    (g/solve-linear-left
     ((partial 2) P)
     (- F
        (+ ((partial 0) P)
           (* ((partial 1) P) velocity))))))

(defn Lagrangian->acceleration
  ([L]
   (let [P ((partial 2) L)
         F ((partial 1) L)]
     (fn [state]
       ;; TODO can we remove the `state` fn wrapper here and below?
       (g/solve-linear-left
        (((partial 2) P) state)
        ((- F
            (+ ((partial 0) P)
               (* ((partial 1) P) velocity)))
         state)))))
  ([L dissipation-function]
   (let [P ((partial 2) L)
         F ((partial 1) L)]
     (fn [state]
       (g/solve-linear-left
        (((partial 2) P) state)
        ((- (- F
               ((partial 2) dissipation-function))
            (+ ((partial 0) P)
               (* ((partial 1) P) velocity)))
         state))))))

;; Thus, for example, we can obtain the general form of the vector of
;; accelerations as a function of the positions, and velocities:

;; (show-expression
;;  ((Lagrangian->acceleration (L-sliding-pend 'm_1 'm_2 'b 'g))
;;   (->local 't
;;           (coordinate-tuple 'x 'theta)
;;           (velocity-tuple 'xdot 'thetadot))))
;; (up
;;  (+
;;   (/ (* b m_2 (expt thetadot 2) (sin theta))
;;      (+ (* m_2 (expt (sin theta) 2)) m_1))
;;   (/ (* g m_2 (sin theta) (cos theta))
;;      (+ (* m_2 (expt (sin theta) 2)) m_1)))
;;  (+
;;   (/ (* -1 m_2 (expt thetadot 2) (sin theta) (cos theta))
;;      (+ (* m_2 (expt (sin theta) 2)) m_1))
;;   (/ (* -1 g m_1 (sin theta))
;;      (+ (* b m_2 (expt (sin theta) 2)) (* b m_1)))
;;   (/ (* -1 g m_2 (sin theta))
;;      (+ (* b m_2 (expt (sin theta) 2)) (* b m_1)))))

;; ### Lagrange equations in first-order form.

(defn qv->local-path [q v]
  (fn [t]
    (->local t (q t) (v t))))

(defn Lagrangian->state-derivative
  "Optionally takes a dissipation function."
  [L & opts]
  (let [acceleration (apply Lagrangian->acceleration L opts)]
    (fn [state]
      (up 1
          (velocity state)
          (acceleration state)))))

;; (print-expression
;;  ((Lagrangian->state-derivative (L-pendulum 'g 'm 'l)
;;                                 (Rayleigh-dissipation 'k))
;;   (up 't 'theta 'thetadot)))
;; (up 1
;;     thetadot
;;     (+ (/ (* -1 g (sin theta)) l)
;;        (/ (* -2 k thetadot) (* (expt l 2) m))))

(defn local-state-derivative
  "The state derivative of a Lagrangian is a function carrying a state
  tuple to its time derivative."
  [L]
  (Lagrangian->state-derivative L))

(defn Lagrange-equations-first-order [L]
  (fn [q v]
    (let [state-path (qv->local-path q v)]
      (- (D state-path)
         (compose (Lagrangian->state-derivative L)
                  state-path)))))

(def Lagrange-equations-1 Lagrange-equations-first-order)

;; (show-expression
;;  (((Lagrange-equations-1 (L-harmonic 'm 'k))
;;    (coordinate-tuple (literal-function 'x)
;;                      (literal-function 'y))
;;    (velocity-tuple (literal-function 'v_x)
;;                    (literal-function 'v_y)))
;;   't))
;; (up 0
;;     (up (+ ((D x) t) (* -1 (v_x t))) (+ ((D y) t) (* -1 (v_y t))))
;;     (up (+ (/ (* k (x t)) m) ((D v_x) t)) (+ (/ (* k (y t)) m) ((D v_y) t))))


;; Given a Lagrangian, we can make an energy function on (t, Q, Qdot).

(defn Lagrangian->energy [L]
  (let [P ((partial 2) L)]
    (- (* P velocity) L)))

;; On a trajectory there may be power lost (if dissipation) The following
;;  produces the power lost.

(defn Lagrangian->power-loss [L]
  (fn [q]
    (D (compose (Lagrangian->energy L)
                (Gamma q)))))

;; For example, on a specified trajectory, we can compute the energy, which
;; turns out to be T+V.

;; (show-expression
;;  ((compose
;;    (Lagrangian->energy (L-central-polar 'm (literal-function 'U)))
;;    (Gamma
;;     (coordinate-tuple (literal-function 'r) (literal-function 'phi))))
;;   't))
;; (+ (* 1/2 m (expt (r t) 2) (expt ((D phi) t) 2))
;;    (* 1/2 m (expt ((D r) t) 2))
;;    (U (r t)))


;; In fact, we can see how the energy is conserved:

;; (show-expression
;;  (((Lagrangian->power-loss (L-central-polar 'm (literal-function 'U)))
;;    (coordinate-tuple (literal-function 'r) (literal-function 'phi)))
;;   't))
;; (+ (* m (((expt D 2) phi) t) ((D phi) t) (expt (r t) 2))
;;    (* m (expt ((D phi) t) 2) (r t) ((D r) t))
;;    (* m (((expt D 2) r) t) ((D r) t))
;;    (* ((D U) (r t)) ((D r) t)))

;; This last expression is (nontrivially!) zero on any trajectory
;; which satisfies Lagrange's equations.

;; TODO note that these are in demo.clj

(defn T3-spherical [m]
  (fn [[_ [r theta] [rdot thetadot phidot]]]
    (* (/ 1 2) m
       (+ (g/square rdot)
          (g/square (* r thetadot))
          (g/square (* r (sin theta) phidot))))))

(defn L3-central [m Vr]
  (letfn [(Vs [[_ [r]]]
            (Vr r))]
    (- (T3-spherical m) Vs)))

;; (show-expression
;;  (((partial 1) (L3-central 'm (literal-function 'V)))
;;   (->local 't
;;            (coordinate-tuple 'r 'theta 'phi)
;;            (velocity-tuple 'rdot 'thetadot 'phidot))))
;; (down
;;  (+ (* m r (expt phidot 2) (expt (sin theta) 2))
;;     (* m r (expt thetadot 2))
;;     (* -1 ((D V) r)))
;;  (* m (expt r 2) (expt phidot 2) (cos theta) (sin theta))
;;  0)

;; (show-expression
;;  (((partial 2) (L3-central 'm (literal-function 'V)))
;;   (->local 't
;;            (coordinate-tuple 'r 'theta 'phi)
;;            (velocity-tuple 'rdot 'thetadot 'phidot))))
;; (down (* m rdot)
;;       (* m (expt r 2) thetadot)
;;       (* m (expt r 2) phidot (expt (sin theta) 2)))

;; TODO: these come from action.scm.

(defn Lagrangian-action
  ([L q t1 t2]
   (Lagrangian-action L q t1 t2 {}))
  ([L q t1 t2 integration-opts]
   (q/definite-integral
     (compose L (Gamma q)) t1 t2 integration-opts)))

;; in ch1 tests, but add more.

;; (define (test-path t)
;;   (coordinate-tuple (+ (* 4 t) 7)
;; 		                (+ (* 3 t) 5)
;; 		                (+ (* 2 t) 1)))

;; (Lagrangian-action (L-free-particle 3) test-path 0 10)
;;                                         ;Value: 435.

;; (define ((variation nu t1 t2 h) t)
;;   (* h (- t t1) (- t t2) (nu t)))

;; (define ((varied-free-particle-action mass path nu t1 t2) h)
;;   (let ((dpath (variation nu t1 t2 h)))
;;     (Lagrangian-action (L-free-particle mass)
;;                        (+ path dpath)
;;                        t1
;;                        t2)))

;; ((varied-free-particle-action 3.0 test-path
;;                               (coordinate-tuple sin cos square)
;;                               0.0 10.0)
;;  0.001)
;;                                         ;Value: 436.29121428571443

;; (minimize
;;  (varied-free-particle-action 3.0 test-path
;;                               (coordinate-tuple sin cos square)
;;                               0.0 10.0)
;;  -2.0 1.0)
;;                                         ;Value: (-5.828670879282072e-16 435.00000000000085 5)

(defn linear-interpolants [x0 x1 n]
  (let [n+1 (inc n)
        dx  (/ (- x1 x0) n+1)]
    (for [i (range 1 n+1)]
      (+ x0 (* i dx)))))

(defn Lagrange-interpolation-function
  "Given `ys` (a sequence of function values) and `xs` (an equal-length sequence
  of function inputs), returns a [[sicmutils.polynomial/Polynomial]] instance
  guaranteed to pass through all supplied `xs` and `ys`.

  The contract for inputs is that `(map vector xs ys)` should return a sequence
  of pairs of points."
  [ys xs]
  (p/from-points
   (map vector xs ys)))

(defn make-path
  "SICM p. 23n"
  [t0 q0 t1 q1 qs]
  (let [n (count qs)
        ts (linear-interpolants t0 t1 n)]
    (Lagrange-interpolation-function
     `[~q0 ~@qs ~q1]
     `[~t0 ~@ts ~t1])))

(defn parametric-path-action
  "SICM p. 23"
  [Lagrangian t0 q0 t1 q1]
  (fn [qs]
    (let [path (make-path t0 q0 t1 q1 qs)]
      (Lagrangian-action
       Lagrangian path t0 t1
       {:compile? false}))))

(defn find-path
  "SICM p. 23. The optional parameter values is a callback which will report
  intermediate points of the minimization."
  [Lagrangian t0 q0 t1 q1 n & {:keys [observe]}]
  (let [initial-qs    (linear-interpolants q0 q1 n)
        minimizing-qs (m/multidimensional-minimize
                       (parametric-path-action Lagrangian t0 q0 t1 q1)
                       initial-qs
                       :callback observe)]
    (make-path t0 q0 t1 q1 minimizing-qs)))

;; TODO: gamma-bar.scm

;; An alternative method allows taking derivatives in the construction of the
;; Lagrangian.

(defn osculating-path
  "Given a state tuple (of finite length), reconstitutes the initial segment of
  the Taylor series corresponding to the state tuple data as a function of t.

  Time is measured beginning at the point of time specified in the input state
  tuple."
  [state0]
  (let [[t0 q0] state0
        k (count state0)]
    (fn [t]
      (let [dt (- t t0)]
        (loop [n 2 sum q0 dt**n:n! dt]
          (if (= n k)
            sum
            (recur (inc n)
                   (+ sum (* (nth state0 n) dt**n:n!))
                   (/ (* dt**n:n! dt) n))))))))

(defn Gamma-bar [f]
  (fn [local]
    ((f (osculating-path local)) (first local))))

;; ### "Total Time Derivative"

(defn Dt-procedure [F]
  (fn DtF [state]
    (let [n (count state)]
      (letfn [(DF-on-path [q]
                (D (f/compose F (Gamma q (dec n)))))]
        ((Gamma-bar DF-on-path) state)))))

;; TODO this was the following before... test that it is better now?
#_
(defn Dt [F]
  (letfn [(G-bar [q]
            (D (compose F (Gamma q))))]
    (Gamma-bar G-bar)))

(def Dt (o/make-operator Dt-procedure 'Dt))

;; (print-expression
;;  ((Dt
;;    (lambda (state)
;;            (let ((t (time state))
;; 	               (q (coordinate state)))
;;              (square q))))
;;   (up 't (up 'x 'y) (up 'vx 'vy))))
;; (+ (* 2 vx x) (* 2 vy y))


;; (print-expression
;;  ((Dt (Dt (lambda (state) (coordinate state))))
;;   (up 't 'x 'v 'a 'j)))
;; a

;; (print-expression
;;  ((Dt (Dt (lambda (state)
;; 	                (square (coordinate state)))))
;;   (up 't 'x 'v 'a 'j)))
;; (+ (* 2 a x) (* 2 (expt v 2)))

;; (define L (literal-function 'L (Lagrangian 2)))

;; (print-expression
;;  ((Dt L) (up 't (up 'x 'y) (up 'vx 'vy))))
;; <error, not enuf args>

;; (print-expression
;;  ((Dt L) (up 't (up 'x 'y) (up 'vx 'vy) (up 'ax 'ay))))
;; (+ (* ax (((partial 2 0) L) (up t (up x y) (up vx vy))))
;;    (* ay (((partial 2 1) L) (up t (up x y) (up vx vy))))
;;    (* vx (((partial 1 0) L) (up t (up x y) (up vx vy))))
;;    (* vy (((partial 1 1) L) (up t (up x y) (up vx vy))))
;;    (((partial 0) L) (up t (up x y) (up vx vy))))

(defn- trim-last-argument [local]
  (s/up* (pop (s/structure->vector local))))

(defn Euler-Lagrange-operator [L]
  (- (Dt ((partial 2) L))
     ;; TODO test this new trim-last BS
     (compose ((partial 1) L) trim-last-argument)))

(def LE Euler-Lagrange-operator)
(def Lagrange-equations-operator Euler-Lagrange-operator)

;; Given a local tuple, produces a finite state.

;; (print-expression
;;  ((LE (L-harmonic 'm 'k))
;;   (up 't 'x 'v 'a)))
;; (+ (* a m) (* k x))

;; (print-expression
;;  ((LE (L-harmonic 'm 'k))
;;   (up 't #(x y) #(vx vy) #(ax ay))))
;; (down (+ (* ax m) (* k x))
;;       (+ (* ay m) (* k y)))


;; (print-expression
;;  ((LE L) (up 't (up 'x 'y) (up 'vx 'vy) (up 'ax 'ay))))
;; (down
;;  (+ (* ax (((partial 2 0) ((partial 2 0) L)) (up t (up x y) (up vx vy))))
;;     (* ay (((partial 2 0) ((partial 2 1) L)) (up t (up x y) (up vx vy))))
;;     (* vx (((partial 1 0) ((partial 2 0) L)) (up t (up x y) (up vx vy))))
;;     (* vy (((partial 1 1) ((partial 2 0) L)) (up t (up x y) (up vx vy))))
;;     (* -1 (((partial 1 0) L) (up t (up x y) (up vx vy))))
;;     (((partial 0) ((partial 2 0) L)) (up t (up x y) (up vx vy))))
;;  (+ (* ax (((partial 2 0) ((partial 2 1) L)) (up t (up x y) (up vx vy))))
;;     (* ay (((partial 2 1) ((partial 2 1) L)) (up t (up x y) (up vx vy))))
;;     (* vx (((partial 1 0) ((partial 2 1) L)) (up t (up x y) (up vx vy))))
;;     (* vy (((partial 1 1) ((partial 2 1) L)) (up t (up x y) (up vx vy))))
;;     (* -1 (((partial 1 1) L) (up t (up x y) (up vx vy))))
;;     (((partial 0) ((partial 2 1) L)) (up t (up x y) (up vx vy)))))


;; ;;; Adding extra state components is harmless, because L-harmonic does
;; ;;; not check the length of the jet.

;; (print-expression
;;  ((LE (L-harmonic 'm 'k))
;;   (up 't 'x 'v 'a 'j)))
;; (+ (* a m) (* k x))

;; ;;; But watch out.  If not enuf local componenents
;; ;;;  are specified we lose.

;; (print-expression
;;  ((LE (L-harmonic 'm 'k))
;;   (up 't 'x 'v)))
;;                                         ;Cannot extract velocity from #((*diff* ... ...) x)
;; ;;; error
;; 
;; (print-expression
;;  ((LE (L-central-polar 'm (literal-function 'V)))
;;   (up 't
;;       (up 'r 'phi)
;;       (up 'rdot 'phidot)
;;       (up 'rdotdot 'phidotdot))))
;; (down (+ (* -1 m (expt phidot 2) r) (* m rdotdot) ((D V) r))
;;       (+ (* 2 m phidot r rdot) (* m phidotdot (expt r 2))))

;; (print-expression
;;  ((compose (LE (L-central-polar 'm (literal-function 'V)))
;; 	         (Gamma
;; 	          (coordinate-tuple (literal-function 'r)
;; 			                        (literal-function 'phi))
;; 	          4))
;;   't))
;; (down
;;  (+ (* -1 m (expt ((D phi) t) 2) (r t))
;;     (* m (((expt D 2) r) t))
;;     ((D V) (r t)))
;;  (+ (* 2 m ((D r) t) ((D phi) t) (r t))
;;     (* m (((expt D 2) phi) t) (expt (r t) 2))))

(defn generalized-LE [Lagrangian]
  (fn [state]
    (let [m (count state)]
      (assert (and (> m 3) (even? m))
	            "Incorrect state size for Lagrange Equations")
      (letfn [(lp [i state]
                (if (zero? i)
	                0
	                (- (((g/expt Dt (dec i))
	                     ((partial i) Lagrangian))
	                    state)
	                   (lp (dec i) (trim-last-argument state)))))]
        (lp (quot m 2) state)))))

;; (define ((L2harmonic m k) state)
;;   (let ((x (coordinate state))
;; 	      (a (acceleration state)))
;;     (+ (* 1/2 m x a) (* 1/2 k (square x)))))

;; (print-expression
;;  ((generalized-LE (L2harmonic 'm 'k))
;;   (up 't 'x 'v 'a 'j 'p)))
;; (+ (* a m) (* k x))


;; (pe ((generalized-LE
;;       (literal-function 'L (-> (UP Real Real Real) Real)))
;;      (up 't 'x 'v 'a)))
;; (+ (* a (((partial 2) ((partial 2) L)) (up t x v)))
;;    (* v (((partial 1) ((partial 2) L)) (up t x v)))
;;    (((partial 0) ((partial 2) L)) (up t x v))
;;    (* -1 (((partial 1) L) (up t x v))))


;; (pe ((generalized-LE
;;       (literal-function 'L (-> (UP Real Real Real Real) Real)))
;;      (up 't 'x 'v 'a 'j 'p)))
;; (+ (* (expt a 2) (((partial 2) ((partial 2) ((partial 3) L))) (up t x v a)))
;;    (* 2 a j (((partial 2) ((partial 3) ((partial 3) L))) (up t x v a)))
;;    (* 2 a v (((partial 1) ((partial 2) ((partial 3) L))) (up t x v a)))
;;    (* (expt j 2) (((partial 3) ((partial 3) ((partial 3) L))) (up t x v a)))
;;    (* 2 j v (((partial 1) ((partial 3) ((partial 3) L))) (up t x v a)))
;;    (* (expt v 2) (((partial 1) ((partial 1) ((partial 3) L))) (up t x v a)))
;;    (* 2 a (((partial 0) ((partial 2) ((partial 3) L))) (up t x v a)))
;;    (* a (((partial 1) ((partial 3) L)) (up t x v a)))
;;    (* -1 a (((partial 2) ((partial 2) L)) (up t x v a)))
;;    (* 2 j (((partial 0) ((partial 3) ((partial 3) L))) (up t x v a)))
;;    (* p (((partial 3) ((partial 3) L)) (up t x v a)))
;;    (* 2 v (((partial 0) ((partial 1) ((partial 3) L))) (up t x v a)))
;;    (* -1 v (((partial 1) ((partial 2) L)) (up t x v a)))
;;    (((partial 0) ((partial 0) ((partial 3) L))) (up t x v a))
;;    (* -1 (((partial 0) ((partial 2) L)) (up t x v a)))
;;    (((partial 1) L) (up t x v a)))

;; ### Coordinate Transformation to State Transformation

;; TODO fill in the rest of the Lagrangian-transformations.scm code.

(defn F->C
  "Accepts a coordinate transformation `F` from a local tuple to a new coordinate
  structure, and returns a function from `local -> local` that applies the
  transformation directly.

  [[F->C]] handles local tuples of arbitrary length."
  [F]
  (fn C [local]
    (let [n (count local)
          f-bar (fn [q-prime]
                  (let [q (compose F (Gamma q-prime))]
                    (Gamma q n)))]
      ((Gamma-bar f-bar) local))))

;; The following transformations are applicable to configuration coordinates.

(defn rectangular->polar [[x y]]
  (let [r (g/sqrt
           (+ (g/square x)
              (g/square y)))
        phi (g/atan y x)]
    (up r phi)))

(defn r->p [tqv]
  (rectangular->polar
   (coordinate tqv)))

(defn polar->rectangular [[r phi]]
  (let [x (* r (cos phi))
        y (* r (sin phi))]
    (up x y)))

(defn p->r
  "SICM p. 47. Polar to rectangular coordinates of state."
  [tqv]
  (polar->rectangular
   (coordinate tqv)))

;; (show-expression
;;  (velocity
;;   ((F->C p->r)
;;    (->local 't
;; 	          (coordinate-tuple 'r 'phi)
;; 	          (velocity-tuple 'rdot 'phidot)))))
;; (up (+ (* -1 r phidot (sin phi)) (* rdot (cos phi)))
;;     (+ (* r phidot (cos phi)) (* rdot (sin phi))))


;; (define (L-central-polar m V)
;;   (compose (L-central-rectangular m V)
;; 	         (F->C p->r)))

;; (show-expression
;;  ((L-central-polar 'm (literal-function 'V))
;;   (->local 't (coordinate-tuple 'r 'phi)
;;            (velocity-tuple 'rdot 'phidot))))
;; (+ (* 1/2 m (expt phidot 2) (expt r 2))
;;    (* 1/2 m (expt rdot 2))
;;    (* -1 (V r)))

;; ### Driven pendulum example

;; (define ((T-pend m l g ys) local)
;;   (let ((t (time local))
;;         (theta (coordinate local))
;;         (thetadot (velocity local)))
;;     (let ((ysdot (D ys)))
;;       (* 1/2 m
;;          (+ (square (* l thetadot))
;;             (square (ysdot t))
;;             (* 2 (ysdot t) l (sin theta) thetadot))))))

;; (define ((V-pend m l g ys) local)
;;   (let ((t (time local))
;;         (theta (coordinate local)))
;;     (* m g (- (ys t) (* l (cos theta))))))

;; (define L-pend (- T-pend V-pend))

;; (show-expression
;;  ((L-pend 'm 'l 'g (literal-function 'y_s))
;;   (->local 't 'theta 'thetadot)))
;; (+ (* 1/2 (expt l 2) m (expt thetadot 2))
;;    (* l m thetadot ((D y_s) t) (sin theta))
;;    (* g l m (cos theta))
;;    (* -1 g m (y_s t))
;;    (* 1/2 m (expt ((D y_s) t) 2)))

;; (show-expression
;;  (((Lagrange-equations
;;     (L-pend 'm 'l 'g (literal-function 'y_s)))
;;    (literal-function 'theta))
;;   't))
;; (+ (* g l m (sin (theta t)))
;;    (* (expt l 2) m (((expt D 2) theta) t))
;;    (* l m (((expt D 2) y_s) t) (sin (theta t))))

;;; Same driven pendulum by coordinate transformation

;; (define ((Lf m g) local)
;;   (let ((q (coordinate local))
;;         (v (velocity local)))
;;     (let ((h (ref q 1)))
;;       (- (* 1/2 m (square v)) (* m g h)))))

;; (define ((dp-coordinates l y_s) local)
;;   (let ((t (time local))
;; 	      (theta (coordinate local)))
;;     (let ((x (* l (sin theta)))
;; 	        (y (- (y_s t) (* l (cos theta)))))
;;       (coordinate-tuple x y))))

;; (define (L-pend m l g y_s)
;;   (compose (Lf m g)
;;            (F->C (dp-coordinates l y_s))))

;; (show-expression
;;  ((L-pend 'm 'l 'g (literal-function 'y_s))
;;   (->local 't 'theta 'thetadot)))
;; (+ (* 1/2 (expt l 2) m (expt thetadot 2))
;;    (* l m thetadot (sin theta) ((D y_s) t))
;;    (* g l m (cos theta))
;;    (* -1 g m (y_s t))
;;    (* 1/2 m (expt ((D y_s) t) 2)))

;; (show-expression
;;  (((Lagrange-equations
;;     (L-pend 'm 'l 'g (literal-function 'y_s)))
;;    (literal-function 'theta))
;;   't))
;; (+ (* g l m (sin (theta t)))
;;    (* (expt l 2) m (((expt D 2) theta) t))
;;    (* l m (((expt D 2) y_s) t) (sin (theta t))))

;; ### Spherical Coordinates (radius, colatitude, longitude)

(defn spherical->rectangular [[r theta phi]]
  (let [x (* r (sin theta) (cos phi))
        y (* r (sin theta) (sin phi))
        z (* r (cos theta))]
    (coordinate-tuple x y z)))

(defn s->r
  "SICM p. 83"
  [local]
  (spherical->rectangular
   (coordinate local)))

(defn rectangular->spherical [[x y z]]
  (let [r (g/sqrt (+ (* x x) (* y y) (* z z)))
        theta (g/acos (/ z r))
        phi (g/atan y x)]
    (coordinate-tuple r theta phi)))

(defn r->s [local]
  (rectangular->spherical
   (coordinate local)))

;; (define (L3-central m Vr)
;;   (define (Vs local)
;;     (let ((r (ref (coordinate local) 0)))
;;       (Vr r)))
;;   (- (T3-spherical m) Vs))

;; (define ((ang-mom-z m) local)
;;   (let ((q (coordinate local))
;;         (v (velocity local)))
;;     (ref (cross-product q (* m v)) 2)))

;; (show-expression
;;  ((compose (ang-mom-z 'm) (F->C s->r))
;;   (->local 't
;;            (coordinate-tuple 'r 'theta 'phi)
;;            (velocity-tuple 'rdot 'thetadot 'phidot))))
;; (* m (expt r 2) phidot (expt (sin theta) 2))

;; (show-expression
;;  ((Lagrangian->energy
;;    (L3-central 'm (literal-function 'V)))
;;   (->local 't
;;            (coordinate-tuple 'r 'theta 'phi)
;;            (velocity-tuple 'rdot 'thetadot 'phidot))))
;; (+ (* 1/2 m (expt r 2) (expt phidot 2) (expt (sin theta) 2))
;;    (* 1/2 m (expt r 2) (expt thetadot 2))
;;    (* 1/2 m (expt rdot 2))
;;    (V r))

;; TODO the rest of the Rx, Ry and Rz are in rotation.cljc.
