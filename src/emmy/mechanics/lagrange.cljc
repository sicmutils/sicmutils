#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.mechanics.lagrange
  (:refer-clojure :exclude [+ - * / partial time])
  (:require [pattern.rule :as r]
            [emmy.calculus.derivative :refer [D partial]]
            [emmy.function :as f :refer [compose]]
            [emmy.generic :as g :refer [cos sin + - * /]]
            [emmy.numerical.minimize :as m]
            [emmy.numerical.quadrature :as q]
            [emmy.operator :as o]
            [emmy.polynomial :as p]
            [emmy.structure :as s :refer [down up up?]]))

;; ## Variational Mechanics

(def coordinate-tuple up)
(def velocity-tuple up)
(def acceleration-tuple up)
(def momentum-tuple down)

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
;; Kinematic states and their derivatives are represented as Scheme vectors,
;; with components time, configuration, and derivatives.

(defn Lagrangian
  "Returns a function signature for a Lagrangian with n degrees of freedom (or an
  unrestricted number if n is not given).

  Useful for constructing Lagrangian literal functions."
  ([] '(-> (UP Real (UP* Real) (UP* Real)) Real))
  ([n]
   (r/template
    (-> (UP Real (UP* Real ~n) (UP* Real ~n)) Real))))

(defn ->L-state
  "Given a time `t`, coordinate tuple (or scalar) `q`, velocity tuple (or scalar)
  `qdot` and any number of additional higher-order derivative tuples (or
  scalars), returns a 'Local tuple', ie, the state expected by a Lagrangian."
  [t q qdot & derivs]
  (apply up t q qdot derivs))

(def ^{:doc "Alias for [[->L-state]]."}
  ->local ->L-state)

(def ^{:doc "Alias for [[->L-state]]."}
  ->state ->L-state)

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

;; Aliases for the selectors above, included for parity with scmutils:

(def ^{:doc "Alias for [[time]]."}
  state->t time)

(def ^{:doc "Alias for [[coordinate]]."}
  state->q coordinate)

(def ^{:doc "Alias for [[velocity]]."}
  state->qdot velocity)

(def ^{:doc "Alias for [[acceleration]]."}
  state->qddot acceleration)

(def ^{:doc "Alias for [[coordinate]]."}
  coordinates coordinate)

(def ^{:doc "Alias for [[velocity]]."}
  velocities velocity)

(def ^{:doc "Alias for [[acceleration]]."}
  accelerations acceleration)

(def ^{:doc "Alias for [[coordinate]]."}
  Q coordinate)

(def ^{:doc "Alias for [[velocity]]."}
  Qdot velocity)

(def ^{:doc "Alias for [[acceleration]]."}
  Qdotdot acceleration)

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

#_{:clj-kondo/ignore [:redundant-fn-wrapper]}
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

;; ### Lagrangians
;;
;; A Lagrangian is an example of an L-function.
;; An L-function takes  a scalar argument and 2 vector arguments
;; (t, q, q-dot).  An L-function produces a scalar result.

(defn make-Lagrangian [kinetic-energy potential-energy]
  (- kinetic-energy potential-energy))

;; ## Library of Lagrangians
;;
;; These should arguably live in their own place.

(defn L-free-particle
  "The lagrangian of a free particle of mass m. The Lagrangian
  returned is a function of the local tuple. Since the particle
  is free, there is no potential energy, so the Lagrangian is
  just the kinetic energy."
  [mass]
  (fn [[_ _ v]]
    (* (/ 1 2) mass (g/square v))))

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

(defn L-uniform-acceleration
  "The Lagrangian of an object experiencing uniform acceleration
  in the negative y direction, i.e. the acceleration due to gravity"
  [m g]
  (fn [[_ [_ y] v]]
    (- (* (/ 1 2) m (g/square v)) (* m g y))))

(defn L-central-rectangular [m U]
  (fn [[_ q v]]
    (- (* (/ 1 2) m (g/square v))
       (U (g/abs q)))))

(defn L-central-polar
  "Consider planar motion in a central force field, with an arbitrary potential,
  `U`, depending only on the radius. The generalized coordinates are polar."
  [m U]
  (fn [[_ [r] [rdot φdot]]]
    (- (* (/ 1 2) m
          (+ (g/square rdot)
             (g/square (* r φdot))))
       (U r))))

(defn L-Kepler-polar [GM m]
  (fn [[_ [r] [rdot phidot]]]
    (+ (* (/ 1 2) m
          (+ (g/square rdot)
             (g/square (* r phidot))) )
       (/ (* GM m) r))))

(defn L-axisymmetric-top [A C gMR]
  (fn [[_ [theta] [thetadot phidot psidot]]]
    (+ (* (/ 1 2) A
          (+ (g/square thetadot)
             (g/square (* phidot (sin theta)))))
       (* (/ 1 2) C
          (g/square (+ psidot (* phidot (cos theta)))))
       (* -1 gMR (cos theta)))))

;; Coupled harmonic oscillators.

(defn L-coupled-harmonic [m k]
  (fn [[_ q qdot]]
    (- (* (/ 1 2) qdot m qdot)
       (* (/ 1 2) q k q))))

(defn ^:no-doc F-sliding-pend [l]
  (fn [[_ [x theta]]]
    (up (up x 0)
        (up (+ x (* l (sin theta)))
            (* -1 l (cos theta))))))

(defn ^:no-doc two-free [m1 m2 g]
  (fn [[_ [[_ h1] [_ h2]] [v1 v2]]]
    (- (* (/ 1 2)
          (+ (* m1 (g/square v1))
             (* m2 (g/square v2))))
       (* g (+ (* m1 h1)
               (* m2 h2))))))

(declare F->C)

(defn L-sliding-pend
  "Pendulum of mass m2 and length b, hanging from a support of mass m1 that is
  free to move horizontally (from Groesberg, Advanced Mechanics, p. 72)"
  [m1 m2 l g]
  (compose (two-free m1 m2 g)
           (F->C (F-sliding-pend l))))

;; Consider a simple pendulum with Rayleigh dissipation:

(defn L-pendulum [g m l]
  (fn [[_ theta thetadot]]
    (+ (* (/ 1 2) m (g/square (* l thetadot)))
       (* g m l (cos theta)))))

(defn Rayleigh-dissipation [k]
  (fn [[_ _ qdot]]
    (* qdot k qdot)))

(defn L-two-particle [m1 m2 V]
  (fn [[_ [x1 x2] [v1 v2]]]
    (- (+ (* (/ 1 2) m1 (g/square v1))
          (* (/ 1 2) m2 (g/square v2)))
       (V x1 x2))))

;; Given a Lagrangian, we can obtain Lagrange's equations of motion.

(defn Lagrange-equations
  ([L]
   (Lagrange-equations L nil))
  ([L dissipation-fn]
   (fn [q]
     (let [state-path (Gamma q)]
       (- (D (compose ((partial 2) L) state-path))
          (compose ((partial 1) L) state-path)
          (if dissipation-fn
            (- (compose ((partial 2) dissipation-fn) state-path))
            0))))))

(defn Lagrangian->acceleration
  ([L]
   (Lagrangian->acceleration L nil))
  ([L dissipation-fn]
   (let [P ((partial 2) L)
         F ((partial 1) L)]
     (g/solve-linear-left
      ((partial 2) P)
      (- F
         (if dissipation-fn
           ((partial 2) dissipation-fn)
           0)
         (+ ((partial 0) P)
            (* ((partial 1) P) velocity)))))))

;; ### Lagrange equations in first-order form

(defn qv->local-path [q v]
  (fn [t]
    (->local t (q t) (v t))))

(defn Lagrangian->state-derivative
  "Optionally takes a dissipation function."
  ([L]
   (Lagrangian->state-derivative L nil))
  ([L dissipation-fn]
   (let [acceleration (Lagrangian->acceleration L dissipation-fn)]
     (fn [state]
       (up 1
           (velocity state)
           (acceleration state))))))

(defn local-state-derivative
  "The state derivative of a Lagrangian is a function carrying a state tuple to
  its time derivative.

  Alias for the non-dissipative, single-arity version
  of [[Lagrangian->state-derivative]]."
  [L]
  (Lagrangian->state-derivative L nil))

(defn Lagrange-equations-first-order [L]
  (fn [q v]
    (let [state-path (qv->local-path q v)]
      (- (D state-path)
         (compose (Lagrangian->state-derivative L)
                  state-path)))))

(def ^{:doc "Alias for [[Lagrange-equations-first-order]]."}
  Lagrange-equations-1
  Lagrange-equations-first-order)

;; Given a Lagrangian, we can make an energy function on (t, Q, Qdot).

(defn Lagrangian->energy [L]
  (let [P ((partial 2) L)]
    (- (* P velocity) L)))

;; On a trajectory there may be power lost (if dissipation) The following
;;  produces the power lost.

(defn Lagrangian->power-loss [L]
  (fn [q]
    (D (compose
        (Lagrangian->energy L)
        (Gamma q)))))

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

(defn Lagrangian-action
  ([L q t1 t2]
   (Lagrangian-action L q t1 t2 {}))
  ([L q t1 t2 integration-opts]
   (q/definite-integral
     (compose L (Gamma q)) t1 t2 integration-opts)))

(defn linear-interpolants [x0 x1 n]
  (let [n+1 (inc n)
        dx  (/ (- x1 x0) n+1)]
    (for [i (range 1 n+1)]
      (+ x0 (* i dx)))))

(defn Lagrange-interpolation-function
  "Given `ys` (a sequence of function values) and `xs` (an equal-length sequence
  of function inputs), returns a [[emmy.polynomial/Polynomial]] instance
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

(def Dt (o/make-operator Dt-procedure 'Dt))

(defn- trim-last-argument [local]
  (s/up* (pop (s/structure->vector local))))

(defn Euler-Lagrange-operator [L]
  (- (Dt ((partial 2) L))
     (compose ((partial 1) L) trim-last-argument)))

(def ^{:doc "Alias for [[Euler-lagrange-operator]]."}
  LE
  Euler-Lagrange-operator)

(def ^{:doc "Alias for [[Euler-lagrange-operator]]."}
  Lagrange-equations-operator
  Euler-Lagrange-operator)

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

;; ### Coordinate Transformation to State Transformation

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
  (let [r (g/sqrt (+ (g/square x)
                     (g/square y)
                     (g/square z)))
        theta (g/acos (/ z r))
        phi (g/atan y x)]
    (coordinate-tuple r theta phi)))

(defn r->s [local]
  (rectangular->spherical
   (coordinate local)))
