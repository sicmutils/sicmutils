;; # Exercise 1.44: The double pendulum

;; This namespace explores [Exercise
;; 1.44](https://tgvaughan.github.io/sicm/chapter001.html#Exe_1-44) from Sussman
;; and Wisdom's [Structure and Interpretation of Classical
;; Mechanics](https://tgvaughan.github.io/sicm/), using
;; the [SICMUtils](https://github.com/sicmutils/sicmutils) Clojure library and
;; the Clerk rendering environment.

(ns sicmutils.expression.pendulum
  (:refer-clojure
   :exclude [+ - * / partial ref zero? numerator denominator compare = run!])
  (:require [nextjournal.clerk :as clerk]
            [sicmutils.env :as e :refer :all]
            [sicmutils.expression.render :as xr]))

;; ## Lagrangian
;;
;; Start with a coordinate transformation from `theta1`, `theta2` to rectangular
;; coordinates. We'll generate our Lagrangian by composing this with an rectangular
;; Lagrangian with the familiar form of `T - V`.

(defn angles->rect [l1 l2]
  (fn [[_ [theta1 theta2]]]
    (let [x1 (* l1 (sin theta1))
          y1 (- (* l1 (cos theta1)))
          x2 (+ x1 (* l2 (sin (+ theta1 theta2))))
          y2 (- y1 (* l2 (cos (+ theta1 theta2))))]
      (up x1 y1 x2 y2))))

;; `T` describes the sum of the kinetic energy of two particles in rectangular
;; coordinates.

(defn T [m1 m2]
  (fn [[_ _ [xdot1 ydot1 xdot2 ydot2]]]
    (+ (* (/ 1 2) m1 (+ (square xdot1)
                        (square ydot1)))
       (* (/ 1 2) m2 (+ (square xdot2)
                        (square ydot2))))))


;; `V` describes a uniform gravitational potential with coefficient `g`, acting
;; on two particles with masses of, respectively, `m1` and `m2`. Again, this is
;; written in rectangular coordinates.

(defn V [m1 m2 g]
  (fn [[_ [_ y1 _ y2]]]
    (+ (* m1 g y1)
       (* m2 g y2))))

;; Form the rectangular Lagrangian `L` by subtracting `(V m1 m2 g)` from `(T m1 m2)`:

(defn L-rect [m1 m2 g]
  (- (T m1 m2)
     (V m1 m2 g)))

;; Form the final Langrangian in generalized coordinates (the angles of each
;; segment) by composing `L-rect` with a properly transformed `angles->rect`
;; coordinate transform!

(defn L-double-pendulum [m1 m2 l1 l2 g]
  (compose (L-rect m1 m2 g)
           (F->C
            (angles->rect l1 l2))))

;; The Lagrangian is big and hairy:

(def symbolic-L
  ((L-double-pendulum 'm_1 'm_2 'l_1 'l_2 'g)
   (up 't
       (up 'theta_1 'theta_2)
       (up 'theta_1dot 'theta_2dot))))

;; Let's simplify that:

(simplify symbolic-L)

;; Better yet, let's render it as LaTeX, and create a helper function,
;; `render-eq` to make it easier to render simplified equations:

(def render-eq
  (comp clerk/tex ->TeX simplify))

(render-eq symbolic-L)

;; And here are the equations of motion for the system:

(let [L (L-double-pendulum 'm_1 'm_2 'l_1 'l_2 'g)]
  (binding [xr/*TeX-vertical-down-tuples* true]
    (render-eq
     (((Lagrange-equations L)
       (up (literal-function 'theta_1)
           (literal-function 'theta_2)))
      't))))

;; What do these mean?
;;
;; - the system has two degrees of freedom: $\theta_1$ and $\theta_2$.
;; - at any point `t` in time, the two equations above, full of first and second
;; - order derivatives of the position functions, will stay true
;; - the system can use these equations to simulate the system, one tick at a time.

;; ## Simulation
;;
;; Next, let's run a simulation using those equations of motion and collect data
;; on each coordinate's evolution.
;;
;; Here are the constants specified in exercise 1.44:
;;
;; masses in kg:

(def m1 1.0)
(def m2 3.0)

;; lengths in meters:

(def l1 1.0)
(def l2 0.9)

;; `g` in units of m/s^2:

(def g 9.8)

;; And two sets of initial pairs of `theta1`, `theta2` angles corresponding to
;; chaotic and regular initial conditions:

(def chaotic-initial-q (up (/ Math/PI 2) Math/PI))
(def regular-initial-q (up (/ Math/PI 2) 0.0))

;; Composing `Lagrangian->state-derivative` with `L-double-pendulum` produces
;; a state derivative that we can use with our ODE solver:

(def state-derivative
  (compose
   Lagrangian->state-derivative
   L-double-pendulum))

;; Finally, two default parameters for our simulation. We'll record data in
;; steps of 0.01 seconds, and simulate to a horizon of 50 seconds.

(def step 0.01)
(def horizon 50)

;; `run!` will return a sequence of 5001 states, one for each measured point in
;; the simulation. The smaller-arity version simply passes in default masses and
;; lengths, but you can override those with the larger arity version if you like.

;; (The interface here could use some work: `integrate-state-derivative` tidies
;; this up a bit, but I want it out in the open for now.)

(defn run!
  ([step horizon initial-coords]
   (run! step horizon l1 l2 m1 m2 g initial-coords))
  ([step horizon l1 l2 m1 m2 g initial-coords]
   (let [collector     (atom (transient []))
         initial-state (up 0.0
                           initial-coords
                           (up 0.0 0.0))]
     ((evolve state-derivative m1 m2 l1 l2 g)
      initial-state
      step
      horizon
      {:compile? true
       :epsilon 1.0e-13
       :observe (fn [_ state]
                  (swap!
                   collector conj! state))})
     (persistent! @collector))))

(time (do (run! step horizon chaotic-initial-q) nil))
