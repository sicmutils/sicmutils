#_"SPDX-License-Identifier: GPL-3.0"

(ns sicmutils.mechanics.pendulum
  (:refer-clojure :exclude [+ - * /])
  (:require [sicmutils.generic :as g :refer [sin cos + - * /]]
            [sicmutils.mechanics.hamilton :as h]
            [sicmutils.mechanics.lagrange :as l]
            [sicmutils.numerical.ode :as ode]
            [sicmutils.numerical.roots.bisect :as bi]
            [sicmutils.special.elliptic :as ell]
            [sicmutils.value :as v]))

;; ## THE PENDULUM

;; definition H = p^2/(2alpha) - beta cos(theta)
;; ASSUME alpha > 0 and beta > 0
;; alpha = ml^2  beta = mgl for ordinary pendulum

(defn Hpendulum [alpha beta]
  (fn [state]
    (let [theta  (l/state->q state)
          ptheta (h/state->p state)]
      (- (/ (g/square ptheta)
            (* 2 alpha))
         (* beta (cos theta))))))

(defn pendulum-sysder [alpha beta]
  (h/Hamiltonian->state-derivative
   (Hpendulum alpha beta)))

(def pendulum-Hamiltonian Hpendulum)

;; ## oscillating case

(declare pendulum-frequency)

(defn pendulum-oscillating-frequency [alpha beta E]
  (let [k (g/sqrt (/ (+ E beta)
                     (* 2.0 beta)))
        omega0 (g/sqrt (g/abs (/ beta alpha)))]
    (/ (* Math/PI omega0)
       (* 2.0 (ell/elliptic-k k)))))

(defn pendulum-oscillating-angle [alpha beta E]
  (let [k (g/sqrt (/ (+ E beta) (* 2.0 beta)))
        omega-0 (g/sqrt (/ beta alpha))]
    (fn [t]
      (ell/jacobi-elliptic-functions
       (* omega-0 t)
       k
       (fn [sn _cn _dn]
         (* 2.0 (g/asin (* k sn))))))))

(defn pendulum-oscillating-angular-momentum [alpha beta E]
  (let [k (g/sqrt (/ (+ E beta) (* 2.0 beta)))
        omega-0 (g/sqrt (/ beta alpha))]
    (fn [t]
      (ell/jacobi-elliptic-functions
       (* omega-0 t)
       k
       (fn [_sn cn _dn]
         (* 2.0 alpha omega-0 k cn))))))

;; freq = (/ (* pi omega0) (* 2 (ell/elliptic-k k)))
;; period = 4 K / omega0

;; omega0 period = 4 K the period of sn

(defn pendulum-oscillating-action [alpha beta E]
  (let [k**2 (/ (+ beta E) (* 2.0 beta))]
    (if (= k**2 1.0)
      (* (/ 8.0 Math/PI) (g/sqrt (* beta alpha)))
      (ell/elliptic-integrals
       (Math/sqrt k**2)
       (fn [Kk Ek]
         (* (/ 8.0 Math/PI)
            (g/sqrt (* beta alpha))
            (- Ek (* (- 1.0 k**2) Kk))))))))

(defn pendulum-f [k]
  (if (= k 1.0)
    1.0
    (ell/elliptic-integrals
     k
     (fn [Kk Ek]
       (- Ek (* (- 1.0 (g/square k)) Kk))))))

(defn pendulum-g [k]
  (/ (ell/elliptic-e k) k))

(defn pendulum-inverse-g [gk]
  (let [inv-gk (/ 1.0 gk)]
    (:result
     (bi/bisect (fn [k]
                  (if (zero? k)
                    (- inv-gk)
                    (- (/ 1.0 (pendulum-g k)) inv-gk)))
                0.0 1.0 1.e-10))))

(defn pendulum-inverse-f [fk]
  (let [sfk (g/sqrt fk)]
    (:result
     (bi/bisect (fn [k]
                  (- (g/sqrt (pendulum-f k)) sfk))
                0.0 1.0 1e-10))))

(defn pendulum-oscillating-action-to-E [alpha beta]
  (fn [action]
    (let [f (/ action (* (/ 8.0 Math/PI)
                         (g/sqrt (* beta alpha))))
          k (pendulum-inverse-f f)]
      (* beta (- (* 2.0 (g/square k)) 1.0)))))

;; action angle -pi to pi

(defn pendulum-oscillating-phase [alpha beta]
  (let [omega-0 (g/sqrt (/ beta alpha))]
    (fn [[_ theta ptheta :as state]]
      (let [E ((Hpendulum alpha beta) state)]
        (if (> E (- beta))
          (let [k (g/sqrt (/ (+ E beta)
                             (* 2.0 beta)))
                period (/ v/twopi
                          (pendulum-frequency
                           alpha beta E))
                sin-phi (/ (sin (/ theta 2.0)) k)
                dt0 (/ (ell/elliptic-f (g/asin sin-phi) k) omega-0)
                dt (if (< ptheta 0) (- (/ period 2.0) dt0) dt0)]
            ((v/principal-value Math/PI) (* v/twopi (/ dt period))))
          (throw
           (ex-info
            "at the fixed point the phase is undefined" {})))))))

;;; time from theta=0 to state
(defn pendulum-oscillating-dt [alpha beta]
  (fn [state]
    (let [E ((Hpendulum alpha beta) state)
          phase ((pendulum-oscillating-phase alpha beta) state)
          period (/ v/twopi (pendulum-frequency alpha beta E))]
      (* phase (/ period v/twopi)))))

(defn pendulum-oscillating-aa-state-to-state [alpha beta]
  (fn [[t angle action]]
    (let [E ((pendulum-oscillating-action-to-E alpha beta) action)
          period (/ v/twopi (pendulum-frequency alpha beta E))
          dt (* (/ period v/twopi) angle)]
      (h/->H-state t
                   ((pendulum-oscillating-angle alpha beta E) dt)
                   ((pendulum-oscillating-angular-momentum alpha beta E) dt)))))

(defn pendulum-oscillating-state-to-aa-state [alpha beta]
  (fn [state]
    (let [E ((Hpendulum alpha beta) state)
          action (pendulum-oscillating-action alpha beta E)
          angle ((pendulum-oscillating-phase alpha beta) state)]
      (h/->H-state (l/state->t state) angle action))))

;;;----------------------------------------------------------------
;;; circulating case

(defn principal-range
  "Defined in kernel/numeric.scm"
  [period]
  (fn [t]
    (let [t (- t (* period (g/floor (/ t period))))]
      (if (< t (/ period 2.0))
        t
        (- t period)))))

(defn pendulum-circulating-frequency [alpha beta E]
  (let [k (g/sqrt (/ (* 2.0 beta) (+ E beta)))
        omegaR (g/sqrt (g/abs (/ (+ E beta) (* 2.0 alpha))))]
    (/ (* Math/PI omegaR) (ell/elliptic-k k))))

(defn pendulum-circulating-angle [alpha beta E]
  (let [k (g/sqrt (/ (* 2.0 beta) (+ E beta)))
        omega-R (g/sqrt (g/abs (/ (+ E beta) (* 2.0 alpha))))
        period (/ v/twopi (pendulum-frequency alpha beta E))]
    (fn [t]
      (ell/jacobi-elliptic-functions
       (* omega-R ((principal-range period) t))
       k
       (fn [sn _ _]
         (* 2.0 (g/asin sn)))))))

(defn pendulum-circulating-angular-momentum [alpha beta E]
  (let [k (g/sqrt (/ (* 2.0 beta) (+ E beta)))
        omega-R (g/sqrt (g/abs (/ (+ E beta) (* 2.0 alpha))))
        period (/ v/twopi (pendulum-frequency alpha beta E))]
    (fn [t]
      (ell/jacobi-elliptic-functions
       (* omega-R ((principal-range period) t))
       k
       (fn [_sn _cn dn]
         (* 2.0 alpha omega-R dn))))))

;; omega =  (/ (* pi omegaR) (ell/elliptic-k k)))))
;; period = 2pi / omega = 2 K / omegaR
;; so period*omegaR = 2 K but the period of sn is 4 K
;; so if period*omegaR is in the range 2K to 4K the
;; program would not work without the principal-range call

(defn pendulum-circulating-action [alpha beta E]
  (let [k (g/sqrt (/ (* 2.0 beta) (+ beta E)))
        Ek (ell/elliptic-e k)]
    (* (/ 4.0 Math/PI)
       (g/sqrt (* beta alpha))
       (/ Ek k))))

(defn pendulum-circulating-action-to-E [alpha beta]
  (fn [action]
    (let [g (/ action (* (/ 4.0 Math/PI) (g/sqrt (* beta alpha))))
          k (pendulum-inverse-g g)
          k**2 (g/square k)]
      (/ (* beta (- 2.0 k**2)) k**2))))

(defn pendulum-circulating-phase [alpha beta]
  (fn [[_ theta :as state]]
    (let [E ((Hpendulum alpha beta) state)
          k (g/sqrt (/ (* 2.0 beta) (+ E beta)))
          omega-R (g/sqrt (g/abs (/ (+ E beta) (* 2.0 alpha))))
          period (/ v/twopi (pendulum-frequency alpha beta E))
          dt (/ (ell/elliptic-f
                 (/ ((v/principal-value Math/PI) theta) 2.0) k)
                omega-R)]
      ((v/principal-value Math/PI)
       (* v/twopi (/ dt period))))))

;; time from theta=0 to state

(defn pendulum-circulating-dt [alpha beta]
  (fn [state]
    (let [E ((Hpendulum alpha beta) state)
          phase ((pendulum-circulating-phase alpha beta) state)
          period (/ v/twopi (pendulum-frequency alpha beta E))]
      (* phase (/ period v/twopi)))))

(defn pendulum-circulating-aa-state-to-state [alpha beta]
  (fn [[t angle action]]
    (let [E ((pendulum-circulating-action-to-E alpha beta) action)
          period (/ v/twopi (pendulum-frequency alpha beta E))
          dt (* (/ period v/twopi) angle)]
      (h/->H-state t
                   ((pendulum-circulating-angle alpha beta E) dt)
                   ((pendulum-circulating-angular-momentum alpha beta E) dt)))))

(defn pendulum-circulating-state-to-aa-state [alpha beta]
  (fn [state]
    (let [E ((Hpendulum alpha beta) state)
          action (pendulum-circulating-action alpha beta E)
          angle ((pendulum-circulating-phase alpha beta) state)]
      (h/->H-state (l/state->t state) angle action))))

;;;----------------------------------------------------------------
;;; separatrix case

(defn gudermannian [x]
  (- (* 2.0 (g/atan (g/exp x))) (/ Math/PI 2)))

(defn inverse-gudermannian [x]
  (g/log (g/tan (+ (/ x 2.0) (/ Math/PI 4)))))

(defn pendulum-separatrix-angle [alpha beta]
  (fn [t]
    (let [omega-0 (g/sqrt (g/abs (/ beta alpha)))]
      (* 2.0 (gudermannian (* omega-0 t))))))

(defn pendulum-separatrix-angular-momentum [alpha beta]
  (fn [t]
    (let [theta ((pendulum-separatrix-angle alpha beta) t)]
      (g/sqrt (* 2.0 alpha beta (+ 1.0 (cos theta)))))))

(defn pendulum-separatrix-action
  "Area of 'eye'"
  [alpha beta]
  (* (/ 8.0 Math/PI) (g/sqrt (* alpha beta))))

;; ## pendulum state advancer

(defn pendulum-advance [alpha beta]
  (fn [state]
    (fn [t]
      (let [E ((Hpendulum alpha beta) state)]
        (if (< E beta)
          (let [dt ((pendulum-oscillating-dt alpha beta) state)
                t' (+ dt (- t (l/state->t state)))]
            (h/->H-state t
                         ((pendulum-oscillating-angle alpha beta E) t')
                         ((pendulum-oscillating-angular-momentum alpha beta E) t')))

          (if (> (h/state->p state) 0)
            (let [dt ((pendulum-circulating-dt alpha beta) state)
                  t' (+ dt (- t (l/state->t state)))]
              (h/->H-state t
                           ((pendulum-circulating-angle alpha beta E) t')
                           ((pendulum-circulating-angular-momentum alpha beta E) t')))
            (let [dt ((pendulum-circulating-dt alpha beta)
                      (h/->H-state (- (l/state->t state))
                                   (- (l/state->q state))
                                   (- (h/state->p state))))
                  t' (+ dt (- t (l/state->t state)))]
              (h/->H-state t
                           (- ((pendulum-circulating-angle alpha beta E) t'))
                           (- ((pendulum-circulating-angular-momentum alpha beta E) t'))))))))))


(defn pendulum-integration [alpha beta eps]
  (fn [state]
    (fn [t]
      (let [state2
            ((ode/state-advancer pendulum-sysder alpha beta)
             state (- t (l/state->t state)) eps)]
        (h/->H-state (l/state->t state2)
                     ((v/principal-value Math/PI) (l/state->q state2))
                     (h/state->p state2))))))

(defn pendulum-frequency [alpha beta E]
  (cond (< E beta) (pendulum-oscillating-frequency alpha beta E)
        (> E beta) (pendulum-circulating-frequency alpha beta E)
        :else 0.0))

;; ## global action angle coordinates for pendulum
;;
;; Oscillation region:
;; -pi < phi < pi
;; 0 <  I < Isep
;;
;; Upper circulation region:
;; -pi < phi < pi  ->  -pi/2 < phi' < pi/2   phi' = phi/2
;; Isep < 2I                  Isep < I'      I' = 2I

;; Lower circulation region:
;; ...

(defn pendulum-state-to-global-aa-state [alpha beta]
  (fn [state]
    (let [E ((Hpendulum alpha beta) state)]
      (cond (< E beta)
            ((pendulum-oscillating-state-to-aa-state alpha beta) state)

            (and (> E beta) (> (h/state->p state) 0.))
            (let [aa-state
                  ((pendulum-circulating-state-to-aa-state alpha beta)
                   state)]
              (h/->H-state (l/state->t state)
                           (* 0.5 (l/state->q aa-state))
                           (* 2.0 (h/state->p aa-state))))

            (and (> E beta) (< (h/state->p state) 0.))
            (let [aa-state
                  ((pendulum-circulating-state-to-aa-state alpha beta)
                   state)]
              (h/->H-state (l/state->t state)
                           ((v/principal-value Math/PI)
                            (- Math/PI (* 0.5 (l/state->q aa-state))))
                           (* 2.0 (h/state->p aa-state))))
            (= E beta) :go-figure))))

(defn pendulum-global-aa-state-to-state [alpha beta]
  (let [separatrix-action (pendulum-separatrix-action alpha beta)]
    (fn [[_ angle action :as aa-state]]
      (cond (< action separatrix-action)
            ((pendulum-oscillating-aa-state-to-state alpha beta)
             aa-state)

            (> action separatrix-action)
            (if (and (< angle (/ Math/PI 2))
                     (>= angle (/ Math/PI -2)))
              ((pendulum-circulating-aa-state-to-state alpha beta)
               (h/->H-state (l/state->t aa-state)
                            (* 2. (l/state->q aa-state))
                            (* 0.5 (h/state->p aa-state))))
              (let [state
                    ((pendulum-circulating-aa-state-to-state alpha beta)
                     (h/->H-state (l/state->t aa-state)
                                  (* 2. (l/state->q aa-state))
                                  (* 0.5 (h/state->p aa-state))))]
                (h/->H-state (l/state->t state)
                             (- (l/state->q state))
                             (- (h/state->p state)))))

            (= action separatrix-action) :oh-well))))
