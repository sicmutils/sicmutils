(require '[math.start :as m])
(m/math-ns math.examples.figure-1-7
           (:require [clojure.data.json :as json])
           :genclass)

(defn- T-pend
  [m l _ ys]
  (fn [local]
    (let [[t θ θdot] local
          vys (D ys)]
      (* 1/2 m
         (+ (square (* l θdot))
            (square (vys t))
            (* 2 l (vys t) θdot (sin θ)))))))

(defn- V-pend
  [m l g ys]
  (fn [local]
    (let [[t θ _] local]
      (* m g (- (ys t) (* l (cos θ)))))))

(def L-pend (- T-pend V-pend))

(defn periodic-drive
  [amplitude frequency phase]
  (fn [t]
    (* amplitude (cos (+ (* frequency t) phase)))))

(defn L-periodically-driven-pendulum
  [m l g a ω]
  (let [ys (periodic-drive a ω 0)]
    (L-pend m l g ys)))

(defn pend-state-derivative  [m l g a ω]
  (Lagrangian->state-derivative
   (L-periodically-driven-pendulum m l g a ω)))

(defn -main
  [& args]
  (let [state-history (atom [])]
    ((evolve pend-state-derivative
             1.0
             1.0
             9.8
             0.1
             (* 2.0 (sqrt 9.8)))
     (up 0.0
         1.
         1e-10)
     (fn [t [_ q _]] (swap! state-history conj [t q]))
     0.01
     5.0
     1.0e-13
     {:compile true})
    (json/write @state-history *out*)))
