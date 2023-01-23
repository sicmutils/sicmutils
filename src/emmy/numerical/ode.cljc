#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.numerical.ode
  "ODE solvers for working with initial value problems."
  (:require [emmy.expression.compile :as c]
            #?(:cljs [emmy.util :as u])
            #?(:cljs [emmy.value :as v])
            [emmy.structure :as struct]
            [emmy.util.stopwatch :as us]
            [taoensso.timbre :as log]
            #?(:cljs ["odex" :as o]))
  #?(:clj
     (:import (org.apache.commons.math3.ode.nonstiff GraggBulirschStoerIntegrator)
              (org.apache.commons.math3.ode FirstOrderDifferentialEquations)
              (org.apache.commons.math3.ode.sampling StepHandler))))

#?(:cljs
   (def ^:private near? (v/within 1e-8)))

#?(:clj
   (defn- round-up
     "Returns `n` rounded up to the nearest multiple of `step-size`. the returned
  value will always equal `0`, mod `step-size`"
     [n step-size]
     (let [offset (mod n step-size)]
       (if (pos? offset)
         (-> (- n offset)
             (+ step-size))
         n))))

#?(:clj
   (defn step-handler
     "Generates a StepHandler instance that can be attached to an integrator.

  When used as an observation callback, the `StepHandler` is not invoked at
  every grid point; rather, it is invoked once in a while over a range of time
  within which the integrated function may be accurately evaluated. The handler
  we install does this, invoking the callback for each requested grid point
  within the valid range, ensuring that we also invoke the callback for the
  final point."
     [observe step-size initial-state]
     (let [array->state #(struct/unflatten % initial-state)]
       (reify StepHandler
         (init [_ _ _ _])
         (handleStep [_ interpolator final-step?]
           (let [it0         (.getPreviousTime interpolator)
                 it1         (.getCurrentTime interpolator)
                 t0          (round-up it0 step-size)
                 final-state (when final-step?
                               (array->state
                                (.getInterpolatedState interpolator)))]
             (doseq [t (range t0 it1 step-size)]
               (.setInterpolatedTime interpolator t)
               (observe t (array->state
                           (.getInterpolatedState interpolator))))
             ;; `range` has an exclusive upper bound, so the final point will
             ;; never be observed in the `doseq`. Handle it here.
             (when final-step?
               (observe it1 final-state))))))))

#?(:clj
   (defn attach-handler
     "We implement the observation callback by adding a StepHandler to the
  integration. The StepHandler is not invoked at every grid point; rather, it is
  invoked once in a while over a range of time within which the integrated
  function may be accurately evaluated. The handler we install does this,
  invoking the callback for each requested grid point within the valid range,
  ensuring that we also invoke the callback for the final point."
     [^GraggBulirschStoerIntegrator integrator observe step-size initial-state]
     (let [handler (step-handler observe step-size initial-state)]
       (.addStepHandler integrator handler))))

(defn integration-opts
  "Returns a map with the following kv pairs:

  - `:integrator` an instance of `GraggBulirschStoerIntegrator`
  - `:equations` instance of `FirstOrderDifferentialEquations`
  - `:dimension` the total number of entries in the flattened initial state tuple
  - `:stopwatch` [[IStopwatch]] instance that records total evaluation time inside
    the derivative function
  - `:counter` an atom containing a `Long` that increments every time derivative fn
    is called."
  [state-derivative derivative-args initial-state
   {:keys [compile? epsilon] :or {epsilon 1e-8}}]
  (let [evaluation-time  (us/stopwatch :started? false)
        evaluation-count (atom 0)
        dimension        (count (flatten initial-state))
        derivative-fn    (if compile?
                           (let [f' (c/compile-state-fn state-derivative derivative-args initial-state)]
                             (fn [y] (f' y derivative-args)))
                           (do (log/warn "Not compiling function for ODE analysis")
                               (let [d:dt (apply state-derivative derivative-args)
                                     array->state #(struct/unflatten % initial-state)]
                                 (comp d:dt array->state))))

        state->array     #?(:clj
                            (comp double-array flatten)

                            :cljs
                            (fn [state]
                              (->> (flatten state)
                                   (map u/double)
                                   (into-array))))

        equations        #?(:clj
                            (reify FirstOrderDifferentialEquations
                              (computeDerivatives [_ _ y out]
                                (us/start evaluation-time)
                                (swap! evaluation-count inc)
                                (let [y' (-> (derivative-fn y)
                                             (state->array)
                                             (doubles))]
                                  (System/arraycopy y' 0 out 0 (alength y')))
                                (us/stop evaluation-time))
                              (getDimension [_] dimension))

                            :cljs
                            (fn [_ y]
                              (us/start evaluation-time)
                              (swap! evaluation-count inc)
                              (let [y' (state->array (derivative-fn y))]
                                (us/stop evaluation-time)
                                y')))
        integrator #?(:clj
                      (GraggBulirschStoerIntegrator. 0. 1.
                                                     (double epsilon)
                                                     (double epsilon))

                      :cljs
                      (let [solver (o/Solver. dimension)]
                        (set! (.-absoluteTolerance solver) epsilon)
                        (set! (.-relativeTolerance solver) epsilon)
                        solver))]
    {:integrator integrator
     :equations equations
     :dimension dimension
     :stopwatch evaluation-time
     :counter evaluation-count}))

(defn make-integrator
  "make-integrator takes a state derivative function (which in this
  system is assumed to be a map from a structure to a structure of the
  same shape, as differentiating a function does not change its
  shape), and returns an integrator, which is a function of several
  arguments:

  - the initial state
  - an intermediate-state observation function
  - the step size desired
  - the final time to seek, and
  - an error tolerance.


  If the `observe` function is not nil, it will be invoked with the time as
  first argument and integrated state as the second, at each intermediate step."
  [state-derivative derivative-args]
  #?(:cljs
     (let [total-time (us/stopwatch :started? false)
           latest     (atom 0)]
       (fn call
         ([initial-state step-size t]
          (call initial-state step-size t {}))
         ([initial-state step-size t {:keys [observe] :as opts}]
          (us/start total-time)
          (let [{:keys [integrator equations stopwatch counter]}
                (integration-opts state-derivative derivative-args initial-state opts)
                initial-state-array (into-array
                                     (flatten initial-state))
                array->state #(struct/unflatten % initial-state)
                observe-fn    (when observe
                                (set! (.-denseOutput integrator) true)
                                (.grid integrator step-size
                                       (fn [t y]
                                         (reset! latest t)
                                         (observe t (array->state y)))))
                output (.solve integrator equations 0 initial-state-array t observe-fn)
                ret    (array->state (.-y output))]
            (when (and observe (not (near? t @latest)))
              (observe t ret))
            (us/stop total-time)
            (log/info "#" @counter "total" (us/repr total-time) "f" (us/repr stopwatch))
            (us/reset total-time)
            (reset! latest 0)
            ret))))

     :clj
     (let [total-time (us/stopwatch :started? false)]
       (fn call
         ([initial-state step-size t]
          (call initial-state step-size t {}))
         ([initial-state step-size t {:keys [observe] :as opts}]
          (us/start total-time)
          (let [{:keys [integrator equations dimension stopwatch counter]}
                (integration-opts state-derivative derivative-args initial-state opts)
                initial-state-array (double-array
                                     (flatten initial-state))
                array->state #(struct/unflatten % initial-state)
                output-buffer (double-array dimension)]
            (when observe
              (attach-handler integrator observe step-size initial-state))
            (.integrate ^GraggBulirschStoerIntegrator
                        integrator equations 0
                        initial-state-array t output-buffer)
            (us/stop total-time)
            (log/info "#" @counter "total" (us/repr total-time) "f" (us/repr stopwatch))
            (us/reset total-time)
            (array->state output-buffer)))))))

(defn state-advancer
  "state-advancer takes a state derivative function constructor followed by the
  arguments to construct it with. The state derivative function is constructed
  and an integrator is produced which takes:

  - initial state
  - target time

  as arguments. Optionally, supply an options map with these optional fields:

  `:compile?`: If true, the ODE solver will compile your state function.

  `:epsilon`: The maximum error tolerance allowed by the ODE solver, both
  relative and absolute.

  Returns the final state.

  The state derivative is expected to map a structure to a structure of the same
  shape, and is required to have the time parameter as the first element."
  [state-derivative & state-derivative-args]
  (let [I (make-integrator state-derivative state-derivative-args)]
    (fn call
      ([initial-state t]
       (call initial-state t {}))
      ([initial-state t opts]
       (I initial-state 0 t opts)))))

(defn evolve
  "evolve takes a state derivative function constructor and its arguments, and
  returns an integrator via make-integrator.

  In particular, the returned function accepts a callback function which will be
  invoked at intermediate grid points of the integration."
  [state-derivative & state-derivative-args]
  (make-integrator state-derivative state-derivative-args))

(defn integrate-state-derivative
  "A wrapper for evolve, which is more convenient when you just
  want a vector of (time, state) pairs over the integration interval
  instead of having to deal with a callback. Integrates the supplied
  state derivative (and its argument package) from [0 to t1] in steps
  of size dt"
  [state-derivative state-derivative-args initial-state t1 dt]
  (let [I (make-integrator state-derivative state-derivative-args)
        out (atom [])
        collector (fn [_ state]
                    (swap! out conj state))]
    (I initial-state dt t1 {:compile? true
                            :epsilon 1e-6
                            :observe collector})
    @out))
