#_"SPDX-License-Identifier: GPL-3.0"

(ns sicmutils.numerical.ode
  "ODE solvers for working with initial value problems."
  (:require [sicmutils.expression.compile :as c]
            #?(:cljs [sicmutils.util :as u])
            #?(:cljs [sicmutils.value :as v])
            [sicmutils.structure :as struct]
            [sicmutils.util.stopwatch :as us]
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
     "

  Generates a StepHandler instance that can be attached to an integrator.

  When used as an observation callback, the `StepHandler` is not invoked at
  every grid point; rather, it is invoked once in a while over a range of time
  within which the integrated function may be accurately evaluated. The handler
  we install does this, invoking the callback for each requested grid point
  within the valid range, ensuring that we also invoke the callback for the
  final point."
     [observe step-size]
     (reify StepHandler
       (init [_ _ _ _])
       (handleStep [_ interpolator final-step?]
         (let [it0         (.getPreviousTime interpolator)
               it1         (.getCurrentTime interpolator)
               t0          (round-up it0 step-size)]
           (doseq [t (range t0 it1 step-size)]
             (.setInterpolatedTime interpolator t)
             (observe t (.getInterpolatedState interpolator)))
           (when final-step?
             ;; `range` has an exclusive upper bound, so the final point will
             ;; never be observed in the `doseq`. Handle it here.
             (.setInterpolatedTime interpolator it1)
             (observe it1 (.getInterpolatedState interpolator))))))))

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
        dimension        (struct/dimension initial-state)
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

;; TODO note these simpler versions, that receive f already modified.
;; `u/counted` gets you back the counter.

#?(:clj
   (defn ->arr
     [^doubles arr ^Iterable xs]
     (let [it (.iterator xs)]
       (loop [i 0]
         (when (.hasNext it)
           (aset arr i (double (.next it)))
           (recur (unchecked-inc i)))))))

#?(:clj
   (defn ^:no-doc gragg-equations
     "TODO notes."
     [f param-atom]
     (if param-atom
       (reify FirstOrderDifferentialEquations
         (computeDerivatives [_ _ y out]
           (deriv-fn y (.-state param-atom) out))
         (getDimension [_] dimension))

       (reify FirstOrderDifferentialEquations
         (computeDerivatives [_ _ y out]
           (deriv-fn y out))
         (getDimension [_] dimension)))))
#?(:clj
   (defn gragg-bulirsch-stoer
     "TODO we should have more granular options at the top level.

  Takes `:param-atom`, if true, state passed as second arg to deriv-fn.

  [JavaDocs](https://commons.apache.org/proper/commons-math/javadocs/api-3.6.1/index.html?org/apache/commons/math3/ode/nonstiff/GraggBulirschStoerIntegrator.html)

  TODO we need to recover the compile? etc function prep for this and the cljs
  below.

  NOTE returned function takes

  `:continue?` if true, the buffer is NOT touched and state evolution continues
  from where it left off.

  `:initial-state` if supplied and `:continue?` is false, overrides the existing
  state. If not supplied and `:continue?` is false, the state is re-populated
  from the prototype.

  `:raw?` returns the actual buffer each time, please don't mutate it

  `:t0` pick a new starting point."
     [deriv-fn prototype {:keys [epsilon observe param-atom raw?] :or {epsilon 1e-8}}]
     ;; all of this setup at the top is trying to save some object.
     ;;
     ;; `f` here is simplified by assuming it's already in the tightest form.
     (let [flat-proto   (flatten prototype)
           dimension    (count flat-proto)
           array->state #(struct/unflatten % prototype)
           eps          (double epsilon)
           integrator   (GraggBulirschStoerIntegrator. 0.0 1.0 eps eps)
           equations    (gragg-equations deriv-fn param-atom)
           buffer     (double-array dimension)]
       (->arr buffer flat-proto)
       (when observe
         (let [{:keys [f step-size]} observe]
           (assert (and f step-size) "both `:f` and `:step-size` required when supplying `:observe`.")
           (let [f (if raw?
                     f
                     (fn [t a]
                       (f t (array->state a))))
                 handler (step-handler f step-size)]
             (.addStepHandler integrator handler))))
       ;; NOTE that I think this is kind of a bad API. Take a look at
       ;; emmy-viewers and see how I'm using the animation code there. It might
       ;; be that instead we WANT an initial state etc, but that we do NOT want
       ;; the t specified; we want to be able to go from t1 to t2 and specify
       ;; ourselves.
       ;;
       ;; yeah, in that use case, we are specifying TWO timesteps rather than
       ;; the observe.
       (fn [t {:keys [t0 initial-state continue? raw?] :or {t0 0.0}}]
         (when-not continue?
           (->arr buffer (if initial-state
                           (flatten initial-state)
                           flat-proto)))
         (.integrate integrator equations t0 buffer t buffer)
         (if raw?
           buffer
           (array->state buffer))))))

#?(:cljs
   ;; TODO finish this conversion once the API feels better. These are both
   ;; versions of make-integrator below.
   (defn gragg-bulirsch-stoer [f dimension {:keys [epsilon] :or {epsilon 1e-8}}]
     {:integrator (let [solver (o/Solver. dimension)]
                    (set! (.-absoluteTolerance solver) epsilon)
                    (set! (.-relativeTolerance solver) epsilon)
                    solver)
      :equations  (let [out (make-array dimension)]
                    (fn [_ y]
                      (f y out)
                      out))
      :dimension  dimension}))

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
  ([state-derivative derivative-args]
   (make-integrator state-derivative derivative-args {}))
  ([state-derivative derivative-args outer-opts]
   #?(:cljs
      (let [total-time (us/stopwatch :started? false)
            latest     (atom 0)]
        (fn call
          ([initial-state t]
           (call initial-state t {}))
          ([initial-state t {:keys [observe step-size] :as opts}]
           (us/start total-time)
           (let [{:keys [integrator equations stopwatch counter]}
                 (integration-opts state-derivative derivative-args initial-state opts)
                 initial-state-array (into-array
                                      (flatten initial-state))
                 array->state #(struct/unflatten % initial-state)
                 observe-fn    (when observe
                                 (assert
                                  step-size
                                  "`:step-size` required when supplying `:observe`.")
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
          ([initial-state t]
           (call initial-state t {}))
          ([initial-state t {:keys [observe step-size] :as opts}]
           (us/start total-time)
           (let [{:keys [integrator equations dimension stopwatch counter]}
                 (integration-opts state-derivative derivative-args initial-state opts)
                 initial-state-array (double-array
                                      (flatten initial-state))
                 array->state #(struct/unflatten % initial-state)
                 output-buffer (double-array dimension)]
             (when observe
               (assert step-size
                       "`:step-size` required when supplying `:observe`.")
               (.addStepHandler ^GraggBulirschStoerIntegrator integrator
                                (step-handler
                                 (fn [t a]
                                   (observe t (array->state a)))
                                 step-size)))
             (.integrate ^GraggBulirschStoerIntegrator
                         integrator equations 0
                         initial-state-array t output-buffer)
             (us/stop total-time)
             (log/info "#" @counter "total" (us/repr total-time) "f" (us/repr stopwatch))
             (us/reset total-time)
             (array->state output-buffer))))))))

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
       (I initial-state t {}))
      ([initial-state t opts]
       (I initial-state t opts)))))

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
        out (atom (transient []))
        collector (fn [_ state]
                    (swap! out conj! state))]
    (I initial-state t1 {:compile? true
                         :epsilon 1e-6
                         :observe collector
                         :step-size dt})
    (persistent! @out)))
