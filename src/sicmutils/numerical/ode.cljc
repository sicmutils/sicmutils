;
; Copyright © 2017 Colin Smith.
; This work is based on the Scmutils system of MIT/GNU Scheme:
; Copyright © 2002 Massachusetts Institute of Technology
;
; This is free software;  you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3 of the License, or (at
; your option) any later version.
;
; This software is distributed in the hope that it will be useful, but
; WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
; General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this code; if not, see <http://www.gnu.org/licenses/>.
;

(ns sicmutils.numerical.ode
  (:require [sicmutils.numerical.compile :as c]
            [sicmutils.util.stopwatch :as us]
            [sicmutils.util :as u]
            [sicmutils.structure :as struct]
            [taoensso.timbre :as log])
  #?(:clj
     (:import (org.apache.commons.math3.ode.nonstiff GraggBulirschStoerIntegrator)
              (org.apache.commons.math3.ode FirstOrderDifferentialEquations)
              (org.apache.commons.math3.ode.sampling StepHandler))))

(defn ^:private round-up
  "Returns `n` rounded up to the nearest multiple of `step-size`. the returned
  value will always equal `0`, mod `step-size`"
  [n step-size]
  (let [offset (mod n step-size)]
    (if (pos? offset)
      (-> (- n offset)
          (+ step-size))
      n)))

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
   (defn integrator
     "Returns a map with the following kv pairs:

  - :integrator an instance of GraggBulirschStoerIntegrator
  - :equations instance of FirstOrderDifferentialEquations
  - :dimension the total number of entries in the flattened initial state tuple
  - :stopwatch IStopwatch instance that records total evaluation time inside the
  computeDerivatives function
  - :counter an atom containing a Long that increments every time
  `computeDerivatives` is called."
     [state-derivative derivative-args initial-state
      & {:keys [compile? epsilon]}]
     (let [evaluation-time     (us/stopwatch :started? false)
           evaluation-count    (atom 0)
           state->array        (comp double-array flatten)
           dimension           (count (flatten initial-state))
           derivative-fn
           (if compile?
             (c/compile-state-function state-derivative derivative-args initial-state)
             (do (log/warn "Not compiling function for ODE analysis")
                 (let [d:dt (apply state-derivative derivative-args)
                       array->state #(struct/unflatten % initial-state)]
                   (comp d:dt array->state))))

           equations
           (reify FirstOrderDifferentialEquations
             (computeDerivatives [_ _ y out]
               (us/start evaluation-time)
               (swap! evaluation-count inc)
               (let [y' (doubles (-> (concat y derivative-args)
                                     derivative-fn
                                     state->array))]
                 (System/arraycopy y' 0 out 0 (alength y')))
               (us/stop evaluation-time))
             (getDimension [_] dimension))
           integrator (GraggBulirschStoerIntegrator. 0. 1. (double epsilon) (double epsilon))]
       {:integrator integrator
        :equations equations
        :dimension dimension
        :stopwatch evaluation-time
        :counter evaluation-count})))

#?(:clj
   (defn attach-handler
     "We implement the observation callback by adding a StepHandler to the
  integration. The StepHandler is not invoked at every grid point; rather, it is
  invoked once in a while over a range of time within which the integrated
  function may be accurately evaluated. The handler we install does this,
  invoking the callback for each requested grid point within the valid range,
  ensuring that we also invoke the callback for the final point."
     [integrator observe step-size initial-state]
     (.addStepHandler
      integrator
      (step-handler observe step-size initial-state))))

(defn make-integrator
  "make-integrator takes a state derivative function (which in this
  system is assumed to be a map from a structure to a structure of the
  same shape, as differentiating a function does not change its
  shape), and returns an integrator, which is a function of several
  arguments: the initial state, an intermediate-state observation
  function, the step size desired, the final time to seek, and an
  error tolerance. If the observe function is not nil, it will be
  invoked with the time as first argument and integrated state as the
  second, at each intermediate step."
  [state-derivative derivative-args]
  #?(:cljs
     (u/unsupported "make-integrator isn't yet implemented in Clojurescript.")

     :clj
     (let [total-time (us/stopwatch :started? false)]
       (fn [initial-state observe step-size t ε & {:keys [compile]}]
         (us/start total-time)
         (let [{:keys [integrator equations dimension stopwatch counter]}
               (integrator state-derivative derivative-args initial-state
                           :epsilon ε
                           :compile? compile)
               initial-state-array (double-array
                                    (flatten initial-state))
               array->state #(struct/unflatten % initial-state)
               output-buffer (double-array dimension)]
           (when observe
             (attach-handler integrator observe step-size initial-state))
           (.integrate integrator equations 0
                       initial-state-array t output-buffer)
           (doto total-time (us/stop) (us/reset))
           (log/info "#" @counter "total" (us/repr total-time) "f" (us/repr stopwatch))
           (array->state output-buffer))))))

(defn state-advancer
  "state-advancer takes a state derivative function constructor
  followed by the arguments to construct it with. The state derivative
  function is constructed and an integrator is produced which takes
  the initial state, target time, and error tolerance as
  arguments. The final state is returned. The state derivative is
  expected to map a structure to a structure of the same shape,
  and is required to have the time parameter as the first element."
  [state-derivative & state-derivative-args]
  (let [I (make-integrator state-derivative state-derivative-args)]
    (fn [initial-state t ε & options]
      (apply I initial-state nil 0 t ε options))))

(defn evolve
"evolve takes a state derivative function constructor and its
  arguments, and returns an integrator via make-integrator. In
  particular, the returned function accepts a callback function which
  will be invoked at intermediate grid points of the integration."
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
        collector (fn [t state]
                    (swap! out conj state))]
    (I initial-state collector dt t1 1e-6 :compile true)
    @out))
