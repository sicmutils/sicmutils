(ns sicmutils.util.stopwatch
  (:require [sicmutils.util :as u]
            [stopwatch.core :as sw])
  #?(:clj
     (:import [java.util.concurrent TimeUnit TimeoutException])))

(defprotocol IStopwatch
  (running? [this])
  (start [this])
  (stop [this])
  (reset [this])
  (-elapsed [this unit] "Displays the current elapsed time in the supplied units.")
  (repr [this] "Prints a string representation of the stopwatch."))

(defn elapsed
  "Wrapper that handles a default implementation."
  ([sw] (-elapsed sw :nanos))
  ([sw unit] (-elapsed sw unit)))

(def units
  "Allowed units of time, ordered from most precise to least."
  [:nanos :micros :millis :seconds :minutes :hours :days])

(def ^:private abbreviate
  {:nanos "ns"
   :micros "\u03bcs"
   :millis "ms"
   :seconds "s"
   :minutes "min"
   :hours "h"
   :days "d"})

;; ## Guava Stopwatch Implementation
;;
;; This is currently the default stopwatch implementation for Clojure (vs
;; Clojurescript), though the home-rolled version works just fine in both
;; languages.

#?(:clj
   (def ^:private unit-map
     {:days TimeUnit/DAYS
      :hours TimeUnit/HOURS
      :minutes TimeUnit/MINUTES
      :seconds TimeUnit/SECONDS
      :micros TimeUnit/MICROSECONDS
      :millis TimeUnit/MILLISECONDS
      :nanos TimeUnit/NANOSECONDS}))

#?(:clj
   (extend-type com.google.common.base.Stopwatch
     IStopwatch
     (running? [this] (.isRunning this))
     (start [this] (.start this))
     (stop [this] (.stop this))
     (reset [this] (.reset this))
     (-elapsed [this unit] (.elapsed ^com.google.common.base.Stopwatch this (unit-map unit)))
     (repr [this] (str this))))

;; ## Native Stopwatch Implementation

;; Conversions from nanoseconds. The stopwatch library stores nanos, so we only
;; need to convert FROM nanos, ever, not between other units.
(let [->micros 1e3
      ->ms (* ->micros 1e3)
      ->s (* ->ms 1e3)
      ->m (* ->s 60)
      ->h (* ->m 60)
      ->d (* ->h 24)]
  (defn- from-nanos
    [ns unit]
    (/ ns
       (case unit
         :nanos 1
         :micros ->micros
         :millis ->ms
         :seconds ->s
         :minutes ->m
         :hours ->h
         :days ->d
         (u/illegal (str "Unknown unit: " unit))))))

(defn- choose-unit
  "Returns a pair of [value, unit]."
  [ns]
  (or (->> (reverse units)
           (map (juxt #(from-nanos ns %) identity))
           (filter (comp #(> % 1) first))
           first)
      [0 :nanos]))

;; Implementation of an "immutable" stopwatch (minus the must-be-mutable
;; `elapsed-fn`, of course). The final stopwatch will wrap this in an `atom`.

(deftype Stopwatch [elapsed-fn offset is-running?]
  IStopwatch
  (running? [this] is-running?)

  (start [this]
    (if is-running?
      this
      (Stopwatch. (sw/start) offset true)))

  (stop [this]
    (if is-running?
      (let [offset' (elapsed this :nanos)]
        (Stopwatch. (constantly offset') offset' false))
      this))

  (reset [this] (Stopwatch. nil 0 false))

  (-elapsed [_ unit]
    (-> (if is-running?
          (+ (elapsed-fn)
             offset)
          offset)
        (from-nanos unit)))

  (repr [this]
    (let [[x unit] (choose-unit (elapsed this :nanos))]
      (str x " " (abbreviate unit)))))

(defn- wrapped
  "Accepts some object implementing `IStopWatch` and returns a mutable
  implementation that wraps an immutable stopwatch in an atom."
  [stopwatch]
  (let [sw (atom stopwatch)]
    (reify IStopwatch
      (running? [_] (running? @sw))
      (start [this] (swap! sw start) this)
      (stop [this] (swap! sw stop) this)
      (reset [this] (swap! sw reset) this)
      (-elapsed [this unit] (-elapsed @sw unit))
      (repr [_] (repr @sw))

      Object
      (toString [this] (repr @sw)))))

(defn stopwatch
  "Returns a platform-specific implementation of IStopwatch. In Clojure, returns
  an instance of Guava's `Stopwatch` class. Clojurescript receives a homerolled
  stopwatch."
  [& {:keys [started?] :or {started? true}}]
  #? (:cljs (let [watch (Stopwatch. nil 0 false)]
              (wrapped
               (if started?
                 (start watch)
                 watch)))
      :clj (if started?
             (com.google.common.base.Stopwatch/createStarted)
             (com.google.common.base.Stopwatch/createUnstarted))))
