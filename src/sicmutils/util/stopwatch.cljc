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

(def abbreviate
  {:nanos "ns"
   :micros "\u03bcs"
   :millis "ms"
   :seconds "s"
   :minutes "min"
   :hours "h"
   :days "d"})

#?(:clj
   (def unit-map
     {:days TimeUnit/DAYS
      :hours TimeUnit/HOURS
      :minutes TimeUnit/MINUTES
      :seconds TimeUnit/SECONDS
      :micros TimeUnit/MICROSECONDS
      :millis TimeUnit/MILLISECONDS
      :nanos TimeUnit/NANOSECONDS}))

;; Conversions from nanoseconds. The stopwatch library stores nanos, so we only
;; need to convert FROM nanos, ever, not between other units.
#?(:cljs
   (let [->micros 1e3
         ->ms (* ->micros 1e3)
         ->s (* ->ms 1e3)
         ->m (* ->s 60)
         ->h (* ->m 60)
         ->d (* ->h 24)]
     (defn from-nanos
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
            (throw (js/Error (str "Unknown unit: " unit))))))))

#?(:cljs
   (defn choose-unit
     "Returns a pair of [value, unit]."
     [ns]
     (or (->> (reverse units)
              (map (juxt #(from-nanos ns %) identity))
              (filter (comp #(> % 1) first))
              first)
         [0 :nanos])))

#?(:cljs
   (defrecord Stopwatch [elapsed-fn offset is-running?]
     IStopwatch
     (running? [this] is-running?)

     (start [this]
       (if is-running?
         this
         (Stopwatch. (sw/start*) offset true)))

     (stop [this]
       (if is-running?
         (let [offset' (elapsed this :nanos)]
           (Stopwatch. (constantly offset') offset' false))
         this))

     (-elapsed [_ unit]
       (-> (if is-running?
             (+ (elapsed-fn)
                offset)
             offset)
           (from-nanos unit)))

     (repr [this]
       (let [[x unit] (choose-unit (elapsed this :nanos))]
         (str x " " (abbreviate unit))))))

#?(:clj
   (extend-type com.google.common.base.Stopwatch
     IStopwatch
     (running? [this] (.isRunning this))
     (start [this] (.start this))
     (stop [this] (.stop this))
     (reset [this] (.reset this))
     (-elapsed [this unit] (.elapsed ^com.google.common.base.Stopwatch this (unit-map unit)))
     (repr [this] (str this))))

(defn stopwatch
  "Returns a platform-specific implementation of IStopwatch."
  ([] (stopwatch true))
  ([running?]
   #? (:cljs (let [watch (->Stopwatch nil 0 false)]
               (if running?
                 (start watch)
                 watch))
       :clj (if running?
              (com.google.common.base.Stopwatch/createStarted)
              (com.google.common.base.Stopwatch/createUnstarted)))))
