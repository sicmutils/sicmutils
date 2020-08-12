(ns sicmutils.util.stopwatch
  (:require [stopwatch.core :as sw])
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

(:clj
 (def unit-map
   {:days TimeUnit/DAYS
    :hours TimeUnit/HOURS
    :minutes TimeUnit/MINUTES
    :seconds TimeUnit/SECONDS
    :micros TimeUnit/MICROSECONDS
    :millis TimeUnit/MILLISECONDS
    :nanos TimeUnit/NANOSECONDS}))

#?(:cljs
   (do
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

       ;; TODO add unit support.
       (-elapsed [_ _]
         (if is-running?
           (+ (bigint (elapsed-fn))
              offset)
           offset))

       (repr [this]
         (str (elapsed this :micros) " ms")))))

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
   #? (:cljs (let [watch (->Stopwatch nil (bigint 0) false)]
               (if running?
                 (start watch)
                 watch))
       :clj (if running?
              (com.google.common.base.Stopwatch/createStarted)
              (com.google.common.base.Stopwatch/createUnstarted)))))
