(ns sicmutils.util
  "Shared utilities between clojure and clojurescript."
  (:refer-clojure :rename {bigint core-bigint
                           biginteger core-biginteger
                           int core-int
                           long core-long}
                  #?@(:cljs [:exclude [bigint long int]]))
  (:require [stopwatch.core :as sw]
            #?(:clj [clojure.math.numeric-tower :as nt])
            #?(:cljs goog.math.Integer)
            #?(:cljs goog.math.Long))
  #?(:clj
     (:import [java.util.concurrent TimeUnit TimeoutException])))

(def compute-sqrt #?(:clj nt/sqrt :cljs Math/sqrt))
(def compute-expt #?(:clj nt/expt :cljs Math/pow))
(def compute-abs #?(:clj nt/abs :cljs Math/abs))
(def inttype #?(:clj Integer :cljs goog.math.Integer))
(def longtype #?(:clj Long :cljs goog.math.Long))

(defn bigint [x]
  #?(:clj (core-bigint x)
     :cljs (js/BigInt x)))

(defn biginteger [x]
  #?(:clj (core-biginteger x)
     :cljs (js/BigInt x)))

(defn int [x]
  #?(:clj (core-int x)
     :cljs (.fromNumber goog.math.Integer x)))

(defn long [x]
  #?(:clj (core-long x)
     :cljs (.fromNumber goog.math.Long x)))

(defn unsupported [s]
  (throw
   #?(:clj (UnsupportedOperationException. s)
      :cljs (js/Error s))))

(defn illegal [s]
  (throw
   #?(:clj (IllegalArgumentException. s)
      :cljs (js/Error s))))

(defn illegal-state [s]
  (throw
   #?(:clj (IllegalStateException. s)
      :cljs (js/Error s))))

(defn arithmetic-ex [s]
  (throw
   #?(:clj (ArithmeticException. s)
      :cljs (js/Error s))))

(defn timeout-ex [s]
  (throw
   #?(:clj (TimeoutException. s)
      :cljs (js/Error s))))


;; Stopwatch interface.

(defprotocol IStopwatch
  (running? [this])
  (start [this])
  (stop [this])
  (reset [this])
  (elapsed [this unit] "Displays the current elapsed time in the supplied units.")
  (repr [this] "Prints a string representation of the stopwatch."))

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

       (elapsed [_ unit]
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
     (elapsed [this unit] (.elapsed ^com.google.common.base.Stopwatch this (unit-map unit)))
     (repr [this] (str this))))


(defn stopwatch
  "Creates and starts the stopwatch.
  The stopwatch is a function which returns the elapsed time in units of
  nanosecond:
    (let [elapsed (start)]
      (do-work)
      (println \"Elapsed:\" (elapsed) \"ns\"))
  Uses the most precise mechanism available on the target platform."
  ([] (stopwatch true))
  ([running?]
   #? (:cljs (let [watch (->Stopwatch nil (bigint 0) false)]
               (if running?
                 (start watch)
                 watch))
       :clj (if running?
              (com.google.common.base.Stopwatch/createStarted)
              (com.google.common.base.Stopwatch/createUnstarted)))))
