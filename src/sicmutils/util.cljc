(ns sicmutils.util
  "Shared utilities between clojure and clojurescript."
  #?(:clj
     (:require [clojure.math.numeric-tower :as nt])))

(def compute-sqrt #?(:clj nt/sqrt :cljs Math/sqrt))
(def compute-expt #?(:clj nt/expt :cljs Math/pow))
(def compute-abs #?(:clj nt/abs :cljs Math/abs))
(def numtype #?(:clj Number :cljs js/Number))

(defn unsupported [s]
  (throw
   #?(:clj (UnsupportedOperationException. s)
      :cljs (js/Error s))))

(defn illegal [s]
  (throw
   #?(:clj (IllegalArgumentException. s)
      :cljs (js/Error s))))

(defn arithmetic-ex [s]
  (throw
   #?(:clj (ArithmeticException. s)
      :cljs (js/Error s))))
