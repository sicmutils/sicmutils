#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.util.logic
  "Logic utilities!"
  (:require [taoensso.timbre :as log]))

(def ^{:doc "If true, logs assumptions."
       :dynamic true}
  *log-assumptions?*
  true)

(defn assume!
  "Log an assumption.

  NOTE that `if-false` is not used right now. Currently this always returns true.

  NOTE: what this WILL do is check if the assumption is correct, to the extent
  that this is possible, and fail if it's provably false."
  ([assumption context]
   (assume! assumption context nil))
  ([assumption context _if-false]
   (when *log-assumptions?*
     (log/warn
      (str "Assuming " assumption " in " context)))
   true))
