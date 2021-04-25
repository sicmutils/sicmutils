;;
;; Copyright © 2021 Sam Ritchie.
;; This work is based on the Scmutils system of MIT/GNU Scheme:
;; Copyright © 2002 Massachusetts Institute of Technology
;;
;; This is free software;  you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or (at
;; your option) any later version.
;;
;; This software is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this code; if not, see <http://www.gnu.org/licenses/>.
;;

(ns sicmutils.util.logic
  "Logic utilities!"
  (:require [taoensso.timbre :as log]))

(def ^{:doc "If true, logs assumptions."
       :dynamic true}
  *log-assumptions?*
  true)

(defn assume!
  "Log an assumption. NOTE that if-false is not used right now. Currently this
  always returns true."
  ([assumption context]
   (assume! assumption context nil))
  ([assumption context if-false]
   (when *log-assumptions?*
     (log/warn
      (str "Assuming " assumption " in " context)))
   true))
