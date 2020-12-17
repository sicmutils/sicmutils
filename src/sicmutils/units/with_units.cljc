(ns sicmutils.units.with-units
  (:require [sicmutils.units.units]
            [sicmutils.value :as v]
            [sicmutils.generic :as g]
            [sicmutils.env :as env]))

;; Next step: Acually multiply two values with units. Defer generic /
;; multimethod variants.

(deftype WithUnits [value units])

(defn with-units? [x]
  (isa? x WithUnits))

(defn with-units [value units]
  (WithUnits. value units))

;; Colons inside names isn't typical Clojure. If the functions are private it
;; probably doesn't matter. Sticking close to Scheme for now.
(defn ^:private u:+ [x y])
(defn ^:private u:* [x y])
