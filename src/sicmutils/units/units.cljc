(ns sicmutils.units.units
  (:refer-clojure :exclude [second])
  (:require [sicmutils.value :as v]
            [sicmutils.generic :as g]))

(def SI '[meter kilogram second ampere kelvin mole candela])

(defn system= [sys1 sys2]
  (= sys1 sys2))

(declare units=)

(deftype Units [system exponents scale]
  )

(defn ->map [units]
  {:system (.system units)
   :exponents (.exponents units)
   :scale (.scale units)})

(defn ->symbolic
  "convert Units to symbolic expression where each base SI unit is a symbol"
  [units]
  (let [{:keys [system exponents scale]} (->map units)]
    ;; [system exponents scale]
    (->> (map g/expt system exponents)
         (reduce g/*)
         (g/* scale)
         (g/simplify))))

(defn units? [x]
  (isa? x Units))

(defn units [system exponents scale]
  (Units. system exponents scale))

(defn unitless? [unit]
  (every? v/zero? (.exponents unit)))

(defn *units [u1 u2]
  (cond (unitless? u1) u2
        (unitless? u2) u1
        :else
        (do
          (assert (unit-system-equal (.system u1) (.system u2)))

          ;; assuming we don't want generic values in exponents and scale =>
          ;; using clojure.core/+ and clojure.core/* here.
          (let [system (.system u1)
                exponents (mapv + (.exponents u1) (.exponents u2))
                scale (* (.scale u1) (.scale u2))]
            (Units. system exponents scale)))))

(comment
  (Units. SI [0 0 0 0 0 0 0] 1)
  ;; can I destructure?
  (let [unit (Units. SI [0 0 0 0 0 0 0] 1)]
    [(.system unit)
     (.exponents unit)
     (.scale unit)]
    )
  )

