(ns sicmutils.units.units
  (:refer-clojure :exclude [second])
  (:require [sicmutils.value :as v]
            [sicmutils.generic :as g]))

(def SI '[meter kilogram second ampere kelvin mole candela])

(defn system= [sys1 sys2]
  (= sys1 sys2))

(declare units= ->symbolic)

(deftype Units [system exponents scale]
  #?@(:clj
      [Object
       (equals [this that] (units= this that))
       (toString [this] (pr-str (->symbolic this)))]

      ;; cljs? I'm not developing with a CLJS REPL, so I should probably ensure
      ;; that I have proper CLJS test coverage. Wonder how Sam & Colin prevent
      ;; CLJS breakage under development.
      :cljs
      [IEquiv
       (-equiv [this that] (units= this that))
       Object
       (toString [this] (pr-str (->symbolic this)))])
  )

(defn units= [this that]
  (and (system= (.system this) (.system that))
       (v/= (.exponents this) (.exponents that))
       (v/= (.scale this) (.scale that))))

(comment
  sicmutils.units.scm-api/meter
  ;; => #object[sicmutils.units.units.Units 0x42fb5800 "meter"]

  (let [m sicmutils.units.scm-api/meter]
    (*units m m))
  ;; => #object[sicmutils.units.units.Units 0x415c107b "(expt meter 2)"]

  ;; There, a little more readable.
  )

;; Should probably delete this before merging, taking a performance penalty for
;; less verbose destructuring.
;;
;; or ... can I implemenet "IMapSomething" on the deftype? Then I won't need it.
(defn ^:private ->map [units]
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
          (assert (system= (.system u1) (.system u2)))

          ;; assuming we don't want generic values in exponents and scale =>
          ;; using clojure.core/+ and clojure.core/* here.
          (let [system (.system u1)
                exponents (mapv + (.exponents u1) (.exponents u2))
                scale (* (.scale u1) (.scale u2))]
            (Units. system exponents scale)))))

(defn invert [units]
  (let [scale (g// 1 (.scale units))
        exponents (mapv #(g/negate %) (.exponents units))]
    (Units. (.system units) exponents scale)))

#_
(let [m sicmutils.units.scm-api/meter]
  (invert
   (*units m m)))

(comment
  (Units. SI [0 0 0 0 0 0 0] 1)
  ;; can I destructure?
  (let [unit (Units. SI [0 0 0 0 0 0 0] 1)]
    [(.system unit)
     (.exponents unit)
     (.scale unit)]
    )
  )

