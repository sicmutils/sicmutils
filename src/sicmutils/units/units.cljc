(ns sicmutils.units.units
  (:require [sicmutils.value :as v]
            [sicmutils.generic :as g]))

;; how to represent units?
;; how to represent a unit system?
;; how do we even add units together?

;; Could start by implementing a unit system with only meters.

(comment
  ;; A unit
  ['unit-system 'exponents 'scale]
  ;; where 'scale allows us to define derived units.

  ;; A unit system
  '????
  ;;
  ;; really just a list of the base units. but ... unit system refers to base
  ;; units, and base units refer back to system. So define-unit-system
  ;; establishes a two way binding of sorts.
  )

;; Skipping all dispatch, unit systems and macros for now, focusing on the data
;; model
(def SI [:meter :kilogram :second :ampere :kelvin :mole :candela])

(defn unit-system-equal [sys1 sys2]
  true)


;; but I need this to be generic, so ... it needs to be stored in a deftype. And
;; I need a kind.

;; Start with matrix.

;; plural or singular?
;;
;; Scheme impl uses plural with-units. From kernel/types.scm:
;;
;;     (define (with-units? x)
;;       (and (pair? x)
;;            (eq? (car x) with-units-type-tag)))
;;
;;     (define (units? x)
;;       (or (eq? x '&unitless)
;;           (and (pair? x)
;;     	   (eq? (car x) unit-tag-type))))


(deftype Units [system exponents scale])

(defn units? [x]
  (isa? x Unit))

(defn units [system exponents scale]
  (Units. system exponents scale))

(defn unitless? [unit]
  (every? v/zero? (.exponents unit)))

(defn ^:private *units [u1 u2]
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
            (Unit. system exponents scale)))))


(def &meter    (Units. SI [1 0 0 0 0 0 0] 1))
(def &kilogram (Units. SI [0 1 0 0 0 0 0] 1))
(def &second   (Units. SI [0 0 1 0 0 0 0] 1))
(def &ampere   (Units. SI [0 0 0 1 0 0 0] 1))
(def &kelvin   (Units. SI [0 0 0 0 1 0 0] 1))
(def &mole     (Units. SI [0 0 0 0 0 1 0] 1))
(def &candela  (Units. SI [0 0 0 0 0 0 1] 1))

;; Looks like &name is allowed in Clojure after all! I'm not persuaded that this
;; is the most idiomatic Clojure naming. It does avoid units/second clashing
;; with clojure.core/second.
;;
;; It might make sense to require the SI unit system qualified anyway.

(comment
  (:require [sicmutils.units.SI :as SI])

  (+ (* 5 SI/meter) (* 10 SI/meter))
  )

(comment
  (let [meter-squared (*units &meter &meter)]
    [(.exponents meter-squared)
     (.scale meter-squared)]))

(comment
  (= (Units. SI [0 0 0 0 0 0 0] 1)
     (Units. SI [0 0 0 0 0 0 0] 1))
  ;; => false

  (.hashCode (Units. SI [0 0 0 0 0 0 0] 1))
  ;; new hashcode on each call

  (Units. SI [0 0 0 0 0 0 0] 1)
  ;; can I destructure?
  (let [unit (Units. SI [0 0 0 0 0 0 0] 1)]
    [(.system unit)
     (.exponents unit)
     (.scale unit)]
    )
  )

