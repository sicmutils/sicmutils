(ns sicmutils.units.scm-api
  "Copying the original scmutils API here, so that we can evaluate it. Defer
  finding an idiomatic Clojure API. Consider not pushing this namespace at all."
  (:require [sicmutils.units.units :as u]
            [sicmutils.units.with-units :as wu]))

(def &meter    (u/units u/SI [1 0 0 0 0 0 0] 1))
(def &kilogram (u/units u/SI [0 1 0 0 0 0 0] 1))
(def &second   (u/units u/SI [0 0 1 0 0 0 0] 1))
(def &ampere   (u/units u/SI [0 0 0 1 0 0 0] 1))
(def &kelvin   (u/units u/SI [0 0 0 0 1 0 0] 1))
(def &mole     (u/units u/SI [0 0 0 0 0 1 0] 1))
(def &candela  (u/units u/SI [0 0 0 0 0 0 1] 1))

(def & wu/with-units)

(comment
  (let [meter-squared (u/*units &meter &meter)]
    [(.exponents meter-squared)
     (.scale meter-squared)])
  ;; => [[2 0 0 0 0 0 0] 1]

  (u/*units &meter &meter)
  )
