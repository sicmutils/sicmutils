(ns sicmutils.units.scm-api
  "Copying the original scmutils API here, so that we can evaluate it. Defer
  finding an idiomatic Clojure API. Consider not pushing this namespace at all."
  (:require [sicmutils.value :as v]
            [sicmutils.generic :as g]
            [sicmutils.env :as e]
            [sicmutils.units.units :as u]
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


  ;;
  )

(comment
  (u/*units &meter &meter)
  ;; => #object[sicmutils.units.units.Units 0x6d3d2d71 "sicmutils.units.units.Units@6d3d2d71"]

  ;; This is completely unreadable. I'd like something a little better for
  ;; debugging. Do I ... implement a toString on Units?

  ;; Look at matrix, again.
  ;; matrix has a tostring calling str: (toString [_] (str v))

  (.toString (u/*units &meter &meter))
  ;; => "sicmutils.units.units.Units@22737a40"

  (str (u/*units &meter &meter))
  ;; => "sicmutils.units.units.Units@3ece403"

  ;; These two already does the same thing.
  ;; What does matrix do?
  (g/* [1 2 3] [10 20 30])
  ;; => #object[sicmutils.structure.Structure 0x6f544435 "(up (up 10 20 30) (up 20 40 60) (up 30 60 90))"]

  (g/* [[1 2] [3 4]] [[10 20] [30 40]])
  (up (up (up (up 10 20) (up 30 40))
          (up (up 20 40) (up 60 80)))
      (up (up (up 30 60) (up 90 120))
          (up (up 40 80) (up 120 160))))
  ;; this doesn't look like a matrix. Wonder what I'm actually doing here. I
  ;; suspect it's related to tensors.

  (e/matrix-by-rows [1  2  3  4  5]
                    [6  7  8  9 10]
                    [11 12 13 14 15])
  ;; this looks like a matrix 🎉
  ;;
  (g/* (e/matrix-by-rows [1 2] [3 4])
       (e/matrix-by-rows [10 20] [30 40]))
  ;; => #object[sicmutils.matrix.Matrix 0x38473da4 "[[70 100] [150 220]]"]

  ;; here we go.
  ;; Now, which function is this?

  ;; How do I control how a deftype prints?
  (u/*units &meter &meter)

  (e/*
   (e/expt '&meter 2))
  )
