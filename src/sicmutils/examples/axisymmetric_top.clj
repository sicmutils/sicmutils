(ns sicmutils.examples.axisymmetric-top
  (:refer-clojure :exclude [+ - * / zero? partial ref])
  (:require [sicmutils.env :refer :all]))

(defn L-axisymmetric-top
  [A C gMR]
  (fn [[t [theta phi psi] [thetadot phidot psidot]]]
    (+ (* 1/2 A
          (+ (square thetadot)
             (square (* phidot (sin theta)))))
       (* 1/2 C
          (square (+ psidot (* phidot (cos theta)))))
       (* -1 gMR (cos theta)))))

