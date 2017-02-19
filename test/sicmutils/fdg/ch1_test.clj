(ns sicmutils.fdg.ch1-test
  (:refer-clojure :exclude [+ - * / zero? ref partial])
  (:require [clojure.test :refer :all]
            [sicmutils.env :refer :all]
            [sicmutils.simplify :refer [hermetic-simplify-fixture]])
  (:import (org.w3c.dom.html HTMLFrameElement)
           (com.sun.org.apache.bcel.internal.generic L2D)))

(defn Lfree
  [mass]
  (fn [[t q v]]
    (* 1/2 mass (square v))))

(defn sphere->R3
  [R]
  (fn [[t [theta phi] v]]
    (up (* R (sin theta) (cos phi))                         ; x
        (* R (sin theta) (sin phi))                         ; y
        (* R (cos theta)))))                                ; z

(defn Lsphere
  [m R]
  (compose (Lfree m) (F->C (sphere->R3 R))))

(defn L2
  [mass metric]
  (fn [place velocity]
    (* 1/2 mass ((metric velocity velocity) place))))

;(defn Lc
;  [mass metric coordsys]
;  (fn [[t x v]]
;    (let [e coordinate-system->vector-basis coordsys]
;      ((L2 mass metric) ((point coordsys) x) (* e v)))))
;
;(def the-metric (literal-metric 'g R2-rect))

(deftest chapter-one
  (is (= '(+ (* 1/2 (expt (sin theta) 2) (expt R 2) m (expt phidot 2))
             (* 1/2 (expt R 2) m (expt thetadot 2)))
         (simplify
           ((Lsphere 'm 'R)
            (up 't (up 'theta 'phi) (up 'thetadot 'phidot)))))))
