#_"SPDX-License-Identifier: GPL-3.0"

#_{:clj-kondo/ignore [:refer-all]}
(ns emmy.tex-web-test
  (:refer-clojure :exclude [+ - * / = compare ref partial zero? numerator denominator])
  (:require [nextjournal.clerk :as clerk]
            [emmy.env :as e :refer :all]
            [emmy.examples.central-potential :as central]
            [emmy.examples.double-pendulum :as double]
            [emmy.examples.driven-pendulum :as driven]))

;; ## Showing Off Renderers

(def ->tex
  (comp nextjournal.clerk/tex ->TeX simplify))

;; ## Series

(->tex
 (series:sum
  (((exp (* 'epsilon D)) (literal-function 'g)) 'x) 5))

;; ## Accents

(->tex
 (+ 'x
    'ydot
    'rhodotdot
    'sigmadotdotdot
    'taudotdotdotdot
    'Xihat
    (expt 'ell 2)
    'gammabar
    'mubardot
    'xivec
    'Ftilde))

;; ## Driven Pendulum

(->tex (driven/equations))

;; ## Double Pendulum

(->tex (double/equations))

;; ## central-potential

(->tex (central/equations))

;; ## down-of-ups

(->tex
 (down (up 'a 'b) (up 'c 'd)))

;; up-of-downs

(->tex
 (up (down 'a 'b) (down 'c 'd)))

;; ## product

(->tex
 (* (down 'a 'b) (up 'c 'd)))

;; ## expansion

(->tex
 (-> ((taylor-series
       (literal-function 'f (up 0 0) 0)
       (up 'x 'y))
      (up 'dx 'dy))
     (series:sum 2)))

;; ## dcot

(->tex
 ((D (/ tan)) 'x))
