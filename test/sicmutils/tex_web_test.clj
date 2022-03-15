#_
"Copyright © 2017 Colin Smith.
This work is based on the Scmutils system of MIT/GNU Scheme:
Copyright © 2002 Massachusetts Institute of Technology

This is free software;  you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or (at
your option) any later version.

This software is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this code; if not, see <http://www.gnu.org/licenses/>."

#_{:clj-kondo/ignore [:refer-all]}
(ns sicmutils.tex-web-test
  (:refer-clojure :exclude [+ - * / = compare ref partial zero? numerator denominator])
  (:require [nextjournal.clerk :as clerk]
            [sicmutils.env :as e :refer :all]
            [sicmutils.examples.central-potential :as central]
            [sicmutils.examples.double-pendulum :as double]
            [sicmutils.examples.driven-pendulum :as driven]))

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
 (-> (taylor-series
      (literal-function 'f (up 0 0) 0)
      (up 'x 'y)
      (up 'dx 'dy))
     (series:sum 2)))

;; ## dcot

(->tex
 ((D (/ tan)) 'x))
