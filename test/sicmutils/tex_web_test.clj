;
; Copyright © 2017 Colin Smith.
; This work is based on the Scmutils system of MIT/GNU Scheme:
; Copyright © 2002 Massachusetts Institute of Technology
;
; This is free software;  you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3 of the License, or (at
; your option) any later version.
;
; This software is distributed in the hope that it will be useful, but
; WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
; General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this code; if not, see <http://www.gnu.org/licenses/>.
;

(ns sicmutils.tex-web-test
  (:refer-clojure :exclude [+ - * / compare ref partial zero?])
  (:require [hiccup.core :refer :all]
            [hiccup.page :refer :all]
            [clojure.string :as s]
            [sicmutils.env :refer :all]
            [sicmutils.calculus.derivative :refer [taylor-series]]
            [sicmutils.examples.driven-pendulum :as driven]
            [sicmutils.examples.double-pendulum :as double]
            [sicmutils.examples.central-potential :as central])
  (:gen-class))

(defn generate-page
  []
  (spit
   "test.html"
   (html5
    [:head
     [:title "TeX rendering test"]
     [:meta {:http-equiv "Content-Type"
             :content "text/html; charset=UTF-8"}]
     (include-css "http://cdnjs.cloudflare.com/ajax/libs/KaTeX/0.5.1/katex.min.css")
     (include-js "http://cdnjs.cloudflare.com/ajax/libs/KaTeX/0.5.1/katex.min.js")]
    [:body
     (for [[name eqn]
           [["series" (simplify (series:sum (((exp (* 'epsilon D)) (literal-function 'g)) 'x) 5))]
            ["accents" (simplify (+ 'x
                                    'ydot
                                    'rhodotdot
                                    'sigmadotdotdot
                                    'taudotdotdotdot
                                    'Xihat
                                    (expt 'ell 2)
                                    'gammabar
                                    'mubardot
                                    'xivec
                                    'Ftilde))]["driven-pendulum" (driven/equations)]
            ["double-pendulum" (double/equations)]
            ["central-potential" (central/equations)]
            ["down-of-ups" (simplify (down (up 'a 'b) (up 'c 'd)))]
            ["up-of-downs" (simplify (up (down 'a 'b) (down 'c 'd)))]
            ["product" (simplify (* (down 'a 'b) (up 'c 'd)))]
            ["expansion" (simplify
                          (-> (taylor-series
                               (literal-function 'f (up 0 0) 0)
                               (up 'x 'y)
                               (up 'dx 'dy))
                              (series:sum 2)))]

            ["dcot" (simplify ((D (/ tan)) 'x))]]]
       (let [t (->TeX eqn)]
         [:div
          [:h3 name]
          [:div {:id name :class :eqn}]
          [:script (format "katex.render(\"%s\", document.getElementById('%s'));"
                           (s/escape t {\\ "\\\\"})
                           name)]]))])))

;; (generate-page)
