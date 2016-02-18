(ns sicmutils.tex-web-test
  (:refer-clojure :exclude [+ - * / ref partial zero?])
  (:require [hiccup.core :refer :all]
            [hiccup.page :refer :all]
            [clojure.string :as s]
            [sicmutils.env :refer :all]
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
           [["driven-pendulum" (driven/equations)]
            ["double-pendulum" (double/equations)]
            ["central-potential" (central/equations)]
            ["down-of-ups" (simplify (down (up 'a 'b) (up 'c 'd)))]
            ["up-of-downs" (simplify (up (down 'a 'b) (down 'c 'd)))]
            ["product" (simplify (* (down 'a 'b) (up 'c 'd)))]
            ["expansion" (simplify
                          (reduce +
                                  (take 3 (taylor-series-terms
                                           (literal-function 'f (up 0 0) 0)
                                           (up 'x 'y)
                                           (up 'dx 'dy)))))]
            ["accents" (simplify (+ 'x
                                    'ydot
                                    'rhodotdot
                                    'sigmadotdotdot
                                    'Xihat
                                    (expt 'ell 2)
                                    'gammabar
                                    'mubardot
                                    'xivec
                                    'Ftilde))]]]
       (let [t (->TeX eqn)]
         [:div
          [:h3 name]
          [:div {:id name :class :eqn}]
          [:script (format "katex.render(\"%s\", document.getElementById('%s'));"
                           (s/escape t {\\ "\\\\"})
                           name)]]))])))

;; (generate-page)
