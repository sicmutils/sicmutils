;;
;; Copyright © 2017 Colin Smith.
;; This work is based on the Scmutils system of MIT/GNU Scheme:
;; Copyright © 2002 Massachusetts Institute of Technology
;;
;; This is free software;  you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or (at
;; your option) any later version.
;;
;; This software is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this code; if not, see <http://www.gnu.org/licenses/>.
;;

(ns sicmutils.util.stream
  (:require [clojure.pprint :as pp]
            [sicmutils.generic :as g]))

(defn pprint [n xs]
  (doseq [x (take n xs)]
    (pp/pprint x)))

(defn doubling [n0]
  (iterate (fn [x] (* 2 x)) n0))

(defn scan [init f present]
  (fn [xs]
    (->> (reductions f init xs)
         (map present)
         (rest))))

;; And then a stream limiter. Generic function to take from a stream until it
;; converges, or some other marker gets hit. We can totally use this on the
;; minimizers, etc... and make a totally functional library.

(defn close-enuf?
  "relative closeness, transitioning to abs closeness when we get significantly
  smaller than 1."
  [tolerance]
  (fn [h1 h2]
    (<= (g/abs (- h1 h2))
        (* 0.5 tolerance (+ 2 (g/abs h1) (g/abs h2))))))

(defn seq-limit
  "A little ugly, but it works!

   TODO remove tolerance."
  ([xs tolerance] (seq-limit xs tolerance {}))
  ([xs tolerance {:keys [minterms maxterms convergence-fn fail-fn]
                  :or {minterms       2
                       convergence-fn (close-enuf? tolerance)}}]
   (if (empty? xs)
     {:converged? false
      :terms-checked 0
      :result        nil}
     (let [converged? convergence-fn
           stop?      (if maxterms
                        (fn [i] (>= i maxterms))
                        (constantly false))]
       (loop [[x1 & [x2 :as more]] xs
              terms-checked 1]
         (if (empty? more)
           {:converged?    false
            :terms-checked terms-checked
            :result        x1}
           (let [terms-checked (inc terms-checked)
                 converged?    (convergence-fn x1 x2)]
             (if (or converged? (stop? terms-checked))
               {:converged?    converged?
                :terms-checked terms-checked
                :result        x2}
               (recur more terms-checked)))))))))
