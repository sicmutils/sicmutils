;;
;; Copyright © 2020 Sam Ritchie.
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

(ns sicmutils.numerical.quadrature.adaptive
  (:require [sicmutils.numerical.quadrature.common :as qc]
            [sicmutils.util.aggregate :as ua]))

;; ## Adaptive Quadrature

(comment
  ;; Comment from scmutils:
  "To make quadrature deterministic, but sensitive to special choices make the
  choice:"
  (binding [*quadrature-neighborhood-width* false]
    ,,,))

;; These all adaptively drop down if they're not able to converge a fit within
;; 10 steps.
;;
;; They recommend 8 in that text.
(def ^:dynamic *adaptive-depth-limit* 10)
(def ^:dynamic *quadrature-neighborhood-width* 0.05)

(defn- split-point
  "Returns a point within`fuzz-factor` of the midpoint of the interval $[a, b]$.
  `fuzz-factor` defaults to 0 (ie, `split-point` returns the midpoint)."
  ([a b] (split-point a b 0))
  ([a b fuzz-factor]
   {:pre [(>= fuzz-factor 0)
          (< fuzz-factor 1)]}
   (let [width  (- b a)
         offset (if (zero? fuzz-factor)
                  0.5
                  (+ 0.5 (* fuzz-factor (dec (rand 2.0)))))]
     (+ a (* offset width)))))

(defn- fill-defaults
  "Populates the supplied `opts` dictionary with defaults required by `adaptive`.
  Two of these have values controlled by dynamic variables in `adaptive.cljc`."
  [opts]
  (merge {:maxterms *adaptive-depth-limit*
          :fuzz-factor *quadrature-neighborhood-width*
          :interval qc/open}
         opts))

(defn adaptive
  "Accepts two 'integrators', ie, functions of:

  - `f`: some integrand
  - `a` and `b`: the lower and upper endpoints of integration
  - `opts`, a dictionary of configuration options

  And returns a new integrator that adaptively subdivides the region $a, b$ into
  intervals if integration fails to converge."
  ([integrator] (adaptive integrator integrator))
  ([open-integrator closed-integrator]
   (fn rec
     ([f a b] (rec f a b {}))
     ([f a b opts]
      (let [opts      (fill-defaults opts)
            integrate (fn [l r interval]
                        (if (qc/closed? interval)
                          (closed-integrator f l r opts)
                          (open-integrator f l r opts)))]
        (loop [stack [[a b (:interval opts)]]
               sum   (ua/kahan-sum)
               iteration 0]
          (if (empty? stack)
            {:converged? true
             :iterations iteration
             :result (first sum)}
            (let [[l r interval] (peek stack)
                  remaining      (pop stack)
                  {:keys [converged? result]} (integrate l r interval)]
              (if converged?
                (recur remaining
                       (ua/kahan-sum sum result)
                       (inc iteration))
                (let [midpoint (split-point l r (:fuzz-factor opts))]
                  (recur (conj remaining
                               [midpoint r (qc/close-l interval)]
                               [l midpoint (qc/close-r interval)])
                         sum
                         (inc iteration))))))))))))
