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
  (:require [sicmutils.util.aggregate :as ua]))

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

(defn split-point
  "If `fuzz-factor` is non-zero, pick a point within $fuzz-factor \\over 2$ of the
  midpoint to split some interval.

  Else, returns the midpoint (ie `fuzz-factor` defaults to 0)."
  ([a b] (split-point a b 0))
  ([a b fuzz-factor]
   {:pre [(>= fuzz-factor 0)
          (< fuzz-factor 1)]}
   (let [width  (- b a)
         offset (if (zero? fuzz-factor)
                  0.5
                  (+ 0.5 (* fuzz-factor (dec (rand 2.0)))))]
     (+ a (* offset width)))))

;; Intervals... these no longer wrap the values themselves, but live alongside.

(def open        [::open ::open])
(def closed      [::closed ::closed])
(def open-closed [::open ::closed])
(def closed-open [::closed ::open])

(defn closed? [x] (= x closed))
(def open? (complement closed?))

(defn close-l [[_ r]] [::closed r])
(defn close-r [[l _]] [l ::closed])
(defn open-l [[_ r]] [::open r])
(defn open-r [[l _]] [l ::open])

(defn- fill-defaults [opts]
  (merge {:maxterms *adaptive-depth-limit*
          :fuzz-factor *quadrature-neighborhood-width*
          :interval open}
         opts))

(defn adaptive
  "Accepts two 'integrator' functions of:

  - `f`: some integrand
  - `a` and `b`: the lower and upper endpoints of integration
  - `opts`, a dictionary of configuration options

  And returns a new function of the same signature that adaptively subdivides
  the region $a, b$ into intervals if integration fails to converge."
  ([integrator] (adaptive integrator integrator))
  ([open-integrator closed-integrator]
   (fn rec
     ([f a b] (rec f a b {}))
     ([f a b opts]
      (let [opts      (fill-defaults opts)
            integrate (fn [l r interval]
                        (if (closed? interval)
                          (closed-integrator f l r opts)
                          (open-integrator f l r opts)))]
        (loop [sum   (ua/kahan-sum)
               stack [[a b (:interval opts)]]]
          (if (empty? stack)
            (first sum)
            (let [[l r interval] (peek stack)
                  remaining      (pop stack)
                  {:keys [converged? result]} (integrate l r interval)]
              (if converged?
                (recur (ua/kahan-sum sum result) remaining)
                (let [midpoint (split-point l r (:fuzz-factor opts))]
                  (recur sum (conj remaining
                                   [midpoint r (close-l interval)]
                                   [l midpoint (close-r interval)]))))))))))))


;; Example:
#_
(let [f (fn [x] (/ 4 (+ 1 (* x x))))
      integrate (adaptive
                 bs/open-integral
                 bs/closed-integral)]
  (time (integrate f 0 1)))
