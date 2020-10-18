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

(ns sicmutils.numerical.quadrature
  (:require [sicmutils.numerical.compile :as c]
            [sicmutils.numerical.quadrature.adaptive :as qa]
            [sicmutils.numerical.quadrature.boole :as boole]
            [sicmutils.numerical.quadrature.common :as qc]
            [sicmutils.numerical.quadrature.bulirsch-stoer :as bs]
            [sicmutils.numerical.quadrature.infinite :as qi]
            [sicmutils.numerical.quadrature.midpoint :as mid]
            [sicmutils.numerical.quadrature.milne :as milne]
            [sicmutils.numerical.quadrature.riemann :as riemann]
            [sicmutils.numerical.quadrature.romberg :as romberg]
            [sicmutils.numerical.quadrature.simpson :as simp]
            [sicmutils.numerical.quadrature.simpson38 :as simp38]
            [sicmutils.numerical.quadrature.trapezoid :as trap]
            [sicmutils.util :as u]))

(def ^:private quad-methods
  {:open        {:method :adaptive-bulirsch-stoer
                 :interval qc/open}
   :closed      {:method :adaptive-bulirsch-stoer
                 :interval qc/closed}
   :closed-open {:method :adaptive-bulirsch-stoer
                 :interval qc/closed-open}
   :open-closed {:method :adaptive-bulirsch-stoer
                 :interval qc/open-closed}

   ;; TODO make it switch internally based on the interval.
   :bulirsch-stoer-open bs/open-integral
   :bulirsch-stoer-closed bs/closed-integral
   :adaptive-bulirsch-stoer (qa/adaptive bs/open-integral bs/closed-integral)
   :left-riemann riemann/left-integral
   :right-riemann riemann/right-integral
   :lower-riemann riemann/lower-integral
   :upper-riemann riemann/upper-integral
   :midpoint mid/integral
   :trapezoid trap/integral
   :boole boole/integral
   :milne milne/integral
   :simpson simp/integral
   :simpson38 simp38/integral

   ;; TODO make it switch internally based on the interval.
   :romberg romberg/closed-integral
   :romberg-open romberg/open-integral})

(defn- extract-method
  "Attempts to turn the supplied argument into an integration method; returns nil
  if method doesn't exist."
  [method]
  (cond (fn? method)
        [method {}]

        (keyword? method)
        (process-method
         (quad-methods method))

        (map? method)
        (let [[f m] (process-method
                     (:method method))]
          [f (merge (dissoc method :method) m)])))

(defn get-integrator
  "Returns:

  - an integrator function
  - new opts

  TODO document and test custom function, and all of the ways we're supposed to
  be able to use this."
  ([method a b] (get-integrator method a b {}))
  ([method a b m]
   (let [[integrate opts] (extract-method method)
         integrate (if (or (qc/infinite? a)
                           (qc/infinite? b))
                     (partial qi/evaluate-infinite-integral integrate)
                     integrate)]
     [integrate (dissoc (merge opts m) :method)])))

;; ## SCMUtils Style Interface
;;
;; This is here so that we can match the refman.

(defrecord DefiniteIntegrator [integrand lower-limit upper-limit error method])

;; Updater functions.

(defn with-integrand [m integrand]
  (assoc m :integrand integrand))

(defn with-lower-limit [m limit]
  (assoc m :lower-limit limit))

(defn with-upper-limit [m limit]
  (assoc m :lower-limit limit))

(defn with-error [m error]
  (assoc m :error error))

(defn with-method [m method]
  (assoc m :method method))

(defn definite-integrator
  "Make a definite integrator with defaults set properly."
  [m]
  (let [defaults {:integrand false
                  :lower-limit false
                  :upper-limit false
                  :error 1e-10
                  :method :open}]
    (map->DefiniteIntegrator (merge defaults m))))

(defn definite-integral
  "Evaluates the definite integral. Takes either the scmutils style thing, or the
  arguments that a general integrator accepts."
  ([{:keys [integrand lower-limit upper-limit error method]}]
   (let [opts {:method method
               :tolerance error}]
     (definite-integral integrand lower-limit upper-limit opts)))

  ([f a b] (definite-integral f a b {}))
  ([f a b {:keys [method compile? info?]
           :or {method :open
                compile? false
                info? false}
           :as opts}]
   (if-let [[integrate m] (get-integrator method a b opts)]
     (let [f      (if compile? (c/compile-univariate-function f) f)
           result (integrate f a b m)]
       (if info? result (:result result)))
     (u/illegal (str "Unknown method: " method)))))
