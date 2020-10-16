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
  "This namespace holds the scmutils-style interface for quadrature methods.

  TODO http://phys.uri.edu/nigh/NumRec/bookfpdf/f4-4.pdf talks about how you can
  do a different change of variables to handle integrable power law
  singularities at the endpoints."
  (:refer-clojure :exclude [methods])
  (:require [sicmutils.numerical.compile :as c]
            [sicmutils.numerical.quadrature.bulirsch-stoer :as bs]
            [sicmutils.numerical.quadrature.infinite :as qi]
            [sicmutils.numerical.quadrature.legendre-gauss :as lg]
            [sicmutils.numerical.quadrature.rational :as qr]
            [sicmutils.numerical.quadrature.riemann :as riemann]
            [sicmutils.numerical.quadrature.romberg :as romberg]
            [sicmutils.numerical.quadrature.trapezoid :as trap]
            [sicmutils.util :as u]
            [sicmutils.value :as v]))

;;; Default compile integrand
(def ^:dynamic *compile-integrand?* true)

;;; Default error specification
(def ^:dynamic *definite-integral-allowable-error* 1.0e-11)

;;; TODO what the hell is this? Default ditherer off (see rational.scm)
(def ^:dynamic *quadrature-neighborhood-width* false)

(def methods
  ;; Integration methods available below.
  {:open qr/integrate-open-open
   :closed-closed qr/integrate-closed-closed
   :closed-open qr/integrate-closed-open
   :open-closed qr/integrate-open-closed
   :open-open qr/integrate-open-open

   ;; specific methods for evaluating finite integrals.
   :left-riemann riemann/left-riemann-sum
   :right-riemann riemann/right-riemann-sum
   :lower-riemann riemann/lower-riemann-sum
   :upper-riemann riemann/upper-riemann-sum
   :midpoint riemann/midpoint-better
   :simpson riemann/simpson-rule
   :trapezoid trap/integrator
   :romberg romberg/romberg-quadrature

   ;; Only supported for Clojure, no CLJS for now.
   :legendre-gauss lg/integrator

   ;; these remain!
   :bulirsch-stoer bs/integrator
   :simpson38 nil
   :boole nil
   })

(defn integration-method
  "Returns the integration method appropriate for the supplied endpoints, or nil
  of the method doesn't match anything known.

  TODO document and test custom function."
  [method a b]
  (cond (fn? method) method

        (or (qi/infinite? a) (qi/infinite? b))
        qi/evaluate-infinite-integral
        :else (methods method)))

(defn evaluate-definite-integral
  ([{:keys [integrand lower-limit upper-limit error method]}]
   (evaluate-definite-integral method
                               integrand
                               lower-limit
                               upper-limit
                               error))
  ([method integrand lower-limit upper-limit allowable-error]
   (if-let [f (integration-method method lower-limit upper-limit)]
     (f integrand lower-limit upper-limit allowable-error)
     (u/illegal (str "Unknown method -- DEFINITE-INTEGRAL" method)))))

;; Boom, record form of the definite integrator from sicmutils.
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

(defn make-definite-integrator
  "Make a definite integrator with defaults set properly."
  [m]
  (let [defaults {:integrand false
                  :lower-limit false
                  :upper-limit false
                  :error 1e-10
                  :method :open}]
    (map->DefiniteIntegrator (merge defaults m))))

;; Then these are from `defint.cljc`... From scmutils: This is the numerical
;; definite integration system interface. This seems to be the thing that we've
;; already implemented, sort of the gateway.

(defn definite-integral-with-tolerance [f x1 x2 tolerance]
  (evaluate-definite-integral
   (make-definite-integrator {:integrand f
                              :lower-limit x1
                              :upper-limit x2
                              :error tolerance})))

;;; Assumes f is purely numerical, and no units on t1, t2

(defn definite-integral-numerical
  ([f t1 t2] (definite-integral-numerical f t1 t2 {}))
  ([f t1 t2 {:keys [tolerance compile?]
             :or {tolerance *definite-integral-allowable-error*
                  compile? *compile-integrand?*}}]
   (if (and (v/number? t1) (v/number? t2) (= t1 t2))
     0
     (let [integrand (if compile? (c/compile-univariate-function f) f)]
       (definite-integral-with-tolerance f t1 t2 tolerance)))))

(defn definite-integral
  "Alias, since we don't support different units."
  ([f t1 t2] (definite-integral f t1 t2 {}))
  ([f t1 t2 opts]
   (definite-integral-numerical f t1 t2 opts)))
