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
  (:require [sicmutils.expression.compile :as c]
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

;; ## Numerical Quadrature
;;
;; This namespace unites all of the work inside `sicmutils.numerical.quadrature`
;; behind a single interface, fronted by the all-powerful `definite-integral`
;; function.
;;
;; The interface takes `f`, an integrand, along with bounds `a` and `b`:
;;
;; (definite-integral f a b)
;;
;; Optionally, you can provide a dictionary of customizing options. These are
;; passed down to whatever method you supply via the `:method` key.
;;
;; (definite-integral f a b opts)
;;
;;
;; ## Implementation
;;
;; The keys in `quad-methods` below define the full range of integration methods
;; available in the package. Each entry in this dictionary is either:
;;
;; - An 'integrator' function that matches the interface above for
;;   `definite-integral` (possibly created with `qc/defintegrator`)
;;
;; - a dictionary of extra options. This must contain a `:method` key.
;;
;; This latter style is used when the method itself is a specialization of a
;; more general method.

(def ^:private quadrature-methods
  {:open                    {:method :adaptive-bulirsch-stoer
                             :interval qc/open}
   :closed                  {:method :adaptive-bulirsch-stoer
                             :interval qc/closed}
   :closed-open             {:method :adaptive-bulirsch-stoer
                             :interval qc/closed-open}
   :open-closed             {:method :adaptive-bulirsch-stoer
                             :interval qc/open-closed}
   :bulirsch-stoer-open     bs/open-integral
   :bulirsch-stoer-closed   bs/closed-integral
   :adaptive-bulirsch-stoer (qa/adaptive bs/open-integral bs/closed-integral)
   :left-riemann            riemann/left-integral
   :right-riemann           riemann/right-integral
   :lower-riemann           riemann/lower-integral
   :upper-riemann           riemann/upper-integral
   :midpoint                mid/integral
   :trapezoid               trap/integral
   :boole                   boole/integral
   :milne                   milne/integral
   :simpson                 simp/integral
   :simpson38               simp38/integral
   :romberg                 romberg/closed-integral
   :romberg-open            romberg/open-integral})

(def available-methods
  (into #{} (keys quadrature-methods)))

;; The user can specify a method by providing the `:method` key in their options
;; with:
;;
;; - a key in the above dictionary
;; - another dict
;; - a custom integration function
;;
;; The latter two are the allowed value types in `quadrature-methods`.

(defn- extract-method
  "Attempts to turn the supplied argument into an integration method; returns nil
  if method doesn't exist."
  [method]
  (cond (fn? method)
        [method {}]

        (keyword? method)
        (extract-method
         (quadrature-methods method))

        (map? method)
        (let [[f m] (extract-method
                     (:method method))]
          [f (merge (dissoc method :method) m)])))

(defn get-integrator
  "Takes:

  - An integration method, specified as either:
    - a keyword naming one of the available methods in `available-methods`
    - a function with the proper integrator signature
    - a dictionary of integrator options with a `:method` key

  - `a` and `b` integration endpoints
  - an optional dictionary of options `m`

  And returns a pair of an integrator function and a possibly-enhanced options
  dictionary.

  (Some integration functions require extra options, so the returned dictionary
  may have more entries than the `m` you pass in.)

  If either endpoint is infinite, the returned integrator is wrapped in
  `qi/improper` and able to handle infinite endpoints (as well as non-infinite
  endpoints by passing through directly to the underlying integrator)."
  ([method a b] (get-integrator method a b {}))
  ([method a b m]
   (when-let [[integrate opts] (extract-method method)]
     (let [integrate (if (or (qc/infinite? a)
                             (qc/infinite? b))
                       (qi/improper integrate)
                       integrate)]
       [integrate (dissoc (merge opts m) :method)]))))

;; ## Final API
;;
;; Here we are! The one function you need care about if you're interested in
;; definite integrals. Learn to use this, and then dig in to the details of
;; individual methods if you run into trouble or want to learn more. Enjoy!

(defn definite-integral
  "Evaluates the definite integral of integrand `f` across the interval $a, b$.
  Optionally accepts a dictionary `opts` of customizing options; All `opts` will
  be passed through to the supplied `integrate` functions.

  If you'd like more control, or to retrieve the integration function directly
  without looking it up via `:method` each time, see `get-integrator`.

  All supplied options are passed through to the underlying integrator; see the
  specific integrator for information on what options are available.

  ## Keyword arguments:

  `:method`: Specifies the integration method used. Must be

  - a keyword naming one of the available methods in `available-methods`
  - a function with the proper integrator signature
  - a dictionary of integrator options with a `:method` key

  Defaults to `:open`, which specifies an adaptive bulirsch-stoer quadrature method.

  `:compile?` If true, the generic function will be simplified and compiled
  before execution.

  `:info?` If true, `definite-integral` will return a map of integration
  information returned by the underlying integrator. Else, returns an estimate
  of the definite integral."
  ([f a b] (definite-integral f a b {}))
  ([f a b {:keys [method compile? info?]
           :or {method :open
                compile? false
                info? false}
           :as opts}]
   (if-let [[integrate m] (get-integrator method a b opts)]
     (let [f      #?(:clj (if compile? (c/compile-univariate-fn f) f)
                     :cljs f)
           result (integrate f a b m)]
       (if info? result (:result result)))
     (u/illegal (str "Unknown method: " method
                     ". Try one of: "
                     available-methods)))))
