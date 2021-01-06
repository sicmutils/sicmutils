;;
;; Copyright © 2020 Colin Smith.
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

(ns sicmutils.env.sci
  (:require [sci.core :as sci]
            [sicmutils.env]
            [sicmutils.env.sci.macros :as macros]
            [sicmutils.util :as u]))

(def macro? (comp :macro meta))

(defn resolve-publics
  "Idempotent version of `ns-publics`. All three forms are identical for a
  namespace symbol:

  (ns-publics sym)
  (resolve-publics sym)
  (resolve-publics (resolve-publics sym))"
  [m-or-sym]
  (if (symbol? m-or-sym)
    (ns-publics m-or-sym)
    m-or-sym))

(defn ns-macros
  "Given a namespace symbol (or a map of symbol => var), returns a sequence of the
  symbols associated with macro value."
  [m-or-sym]
  (mapcat (fn [[sym var]]
            (if (macro? var) [sym] []))
          (resolve-publics m-or-sym)))

(defn sci-ns
  "Given a namespace symbol (or a map of symbol => var), returns a map of symbol
  => var with:

  - any pair removed whose value is a macro (tagged with `:macro true` metadata)
  - all other values resolved"
  [m-or-sym]
  (letfn [(process [[sym var]]
            (if-not (macro? var)
              [[sym @var]]
              (if-let [sci-macro (macros/all sym)]
                [[sym sci-macro]]
                [])))]
    (let [sym->var (resolve-publics m-or-sym)]
      (into {} (mapcat process) sym->var))))

(def ^{:doc "Set of all namespaces explicitly checked and whitelisted for SCI
compilation and interesting enough in their own right to expose to a user by
default."}
  namespaces
  #{'sicmutils.env
    'sicmutils.generic
    'sicmutils.function
    'sicmutils.operator
    'sicmutils.series
    'sicmutils.structure
    'sicmutils.matrix
    'sicmutils.abstract.function
    'sicmutils.calculus.basis
    'sicmutils.calculus.coordinate
    'sicmutils.calculus.covariant
    'sicmutils.calculus.derivative
    'sicmutils.calculus.form-field
    'sicmutils.calculus.manifold
    'sicmutils.calculus.map
    'sicmutils.calculus.vector-field
    'sicmutils.mechanics.lagrange
    'sicmutils.mechanics.hamilton
    'sicmutils.mechanics.rigid
    'sicmutils.mechanics.rotation
    'sicmutils.numerical.derivative
    'sicmutils.numerical.quadrature
    'sicmutils.numerical.ode
    'sicmutils.numerical.minimize
    'sicmutils.numerical.interpolate.polynomial
    'sicmutils.numerical.interpolate.rational
    'sicmutils.numerical.interpolate.richardson
    'sicmutils.numerical.multimin.nelder-mead
    'sicmutils.numerical.unimin.bracket
    'sicmutils.numerical.unimin.brent
    'sicmutils.numerical.unimin.golden})

(def ^{:doc "Default sci context options required (currently only `:namespace`
  bindings) required to evaluate SICMUtils forms from inside of an SCI
  context. Pass these to `sci/init` to generate an sci context."}
  context-opts
  (let [ns-map (u/keys->map sci-ns namespaces)]
    {:namespaces
     (assoc ns-map 'user (ns-map 'sicmutils.env))}))

(def ^{:doc "sci context (currently only `:namespace` bindings) required to
  evaluate SICMUtils forms via SCI"}
  context
  (sci/init context-opts))
