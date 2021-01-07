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

(defn ns-macros
  "Given a map of symbol => var, returns a sequence of the symbols associated with
  macro value."
  [sym->var]
  (mapcat (fn [[sym var]]
            (if (macro? var) [sym] []))
          sym->var))

(defn sci-ns
  "Given a map of symbol => var, returns a map of symbol => var with:

  - any pair removed whose value is a macro (tagged with `:macro true` metadata)
  - all other values resolved"
  [sym->var]
  (letfn [(process [[sym var]]
            (if-not (macro? var)
              [[sym @var]]
              (if-let [sci-macro (macros/all sym)]
                [[sym sci-macro]]
                [])))]
    (into {} (mapcat process) sym->var)))

(def ^{:doc "Map whose values are the symbols of of all namespaces explicitly
checked and whitelisted for SCI compilation and interesting enough in their own
right to expose to a user by default. Each value is the sym->var map for the
corresponding namespace."}
  ns-map
  {'sicmutils.env                              (ns-publics 'sicmutils.env)
   'sicmutils.generic                          (ns-publics 'sicmutils.generic)
   'sicmutils.function                         (ns-publics 'sicmutils.function)
   'sicmutils.operator                         (ns-publics 'sicmutils.operator)
   'sicmutils.series                           (ns-publics 'sicmutils.series)
   'sicmutils.structure                        (ns-publics 'sicmutils.structure)
   'sicmutils.matrix                           (ns-publics 'sicmutils.matrix)
   'sicmutils.abstract.function                (ns-publics 'sicmutils.abstract.function)
   'sicmutils.calculus.basis                   (ns-publics 'sicmutils.calculus.basis)
   'sicmutils.calculus.coordinate              (ns-publics 'sicmutils.calculus.coordinate)
   'sicmutils.calculus.covariant               (ns-publics 'sicmutils.calculus.covariant)
   'sicmutils.calculus.derivative              (ns-publics 'sicmutils.calculus.derivative)
   'sicmutils.calculus.form-field              (ns-publics 'sicmutils.calculus.form-field)
   'sicmutils.calculus.manifold                (ns-publics 'sicmutils.calculus.manifold)
   'sicmutils.calculus.map                     (ns-publics 'sicmutils.calculus.map)
   'sicmutils.calculus.vector-field            (ns-publics 'sicmutils.calculus.vector-field)
   'sicmutils.mechanics.lagrange               (ns-publics 'sicmutils.mechanics.lagrange)
   'sicmutils.mechanics.hamilton               (ns-publics 'sicmutils.mechanics.hamilton)
   'sicmutils.mechanics.rigid                  (ns-publics 'sicmutils.mechanics.rigid)
   'sicmutils.mechanics.rotation               (ns-publics 'sicmutils.mechanics.rotation)
   'sicmutils.numerical.derivative             (ns-publics 'sicmutils.numerical.derivative)
   'sicmutils.numerical.quadrature             (ns-publics 'sicmutils.numerical.quadrature)
   'sicmutils.numerical.ode                    (ns-publics 'sicmutils.numerical.ode)
   'sicmutils.numerical.minimize               (ns-publics 'sicmutils.numerical.minimize)
   'sicmutils.numerical.interpolate.polynomial (ns-publics 'sicmutils.numerical.interpolate.polynomial)
   'sicmutils.numerical.interpolate.rational   (ns-publics 'sicmutils.numerical.interpolate.rational)
   'sicmutils.numerical.interpolate.richardson (ns-publics 'sicmutils.numerical.interpolate.richardson)
   'sicmutils.numerical.multimin.nelder-mead   (ns-publics 'sicmutils.numerical.multimin.nelder-mead)
   'sicmutils.numerical.unimin.bracket         (ns-publics 'sicmutils.numerical.unimin.bracket)
   'sicmutils.numerical.unimin.brent           (ns-publics 'sicmutils.numerical.unimin.brent)
   'sicmutils.numerical.unimin.golden          (ns-publics 'sicmutils.numerical.unimin.golden)})

(def ^{:doc "Default sci context options required (currently only `:namespace`
  bindings) required to evaluate SICMUtils forms from inside of an SCI
  context. Pass these to `sci/init` to generate an sci context."}
  context-opts
  (let [ns-map (into {}
                     (map (fn [[k v]] [k (sci-ns v)]))
                     ns-map)]
    {:namespaces
     (assoc ns-map 'user (ns-map 'sicmutils.env))}))

(def ^{:doc "sci context (currently only `:namespace` bindings) required to
  evaluate SICMUtils forms via SCI"}
  context
  (sci/init context-opts))
