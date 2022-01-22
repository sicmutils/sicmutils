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
  (:refer-clojure :exclude [ns-map])
  (:require [sci.core :as sci]
            [sicmutils.env]
            [sicmutils.env.sci.macros :as macros]
            [sicmutils.util :as u]))

(def macro? (comp :macro meta))
(def dynamic? (comp :dynamic meta))

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
            (cond
              ;; Inside SCI, macros are replaced by rewritten-as-functions
              ;; versions of themselves, with additional slots for `&form` and
              ;; `&env`. We exclude them here so they can be replaced later.
              (macro? var) []

              ;; Keep dynamic variables as unresolved vars, so that they can
              ;; at least be inspected (at which point they'll reveal any
              ;; rebindings applied by the system)
              (dynamic? var) [[sym var]]

              ;; by default, the SCI environment holds values, not the vars
              ;; that they were attached to in non-SCI land.
              :else [[sym @var]]))]
    (into {} (mapcat process) sym->var)))

(def ^{:doc "Map whose values are the symbols of of all namespaces explicitly
 checked and whitelisted for SCI compilation and interesting enough in their own
 right to expose to a user by default. Each value is the sym->var map for the
 corresponding namespace."}
  ns->publics
  {'pattern.consequence                      (ns-publics 'pattern.consequence)
   'pattern.match                            (ns-publics 'pattern.match)
   'pattern.rule                             (ns-publics 'pattern.rule)
   'pattern.syntax                           (ns-publics 'pattern.syntax)
   'sicmutils.algebra.fold                   (ns-publics 'sicmutils.algebra.fold)
   'sicmutils.complex                        (ns-publics 'sicmutils.complex)
   'sicmutils.differential                   (ns-publics 'sicmutils.differential)
   'sicmutils.env                            (ns-publics 'sicmutils.env)
   'sicmutils.expression                     (ns-publics 'sicmutils.expression)
   'sicmutils.function                       (ns-publics 'sicmutils.function)
   'sicmutils.generic                        (ns-publics 'sicmutils.generic)
   'sicmutils.matrix                         (ns-publics 'sicmutils.matrix)
   'sicmutils.modint                         (ns-publics 'sicmutils.modint)
   'sicmutils.numsymb                        (ns-publics 'sicmutils.numsymb)
   'sicmutils.operator                       (ns-publics 'sicmutils.operator)
   'sicmutils.polynomial                     (ns-publics 'sicmutils.polynomial)
   'sicmutils.polynomial.factor              (ns-publics 'sicmutils.polynomial.factor)
   'sicmutils.polynomial.gcd                 (ns-publics 'sicmutils.polynomial.gcd)
   'sicmutils.polynomial.interpolate         (ns-publics 'sicmutils.polynomial.interpolate)
   'sicmutils.polynomial.richardson          (ns-publics 'sicmutils.polynomial.richardson)
   'sicmutils.quaternion                     (ns-publics 'sicmutils.quaternion)
   'sicmutils.ratio                          (ns-publics 'sicmutils.ratio)
   'sicmutils.rational-function              (ns-publics 'sicmutils.rational-function)
   'sicmutils.rational-function.interpolate  (ns-publics 'sicmutils.rational-function.interpolate)
   'sicmutils.series                         (ns-publics 'sicmutils.series)
   'sicmutils.simplify                       (ns-publics 'sicmutils.simplify)
   'sicmutils.simplify.rules                 (ns-publics 'sicmutils.simplify.rules)
   'sicmutils.structure                      (ns-publics 'sicmutils.structure)
   'sicmutils.util                           (ns-publics 'sicmutils.util)
   'sicmutils.value                          (ns-publics 'sicmutils.value)
   'sicmutils.abstract.function              (ns-publics 'sicmutils.abstract.function)
   'sicmutils.abstract.number                (ns-publics 'sicmutils.abstract.number)
   'sicmutils.calculus.basis                 (ns-publics 'sicmutils.calculus.basis)
   'sicmutils.calculus.connection            (ns-publics 'sicmutils.calculus.connection)
   'sicmutils.calculus.coordinate            (ns-publics 'sicmutils.calculus.coordinate)
   'sicmutils.calculus.covariant             (ns-publics 'sicmutils.calculus.covariant)
   'sicmutils.calculus.curvature             (ns-publics 'sicmutils.calculus.curvature)
   'sicmutils.calculus.derivative            (ns-publics 'sicmutils.calculus.derivative)
   'sicmutils.calculus.form-field            (ns-publics 'sicmutils.calculus.form-field)
   'sicmutils.calculus.frame                 (ns-publics 'sicmutils.calculus.frame)
   'sicmutils.calculus.hodge-star            (ns-publics 'sicmutils.calculus.hodge-star)
   'sicmutils.calculus.indexed               (ns-publics 'sicmutils.calculus.indexed)
   'sicmutils.calculus.manifold              (ns-publics 'sicmutils.calculus.manifold)
   'sicmutils.calculus.metric                (ns-publics 'sicmutils.calculus.metric)
   'sicmutils.calculus.map                   (ns-publics 'sicmutils.calculus.map)
   'sicmutils.calculus.vector-calculus       (ns-publics 'sicmutils.calculus.vector-calculus)
   'sicmutils.calculus.vector-field          (ns-publics 'sicmutils.calculus.vector-field)
   'sicmutils.expression.analyze             (ns-publics 'sicmutils.expression.analyze)
   'sicmutils.expression.compile             (ns-publics 'sicmutils.expression.compile)
   'sicmutils.expression.render              (ns-publics 'sicmutils.expression.render)
   'sicmutils.mechanics.lagrange             (ns-publics 'sicmutils.mechanics.lagrange)
   'sicmutils.mechanics.hamilton             (ns-publics 'sicmutils.mechanics.hamilton)
   'sicmutils.mechanics.rigid                (ns-publics 'sicmutils.mechanics.rigid)
   'sicmutils.mechanics.rotation             (ns-publics 'sicmutils.mechanics.rotation)
   'sicmutils.numerical.derivative           (ns-publics 'sicmutils.numerical.derivative)
   'sicmutils.numerical.minimize             (ns-publics 'sicmutils.numerical.minimize)
   'sicmutils.numerical.ode                  (ns-publics 'sicmutils.numerical.ode)
   'sicmutils.numerical.quadrature           (ns-publics 'sicmutils.numerical.quadrature)
   'sicmutils.numerical.multimin.nelder-mead (ns-publics 'sicmutils.numerical.multimin.nelder-mead)
   'sicmutils.numerical.unimin.bracket       (ns-publics 'sicmutils.numerical.unimin.bracket)
   'sicmutils.numerical.unimin.brent         (ns-publics 'sicmutils.numerical.unimin.brent)
   'sicmutils.numerical.unimin.golden        (ns-publics 'sicmutils.numerical.unimin.golden)
   'sicmutils.special.elliptic               (ns-publics 'sicmutils.special.elliptic)
   'sicmutils.special.factorial              (ns-publics 'sicmutils.special.factorial)
   'sicmutils.sr.boost                       (ns-publics 'sicmutils.sr.boost)
   'sicmutils.sr.frames                      (ns-publics 'sicmutils.sr.frames)
   'sicmutils.util.aggregate                 (ns-publics 'sicmutils.util.aggregate)
   'sicmutils.util.def                       (ns-publics 'sicmutils.util.def)
   'sicmutils.util.logic                     (ns-publics 'sicmutils.util.logic)
   'sicmutils.util.permute                   (ns-publics 'sicmutils.util.permute)
   'sicmutils.util.stream                    (ns-publics 'sicmutils.util.stream)})

(def ^{:doc "SCI namespace map generated from `ns->publics`. Consumers wishing
 to use a more minmal SCI environment, should can select interested namespaces
 from this map. Since in normal (not self-hosted) ClojureScript `ns-publics`
 does not include macros, they are added explicitly."}
  namespaces
  (let [ns-map (into {}
                     (map (fn [[k v]] [k (sci-ns v)]))
                     ns->publics)]
    (merge-with merge ns-map macros/ns-bindings)))

(def ^{:doc "Default sci context options required (currently only `:namespace`
  bindings) required to evaluate SICMUtils forms from inside of an SCI
  context. Pass these to `sci/init` to generate an sci context."}
  context-opts
  {:namespaces namespaces

   ;; NOTE that these entries are required if you'd like to call the
   ;; `sicmutils.algebra.fold/kbk-n` macro, which generates code using
   ;; `Math/abs`. JVM and js forms are shown.
   :classes #?(:clj  {'java.lang.Math java.lang.Math}
               :cljs {'js goog/global :allow :all})})

(def ^{:doc "sci context (currently only `:namespace` bindings) required to
  evaluate SICMUtils forms via SCI"}
  context
  (sci/init context-opts))
