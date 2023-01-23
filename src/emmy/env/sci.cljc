#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.env.sci
  (:refer-clojure :exclude [ns-map])
  (:require [sci.core :as sci]
            [emmy.env]
            [emmy.env.sci.macros :as macros]
            [emmy.util :as u]))

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
   'emmy.algebra.fold                   (ns-publics 'emmy.algebra.fold)
   'emmy.complex                        (ns-publics 'emmy.complex)
   'emmy.differential                   (ns-publics 'emmy.differential)
   'emmy.env                            (ns-publics 'emmy.env)
   'emmy.expression                     (ns-publics 'emmy.expression)
   'emmy.function                       (ns-publics 'emmy.function)
   'emmy.generic                        (ns-publics 'emmy.generic)
   'emmy.matrix                         (ns-publics 'emmy.matrix)
   'emmy.modint                         (ns-publics 'emmy.modint)
   'emmy.numsymb                        (ns-publics 'emmy.numsymb)
   'emmy.operator                       (ns-publics 'emmy.operator)
   'emmy.polynomial                     (ns-publics 'emmy.polynomial)
   'emmy.polynomial.factor              (ns-publics 'emmy.polynomial.factor)
   'emmy.polynomial.gcd                 (ns-publics 'emmy.polynomial.gcd)
   'emmy.polynomial.interpolate         (ns-publics 'emmy.polynomial.interpolate)
   'emmy.polynomial.richardson          (ns-publics 'emmy.polynomial.richardson)
   'emmy.quaternion                     (ns-publics 'emmy.quaternion)
   'emmy.ratio                          (ns-publics 'emmy.ratio)
   'emmy.rational-function              (ns-publics 'emmy.rational-function)
   'emmy.rational-function.interpolate  (ns-publics 'emmy.rational-function.interpolate)
   'emmy.series                         (ns-publics 'emmy.series)
   'emmy.simplify                       (ns-publics 'emmy.simplify)
   'emmy.simplify.rules                 (ns-publics 'emmy.simplify.rules)
   'emmy.structure                      (ns-publics 'emmy.structure)
   'emmy.util                           (ns-publics 'emmy.util)
   'emmy.value                          (ns-publics 'emmy.value)
   'emmy.abstract.function              (ns-publics 'emmy.abstract.function)
   'emmy.abstract.number                (ns-publics 'emmy.abstract.number)
   'emmy.calculus.basis                 (ns-publics 'emmy.calculus.basis)
   'emmy.calculus.connection            (ns-publics 'emmy.calculus.connection)
   'emmy.calculus.coordinate            (ns-publics 'emmy.calculus.coordinate)
   'emmy.calculus.covariant             (ns-publics 'emmy.calculus.covariant)
   'emmy.calculus.curvature             (ns-publics 'emmy.calculus.curvature)
   'emmy.calculus.derivative            (ns-publics 'emmy.calculus.derivative)
   'emmy.calculus.form-field            (ns-publics 'emmy.calculus.form-field)
   'emmy.calculus.frame                 (ns-publics 'emmy.calculus.frame)
   'emmy.calculus.hodge-star            (ns-publics 'emmy.calculus.hodge-star)
   'emmy.calculus.indexed               (ns-publics 'emmy.calculus.indexed)
   'emmy.calculus.manifold              (ns-publics 'emmy.calculus.manifold)
   'emmy.calculus.metric                (ns-publics 'emmy.calculus.metric)
   'emmy.calculus.map                   (ns-publics 'emmy.calculus.map)
   'emmy.calculus.vector-calculus       (ns-publics 'emmy.calculus.vector-calculus)
   'emmy.calculus.vector-field          (ns-publics 'emmy.calculus.vector-field)
   'emmy.expression.analyze             (ns-publics 'emmy.expression.analyze)
   'emmy.expression.compile             (ns-publics 'emmy.expression.compile)
   'emmy.expression.render              (ns-publics 'emmy.expression.render)
   'emmy.mechanics.lagrange             (ns-publics 'emmy.mechanics.lagrange)
   'emmy.mechanics.hamilton             (ns-publics 'emmy.mechanics.hamilton)
   'emmy.mechanics.noether              (ns-publics 'emmy.mechanics.noether)
   'emmy.mechanics.rigid                (ns-publics 'emmy.mechanics.rigid)
   'emmy.mechanics.rotation             (ns-publics 'emmy.mechanics.rotation)
   'emmy.mechanics.routhian             (ns-publics 'emmy.mechanics.routhian)
   'emmy.mechanics.time-evolution       (ns-publics 'emmy.mechanics.time-evolution)
   'emmy.numerical.derivative           (ns-publics 'emmy.numerical.derivative)
   'emmy.numerical.minimize             (ns-publics 'emmy.numerical.minimize)
   'emmy.numerical.ode                  (ns-publics 'emmy.numerical.ode)
   'emmy.numerical.quadrature           (ns-publics 'emmy.numerical.quadrature)
   'emmy.numerical.multimin.nelder-mead (ns-publics 'emmy.numerical.multimin.nelder-mead)
   'emmy.numerical.unimin.bracket       (ns-publics 'emmy.numerical.unimin.bracket)
   'emmy.numerical.unimin.brent         (ns-publics 'emmy.numerical.unimin.brent)
   'emmy.numerical.unimin.golden        (ns-publics 'emmy.numerical.unimin.golden)
   'emmy.special.elliptic               (ns-publics 'emmy.special.elliptic)
   'emmy.special.factorial              (ns-publics 'emmy.special.factorial)
   'emmy.sr.boost                       (ns-publics 'emmy.sr.boost)
   'emmy.sr.frames                      (ns-publics 'emmy.sr.frames)
   'emmy.util.aggregate                 (ns-publics 'emmy.util.aggregate)
   'emmy.util.def                       (ns-publics 'emmy.util.def)
   'emmy.util.logic                     (ns-publics 'emmy.util.logic)
   'emmy.util.permute                   (ns-publics 'emmy.util.permute)
   'emmy.util.stream                    (ns-publics 'emmy.util.stream)})

(def ^{:doc "SCI namespace map generated from `ns->publics`. Consumers wishing
 to use a more minmal SCI environment, should can select interested namespaces
 from this map. Since in normal (not self-hosted) ClojureScript `ns-publics`
 does not include macros, they are added explicitly."}
  namespaces
  (let [ns-map (u/map-vals sci-ns ns->publics)]
    (merge-with merge ns-map macros/ns-bindings)))

(def ^{:doc "Default sci context options required (currently only `:namespace`
  bindings) required to evaluate Emmy forms from inside of an SCI
  context. Pass these to `sci/init` to generate an sci context."}
  context-opts
  {:namespaces namespaces

   ;; NOTE that these entries are required if you'd like to call the
   ;; `emmy.algebra.fold/kbk-n` macro, which generates code using
   ;; `Math/abs`. JVM and js forms are shown.
   :classes #?(:clj  {'java.lang.Math java.lang.Math}
               :cljs {'js goog/global :allow :all})})

(def ^{:doc "sci context (currently only `:namespace` bindings) required to
  evaluate Emmy forms via SCI"}
  context
  (sci/init context-opts))
