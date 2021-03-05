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

(ns sicmutils.env
  "Batteries-included namespace for the [SICMUtils](https://github.com/sicmutils/sicmutils/) library.

  The purpose of [[sicmutils.env]] is to bundle all of the functions used
  in [Structure and Interpretation of Classical
  Mechanics](https://tgvaughan.github.io/sicm/) and [Functional Differential
  Geometry](https://mitpress.mit.edu/books/functional-differential-geometry)
  into a single scope. The following form will import everything
  from [[sicmutils.env]] into your REPL:

  ```clojure
  (require '[sicmutils.env :as e])
  (e/bootstrap-repl!)
  ```

  Or, in Clojure:

  ```clojure
  (require '[sicmutils.env :as e :refer :all])
  ```"
  (:refer-clojure :rename {ref core-ref
                           partial core-partial
                           compare core-compare
                           = core=}
                  :exclude [+ - * / zero? compare divide #?@(:cljs [= partial])])
  (:require #?(:clj [potemkin :refer [import-def import-vars]])
            #?(:clj [nrepl.middleware.print])
            [sicmutils.abstract.function :as af #?@(:cljs [:include-macros true])]
            [sicmutils.abstract.number :as an]
            [sicmutils.complex]
            [sicmutils.expression]
            [sicmutils.expression.render :as render]
            [sicmutils.function :as f]
            [sicmutils.generic :as g]
            [sicmutils.modint]
            [sicmutils.operator :as o]
            [sicmutils.ratio]
            [sicmutils.simplify :as simp]
            [sicmutils.structure :as structure]
            [sicmutils.value :as v]
            [sicmutils.matrix :as matrix]
            [sicmutils.series :as series]
            [sicmutils.util.aggregate]
            [sicmutils.util.def :as util.def
             #?@(:cljs [:refer [import-def import-vars]
                        :include-macros true])]
            [sicmutils.util.stream :as us]
            [sicmutils.numerical.derivative]
            [sicmutils.numerical.elliptic]
            [sicmutils.numerical.minimize]
            [sicmutils.numerical.ode]
            [sicmutils.numerical.quadrature]
            [sicmutils.mechanics.lagrange]
            [sicmutils.mechanics.hamilton]
            [sicmutils.mechanics.rigid]
            [sicmutils.mechanics.rotation]
            [sicmutils.calculus.basis]
            [sicmutils.calculus.covariant]
            [sicmutils.calculus.derivative :as d]
            [sicmutils.calculus.form-field]
            [sicmutils.calculus.manifold]
            [sicmutils.calculus.map]
            [sicmutils.calculus.coordinate :as cc]
            [sicmutils.calculus.vector-field]))

#?(:clj
   (defn sicmutils-repl-init
     []
     (set! nrepl.middleware.print/*print-fn* simp/expression->stream)))

(defmacro bootstrap-repl!
  "Bootstraps a repl or Clojure namespace by requiring all public vars
  from [[sicmutils.env]].

  (This will only work at a repl in Clojurescript.)

  TODO add support for `refer-macros` in Clojurescript
  TODO add rename, exclude support."
  []
  `(require '~['sicmutils.env
               :refer
               (into [] (keys (ns-publics 'sicmutils.env)))]))

(defmacro literal-function
  ([f] `(af/literal-function ~f))
  ([f sicm-signature]
   (if (and (list? sicm-signature)
            (core= '-> (first sicm-signature)))
     `(af/literal-function ~f '~sicm-signature)
     `(af/literal-function ~f ~sicm-signature)))
  ([f domain range] `(af/literal-function ~f ~domain ~range)))

(defmacro with-literal-functions [& args]
  `(af/with-literal-functions ~@args))

(defmacro let-coordinates [& args]
  `(cc/let-coordinates ~@args))

(defmacro using-coordinates [& args]
  `(cc/using-coordinates ~@args))

(import-def simp/print-expression)

(defn ref
  "A shim so that ref can act like nth in SICM contexts, as clojure core ref
  elsewhere."
  ([a] #?(:clj (core-ref a) :cljs a))
  ([a & ks]
   (cond (f/function? a) (f/compose #(apply ref % ks) a)
         (o/operator? a) (o/make-operator
                          (f/compose #(apply ref % ks) (o/procedure a))
                          `(~'compose (~'component ~@ks)
                            ~(o/name a)))
         :else (if (and (associative? a)
                        (every? v/integral? ks))
                 (if (matrix/matrix? a)
                   (matrix/get-in a ks)
                   (get-in a ks))
                 #?(:clj (apply core-ref a ks)
                    :cljs (get-in a ks))))))

(defn component
  "Given a sequence of `selectors`, return a function that accepts some object `x`
  and returns:

  ```clojure
  (apply ref x selectors)
  ```
  "
  [& selectors]
  (fn [x] (apply ref x selectors)))

(defn partial
  "A shim. Dispatches to [[d/partial]] when all the arguments are integers; falls
  back to [[clojure.core/partial]] (partial function application) otherwise."
  [& selectors]
  (if (every? integer? selectors)
    (apply d/partial selectors)
    (apply core-partial selectors)))

;; Constants

(def ^{:doc "The mathematical constant [Pi](https://en.wikipedia.org/wiki/Pi)."}
  pi Math/PI)
(def ^{:doc "The negation of the mathematical
constant [Pi](https://en.wikipedia.org/wiki/Pi)."}
  -pi (g/- Math/PI))

(def ^{:doc "The mathematical
  constant [e](https://en.wikipedia.org/wiki/E_(mathematical_constant)),
  sometimes known as Euler's Number."}
  euler
  0.57721566490153286)

(def ^{:doc "The mathematical
  constant [𝜑](https://en.wikipedia.org/wiki/Golden_ratio), also known as the
  Golden Ratio."}
  phi
  (g/divide
   (inc (Math/sqrt 5.0)) 2.0))

(import-def structure/generate s:generate)
(import-def matrix/generate m:generate)
(import-def structure/basis-unit v:make-basis-unit)

(import-def matrix/by-rows matrix-by-rows)
(import-def matrix/by-cols matrix-by-cols)
(import-def matrix/row row-matrix)
(import-def matrix/column column-matrix)

(import-def v/principal-value principal-value)

(import-def series/series series)
(import-def series/power-series power-series)
(import-def series/constant constant-series)
(import-def series/sum series:sum)

(import-def us/seq-print seq:print)
(import-def us/pprint seq:pprint)

(defn tex$
  "Returns a string containing a LaTeX representation of `expr`, wrapped in single
  `$` to mark the string as an inline LaTeX form."
  [expr]
  (str "$" (-> expr g/simplify render/->TeX) "$"))

(defn tex$$
  "Returns a string containing a LaTeX representation of `expr`, wrapped in double
  `$$` to mark the string as a block LaTeX form."
  [expr]
  (str "$$" (-> expr g/simplify render/->TeX) "$$"))

(defn ->tex-equation
  "Returns a string containing a LaTeX representation of `expr`, wrapped in an
  `equation` environment.

  Optionally supply a `:label` keyword argument to set a custom label."
  [expr & {:keys [label]}]
  (-> (g/simplify expr)
      (render/->TeX :equation (or label true))))

(import-vars
 [sicmutils.abstract.number literal-number]
 [sicmutils.complex complex complex?]
 #?(:cljs [sicmutils.ratio
           ratio? rationalize numerator denominator])
 [sicmutils.util bigint? #?@(:cljs [bigint])]
 [sicmutils.function arity compose arg-shift arg-scale I]
 [sicmutils.modint chinese-remainder]
 [sicmutils.operator commutator anticommutator]
 [sicmutils.series binomial-series partial-sums]
 [sicmutils.generic
  * + - / divide
  negate
  negative?
  invert
  abs
  sqrt
  quotient remainder modulo
  floor ceiling
  integer-part fractional-part
  expt
  exp exp2 exp10
  log log2 log10
  gcd lcm
  exact-divide
  square cube
  cos sin tan
  acos asin atan
  cosh sinh tanh
  acosh asinh atanh
  sec csc cot
  sech csch
  make-rectangular make-polar
  real-part imag-part
  magnitude angle conjugate
  transpose trace determinant dimension
  dot-product inner-product outer-product cross-product
  partial-derivative Lie-derivative
  solve-linear solve-linear-left solve-linear-right
  simplify
  factorial]
 [sicmutils.structure
  compatible-shape
  down
  mapr
  orientation
  structure->vector
  structure?
  up
  up?
  vector->down vector->up
  literal-down literal-up]
 [sicmutils.expression expression-of]
 [sicmutils.expression.render
  ->infix
  ->TeX
  ->JavaScript]
 [sicmutils.calculus.covariant
  covariant-derivative
  interior-product
  Cartan-transform
  Christoffel->Cartan
  make-Christoffel
  ]
 [sicmutils.calculus.derivative
  derivative D Div Grad Curl Lap taylor-series]
 [sicmutils.calculus.form-field
  d
  components->oneform-field
  literal-oneform-field
  wedge]
 [sicmutils.calculus.manifold
  chart
  point
  literal-manifold-function
  Euler-angles
  alternate-angles
  R1-rect
  R2-rect
  R2-polar
  R3-rect
  R3-cyl
  S2-spherical
  S2-stereographic
  S2-Riemann
  SO3]
 [sicmutils.calculus.basis
  basis->vector-basis
  basis->oneform-basis]
 [sicmutils.calculus.coordinate
  Jacobian
  coordinate-system->basis
  coordinate-system->oneform-basis
  coordinate-system->vector-basis
  vector-basis->dual]
 [sicmutils.calculus.map
  basis->basis-over-map
  differential
  pullback
  pushforward-vector
  literal-manifold-map
  form-field->form-field-over-map
  vector-field->vector-field-over-map]
 [sicmutils.calculus.vector-field
  components->vector-field
  coordinatize
  evolution
  literal-vector-field
  vector-field->components]
 [sicmutils.mechanics.lagrange
  ->L-state
  ->local
  Euler-Lagrange-operator
  F->C
  Gamma Γ
  Gamma-bar Γ-bar
  Lagrange-equations
  Lagrange-equations-first-order
  Lagrange-interpolation-function
  Lagrangian->energy
  Lagrangian->state-derivative
  Lagrangian-action
  find-path
  linear-interpolants
  osculating-path
  p->r
  s->r
  state->t coordinate velocity acceleration
  coordinate-tuple velocity-tuple acceleration-tuple]
 [sicmutils.matrix
  s->m
  m->s
  literal-matrix
  submatrix
  up->column-matrix
  column-matrix->up
  column-matrix->vector
  down->row-matrix
  row-matrix->down
  row-matrix->vector]
 [sicmutils.mechanics.hamilton
  ->H-state
  F->CT
  Hamilton-equations
  Hamiltonian
  Hamiltonian->state-derivative
  Lagrangian->Hamiltonian
  Legendre-transform
  Lie-transform
  Poisson-bracket
  compositional-canonical?
  iterated-map
  momentum
  momentum-tuple
  polar-canonical
  standard-map
  qp-submatrix
  symplectic-transform?
  symplectic-unit
  time-independent-canonical?]
 [sicmutils.mechanics.rotation Rx Ry Rz]
 [sicmutils.numerical.ode
  evolve
  integrate-state-derivative
  state-advancer]
 [sicmutils.numerical.elliptic elliptic-f]
 [sicmutils.numerical.derivative D-numeric]
 [sicmutils.numerical.quadrature definite-integral]
 [sicmutils.numerical.unimin.brent
  brent-min brent-max]
 [sicmutils.numerical.multimin.nelder-mead nelder-mead]
 [sicmutils.numerical.unimin.golden
  golden-section-min golden-section-max]
 [sicmutils.numerical.minimize minimize multidimensional-minimize]
 [sicmutils.util.aggregate sum]
 [sicmutils.util.stream vector:generate]
 [sicmutils.value = compare exact? zero? one? identity?
  zero-like one-like identity-like
  numerical? freeze kind kind-predicate])
