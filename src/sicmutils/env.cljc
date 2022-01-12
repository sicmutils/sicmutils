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
                  :exclude [+ - * / zero? compare divide
                            numerator denominator
                            #?@(:cljs [= partial infinite?])])
  (:require #?(:clj [potemkin :refer [import-def import-vars]])
            [sicmutils.abstract.function :as af #?@(:cljs [:include-macros true])]
            [sicmutils.abstract.number :as an]
            [sicmutils.algebra.fold]
            [sicmutils.complex]
            [sicmutils.expression :as x]
            [sicmutils.expression.render :as render]
            [sicmutils.function :as f]
            [sicmutils.generic :as g]
            [sicmutils.modint]
            [sicmutils.operator :as o]
            [sicmutils.polynomial.factor :as pf]
            [sicmutils.ratio]
            [sicmutils.simplify]
            [sicmutils.structure :as structure]
            [sicmutils.value :as v]
            [sicmutils.matrix :as matrix]
            [sicmutils.series :as series]
            [sicmutils.util.aggregate]
            [sicmutils.util.def :as util.def
             #?@(:cljs [:refer [import-def import-vars]
                        :include-macros true])]
            [sicmutils.util.permute]
            [sicmutils.util.stream :as us]
            [sicmutils.numerical.derivative]
            [sicmutils.numerical.minimize]
            [sicmutils.numerical.ode]
            [sicmutils.numerical.quadrature]
            [sicmutils.mechanics.lagrange]
            [sicmutils.mechanics.hamilton]
            [sicmutils.mechanics.rigid]
            [sicmutils.mechanics.rotation]
            [sicmutils.calculus.basis]
            [sicmutils.calculus.connection]
            [sicmutils.calculus.coordinate :as cc]
            [sicmutils.calculus.covariant]
            [sicmutils.calculus.curvature]
            [sicmutils.calculus.derivative :as d]
            [sicmutils.calculus.form-field]
            [sicmutils.calculus.frame :as cf]
            [sicmutils.calculus.hodge-star]
            [sicmutils.calculus.indexed :as ci]
            [sicmutils.calculus.manifold]
            [sicmutils.calculus.metric :as cm]
            [sicmutils.calculus.map]
            [sicmutils.calculus.vector-calculus]
            [sicmutils.calculus.vector-field]
            [sicmutils.special.elliptic]
            [sicmutils.special.factorial]
            [sicmutils.sr.boost]
            [sicmutils.sr.frames]))

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

(defmacro define-coordinates [& args]
  `(cc/define-coordinates ~@args))

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
  Math/E)

(def ^{:doc "The mathematical constant known as the [Euler–Mascheroni
  constant](https://en.wikipedia.org/wiki/Euler%E2%80%93Mascheroni_constant) and
  sometimes as Euler's constant."}
  euler-gamma
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
(import-def cm/invert metric:invert)

(import-def us/seq-print seq:print)
(import-def us/pprint seq:pprint)

(import-def ci/outer-product i:outer-product)
(import-def ci/contract i:contract)

(import-def cf/params frame-params)

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
 [sicmutils.function arity compose arg-shift arg-scale I]
 [sicmutils.modint chinese-remainder]
 [sicmutils.operator commutator anticommutator]
 [sicmutils.polynomial.factor factor]
 [sicmutils.ratio numerator denominator #?@(:cljs [ratio? rationalize])]
 [sicmutils.series binomial-series partial-sums]
 [sicmutils.util bigint? #?@(:cljs [bigint])]

 [sicmutils.generic
  * + - / divide
  negate
  negative? infinite?
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
  simplify]
 [sicmutils.structure
  compatible-shape
  compatible-zero dual-zero
  down
  mapr sumr
  orientation
  structure->vector
  structure?
  up
  up?
  vector->down vector->up
  literal-down literal-up]
 [sicmutils.expression
  expression-of
  expression->stream expression->string print-expression pe]
 [sicmutils.expression.render
  ->infix
  ->TeX
  ->JavaScript]

 ;; Calculus Namespaces

 [sicmutils.calculus.basis
  basis? coordinate-basis? make-basis
  coordinate-system->basis
  basis->coordinate-system
  basis->oneform-basis
  basis->vector-basis
  basis->dimension
  contract
  vector-basis->dual
  make-constant-vector-field
  Jacobian]

 [sicmutils.calculus.connection
  make-Christoffel-1
  metric->Christoffel-1 metric->Christoffel-2
  literal-Christoffel-1 literal-Christoffel-2
  metric->connection-1 metric->connection-2
  literal-Cartan
  structure-constant]

 [sicmutils.calculus.covariant
  covariant-derivative
  covariant-differential
  Lie-D
  interior-product
  make-Cartan Cartan? Cartan->forms Cartan->basis
  make-Christoffel Christoffel? Christoffel->symbols Christoffel->basis
  Cartan->Christoffel
  Christoffel->Cartan
  symmetrize-Christoffel
  symmetrize-Cartan
  Cartan-transform
  Cartan->Cartan-over-map
  geodesic-equation parallel-transport-equation]

 [sicmutils.calculus.curvature
  Riemann-curvature Riemann Ricci torsion-vector torsion
  curvature-components]

 [sicmutils.calculus.derivative
  derivative D D-as-matrix taylor-series]

 [sicmutils.calculus.form-field
  form-field? nform-field? oneform-field?
  ff:zero
  components->oneform-field
  oneform-field->components
  literal-oneform-field
  coordinate-basis-oneform-field
  coordinate-system->oneform-basis
  basis-components->oneform-field
  oneform-field->basis-components
  function->oneform-field
  wedge
  Alt alt-wedge
  exterior-derivative d]

 [sicmutils.calculus.frame
  frame? make-event event? claim
  coords->event event->coords ancestor-frame frame-name
  frame-owner frame-maker]

 [sicmutils.calculus.hodge-star
  Gram-Schmidt orthonormalize
  Hodge-star]

 [sicmutils.calculus.indexed
  argument-types with-argument-types
  index-types with-index-types
  typed->indexed indexed->typed
  typed->structure structure->typed]

 [sicmutils.calculus.manifold
  make-manifold coordinate-system-at
  manifold-type
  patch-names coordinate-system-names
  manifold?
  manifold-family?
  manifold-point?
  chart point
  typical-coords typical-point transfer-point
  corresponding-velocities
  literal-manifold-function
  zero-manifold-function one-manifold-function
  constant-manifold-function
  coordinate-system?
  Rn
  R1 R1-rect the-real-line
  R2 R2-rect R2-polar
  R3 R3-rect R3-cyl R3-spherical
  R4 R4-rect R4-cyl
  spacetime spacetime-rect spacetime-sphere
  Sn
  S1 S1-circular S1-tilted S1-slope S1-gnomonic
  S2-type S2 S2-spherical S2-tilted S2-stereographic S2-Riemann S2-gnomonic
  S2p S2p-spherical S2p-tilted S2p-stereographic S2p-Riemann S2p-gnomonic
  S3 S3-spherical S3-tilted S3-stereographic S3-gnomonic
  SO3-type SO3 Euler-angles alternate-angles]

 [sicmutils.calculus.metric
  coordinate-system->metric-components
  coordinate-system->metric
  coordinate-system->inverse-metric
  literal-metric
  components->metric metric->components
  metric->inverse-components metric-over-map
  lower vector-field->oneform-field drop1
  raise oneform-field->vector-field raise1
  drop2 raise2 trace2down trace2up sharpen
  S2-metric]

 [sicmutils.calculus.map
  pullback-function pushforward-function
  differential-of-map differential
  pushforward-vector
  literal-manifold-map
  vector-field->vector-field-over-map
  form-field->form-field-over-map
  basis->basis-over-map
  pullback-form pullback-vector-field
  pullback]

 [sicmutils.calculus.vector-calculus
  Div Grad Curl Lap
  divergence curl gradient Laplacian]

 [sicmutils.calculus.vector-field
  vector-field?
  components->vector-field
  vector-field->components
  vf:zero
  literal-vector-field
  coordinate-basis-vector-field
  coordinate-system->vector-basis
  basis-components->vector-field
  vector-field->basis-components
  coordinatize evolution]

 ;; Special Relativity

 [sicmutils.sr.boost
  make-four-tuple
  four-tuple->ct four-tuple->space
  proper-time-interval proper-space-interval
  general-boost general-boost2 extended-rotation]

 [sicmutils.sr.frames
  make-SR-coordinates SR-coordinates? SR-name make-SR-frame
  base-frame-maker
  the-ether boost-direction v:c coordinate-origin
  add-v:cs add-velocities]

 ;; Mechanics Namespaces

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
  phase-space-derivative
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
 [sicmutils.mechanics.rotation
  rotate-x-matrix rotate-y-matrix rotate-z-matrix
  angle-axis->rotation-matrix
  rotate-x-tuple rotate-y-tuple rotate-z-tuple
  Rx Ry Rz rotate-x rotate-y rotate-z
  Euler->M wcross->w]
 [sicmutils.numerical.ode
  evolve
  integrate-state-derivative
  state-advancer]
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
 [sicmutils.special.elliptic elliptic-f]
 [sicmutils.special.factorial factorial]
 [sicmutils.value = compare exact? zero? one? identity?
  zero-like one-like identity-like
  numerical? freeze kind kind-predicate])
