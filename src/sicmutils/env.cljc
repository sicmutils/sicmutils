;
; Copyright © 2017 Colin Smith.
; This work is based on the Scmutils system of MIT/GNU Scheme:
; Copyright © 2002 Massachusetts Institute of Technology
;
; This is free software;  you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3 of the License, or (at
; your option) any later version.
;
; This software is distributed in the hope that it will be useful, but
; WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
; General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this code; if not, see <http://www.gnu.org/licenses/>.
;

(ns sicmutils.env
  "The purpose of these definitions is to let the import of sicmutils.env
   bring all the functions in the book into scope without qualification,
   so you can just start working with examples."
  (:refer-clojure :rename {ref core-ref
                           partial core-partial}
                  :exclude [+ - * / zero? #?(:cljs partial)])
  (:require #?(:clj [potemkin :refer [import-vars]])
            #?(:clj [nrepl.middleware.print])
            [sicmutils.abstract.function :as af #?@(:cljs [:include-macros true])]
            [sicmutils.abstract.number :as an]
            [sicmutils.complex]
            [sicmutils.expression.render :as render]
            [sicmutils.function]
            [sicmutils.generic :as g]
            [sicmutils.operator]
            [sicmutils.simplify :as simp]
            [sicmutils.structure]
            [sicmutils.value :as v]
            [sicmutils.matrix :as matrix]
            [sicmutils.series :as series]
            [sicmutils.util :as u #?@(:cljs [:refer-macros [import-vars]])]
            [sicmutils.util.aggregate]
            [sicmutils.numerical.elliptic]
            [sicmutils.numerical.minimize]
            [sicmutils.numerical.ode]
            [sicmutils.numerical.quadrature]
            [sicmutils.mechanics.lagrange]
            [sicmutils.mechanics.hamilton]
            [sicmutils.mechanics.rotation]
            [sicmutils.calculus.basis]
            [sicmutils.calculus.covariant]
            [sicmutils.calculus.derivative :as d]
            [sicmutils.calculus.form-field]
            [sicmutils.calculus.manifold]
            [sicmutils.calculus.map]
            [sicmutils.calculus.coordinate]
            [sicmutils.calculus.vector-field]))

#?(:clj
   (defn sicmutils-repl-init
     []
     (set! nrepl.middleware.print/*print-fn* simp/expression->stream)))

(defmacro bootstrap-repl!
  "Bootstraps a repl or Clojure namespace by requiring all public vars from
     sicmutils.env. From (This will only work at a repl in Clojurescript.)

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
            (= '-> (first sicm-signature)))
     `(af/literal-function ~f '~sicm-signature)
     `(af/literal-function ~f ~sicm-signature)))
  ([f domain range] `(af/literal-function ~f ~domain ~range)))

(defmacro with-literal-functions
  [& args]
  `(af/with-literal-functions ~@args))

(def print-expression simp/print-expression)

(defn ref
  "A shim so that ref can act like nth in SICM contexts, as clojure core ref
  elsewhere."
  ([a] #?(:clj (core-ref a) :cljs a))
  ([a & ks]
   (if (and (associative? a)
            (every? v/integral? ks))
     (if (matrix/matrix? a)
       (matrix/get-in a ks)
       (get-in a ks))
     #?(:clj (apply core-ref a ks)
        :cljs (get-in a ks)))))

(defn partial
  "A shim. Dispatches to partial differentiation when all the arguments
  are integers; falls back to the core meaning (partial function application)
  otherwise."
  [& selectors]
  (if (every? integer? selectors)
    (apply d/partial selectors)
    (apply core-partial selectors)))

(def m:transpose matrix/transpose)
(def qp-submatrix #(matrix/without % 0 0))
(def m:dimension matrix/dimension)
(def matrix-by-rows matrix/by-rows)
(def matrix-by-cols matrix/by-cols)
(def row-matrix matrix/row)
(def column-matrix matrix/column)

(def pi Math/PI)
(def principal-value v/principal-value)

(def series series/series)
(def power-series series/power-series)
(def series:sum series/sum)

(defn arg-shift
  "Takes a function and a series of "
  [f & shifts]
  (let [shifts (concat shifts (repeat 0))]
    (fn [& xs]
      (apply f (map + xs shifts)))))

(defn arg-scale [f & shifts]
  (let [shifts (concat shifts (repeat 1))]
    (fn [& xs]
      (apply f (map * xs shifts)))))

(defn vector:generate [n f]
  (mapv f (range n)))

(defn tex$
  "Render expression in a form convenient for rendering with clojupyter.
  In this case, we want the TeX material wrapped with dollar signs."
  [expr]
  (str "$" (-> expr g/simplify render/->TeX) "$"))

(defn tex$$
  "Render expression in a form convenient for rendering with clojupyter.
  In this case, we want the TeX material wrapped with dollar signs."
  [expr]
  (str "$$" (-> expr g/simplify render/->TeX) "$$"))

(import-vars
 [sicmutils.abstract.number literal-number]
 [sicmutils.complex complex]
 [sicmutils.function arity compose]
 [sicmutils.operator commutator]
 [sicmutils.generic
  * + - /
  abs
  square
  cube
  exp exp2 exp10
  expt
  invert
  log log2 log10
  negate
  simplify
  sqrt
  gcd lcm
  cos
  sin
  tan
  acos
  asin
  atan
  cosh
  sinh
  cot
  sec
  csc
  tanh
  sech
  csch
  acosh
  asinh
  atanh
  transpose
  determinant
  dot-product
  inner-product
  outer-product
  cross-product
  make-rectangular
  make-polar
  real-part
  imag-part
  magnitude
  angle
  conjugate
  Lie-derivative]
 [sicmutils.structure
  compatible-shape
  component
  down
  mapr
  orientation
  structure->vector
  structure?
  up
  up?
  vector->down vector->up
  literal-down literal-up]
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
 [sicmutils.calculus.derivative D]
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
  Gamma
  Gamma-bar
  Lagrange-equations
  Lagrange-equations-first-order
  Lagrange-interpolation-function
  Lagrangian->energy
  Lagrangian->state-derivative
  Lagrangian-action
  coordinate
  coordinate-tuple
  find-path
  linear-interpolants
  osculating-path
  p->r
  s->r
  velocity
  velocity-tuple
  state->t
  Γ]
 [sicmutils.matrix
  s->m
  m->s
  literal-matrix
  submatrix]
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
  symplectic-transform?
  symplectic-unit
  time-independent-canonical?]
 [sicmutils.mechanics.rotation Rx Ry Rz]
 [sicmutils.numerical.ode
  evolve
  integrate-state-derivative
  state-advancer]
 [sicmutils.numerical.quadrature definite-integral]
 [sicmutils.numerical.elliptic elliptic-f]
 [sicmutils.numerical.minimize minimize multidimensional-minimize]
 [sicmutils.util.aggregate sum]
 [sicmutils.value exact? zero? one? identity?
  zero-like one-like identity-like
  numerical? freeze kind kind-predicate])

;; Macros. These work with Potemkin's import, but not with the Clojure version.
#?(:clj
   (import-vars
    [sicmutils.calculus.coordinate let-coordinates using-coordinates]))
