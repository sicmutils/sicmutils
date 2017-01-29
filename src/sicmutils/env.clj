;
; Copyright (C) 2016 Colin Smith.
; This work is based on the Scmutils system of MIT/GNU Scheme.
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
  (:refer-clojure :exclude [+ - * / zero?]
                  :rename {ref core-ref partial core-partial})
  (:require [sicmutils
             [generic :as g]
             [structure :as s]
             [numsymb]
             [numbers]
             [simplify :as simp]
             [expression]
             [function :as f]
             [complex :as c]
             [matrix :as matrix]
             [series :as series]
             [operator]
             [infix :as i]]
            [sicmutils.numerical
             [minimize :as min]
             [ode :as ode]
             [integrate :as intg]]
            [sicmutils.mechanics.lagrange :as L]
            [sicmutils.mechanics.hamilton :as H]
            [sicmutils.mechanics.rotation :as R]
            [sicmutils.calculus.derivative :as d]
            [sicmutils.value :as v]))

(def + g/+)
(def - g/-)
(def * g/*)
(def / g/divide)
(def square g/square)
(def cube g/cube)
(def sqrt g/sqrt)
(def abs g/abs)
(def negate g/negate)
(def complex c/complex)
(def invert g/invert)
(def zero? g/zero?)

(def evolve ode/evolve)
(def state-advancer ode/state-advancer)

(defmacro literal-function
  ([f] `(f/literal-function ~f))
  ([f sicm-signature] `(f/literal-function ~f '~sicm-signature))
  ([f domain range] `(f/literal-function ~f ~domain ~range)))

(defmacro with-literal-functions
  [& args]
  `(f/with-literal-functions ~@args))

(def compose f/compose)

(def sin g/sin)
(def cos g/cos)
(def tan g/tan)
(def asin g/asin)
(def acos g/acos)
(def atan g/atan)
(def cot (g/divide g/cos g/sin))
(def csc (g/invert g/sin))
(def sec (g/invert g/cos))
(def exp g/exp)
(def log g/log)
(def expt g/expt)
(def simplify g/simplify)
(def cross-product g/cross-product)
(def print-expression simp/print-expression)

(def magnitude g/magnitude)
(def real-part c/real-part)
(def imag-part c/imag-part)
(def conjugate c/conjugate)
(def angle c/angle)


(defn ref
  "A shim so that ref can act like nth in SICM contexts, as clojure
  core ref elsewhere."
  [a & as]
  (let [m? (matrix/matrix? a)]
    (if (and as
             (or (sequential? a) m?)
             (every? integer? as))
      (if m?
        (matrix/get-in a as)
        (get-in a as))
     (apply core-ref a as))))

(defn partial
  "A shim. Dispatches to partial differentiation when all the arguments
  are integers; falls back to the core meaning (partial function application)
  otherwise."
  [& selectors]
  (if (every? integer? selectors)
    (apply d/∂ selectors)
    (apply core-partial selectors)))

(def up s/up)
(def down s/down)
(def transpose g/transpose)
(def determinant g/determinant)
(def component s/component)
(def structure? s/structure?)
(def orientation s/orientation)
(def structure->vector s/structure->vector)
(def vector->up s/vector->up)
(def vector->down s/vector->down)
(def m:transpose matrix/transpose)
(def compatible-shape s/compatible-shape)
(def mapr s/mapr)
(def s->m matrix/s->m)
(def qp-submatrix #(matrix/without % 0 0))
(def m:dimension matrix/dimension)
(def matrix-by-rows matrix/by-rows)
(def column-matrix matrix/column)

(def D d/D)
(def ∂ d/∂)
(def pi Math/PI)
(def taylor-series-terms d/taylor-series-terms)

(def minimize min/minimize)

(def definite-integral intg/definite-integral)

(def ->infix i/->infix)
(def ->TeX i/->TeX)
(def ->JavaScript i/->JavaScript)

(def principal-value v/principal-value)

(def series series/starting-with)
(def series:sum series/sum)

(def Lagrangian-action L/Lagrangian-action)
(def Lagrangian->state-derivative L/Lagrangian->state-derivative)
(def Lagrange-equations L/Lagrange-equations)
(def Lagrange-equations-first-order L/Lagrange-equations-first-order)
(def Lagrangian->energy L/Lagrangian->energy)
(def Euler-Lagrange-operator L/Euler-Lagrange-operator)
(def Gamma L/Gamma)
(def Γ Gamma)
(def Gamma-bar L/Gamma-bar)
(def Lagrange-interpolation-function L/Lagrange-interpolation-function)
(def linear-interpolants L/linear-interpolants)
(def osculating-path L/osculating-path)

(def F->C L/F->C)
(def coordinate L/coordinate)
(def coordinate-tuple L/coordinate-tuple)
(def velocity L/velocity)
(def ->local L/->L-state)
(def ->L-state L/->L-state)
(def p->r L/p->r)
(def s->r L/s->r)
(def find-path L/find-path)

(def momentum H/momentum)
(def momentum-tuple H/momentum-tuple)
(def ->H-state H/->H-state)
(def Legendre-transform H/Legendre-transform)
(def Hamilton-equations H/Hamilton-equations)
(def Poisson-bracket H/Poisson-bracket)
(def Lagrangian->Hamiltonian H/Lagrangian->Hamiltonian)
(def Hamiltonian->state-derivative H/Hamiltonian->state-derivative)
(def compositional-canonical? H/compositional-canonical?)
(def time-independent-canonical? H/time-independent-canonical?)
(def F->CT H/F->CT)
(def polar-canonical H/polar-canonical)
(def symplectic-transform? H/symplectic-transform?)
(def Lie-derivative H/Lie-derivative)
(def Lie-transform H/Lie-transform)
(def symplectic-unit H/symplectic-unit)
(def iterated-map H/iterated-map)

(def Rx R/Rx)
(def Ry R/Ry)
(def Rz R/Rz)

