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

(ns sicmutils.env.sci
  (:refer-clojure :exclude [eval])
  (:require [clojure.set :as set]
            [sci.core :as sci]
            [sicmutils.env]
            [sicmutils.env.sci.macros :as macros]
            [sicmutils.util :as u]))

(def macro? (comp :macro meta))

(defn resolve-publics [m-or-sym]
  (if (symbol? m-or-sym)
    (ns-publics m-or-sym)
    m-or-sym))

(defn ns-macros
  "Returns a sequence of all macros in the supplied namespace sym->var mapping.

  You can also provide the name of a namespace as a symbol."
  [m-or-sym]
  (mapcat (fn [[sym var]]
            (if (macro? var) [sym] []))
          (resolve-publics m-or-sym)))

(defn sci-ns
  "Returns a new map identical to the supplied namespace binding map `sym->var`,
  with any macro value removed and all var-values resolved."
  [m-or-sym]
  (letfn [(process [[sym var]]
            (if-not (macro? var)
              [[sym @var]]
              (if-let [sci-macro (macros/all sym)]
                [[sym sci-macro]]
                [])))]
    (let [sym->var (resolve-publics m-or-sym)]
      (into {} (mapcat process) sym->var))))

(def namespaces
  #{'sicmutils.env
    'sicmutils.generic
    'sicmutils.abstract.function
    'sicmutils.calculus.coordinate
    'sicmutils.function
    'sicmutils.operator
    'sicmutils.series
    'sicmutils.structure
    'sicmutils.matrix
    'sicmutils.calculus.manifold
    'sicmutils.calculus.vector-field
    'sicmutils.calculus.form-field})

(def context-opts
  {:namespaces
   (-> (u/keys->map sci-ns namespaces)
       (set/rename-keys
        {'sicmutils.env 'user}))})

(def context
  (sci/init context-opts))

(comment
  (defn eval [form]
    (sci/eval-form (sci/fork context) form))

  (eval '(simplify (+ (square (sin 'x))
                      (square (cos 'x)))))

  (eval '(->TeX (simplify (+ (square (sin (square 'x)))
                             (square (cos 'x))))))

  (eval '(literal-function 'U))

  (eval '(do (require '[sicmutils.operator :as o])
             o/identity-operator))

  (eval '(let-coordinates [[x y]     R2-rect
                           [r theta] R2-polar]
           (let [p ((point R2-rect) (up 1 2))]
             [(= 1 (x p))
              (= 2 (y p))
              (= (sqrt 5) (r p))
              (= (atan 2) (theta p))])))

  (eval '(using-coordinates
          [x y] R2-rect
          (let [p ((point R2-rect) (up 1 2))]
            [(= 1 (x p))
             (= 2 (y p))])))

  (eval '(do (defn L-central-polar [m U]
               (fn [[_ [r] [rdot φdot]]]
                 (- (* 1/2 m
                       (+ (square rdot)
                          (square (* r φdot))))
                    (U r))))
             (with-literal-functions [U r φ]
               (let [L     (L-central-polar 'm U)
                     state (up r φ)]
                 (->TeX
                  (simplify
                   (((Lagrange-equations L) state) 't))))))))
