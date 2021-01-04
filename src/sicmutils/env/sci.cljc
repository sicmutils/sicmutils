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
            [sicmutils.env.sci.macros :as macros]
            [sicmutils.env :as env]
            [sicmutils.abstract.function :as af]
            [sicmutils.calculus.coordinate :as cc]))

(defn ->sci-var [[var-name var]]
  (let [macro? (:macro (meta var))]
    [var-name (if macro?
                (with-meta @var {:sci/macro true})
                @var)]))

(defn ->sci-ns [publics]
  (into {} (map ->sci-var) publics))

(def namespaces
  {'sicmutils.env (-> 'sicmutils.env
                      ns-publics
                      (dissoc 'literal-function
                              'with-literal-functions
                              'bootstrap-repl!
                              'let-coordinates
                              'using-coordinates)
                      ->sci-ns

                      (merge (select-keys macros/all ['literal-function
                                                      'with-literal-functions
                                                      'let-coordinates
                                                      'using-coordinates])))
   'sicmutils.abstract.function (-> 'sicmutils.abstract.function ns-publics ->sci-ns)
   'sicmutils.calculus.coordinate (-> 'sicmutils.calculus.coordinate
                                      ns-publics
                                      ->sci-ns
                                      (merge (select-keys macros/all ['let-coordinates
                                                                      'using-coordinates])))})

(def opts {:namespaces (set/rename-keys namespaces {'sicmutils.env 'user})})

(def ctx (sci/init opts))

(comment
  (defn eval [form]
    (sci/eval-string* ctx (pr-str form)))

  (eval '(simplify (+ (square (sin 'x))
                      (square (cos 'x)))))

  (eval '(->TeX (simplify (+ (square (sin (square 'x)))
                             (square (cos 'x))))))

  (eval '(literal-function 'U))
  (eval '(do (defn L-central-polar [m U]
               (fn [[_ [r] [rdot φdot]]]
                 (- (* 1/2 m
                       (+ (square rdot)
                          (square (* r φdot))))
                    (U r))))
             (let [potential-fn (literal-function 'U)
                   L     (L-central-polar 'm potential-fn)
                   state (up (literal-function 'r)
                             (literal-function 'φ))]
               (->TeX
                (simplify
                 (((Lagrange-equations L) state) 't)))))))
