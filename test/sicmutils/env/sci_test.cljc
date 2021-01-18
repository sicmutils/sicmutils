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

(ns sicmutils.env.sci-test
  (:refer-clojure :exclude [eval])
  (:require [clojure.test :refer [is deftest testing]]
            [sci.core :as sci]
            [sicmutils.env :as e]
            [sicmutils.env.sci :as es]
            [sicmutils.operator :as o]))

(defn eval [form]
  (sci/eval-form (sci/fork es/context) form))

(deftest basic-sci-tests
  (is (= 1 (eval '(simplify (+ (square (sin 'x))
                               (square (cos 'x))))))
      "simplifications work inside sci")

  (is (= "{\\cos}^{2}\\left(x\\right) + {\\sin}^{2}\\left({x}^{2}\\right)"
         (eval '(->TeX
                 (simplify (+ (square (sin (square 'x)))
                              (square (cos 'x))))))))

  (is (= (e/literal-function 'U)
         (eval '(literal-function 'U)))
      "Literal functions use value equality.")

  (is (= o/identity
         (eval
          '(do (require '[sicmutils.operator :as o])
               o/identity)))
      "can sci internally require namespaces?")

  (is (= '(* 10 face)
         (eval
          '(do (require '[sicmutils.env :as e])
               (e/simplify (e/* 'face 10)))))
      "sicmutils.env is available as a namespace and also included as the
      default bindings in `user`.")

  (testing "sci-specific macro definitions"
    (is (= [true true true true]
           (eval '(let-coordinates [[x y]     R2-rect
                                    [r theta] R2-polar]
                    (let [p ((point R2-rect) (up 1 2))]
                      [(= 1 (x p))
                       (= 2 (y p))
                       (= (sqrt 5) (r p))
                       (= (atan 2) (theta p))]))))
        "let-coordinates macro works")

    (is (= [true true]
           (eval '(using-coordinates
                   [x y] R2-rect
                   (let [p ((point R2-rect) (up 1 2))]
                     [(= 1 (x p))
                      (= 2 (y p))]))))
        "using-coordinates works")

    (testing "internal defn, funky symbols, internal with-literal-functions macro"
      (is (= "down(- m (Dφ(t))² r(t) + m D²r(t) + DU(r(t)), 2 m Dφ(t) r(t) Dr(t) + m (r(t))² D²φ(t))"
             (eval
              '(do (defn L-central-polar [m U]
                     (fn [[_ [r] [rdot φdot]]]
                       (- (* (/ 1 2) m
                             (+ (square rdot)
                                (square (* r φdot))))
                          (U r))))
                   (with-literal-functions [U r φ]
                     (let [L     (L-central-polar 'm U)
                           state (up r φ)]
                       (->infix
                        (simplify
                         (((Lagrange-equations L) state) 't))))))))))))
