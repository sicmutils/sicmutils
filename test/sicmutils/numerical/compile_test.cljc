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

(ns sicmutils.numerical.compile-test
  (:refer-clojure :exclude [+ - * /])
  (:require [clojure.test :refer [is deftest testing]]
            [clojure.walk :as w]
            #?(:cljs [goog.string :refer [format]])
            [sicmutils.generic :as g :refer [+ - * /]]
            [sicmutils.structure :refer [up down]]
            [sicmutils.value :as v]
            [sicmutils.numerical.compile :as c]))

(def ^:private near (v/within 1e-6))

(deftest compile-univariate
  (let [f (fn [x] (+ 1 (g/square (g/sin x))))
        cf (c/compile-univariate-fn f)]
    (is (near (f 0.5) (cf 0.5)))))

(deftest compile-state
  (let [f (fn [[[a b] [c d]]] (- (* a d) (* b c)))
        sf (fn [k] (fn [s] (* k (f s))))
        s (up (down 2 3) (down 4 5))
        t (up (down 3 4) (down -1 2))]
    (is (= -2 (f s)))
    (is (= 10 (f t)))
    (is (= -4 ((sf 2) s)))
    (is (= 20 ((sf 2) t)))
    (let [cf (c/compile-state-fn sf [1] s)]
      (is (= -2 (cf [2 3 4 5] [1])))
      (is (= -4 (cf [2 3 4 5] [2])))
      (is (= 10 (cf (flatten t) [1])))
      (is (= 20 (cf [3 4 -1 2] [2]))))))

(defn ^:private make-generator
  [s]
  (let [i (atom 0)]
    (fn []
      (symbol (format "%s%d" s (swap! i inc))))))

(deftest subexp-tests
  (is (= '[(* g1 (+ x z) g1) ([g1 (+ x y)])]
         (c/extract-common-subexpressions
          '(* (+ x y) (+ x z) (+ x y))
          vector
          {:symbol-generator (make-generator "g")})))
  (is (= '[(+ K1 (expt K1 2) K2 (sqrt K2)) ([K1 (sin x)] [K2 (cos x)])]
         (c/extract-common-subexpressions
          '(+ (sin x) (expt (sin x) 2) (cos x) (sqrt (cos x)))
          vector
          {:symbol-generator (make-generator "K")})))


  (let [expr            '(+ (sin x) (expt (sin x) 2)
                            (cos x) (sqrt (cos x)))
        [slimmed sym->subexpr]
        (c/extract-common-subexpressions
         expr
         (fn [e bindings]
           [e (into {} bindings)]))]
    (is (= expr (w/postwalk-replace sym->subexpr slimmed))
        "Rehydrating the slimmed expression should result in the original
        expression. (This test involves a single level of replacement. A better
        test would recursively postwalk-replace until no change occured.)")))

(deftest subexp-compile
  (let [expr '(+ (sin x) (expt (sin x) 2)
                 (cos x) (sqrt (cos x))
                 (tan x))]
    (is (= '(#?(:clj clojure.core/let :cljs cljs.core/let)
             [g1 (sin x)
              g2 (cos x)]
             (+ g1 (expt g1 2)
                g2 (sqrt g2) (tan x)))
           (c/cse-form expr {:symbol-generator (make-generator "g")})))

    (is (= '(+ a b (sin x) (cos y))
           (c/cse-form '(+ a b (sin x) (cos y)))))))
