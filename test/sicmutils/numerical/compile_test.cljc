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

(deftest compile-univariate-tests
  (let [f  (fn [x] (+ 1 (g/square (g/sin x))))
        cf         (c/compile-univariate-fn f)
        cf2        (c/compile-univariate-fn f)
        cf-nocache (c/compile-univariate-fn* f)]
    (is (= (f 0.5) (cf 0.5) (cf2 0.5) (cf-nocache 0.5))
        "the fn has no simplifications available so the results are identical;
        the compiled fn is faster.")))

(deftest compile-state-tests
  (let [f  (fn [[[a b] [c d]]]
             (- (* a d) (* b c)))
        sf (fn [k] (fn [s] (* k (f s))))
        s (up (down 2 3) (down 4 5))
        t (up (down 3 4) (down -1 2))]
    (testing "non-compiled, generic state function results"
      (is (= -2 (f s)))
      (is (= 10 (f t)))
      (is (= -4 ((sf 2) s)))
      (is (= 20 ((sf 2) t))))

    (testing "compiled state function matches the original."
      (let [cf (c/compile-state-fn sf [1] s)]
        (is (= ((sf 1) s) (cf (flatten s) [1])))
        (is (= ((sf 1) t) (cf (flatten t) [1])))
        (is (= ((sf 2) s) (cf (flatten s) [2])))
        (is (= ((sf 2) t) (cf (flatten t) [2])))))))

(defn ^:private make-generator
  [s]
  (let [i (atom 0)]
    (fn []
      (symbol (format "%s%d" s (swap! i inc))))))

(defn- rehydrate
  "Takes a slimmed-down expression and a potentially-multi-level substitution map
  and rebuilds the original expression."
  [slimmed sym->expr]
  (let [substitute (partial w/postwalk-replace sym->expr)]
    (reduce #(if (= %1 %2) (reduced %1) %2)
            (iterate substitute slimmed))))

(deftest subexp-tests
  (is (= '[(* g1 (+ x z) g1) ([g1 (+ x y)])]
         (c/extract-common-subexpressions
          '(* (+ x y) (+ x z) (+ x y))
          vector
          {:deterministic? true
           :symbol-generator (make-generator "g")}))
      "common (+ x y) variable is extracted.")

  (let [expr '(+ (* (sin x) (cos x))
                 (* (sin x) (cos x))
                 (* (sin x) (cos x)))
        opts {:deterministic? true
              :symbol-generator (make-generator "g")}
        slimmed '(+ g4 g4 g4)
        expected-subs '([g2 (cos x)]
                        [g3 (sin x)]
                        [g4 (* g3 g2)])

        sym->subexpr  (into {} expected-subs)]
    (is (= [slimmed expected-subs]
           (c/extract-common-subexpressions expr vector opts))
        "nested subexpressions are extracted in order, and the substitution map
        is suitable for a let binding (and has no extra variables).")

    (is (= expr (rehydrate slimmed sym->subexpr))
        "Rehydrating the slimmed expression should result in the original
        expression."))

  (let [expr '(+ (sin x) (expt (sin x) 2)
                 (cos x) (sqrt (cos x)))
        opts {:deterministic? true
              :symbol-generator (make-generator "K")}
        slimmed '(+ K2 (expt K2 2) K1 (sqrt K1))
        expected-subs '([K1 (cos x)]
                        [K2 (sin x)])]
    (is (= expr (rehydrate slimmed (into {} expected-subs)))
        "The substitutions are correct.")

    (is (= [slimmed expected-subs]
           (c/extract-common-subexpressions expr vector opts))
        "subexpressions are again extracted in order.")))

(def letsym
  #?(:clj 'clojure.core/let :cljs 'cljs.core/let))

(deftest subexp-compile-tests
  (let [expr '(+ (* (sin x) (cos x))
                 (* (sin x) (cos x))
                 (* (sin x) (cos x))
                 (sin x)
                 (expt (sin x) 2)
                 (cos x)
                 (sqrt (cos x))
                 (tan x))]
    (is (= (list letsym
                 '[g2 (sin x)
                   g3 (cos x)
                   g4 (* g2 g3)]
                 '(+ g4 g4 g4
                     g2 (expt g2 2)
                     g3 (sqrt g3) (tan x)))
           (c/cse-form expr {:symbol-generator (make-generator "g")}))
        "Bindings appear in the correct order for subs.")

    (is (= '(+ a b (sin x) (cos y))
           (c/cse-form '(+ a b (sin x) (cos y))))
        "No substitutions means no let binding.")))
