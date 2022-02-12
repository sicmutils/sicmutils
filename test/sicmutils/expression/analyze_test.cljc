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

(ns sicmutils.expression.analyze-test
  (:require [clojure.test :refer [is deftest testing]]
            [clojure.string :as cs]
            [sicmutils.expression.analyze :as a]
            [sicmutils.polynomial :as poly]))

(deftest symbol-generator-test
  (let [gen (a/monotonic-symbol-generator "cake")
        symbols (repeatedly 1000 gen)]
    (is (= symbols (sort symbols))
        "Generated symbols sort into the same order in which they were
        generated.")

    (is (every? #(cs/starts-with? (str %) "cake")
                symbols)
        "The prefix gets prepended to every generated symbol.")))

(deftest analyzer-test
  (testing "interactive expression analysis and simplification"
    (let [backend poly/analyzer
          gensym (a/monotonic-symbol-generator "-s-")
          pa (a/make-analyzer backend gensym)
          simplify (a/expression-simplifier pa)
          simplify-and-reset (a/default-simplifier pa)
          analyze (a/expression-analyzer pa)
          get-tables (a/auxiliary-variable-fetcher pa)
          reset-tables! (a/initializer pa)]
      (is (= '(+ (* 3 x) (* 2 -s-0000000000000000))
             (analyze '(+ x x x (sin x) (sin x)))))

      (is (= '(+ (* 2 -s-0000000000000000)
                 -s-0000000000000001)
             (analyze '(+ (sin x) (sin x) (cos y))))
          "A second call uses the same variables for any subexpressions it's
    seen.")

      (is (= '(+ (* 3 x) (* 2 (sin x)))
             (simplify '(+ x x x (sin x) (sin x)))
             (simplify '(+ x x x (sin x) (sin x))))
          "two simplify calls return the same thing, and don't reset the variable
        cache (see next tests!)")

      (is (= '{-s-0000000000000000 (sin x)
               -s-0000000000000001 (cos y)}
             (get-tables))
          "get the replacement tables out.")

      (is (= '(+ (* 3 x) (* 2 (sin x)))
             (simplify-and-reset
              '(+ x x x (sin x) (sin x))))
          "this version resets the internal tables on each invocation.")

      (is (= '{-s-0000000000000002 (sin x)}
             (get-tables))
          "get the replacement tables out.")

      (reset-tables!)

      (is (= '{} (get-tables))
          "After an explicit reset the tables are empty again.") )))
