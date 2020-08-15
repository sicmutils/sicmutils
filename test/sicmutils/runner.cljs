(ns sicmutils.runner
  (:require [doo.runner :refer-macros [doo-tests]]
            pattern.match-test
            pattern.rule-test
            sicmutils.calculus.derivative-test
            sicmutils.analyzer-test
            sicmutils.complex-test
            sicmutils.euclid-test
            sicmutils.expression-test
            sicmutils.function-test
            sicmutils.generic-test
            sicmutils.matrix-test
            sicmutils.modint-test
            sicmutils.operator-test
            sicmutils.polynomial-test
            sicmutils.polynomial-gcd-test
            sicmutils.polynomial-factor-test
            sicmutils.numbers-test
            sicmutils.numsymb-test
            sicmutils.polynomial-test
            sicmutils.rational-function-test
            sicmutils.rules-test
            sicmutils.series-test
            sicmutils.simplify-test
            sicmutils.structure-test
            sicmutils.value-test))

(doo-tests
 'sicmutils.calculus.derivative-test
 ;; 'sicmutils.function-test
 ;; 'sicmutils.operator-test
 )

(comment
  (doo-tests 'pattern.match-test
             'pattern.rule-test
             'sicmutils.calculus.derivative-test
             'sicmutils.analyzer-test
             'sicmutils.complex-test
             'sicmutils.euclid-test
             'sicmutils.expression-test
             'sicmutils.function-test
             'sicmutils.generic-test
             'sicmutils.matrix-test
             'sicmutils.modint-test
             'sicmutils.numbers-test
             'sicmutils.numsymb-test
             'sicmutils.operator-test
             'sicmutils.polynomial-test
             'sicmutils.polynomial-gcd-test
             'sicmutils.polynomial-factor-test
             'sicmutils.rational-function-test
             'sicmutils.rules-test
             'sicmutils.series-test
             'sicmutils.simplify-test
             'sicmutils.structure-test
             'sicmutils.value-test))
