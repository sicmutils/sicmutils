(ns sicmutils.runner
  (:require [doo.runner :refer-macros [doo-tests]]
            pattern.match-test
            pattern.rule-test
            sicmutils.calculus.coordinate-test
            sicmutils.calculus.derivative-test
            sicmutils.calculus.form-field-test
            sicmutils.calculus.manifold-test
            sicmutils.calculus.map-test
            sicmutils.calculus.vector-field-test
            sicmutils.mechanics.lagrange-test
            sicmutils.mechanics.rotation-test
            sicmutils.numerical.compile-test
            sicmutils.numerical.integrate-test
            sicmutils.numerical.minimize-test
            sicmutils.numerical.ode-test
            sicmutils.analyzer-test
            sicmutils.complex-test
            sicmutils.euclid-test
            sicmutils.expression-test
            sicmutils.infix-test
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

(doo-tests 'pattern.match-test
           'pattern.rule-test

           ;; TODO this currently triggers a lot of timeout errors. Something
           ;; odd is going on.
           ;;
           ;; 'sicmutils.calculus.coordinate-test

           'sicmutils.calculus.derivative-test
           'sicmutils.calculus.form-field-test
           'sicmutils.calculus.manifold-test
           'sicmutils.calculus.map-test
           'sicmutils.calculus.vector-field-test
           'sicmutils.numerical.compile-test
           'sicmutils.numerical.integrate-test
           'sicmutils.numerical.minimize-test
           'sicmutils.numerical.ode-test
           'sicmutils.mechanics.lagrange-test
           'sicmutils.mechanics.rotation-test
           'sicmutils.analyzer-test
           'sicmutils.complex-test
           'sicmutils.euclid-test
           'sicmutils.infix-test
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
           'sicmutils.value-test)
