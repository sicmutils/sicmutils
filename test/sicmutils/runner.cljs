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

            sicmutils.examples.central-potential-test
            sicmutils.examples.double-pendulum-test
            sicmutils.examples.driven-pendulum-test
            sicmutils.examples.pendulum-test
            sicmutils.examples.rigid-rotation-test
            sicmutils.examples.top-test

            sicmutils.mechanics.hamilton-test
            sicmutils.mechanics.lagrange-test
            sicmutils.mechanics.rotation-test

            sicmutils.sicm.ch1-test
            sicmutils.sicm.ch2-test
            sicmutils.sicm.ch3-test
            sicmutils.sicm.ch5-test
            sicmutils.sicm.ch6-test
            sicmutils.sicm.ch7-test

            sicmutils.numerical.compile-test
            sicmutils.numerical.integrate-test
            sicmutils.numerical.minimize-test
            sicmutils.numerical.ode-test

            sicmutils.analyzer-test
            sicmutils.complex-test
            sicmutils.euclid-test
            sicmutils.expression-test
            sicmutils.env-test
            sicmutils.function-test
            sicmutils.generic-test
            sicmutils.infix-test
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
 'sicmutils.sicm.ch1-test
 'sicmutils.sicm.ch2-test
 ;; 'sicmutils.sicm.ch3-test
 ;; 'sicmutils.sicm.ch5-test
 ;; 'sicmutils.sicm.ch6-test
 'sicmutils.sicm.ch7-test
 )

(comment
  (doo-tests 'pattern.match-test
             'pattern.rule-test

             'sicmutils.calculus.coordinate-test
             'sicmutils.calculus.derivative-test
             'sicmutils.calculus.form-field-test
             'sicmutils.calculus.manifold-test
             'sicmutils.calculus.map-test
             'sicmutils.calculus.vector-field-test

             'sicmutils.examples.central-potential-test
             'sicmutils.examples.double-pendulum-test
             'sicmutils.examples.driven-pendulum-test
             'sicmutils.examples.pendulum-test
             'sicmutils.examples.rigid-rotation-test
             'sicmutils.examples.top-test

             'sicmutils.numerical.compile-test
             'sicmutils.numerical.integrate-test
             'sicmutils.numerical.minimize-test
             'sicmutils.numerical.ode-test

             'sicmutils.mechanics.hamilton-test
             'sicmutils.mechanics.lagrange-test
             'sicmutils.mechanics.rotation-test

             'sicmutils.sicm.ch1-test
             'sicmutils.sicm.ch2-test
             ;; 'sicmutils.sicm.ch3-test
             ;; 'sicmutils.sicm.ch5-test
             ;; 'sicmutils.sicm.ch6-test
             'sicmutils.sicm.ch7-test

             'sicmutils.analyzer-test
             'sicmutils.complex-test
             'sicmutils.euclid-test
             'sicmutils.expression-test
             'sicmutils.env-test
             'sicmutils.function-test
             'sicmutils.generic-test
             'sicmutils.infix-test
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
