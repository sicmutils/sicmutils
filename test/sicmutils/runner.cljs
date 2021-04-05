(ns sicmutils.runner
  (:require [doo.runner :refer-macros [doo-tests]]

            pattern.match-test
            pattern.rule-test

            sicmutils.abstract.function-test
            sicmutils.abstract.number-test

            sicmutils.calculus.basis-test
            sicmutils.calculus.connection-test
            sicmutils.calculus.coordinate-test
            sicmutils.calculus.covariant-test
            sicmutils.calculus.curvature-test
            sicmutils.calculus.derivative-test
            sicmutils.calculus.form-field-test
            sicmutils.calculus.hodge-star-test
            sicmutils.calculus.indexed-test
            sicmutils.calculus.manifold-test
            sicmutils.calculus.map-test
            sicmutils.calculus.metric-test
            sicmutils.calculus.tensor-test
            sicmutils.calculus.vector-calculus-test
            sicmutils.calculus.vector-field-test

            sicmutils.env.sci-test

            sicmutils.examples.central-potential-test
            sicmutils.examples.double-pendulum-test
            sicmutils.examples.driven-pendulum-test
            sicmutils.examples.pendulum-test
            sicmutils.examples.rigid-rotation-test
            sicmutils.examples.top-test

            sicmutils.expression.analyze-test
            sicmutils.expression.compile-test
            sicmutils.expression.render-test

            sicmutils.mechanics.hamilton-test
            sicmutils.mechanics.lagrange-test
            sicmutils.mechanics.rotation-test

            sicmutils.fdg.ch1-test
            sicmutils.fdg.ch2-test
            sicmutils.fdg.ch3-test
            sicmutils.fdg.ch4-test
            sicmutils.fdg.ch5-test
            sicmutils.fdg.ch6-test
            sicmutils.fdg.ch7-test

            sicmutils.sicm.ch1-test
            sicmutils.sicm.ch2-test
            sicmutils.sicm.ch3-test
            sicmutils.sicm.ch5-test
            sicmutils.sicm.ch6-test
            sicmutils.sicm.ch7-test

            sicmutils.numerical.elliptic-test
            sicmutils.numerical.minimize-test
            sicmutils.numerical.ode-test
            sicmutils.numerical.quadrature-test

            sicmutils.numerical.interpolate.polynomial-test
            sicmutils.numerical.interpolate.rational-test
            sicmutils.numerical.interpolate.richardson-test

            sicmutils.numerical.quadrature.adaptive-test
            sicmutils.numerical.quadrature.boole-test
            sicmutils.numerical.quadrature.bulirsch-stoer-test
            sicmutils.numerical.quadrature.infinite-test
            sicmutils.numerical.quadrature.midpoint-test
            sicmutils.numerical.quadrature.milne-test
            sicmutils.numerical.quadrature.riemann-test
            sicmutils.numerical.quadrature.romberg-test
            sicmutils.numerical.quadrature.simpson-test
            sicmutils.numerical.quadrature.simpson38-test
            sicmutils.numerical.quadrature.substitute-test
            sicmutils.numerical.quadrature.trapezoid-test

            sicmutils.numerical.unimin.bracket-test
            sicmutils.numerical.unimin.brent-test
            sicmutils.numerical.unimin.golden-test

            sicmutils.util-test
            sicmutils.util.aggregate-test
            sicmutils.util.permute-test
            sicmutils.util.stream-test
            sicmutils.util.vector-set-test

            sicmutils.complex-test
            sicmutils.differential-test
            sicmutils.euclid-test
            sicmutils.expression-test
            sicmutils.env-test
            sicmutils.function-test
            sicmutils.generic-test
            sicmutils.matrix-test
            sicmutils.modint-test
            sicmutils.operator-test
            sicmutils.polynomial-test
            sicmutils.polynomial.gcd-test
            sicmutils.polynomial.factor-test
            sicmutils.ratio-test
            sicmutils.numbers-test
            sicmutils.rational-function-test
            sicmutils.series-test
            sicmutils.simplify-test
            sicmutils.simplify.rules-test
            sicmutils.structure-test
            sicmutils.value-test))

(doo-tests 'pattern.match-test
           'pattern.rule-test

           'sicmutils.abstract.function-test
           'sicmutils.abstract.number-test

           'sicmutils.calculus.basis-test
           'sicmutils.calculus.connection-test
           'sicmutils.calculus.coordinate-test
           'sicmutils.calculus.covariant-test
           'sicmutils.calculus.curvature-test
           'sicmutils.calculus.derivative-test
           'sicmutils.calculus.form-field-test
           'sicmutils.calculus.hodge-star-test
           'sicmutils.calculus.indexed-test
           'sicmutils.calculus.manifold-test
           'sicmutils.calculus.map-test
           'sicmutils.calculus.metric-test
           'sicmutils.calculus.tensor-test
           'sicmutils.calculus.vector-calculus-test
           'sicmutils.calculus.vector-field-test

           'sicmutils.env.sci-test

           'sicmutils.examples.central-potential-test
           'sicmutils.examples.double-pendulum-test
           'sicmutils.examples.driven-pendulum-test
           'sicmutils.examples.pendulum-test
           'sicmutils.examples.rigid-rotation-test
           'sicmutils.examples.top-test

           'sicmutils.expression.analyze-test
           'sicmutils.expression.compile-test
           'sicmutils.expression.render-test

           'sicmutils.numerical.elliptic-test
           'sicmutils.numerical.minimize-test
           'sicmutils.numerical.ode-test
           'sicmutils.numerical.quadrature-test

           'sicmutils.numerical.interpolate.polynomial-test
           'sicmutils.numerical.interpolate.rational-test
           'sicmutils.numerical.interpolate.richardson-test

           'sicmutils.numerical.quadrature.adaptive-test
           'sicmutils.numerical.quadrature.boole-test
           'sicmutils.numerical.quadrature.bulirsch-stoer-test
           'sicmutils.numerical.quadrature.infinite-test
           'sicmutils.numerical.quadrature.midpoint-test
           'sicmutils.numerical.quadrature.milne-test
           'sicmutils.numerical.quadrature.riemann-test
           'sicmutils.numerical.quadrature.romberg-test
           'sicmutils.numerical.quadrature.simpson-test
           'sicmutils.numerical.quadrature.simpson38-test
           'sicmutils.numerical.quadrature.substitute-test
           'sicmutils.numerical.quadrature.trapezoid-test

           'sicmutils.numerical.unimin.bracket-test
           'sicmutils.numerical.unimin.brent-test
           'sicmutils.numerical.unimin.golden-test

           'sicmutils.util-test
           'sicmutils.util.aggregate-test
           'sicmutils.util.permute-test
           'sicmutils.util.stream-test
           'sicmutils.util.vector-set-test

           'sicmutils.mechanics.hamilton-test
           'sicmutils.mechanics.lagrange-test
           'sicmutils.mechanics.rotation-test

           'sicmutils.fdg.ch1-test
           'sicmutils.fdg.ch2-test
           'sicmutils.fdg.ch3-test
           'sicmutils.fdg.ch4-test
           'sicmutils.fdg.ch5-test
           'sicmutils.fdg.ch6-test

           ;; TODO re-enable once we speed up simplification.
           ;; 'sicmutils.fdg.ch7-test

           'sicmutils.sicm.ch1-test
           'sicmutils.sicm.ch2-test
           'sicmutils.sicm.ch3-test
           'sicmutils.sicm.ch5-test
           'sicmutils.sicm.ch6-test
           'sicmutils.sicm.ch7-test

           'sicmutils.complex-test
           'sicmutils.differential-test
           'sicmutils.euclid-test
           'sicmutils.expression-test
           'sicmutils.env-test
           'sicmutils.function-test
           'sicmutils.generic-test
           'sicmutils.matrix-test
           'sicmutils.modint-test
           'sicmutils.numbers-test
           'sicmutils.operator-test
           'sicmutils.polynomial-test
           'sicmutils.polynomial.gcd-test
           'sicmutils.polynomial.factor-test
           'sicmutils.ratio-test
           'sicmutils.rational-function-test
           'sicmutils.series-test
           'sicmutils.simplify-test
           'sicmutils.simplify.rules-test
           'sicmutils.structure-test
           'sicmutils.value-test)
