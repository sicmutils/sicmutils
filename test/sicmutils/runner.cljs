(ns sicmutils.runner
  (:require [doo.runner :refer-macros [doo-tests]]
            [pattern.match-test]
            [pattern.rule-test]
            [sicmutils.complex-test]
            [sicmutils.euclid-test]
            [sicmutils.expression-test]
            [sicmutils.generic-test]
            [sicmutils.matrix-test]
            [sicmutils.modint-test]
            [sicmutils.numbers-test]
            [sicmutils.numsymb-test]
            [sicmutils.rules-test]
            [sicmutils.structure-test]
            [sicmutils.value-test]))

(doo-tests 'pattern.match-test
           'pattern.rule-test
           'sicmutils.complex-test
           'sicmutils.euclid-test
           'sicmutils.expression-test
           'sicmutils.generic-test
           'sicmutils.matrix-test
           'sicmutils.modint-test
           'sicmutils.numbers-test
           'sicmutils.numsymb-test
           'sicmutils.rules-test
           'sicmutils.structure-test
           'sicmutils.value-test)
