(ns sicmutils.runner
  (:require [doo.runner :refer-macros [doo-tests]]
            [pattern.match-test]
            [pattern.rule-test]
            [sicmutils.expression-test]
            [sicmutils.rules-test]
            [sicmutils.value-test]))

(doo-tests 'pattern.match-test
           'pattern.rule-test
           'sicmutils.expression-test
           'sicmutils.rules-test
           'sicmutils.value-test)
