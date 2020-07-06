(ns sicmutils.runner
  (:require [doo.runner :refer-macros [doo-tests]]
            [pattern.match-test]
            [pattern.rule-test]
            [sicmutils.rules-test]))

(doo-tests 'pattern.match-test
           'pattern.rule-test
           'sicmutils.rules-test)
