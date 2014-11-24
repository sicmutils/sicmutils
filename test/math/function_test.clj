(ns math.function-test
  (:require [clojure.test :refer :all]
            [math.generic :as g]
            [math.expression :as x]
            [math.function :refer :all]
            ))

(defmacro mx [x] `(x/make '~x))

(deftest function-basic
  (let [f (literal-function 'F)]
    (testing "a"
      (is (= (mx (F x)) (f 'x)))
      (is (= (mx (F 7)) (f (g/+ 3 4))))
      )
    )
  )

(deftest function-differential
  (testing "structural utilities"
    (is (symbolic-derivative? `(g/D f)))
    (is (not (symbolic-derivative? '(e f))))
    (is (not (iterated-symbolic-derivative? `(g/expt g/D 2))))
    (is (iterated-symbolic-derivative? `((g/expt g/D 2) f)))
    (is (= `((g/expt g/D 3) f) (symbolic-increase-derivative `((g/expt g/D 2) f))))
    ))
