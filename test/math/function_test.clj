(ns math.function-test
  (:require [clojure.test :refer :all]
            [math.generic :as g]
            [math.numbers]
            [math.value :as v]
            [math.expression :as x]
            [math.function :refer :all]
            ))

(def ^:private near (v/within 1.0e-6))

(deftest function-basic
  (let [f (literal-function 'F)]
    (testing "a"
      (is (= '(F x) (x/freeze-expression (f 'x))))
      (is (= '(F 7) (x/freeze-expression (f (g/+ 3 4)))))
      )
    )
  )

(deftest function-algebra
  (let [add2 (fn [x] (g/+ x 2))
        explog (g/exp g/log)
        mul3 #(* 3 %)]
    (testing "unary"
      (is (= 4 (add2 2)))
      (is (= -4 ((g/- add2) 2)))
      (is (= 9 ((g/sqrt add2) 79)))
      (is (= 1/9 ((g/invert add2) 7)))
      (is (= 1.0 (explog 1.0)))
      (is (near 99.0 (explog 99.0)))
      (is (near 20.08553692 ((g/exp add2) 1.0)))
      (is (near 4.718281828 ((add2 g/exp) 1.0)))
      )
    (testing "binary"
      (is (= 12 ((g/+ add2 4) 6)))
      (is (= 14 ((g/+ add2 mul3) 3)))
      (is (= 10 ((g/+ mul3 4) 2))))
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
