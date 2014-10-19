(ns math.poly-test
  (:require [clojure.test :refer :all]
            [math.poly :refer :all]
            [math.generic :as g]
            [math.expression :as x] ;; XXX
            ))

(deftest poly-core
  (testing "zero"
    (is (= 0 (make))))
  (testing "degree"
    (is (= (degree (make)) -1))
    (is (= (degree (make -1 1)) 1))
    (is (= (degree (make 0 1)) 1))
    (is (= (degree (make -1 0 2)) 2))
    (is (= (degree (make -1 2 0)) 1))
    (is (= (degree (make 0 0)) -1)))
  (testing "add constant"
    (is (= (make 3 0 2) (add (make 0 0 2) 3)))
    (is (= (make 0 0 2) (add (make 2 0 2) -2))))
  (testing "add/sub"
    (is (= (make) (add (make 0 0 2) (make 0 0 -2))))
    (is (= 0 (add (make 0 0 2) (make 0 0 -2))))
    (is (= 3 (add (make 3 0 2) (make 0 0 -2))))
    (is (= (make -1 1) (add (make 0 1) (make -1))))
    (is (= (make) (sub (make 0 0 2) (make 0 0 2))))
    (is (= 0 (sub (make 0 0 2) (make 0 0 2))))
    (is (= -3 (sub (make 0 0 2) (make 3 0 2)))))
  (testing "mul"
    (is (= 0 (mul (make 1 2 3) 0)))
    (is (= 0 (mul 0 (make 1 2 3))))
    (is (= 0 (mul (make) (make 1 2 3))))
    (is (= (make 1 2 3) (mul (make 1 2 3) 1)))
    (is (= (make 1 2 3) (mul 1 (make 1 2 3))))
    (is (= (make 3 6 9) (mul (make 1 2 3) 3)))
    (is (= (make 0 1 2 3) (mul (make 0 1) (make 1 2 3))))
    (is (= (make 0 -1 -2 -3) (mul (make 0 -1) (make 1 2 3))))
    (is (= (make -1 0 1) (mul (make 1 1) (make -1 1))))
    (is (= (make 1 3 3 1) (mul (make 1 1) (mul (make 1 1) (make 1 1)))))
    (is (= (make 1 -4 6 -4 1) (mul (mul (make -1 1) (make -1 1))
                                   (mul (make -1 1) (make -1 1))))))
  (testing "expt"
    (let [x+1 (make 1 1)]
      (is (= 1 (expt x+1 0)))
      (is (= x+1 (expt x+1 1)))
      (is (= (make 1 2 1) (expt x+1 2)))
      (is (= (make 1 3 3 1) (expt x+1 3)))
      (is (= (make 1 4 6 4 1) (expt x+1 4)))
      (is (= (make 1 5 10 10 5 1) (expt x+1 5)))
      ))
  (testing "identity"
    (is (= (make 0 1) (make-identity 1)))
    ;; what we want to test is that identity is the identity function
    ;; vs. application, not the multiplicative identity, but for that
    ;; we need apply
    (is (= (make 0 0 0 0 4 5) (mul (make-identity 1) (make 0 0 0 4 5))))
    )
  (testing "arity"
    (is (= 1 (arity (make 0 1)))))
  (testing "make-vars"
    (is (= (list (make 0 1)) (make-vars 1)))
    ;(is (= 'bar (make-vars 2)))
    )
  (testing "expr"
    (let [exp1 (g/* (g/+ 1 'x) (g/+ -3 'x))
          exp2 (g/expt (g/+ 1 'y) 5)
          receive (fn [a b] [a b])]
      (is (= '#{math.generic/* math.generic/+ x} (x/variables-in exp1)))
      (is (= [(make -3 -2 1) '#{x}] (expression-> exp1 receive)))
      (is (= [(make -3 -2 1) '#{x}] (expression-> exp1 receive)))
      (is (= [(make 1 5 10 10 5 1) '#{y}] (expression-> exp2 receive)))
      ))

  )
