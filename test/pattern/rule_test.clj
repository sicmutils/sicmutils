(ns pattern.rule-test
  (:require [clojure.test :refer :all]
            [pattern.rule :refer :all]))

(deftest rule-test
  (testing "simple"
   (let [R (rule ((:? a) (:? b) (:?? cs))
                 (a b c (:? a) (:? b) y z))]
     (is (= '(a b c 9 8 y z) (R '(9 8 7 6 5))))
     (is (nil? (R '(9)))))
   )

 (testing "simple2"
   (let [R (rule ((:? a) (:?? b) (:? a))
                 (2 (:? a) (:?? b)))]
     (is (= '(2 a x y z) (R '(a x y z a))))
     (is (= '(2 a) (R '(a a))))
     (is (= '(2 a b) (R '(a b a))))
     )
   )

 (testing "simple3"
   (let [R (rule (+ (:?? b1) (:? a) (:?? b2) (:? a) (:?? b3))
                 (+ (* 2 (:? a)) (:?? b1) (:?? b2) (:?? b3)))]
     (is (= '(+ (* 2 a) b c d e) (R '(+ a b c d a e))))
     (is (= '(+ (* 2 a) b c d e) (R '(+ a a b c d e))))
     (is (= '(+ (* 2 a) b c d e) (R '(+ b c d e a a))))
     (is (= '(+ (* 2 a)) (R '(+ a a))))
     (is (not (R '(+ a))))
     (is (not (R '(+ a b c d e))))
     (is (= '(+ (* 2 b) a a b a) (R '(+ b a b a b a))))
     (is (= '(+ (* 2 a) b b a b) (R '(+ a b a b a b))))
     )
   )
)
