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
    (let [R (rule (+ (:? a))
                  (:? a))]
      (is (= 3 (R '(+ 3))))
      )
    )

 (testing "two"
   (let [R (rule ((:? a) (:? b)) ((:? b) (:? a)))]
     (is (= [20 10] (R [10 20])))
     (is (not (R [10 20 30])))
     (is (not (R [10])))
     (is (not (R [])))
     (is (not (R nil)))
     (is (not (R "")))))

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

(deftest ruleset-test
  (testing "simple"
    (let [RS (ruleset
              ((:? a) (:? a))
              (* 2 (:? a))
              ((:? a) (:? b))
              ((:? b) (:? a))
              ((:? a) (:? b) (:? c))
              ((:? c) (:? b) (:? a)))]
      (is (= '(4 3) (RS '(3 4))))
      (is (= '(8 7 6) (RS '(6 7 8))))
      (is (= '(* 2 5) (RS '(5 5))))
      (is (= '(* 2 f) (RS '(f f))))
      (is (= '(4) (RS '(4))))
      (is (= '(5 6 7 8) (RS '(5 6 7 8))))
      (is (= -2 (RS '(10 8) #(apply - %))))
      (is (= 4/5 (RS '(10 8) #(apply / %))))
      (is (= 3/40 (RS '(10 8 6) #(apply / %))))
      ))

  (testing "algebra-1"
    (let [RS (ruleset
              (+ (:? a) (+ (:? b) (:? c)))
              (+ (+ (:? a) (:? b) (:? c)))

              (+ (:? a))
              (:? a)

              (* (:? a) (+ (:? b) (:?? c)))
              (+ (* (:? a) (:? b)) (* (:? a) (:?? c)))
              )
          S (rule-simplifier RS)]
      (is (= 3 (S '(+ 3))))
      (is (= '(+ 3 4 5) (S '(+ 3 (+ 4 5)))))
      (is (= '(+ (* 6 3) (* 6 4)) (S '(* 6 (+ 3 4)))))
      ;; note: we don't have the expr< feature alluded to in the problem
      ;; set, since we plan to rely on constructor canonicalization to
      ;; handle this. XXX come back to this when we see if that plan
      ;; works out. (Is simplification an output-only experience in
      ;; scmutils?)
      (is (= '(* (+ y z w) x) (S '(* (+ y (+ z w)) x))))
      ))
  )
