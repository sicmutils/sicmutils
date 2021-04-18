;
; Copyright © 2017 Colin Smith.
; This work is based on the Scmutils system of MIT/GNU Scheme:
; Copyright © 2002 Massachusetts Institute of Technology
;
; This is free software;  you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3 of the License, or (at
; your option) any later version.
;
; This software is distributed in the hope that it will be useful, but
; WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
; General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this code; if not, see <http://www.gnu.org/licenses/>.
;

(ns sicmutils.simplify.rules-test
  (:require [clojure.test :refer [is deftest testing]]
            [pattern.rule :refer [rule-simplifier]]
            [sicmutils.simplify.rules :as r]))

(deftest simplify-square-roots-test
  (let [s r/simplify-square-roots]
    (testing "even powers"
      (is (= '(expt x 4)
             (s '(expt (sqrt x) 8)))
          "sqrt inside of expt")

      (is (= '(expt x 4)
             (s '(sqrt (expt x 8))))
          "expt inside of sqrt"))

    (testing "odd powers"
      (is (= '(* (sqrt x) (expt x 3))
             (s '(expt (sqrt x) 7)))
          "sqrt inside of expt")

      (is (= '(* (sqrt x) (expt x 3))
             (s '(sqrt (expt x 7))))
          "expt inside of sqrt"))

    (testing "simplify across division boundary"
      (testing "no products, straight division"
        (is (= '(sqrt x) (s '(/ x (sqrt x)))))
        (is (= '(/ 1 (sqrt x)) (s '(/ (sqrt x) x)))))

      (testing "product on top only"
        (is (= '(* 2 (sqrt x) 3)
               (s '(/ (* 2 x 3) (sqrt x)))))
        (is (= '(/ (* 2 3) (sqrt x))
               (s '(/ (* 2 (sqrt x) 3) x)))))

      (testing "product on bottom only"
        (is (= '(/ 1 (* 2 (sqrt x) 3))
               (s '(/ (sqrt x) (* 2 x 3)))))
        (is (= '(/ (sqrt x) (* 2 3))
               (s '(/ x (* 2 (sqrt x) 3))))))

      (testing "product in num, denom"
        (is (= '(/ (* 2 (sqrt x) 3)
                   (* y z))
               (s '(/ (* 2 x 3)
                      (* y z (sqrt x)))))
            "sqrt on bottom")

        (is (= '(/ (* 2 3)
                   (* y z (sqrt x)))
               (s '(/ (* 2 (sqrt x) 3)
                      (* y z x))))
            "sqrt on top")))))

(deftest divide-numbers-through-test
  (let [d r/divide-numbers-through]
    (is (= #sicm/ratio 1/2 (d '(/ 1 2))))
    (is (= 'x (d '(* 1 x))))
    (is (= '(* x y z) (d '(* 1 x y z))))
    (is (= '(*) (d '(* 1))))
    (is (= '(+ (/ a 3) (/ b 3) (/ c 3)) (d '(/ (+ a b c) 3))))))

(deftest sincos-flush-ones-test
  (let [s r/sincos-flush-ones]
    (is (= '(+ 1 a b c c d e f g)
           (s '(+ a b c (expt (sin x) 2) c d (expt (cos x) 2) e f g))))
    (is (= '(+ (* (expt (cos x) 2) (expt (cos x) 1)) c (expt (sin x) 2) d e)
           (s '(+ c (expt (sin x) 2)  d (expt (cos x) 3) e ))))
    (is (= '(+ (* (expt (sin x) 2) (expt (sin x) 1) (expt (sin x) 2))
               (* (expt (cos x) 2) (expt (cos x) 1))
               c d e)
           (s '(+ c (expt (sin x) 5)  d (expt (cos x) 3) e ))))))

(deftest sin-sq->cos-sq-test
  (let [s r/sin-sq->cos-sq]
    (is (= '(+ 3 x
               (* (* (* (expt (sin x) 1)
                        (- 1 (expt (cos x) 2)))
                     (- 1 (expt (cos x) 2))) (- 1 (expt (cos x) 2))))
           (s '(+ 3 x (expt (sin x) 7)))))))

(deftest sqrt-expand-contract-test
  (testing "sqrt-expand works with division"
    (is (= '(+ (/ (sqrt a) (sqrt b)) (/ (sqrt c) (sqrt b)))
           (r/sqrt-expand '(+ (sqrt (/ a b)) (sqrt (/ c b))))))
    (is (= '(- (/ (sqrt a) (sqrt b)) (/ (sqrt c) (sqrt b)))
           (r/sqrt-expand '(- (sqrt (/ a b)) (sqrt (/ c b)))))))

  (let [sqrt-contract (r/sqrt-contract identity)]
    (testing "cancels square roots if the values are equal"
      (is (= '(* a c e (sqrt (* b d)))
             (sqrt-contract
              '(* a (sqrt b) c (sqrt d) e)))
          "square roots get pushed to the end.")

      (is (= '(* a b c e)
             (sqrt-contract
              '(* a (sqrt b) c (sqrt b) e)))))

    (testing "sqrt-contract undoes expansion over division"
      (is (= '(+ (sqrt (/ a b)) (sqrt (/ c b)))
             (sqrt-contract
              '(+ (/ (sqrt a) (sqrt b)) (/ (sqrt c) (sqrt b))))))

      (is (= '(- (sqrt (/ a b)) (sqrt (/ c b)))
             (sqrt-contract
              '(- (/ (sqrt a) (sqrt b)) (/ (sqrt c) (sqrt b)))))))))

(deftest new-tests
  (let [rule (r/constant-promotion '* 0)
        f    (rule-simplifier rule)]
    (is (= 0 (f '(* x 0)))
        "works")))

(comment
  ;; TODO note that there are lots more simplifiers we can grab to run these tests!

  (define (assert-unchanged datum rule)
    (assert-eq datum (rule datum)))

  (define-each-check
    (equal?
     '(+ (* w x) (* x y) (* x z))
     (simplify-algebra '(* (+ y (+ z w)) x)))

    (equal?
     '(* 3 x)
     (simplify-algebra '(+ (* 3 (+ x 1)) -3)))

    (equal? 0 (simplify-algebra '(+)))
    (equal? true (simplify-logic '(and)))
    (equal? false (simplify-logic '(or)))
    (equal?
     '(* 3 x)
     (simplify-algebra '(+ (* 3 (+ x 1)) -3
			                     (* y (+ 1 2 -3) z))))
    (equal?
     '(/ (* r1 r2) (+ r1 r2))
     ((pipe ->quotient-of-sums simplify-quotient)
      '(/ 1 (+ (/ 1 r1) (/ 1 r2))))))

  (define associate-addition
    (rule '(+ (? a) (+ (? b) (? c)))
	        `(+ (+ ,a ,b) ,c)))

  (define-test (rule-smoke)
    (assert-equal
     '(+ (+ 2 3) 4)
     (associate-addition '(+ 2 (+ 3 4)))))

  (define-test (rule-that-can-refuse)
    (define sort-numbers
      (rule '(+ (? a) (? b))
	          (and (> a b)
		             `(+ ,b ,a))))
    (assert-equal
     '(+ 2 3)
     (sort-numbers '(+ 3 2)))
    (assert-unchanged '(+ 2 3) sort-numbers))
  (define-test (parametric-rule-smoke)
    (assert-equal
     false
     (simplify-ors '(or))))

  (define-test (negation-pushing-smoke)
    (assert-equal
     '(and (not a) (not b))
     (simplify-negations '(not (or a b)))))

  (define-test (or-pushing)
    (assert-equal
     '(and (or (not (< -1/4 (- x2 x1)))
	             (and (not (< (- x2 x1) 1/4)) (not (< (- x2 x1) 0)))
	             (and (< (- x2 x1) 1/4) (not (< (- x2 x1) 0)))
	             (and (< -1/4 (- x2 x1)) (< (- x2 x1) 0)))
	         (or (< (- x2 x1) 0)
	             (and (not (< (- x2 x1) 1/4)) (not (< (- x2 x1) 0)))
	             (and (< (- x2 x1) 1/4) (not (< (- x2 x1) 0)))
	             (and (< -1/4 (- x2 x1)) (< (- x2 x1) 0))))
     (push-or-through-and
      '(or (and (not (< -1/4 (- x2 x1)))
	              (< (- x2 x1) 0))
	         (and (not (< (- x2 x1) 1/4))
	              (not (< (- x2 x1) 0)))
	         (and (< (- x2 x1) 1/4)
	              (not (< (- x2 x1) 0)))
	         (and (< -1/4 (- x2 x1))
	              (< (- x2 x1) 0))))))

  (define-test (cnf)
    (assert-true
     (->conjunctive-normal-form
      '(or (and (not (< -1/4 (- x2 x1)))
	              (< (- x2 x1) 0))
	         (and (not (< (- x2 x1) 1/4))
	              (not (< (- x2 x1) 0)))
	         (and (< (- x2 x1) 1/4)
	              (not (< (- x2 x1) 0)))
	         (and (< -1/4 (- x2 x1))
	              (< (- x2 x1) 0))))))

  (define-test (more-cnf)
    (assert-equal
     '(and (or a b)
	         (or a (not b))
	         (or b (not a))
	         (or (not a) (not b)))
     (->conjunctive-normal-form
      '(and (or a b)
	          (or a (not b))
	          (or (not a) b)
	          (or (not a) (not b))))))

  (define-test (scanning-for-duplicates)
    (define find-consecutive-dups
      (rule '((?? stuff1) (? x) (? x) (?? stuff2))
	          `(,@stuff1 ,x ,@stuff2)))
    (let ((items (iota 10))) ; linear
      (assert-equal
       items
       ((iterated-bottom-up find-consecutive-dups)
        items))))

  (define-test (associativity-test)
    (define plus-assoc (associativity '+))
    (let* ((sublist '(1 2 3))
	         (len 10) ; linear (I think)
	         (items (cons '+ (make-list len (cons '+ sublist)))))
      (check (equal?
	            (cons '+ (apply append (make-list len sublist)))
	            ((iterated-bottom-up plus-assoc)
	             items)))))

  (define-test (removing-duplicates)
    (define find-consecutive-dups
      (rule '((?? stuff1) (? x) (? x) (?? stuff2))
	          `(,@stuff1 ,x ,@stuff2)))
    (let ((items (make-list 10 'foo))) ; quadratic + gc pressure
      (assert-equal
       '(foo)
       ((iterated-bottom-up find-consecutive-dups)
        items)))
    (let* ((len 10) ; quadratic + gc pressure
	         (items (append (iota len) (make-list len 'foo))))
      (assert-equal
       (append (iota len) '(foo))
       ((iterated-bottom-up find-consecutive-dups)
        items))))

  (define-test (removing-duplicates-the-easy-way)
    (define or-idempotent (idempotence 'or))
    (let ((items (cons 'or (make-list 10 'foo)))) ; linear
      (assert-equal
       '(or foo)
       ((iterated-bottom-up or-idempotent)
        items)))
    (let* ((len 10) ; linear
	         (items (cons 'or (append (iota len) (make-list len 'foo)))))
      (assert-equal
       (cons 'or (append (iota len) '(foo)))
       ((iterated-bottom-up or-idempotent)
        items))))

  (define-test (commutativity-check-test)
    (let* ((len 10) ; linear
	         (items `(and ,@(iota len))))
      (check (eq? items ((commutativity 'and) items)))))

  (define-test (commutativity-rule-test)
    (let* ((len 10) ; N log N
	         (items `(and ,@(reverse (iota len)))))
      (check
       (equal?
        `(and ,@(iota len))
        ((commutativity 'and) items)))))

  (define-test (commutativity-test)
    (let* ((len 10) ; linear
	         (items `(and ,@(reverse (iota len)))))
      (check
       (equal?
        `(and ,@(iota len))
        ((iterated-bottom-up (commutativity 'and)) items)))))

  (define-test (simplifying-sums)
    (let ((len 10)) ; linear
      (check (equal? len (simplify-sums `(+ ,@(make-list len 1)))))))

  (define-test (simplifying-ands)
    (let* ((len 10) ; TODO quadratic, presumably because of checking
           ;; for (and ... (? a) ... (not (? a)) ...)
	         (items `(and ,@(iota len) ,@(make-list len 'foo))))
      (check
       (equal?
        `(and ,@(iota len) foo)
        (simplify-ands items))))))
