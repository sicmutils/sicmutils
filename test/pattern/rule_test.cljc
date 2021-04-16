;;
;; Copyright © 2017 Colin Smith.
;; This work is based on the Scmutils system of MIT/GNU Scheme:
;; Copyright © 2002 Massachusetts Institute of Technology
;;
;; This is free software;  you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or (at
;; your option) any later version.
;;
;;; This software is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this code; if not, see <http://www.gnu.org/licenses/>.
;;

(ns pattern.rule-test
  #?(:cljs  (:require-macros [pattern.rule-test :refer [rule-1]]))
  (:require [clojure.test :as t :refer [is deftest testing]]
            [pattern.rule :as r]
            [sicmutils.ratio]))

(def ^:private !=> (constantly false))

(defmacro rule-1
  "Compiling a rule produces an arity 2 function which takes the data to match
  and a success continuation. For testing we provide this arity-1 wrapper which
  provides a continuation that immediately returns."
  [& pattern-components]
  `(let [compiled-rule# (r/rule ~@pattern-components)]
     (fn [data#]
       (compiled-rule# data# identity))))

(defn ^:private apply-ruleset
  "Like the above, supplies trivial success and failure continuations to a
  ruleset so that it may be invoked with data alone."
  [ruleset data]
  (ruleset data identity (constantly nil)))

(deftest rule-test
  (testing "simple"
    (let [R (rule-1 ((:? a) (:? b) (:?? cs)) =>
                    (a b c (:? a) (:? b) y z))]
      (is (= '(a b c 9 8 y z) (R '(9 8 7 6 5))))
      (is (nil? (R '(9))))))

  (testing "continuation"
    (let [R (r/rule ((:? a) (:? b) (:?? cs)) =>
                    (a b c (:? a) (:? b) y z))]
      (is (= '(z y 8 9 c b a) (R '(9 8 7 6 5) reverse)))
      (is (nil? (R '(9) reverse)))))

  (testing "simple2"
    (let [R (rule-1 ((:? a) (:?? b) (:? a)) =>
                    (2 (:? a) (:?? b)))]
      (is (= '(2 a x y z) (R '(a x y z a))))
      (is (= '(2 a) (R '(a a))))
      (is (= '(2 a b) (R '(a b a))))))

  (testing "simple3"
    (let [R (rule-1 (+ (:? a)) => (:? a))
          notR (rule-1 (+ (:? a)) !=> (:? a))
          evenR (rule-1 (+ (:? a)) #(even? ('a %)) (:? a))]
      (is (= 3 (R '(+ 3))))
      (is (nil? (notR '(+ 3))))
      (is (nil? (notR '(+ 8))))
      (is (nil? (evenR '(+ 3))))
      (is (= 8 (evenR '(+ 8))))))

  (testing "two"
    (let [R (rule-1 ((:? a) (:? b)) => ((:? b) (:? a)))]
      (is (= [20 10] (R [10 20])))
      (is (not (R [10 20 30])))
      (is (not (R [10])))
      (is (not (R [])))
      (is (not (R nil)))
      (is (not (R "")))))

  (testing "simple3"
    (let [R (rule-1 (+ (:?? b1) (:? a) (:?? b2) (:? a) (:?? b3)) =>
                    (+ (* 2 (:? a)) (:?? b1) (:?? b2) (:?? b3)))]
      (is (= '(+ (* 2 a) b c d e) (R '(+ a b c d a e))))
      (is (= '(+ (* 2 a) b c d e) (R '(+ a a b c d e))))
      (is (= '(+ (* 2 a) b c d e) (R '(+ b c d e a a))))
      (is (= '(+ (* 2 a)) (R '(+ a a))))
      (is (not (R '(+ a))))
      (is (not (R '(+ a b c d e))))
      (is (= '(+ (* 2 b) a a b a) (R '(+ b a b a b a))))
      (is (= '(+ (* 2 a) b b a b) (R '(+ a b a b a b)))))))

(deftest ruleset-test
  (testing "simple"
    (let [RS (r/ruleset
              ((:? a) (:? a)) => (* 2 (:? a))
              ((:? a) (:? b)) => ((:? b) (:? a))
              ((:? a) (:? b) (:? c)) => ((:? c) (:? b) (:? a)))]
      (is (= '(4 3) (apply-ruleset RS '(3 4))))
      (is (= '(8 7 6) (apply-ruleset RS '(6 7 8))))
      (is (= '(* 2 5) (apply-ruleset RS '(5 5))))
      (is (= '(* 2 f) (apply-ruleset RS '(f f))))
      (is (nil? (apply-ruleset RS '(4))))
      (is (nil? (apply-ruleset RS '(5 6 7 8))))
      (is (= -2 (RS '(10 8) #(apply - %) (constantly nil))))
      (is (= #sicm/ratio 4/5
             (RS '(10 8) #(apply / %) (constantly nil))))

      (is (= #sicm/ratio 3/40
             (RS '(10 8 6) #(apply / %) (constantly nil))))))

  (testing "algebra-1"
    (let [RS (r/ruleset
              (+ (:? a) (+ (:? b) (:? c))) =>
              (+ (+ (:? a) (:? b) (:? c)))

              (+ (:? a)) =>
              (:? a)

              (* (:? a) (+ (:? b) (:?? c))) =>
              (+ (* (:? a) (:? b)) (* (:? a) (:?? c))))
          S (r/rule-simplifier RS)]
      (is (= 3 (S '(+ 3))))
      (is (= '(+ 3 4 5) (S '(+ 3 (+ 4 5)))))
      (is (= '(+ (* 6 3) (* 6 4)) (S '(* 6 (+ 3 4)))))

      ;; note: we don't have the expr< feature alluded to in the problem
      ;; set, since we plan to rely on canonicalization to
      ;; handle this.
      (is (= '(* (+ y z w) x) (S '(* (+ y (+ z w)) x))))))

  (testing "associative (multiple rulesets)"
    (let [R1 (r/ruleset
              (+ (:?? as) (+ (:?? bs)) (:?? cs)) =>
              (+ (:?? as) (:?? bs) (:?? cs)))
          R2 (r/ruleset
              (* (:?? as) (* (:?? bs)) (:?? cs)) =>
              (* (:?? as) (:?? bs) (:?? cs))

              (* (:?? as) 1 (:?? bs)) =>
              (* (:?? as) (:?? bs)))
          S1 (r/rule-simplifier R1)
          S2 (r/rule-simplifier R2)
          S12 (r/rule-simplifier R1 R2)]
      (is (= '(+ 1 2 3 4 5 6) (S1 '(+ 1 (+ 2 3) (+ 4 (+ 5 6))))))
      (is (= '(* a (+ 1 2 3 4 5 6)) (S1 '(* a (+ 1 (+ 2 3) (+ 4 (+ 5 6)))))))
      (is (= '(cos (sin (+ 1 2 3 4 5 6))) (S1 '(cos (sin (+ 1 (+ 2 3) (+ 4 (+ 5 6))))))))
      (is (= '(+ 1 2 3 4 5 6) (S1 '(+ 1 2 3 (+ 4 (+ 5 6))))))
      (is (= '(+ 1 2 3 4 5 6) (S1 '(+ 1 (+ 2 3) (+ 4 (+ 5 6))))))
      (is (= '(* 1 (* 2 3) (* 4 (* 5 6))) (S1 '(* 1 (* 2 3) (* 4 (* 5 6))))))
      (is (= '(* 2 3 4 5 6) (S2 '(* 1 (* 2 3) (* 4 (* 5 6))))))
      (is (= '(* 2 3 4 5 6) (S2 '(* 1 2 3 (* 4 (* 5 6))))))
      (is (= '(* 2 3 4 5 6) (S2 '(* 1 (* 2 3) (* 4 (* 5 6))))))
      (is (= '(+ 1 (+ 2 3) (+ 4 (+ 5 6))) (S2 '(+ 1 (+ 2 3) (+ 4 (+ 5 6))))))
      (is (= '(+ 1 2 3 4 5 6) (S12 '(+ 1 (+ 2 3) (+ 4 (+ 5 6))))))
      (is (= '(+ 1 2 3 4 5 6) (S12 '(+ 1 2 3 (+ 4 (+ 5 6))))))
      (is (= '(+ 1 2 3 4 5 6) (S12 '(+ 1 (+ 2 3) (+ 4 (+ 5 6))))))
      (is (= '(* 2 3 4 5 6) (S12 '(* 1 (* 2 3) (* 4 (* 5 6))))))
      (is (= '(* 2 3 4 (+ 8 9 7 6) 5) (S12 '(* 1 2 3 (* 4 (+ (+ 8 9) (+ 7 6)) (* 5 1))))))
      (is (= '(* 3 4 5 6) (S12 '(* 1 (* 1 3) (* 4 (* 5 6))))))
      (is (= '(* (+ 2 3) 4 5 6) (S12 '(* 1 (+ 2 3) (* 4 (* 5 6))))))))

  (testing "rules with constraints and/or frame-function substitutions"
    (let [more-than-two? #(> % 2)
          at-least-two? #(>= % 2)
          subtract-from (fn [symbol amount] #(- (% symbol) amount))
          R (r/ruleset
             (a (:? x integer?) (:? y)) => (b (:? y) (:? x))
             (a (:? x float?) (:? y)) => (c (:? y) (:? x))
             (* (expt (cos (:? x)) (:? n more-than-two?))) => success
             (* (expt (tan (:? x)) (:? n #(> % 2)))) => (:? n)
             (* (expt (sin (:? x)) (:? n #(> % 2)))) => (:? #(- (% 'n) 2))
             (* (expt (bzz (:? x)) (:? n #(> % 2)))) => (:? (subtract-from 'n -2))
             (expt (sin (:? x)) (:? n at-least-two?)) => (* (expt (sin (:? x)) (:? #(- (% 'n) 2)))
                                                            (- 1 (expt (cos (:? x)) 2))))
          RS (r/rule-simplifier R)]
      (is (= '(b 4 3) (apply-ruleset R '(a 3 4))))
      (is (= '(c 4 3.1) (apply-ruleset R '(a 3.1 4))))
      (is (nil? (apply-ruleset R '(a "foo" 4))))
      (is (= 'success (apply-ruleset R '(* (expt (cos y) 3)))))
      (is (= 4 (apply-ruleset R '(* (expt (tan y) 4)))))
      (is (= 3 (apply-ruleset R '(* (expt (sin z) 5)))))
      (is (= 6 (apply-ruleset R '(* (expt (bzz t) 4)))))
      (is (= '(+ (expt (cos x) 2) (* (expt (sin x) 0) (- 1 (expt (cos x) 2))))
             (RS '(+ (expt (cos x) 2) (expt (sin x) 2)))))))

  (testing "rearrangement"
    (let [R (rule-1 (expt (:T :X) :N) => ((expt :T :N) :X))]
      (is (= '((expt sin 2) t) (R '(expt (sin t) 2))))
      (is (= '((expt cos 2) t) (R '(expt (cos t) 2))))
      (is (= nil (R '(expt x 2)))))))

(deftest new-tests
  (let [R (rule-1
           ((:splice (pattern.match/match-eq '+)) () (:? a) (:? a))
           =>
           (* 2 (:? a)))]
    (is (= '(* 2 x)
           (R '(+ () x x)))
        "testing splice, splicing in an actual matcher vs a literal, and empty
        list matching.")))
