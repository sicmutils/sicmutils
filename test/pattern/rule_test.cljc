#_"SPDX-License-Identifier: GPL-3.0"

(ns pattern.rule-test
  (:require [clojure.test :as t :refer [is deftest testing]]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]]
            [pattern.match :as m]
            [pattern.rule :as r :refer [=> !=>]]
            [emmy.ratio]))

(deftest consequence-tests
  (testing "consequence preserves empty containers with correct type"
    (is (= [] ((r/consequence []) {})))
    (is (= () ((r/consequence ()) {})))
    (is (= {} ((r/consequence {}) {})))

    (testing "succeed wrappers are applied where appropriate"
      (is (= (r/succeed nil)
             ((r/consequence nil) {})))

      (is (= (r/succeed false)
             ((r/consequence false) {})))

      (is (= (r/succeed false)
             ((r/consequence ?x) {'?x false})))

      (is (= (r/succeed nil)
             ((r/consequence ?x) {})))))

  (is (= '(+ 10 12)
         ((r/consequence (+ ?b ?a)) {'?b 10 '?a 12}))
      "consequences build functions.")

  (let [z '?x]
    (is (= '(+ 1 2 3)
           ((r/consequence (+ (? ~z) ?y (? y)))
            {'?x 1 '?y 2 'y 3}))
        "consequence can splice in a matching symbol")))

#_{:clj-kondo/ignore [:redundant-fn-wrapper]}
(deftest template-tests
  ;; `f` is here to check the linter.
  (let [f identity]
    (is (= '(+ 1 {})
           (r/template (+ 1 (? (fn [m] (f m))))))
        "one-arg template works even with a binding form inside expecting a map.
        In this case the map will ALWAYS be equal to {}")))

(deftest rule-tests
  (testing "pattern* builds a matcher"
    (is (= {'?x 10} ((r/pattern* '?x) 10)))

    (is (= ((r/pattern* '?x) 10)
           ((r/pattern* '?x =>) 10))
        "=> always passes")

    (is (r/failed?
         ((r/pattern* '?x (comp odd? '?x)) 10))
        "predicate acts on the binding map."))

  (checking "pass, => always succeed" 100
            [x gen/any-equatable]
            (is (= x (r/pass x)))
            (is (= x ((r/rule ?x => ?x) x))))

  (checking "fail, !=> always fail" 100
            [x gen/any-equatable]
            (is (r/failed? (r/fail x)))
            (is (r/failed? ((r/rule ?x !=> ?x) x))))

  (testing "pattern with spliced bindings"
    (let [z 'x]
      (is (= {'x [1 2], 'z 3}
             ((r/pattern (+ (?? ~z) (? z odd?)))
              '(+ 1 2 3)))
          "binding attaches to `'x`, since the symbol is spliced in"))

    (let [z ['z odd?]]
      (is (= {'x [1 2], 'z 3}
             ((r/pattern (+ (?? x) (? ~@z)))
              '(+ 1 2 3))))

      (is (r/failed?
           ((r/pattern (+ (?? x) (? ~@z)))
            '(+ 1 2 4)))
          "the pattern doesn't end on an `odd?`, so it fails.")))

  (testing "simple rule test"
    (let [R (r/rule ((? a) (? b) (?? cs))
                    =>
                    (a b c (? a) (? b) y z))]
      (is (= '(a b c 9 8 y z)
             (R '(9 8 7 6 5))))
      (is (r/failed? (R '(9))))))

  (testing "simple2"
    (let [R (r/rule ((? a) (?? b) (? a))
                    =>
                    (2 (? a) (?? b)))]
      (is (= '(2 a x y z) (R '(a x y z a))))
      (is (= '(2 a) (R '(a a))))
      (is (= '(2 a b) (R '(a b a))))))

  (testing "simple3"
    (let [R (r/rule (+ (? a)) => (? a))
          notR (r/rule (+ (? a)) !=> (? a))
          evenR (r/rule (+ (? a)) (comp even? 'a) (? a))]
      (is (= 3 (R '(+ 3))))
      (is (r/failed? (notR '(+ 3))))
      (is (r/failed? (notR '(+ 8))))
      (is (r/failed? (evenR '(+ 3))))
      (is (= 8 (evenR '(+ 8))))))

  (testing "two"
    (let [R (r/rule ((? a) (? b)) => ((? b) (? a)))]
      (is (= [20 10] (R [10 20])))
      (is (r/failed? (R [10 20 30])))
      (is (r/failed? (R [10])))
      (is (r/failed? (R [])))
      (is (r/failed? (R nil)))
      (is (r/failed? (R "")))))

  (testing "simple3"
    (let [R (r/rule (+ (?? b1) (? a) (?? b2) (? a) (?? b3)) =>
                    (+ (* 2 (? a)) (?? b1) (?? b2) (?? b3)))]
      (is (= '(+ (* 2 a) b c d e) (R '(+ a b c d a e))))
      (is (= '(+ (* 2 a) b c d e) (R '(+ a a b c d e))))
      (is (= '(+ (* 2 a) b c d e) (R '(+ b c d e a a))))
      (is (= '(+ (* 2 a)) (R '(+ a a))))
      (is (r/failed? (R '(+ a))))
      (is (r/failed? (R '(+ a b c d e))))
      (is (= '(+ (* 2 b) a a b a) (R '(+ b a b a b a))))
      (is (= '(+ (* 2 a) b b a b) (R '(+ a b a b a b))))))

  (testing "2-arity form"
    (let [R (r/rule (?a ?b ??cs)
                    (r/consequence
                     {:c [??cs]
                      :b ?b}))]
      (is (= {:c [3], :b 2}
             (R '(1 2 3)))
          "r/consequence builds a function of a binding map"))

    (let [R (r/rule
             (?a ?b ??cs)
             (fn [m]
               (r/template m {:c [??cs]
                              :b ?b})))]
      (is (= {:c [3], :b 2}
             (R '(1 2 3)))
          "r/template does this too!")))

  (testing "wildcards are ignored"
    (let [R (r/rule (+ _ ?a ?a _) => ?a)]
      (is (= 2 (R '(+ () 2 2 3)))))

    (let [R (r/rule (+ ?a (?? _) ?b) => (+ ?a ?b))]
      (is (= '(+ 1 2)
             (R '(+ 1 9 9 9 9 9 2)))
          "wildcard on sequence.")))

  (testing "Rules can fill in dictionaries on the right side."
    (let [R (r/rule (?a ?b (? c odd?)) => {:key [?a]})]
      (is (= {:key [1]}
             (R [1 1 1])))))

  (testing "testing unquote, unquoting in TWO actual matchers vs a literal,
        empty list matching and unquote splicing in the result."
    (let [z 2
          R (r/rule
             (~(m/eq '+) () ~(m/match-when odd? (m/bind '?a))
              ?a ??b)
             => (* ~@[z] ?a ??b))]
      (is (= '(* 2 3 y z)
             (R '(+ () 3 3 y z)))))

    (let [z 2
          R (r/rule
             (~(m/eq '+) () ?a ?a ??b) => (* ~@[z] ?a ??b))]
      (is (= '(* 2 x y z)
             (R '(+ () x x y z)))
          "testing unquote, unquoting in an actual matcher vs a literal, and empty
        list matching.")))

  (testing "Rules can return `false` or `nil` without failing."
    (let [id-rule (r/rule ?r => ?r)]
      (is (false? (id-rule false)))
      (is (nil? (id-rule nil)))
      (is (not (r/failed? (id-rule false))))
      (is (not (r/failed? (r/return false)))))

    (is (false?
         ((r/rule (+ ?a ?b) => false) '(+ 1 2))))

    (is (nil?
         ((r/rule (+ ?a ?b) => nil) '(+ 1 2))))

    (is (not
         (r/failed?
          ((r/rule (+ ?a ?b) => false) '(+ 1 2))))
        "You do NOT fail for returning false.")

    (is (r/failed?
         ((r/rule (+ ?a ?b) !=> false) '(+ 1 2)))
        "You do fail for failing your predicate with `!=>`.")

    (is (r/failed?
         ((r/rule (+ ?a ?b) => ~r/failure) '(+ 1 2)))
        "Or if you splice in failure and return it!")
    )

  (testing "new syntax"
    (let [R (r/rule (?a ?b ??cs) => (a b c ?a ?b y z))]
      (is (= '(a b c 9 8 y z) (R '(9 8 7 6 5))))
      (is (r/failed? (R '(9)))))

    (is (= 2 ((r/rule* ['? '?x odd?]
                       (fn [m] (inc (m '?x))))
              1))
        "make an explicit rule, still a function.")))

(deftest combinator-tests
  (testing "pipe, predicate"
    (let [R (r/pipe
             (r/predicate odd?)
             (r/rule ?x => (+ ?x (? #(inc (% '?x))) ?x))
             (r/rule (+ ?x ??tail) => {:x ?x :tail [??tail]}))]
      (is (= {:x 11 :tail [12 11]}
             (R 11)))
      (is (r/failed? (R 12)))))

  (checking "return returns anything" 100
            [x gen/nat
             if-t gen/any-equatable
             if-f gen/any-equatable]
            (is (= (if (even? x) if-t if-f)
                   ((r/branch (r/predicate even?)
                              (r/return if-t)
                              (r/return if-f))
                    x))))

  (checking "n-times" 100 [n (gen/choose 0 50)
                           x gen/small-integer]
            (let [R (r/n-times n (r/rule ?x => (? #(inc (% '?x)))))]
              (is (= (+ n x)
                     (R x)))))

  (checking "guard" 100 [x gen/small-integer]
            (let [inc-rule (r/rule ?x => (? #(inc (% '?x))))
                  R (r/attempt
                     (r/guard odd? inc-rule))]
              (is (even? (R x))
                  "inc if odd, else leave alone")))

  (checking "iterated" 100
            [x (gen/choose 0 50)]
            (let [inc-if-under-100 (r/rule ?x
                                           #(< (% '?x) 100)
                                           (? #(inc (% '?x))))
                  R (r/iterated inc-if-under-100)]
              (is (= 100 (R x)))))

  (checking "while, until" 100
            [x (gen/choose 0 50)]
            (let [inc (r/rule ?x => (? #(inc (% '?x))))]
              (is (= 101  ((r/while (fn [l _] (< l 100)) inc) x)))
              (is (= 100 ((r/while (fn [_ r] (< r 100)) inc) x)))

              (is (= 102  ((r/until (fn [l _] (> l 100)) inc) x)))
              (is (= 101 ((r/until (fn [_ r] (> r 100)) inc) x)))))

  (checking "fixed-point" 100
            [x (gen/choose 0 50)]
            (let [R (r/rule ?x => (? #(min 100 (inc (% '?x)))))]
              (is (= 100 ((r/fixed-point R) x))
                  "rule stops changing at 100")))

  (testing "trace"
    (let [R-inc (r/rule ?x => (? #(inc (% '?x))))
          acc (atom [])
          f #(swap! acc conj %)
          R (r/n-times 5 (r/trace R-inc f))]
      (is (= 10 (R 5))
          "the rule succeeds.")

      (is (= [5 6 6 7 7 8 8 9 9 10]
             (map (some-fn :in :out) @acc))
          "gather alternating in, out values captured from the rule!")))

  (testing "bottom-up, top down"
    (let [R (r/rule (? _ integer? odd?) => "face!")]
      (is (= {:note [10 "face!" 12]}
             ((r/bottom-up R) {:note [10 11 12]})
             ((r/top-down R) {:note [10 11 12]}))
          "Replacements can dive into vectors and dictionaries.")))

  (let [R (r/attempt
           (r/rule (??pre (? ?x odd?) $$pre) => [??pre "Palindrome!" $$pre]))]
    (is (= [1 2 3 "Palindrome!" 3 2 1]
           (R [1 2 3 11 3 2 1]))
        "Splicing of reverse segments works.")

    (is (= 12 (R 12))
        "attempt rule returns identity when it doesn't match"))

  (testing "iterated-top-down"
    (let [R (r/iterated-top-down
             (r/rule (?a ?b ?c) => ?c))]
      (is (= 3 (R [1 2 [1 2 [1 2 [1 2 3]]]]))))))

(deftest ruleset-tests
  (testing "simple"
    (let [RS (r/ruleset
              ((? a) (? a)) => (* 2 (? a))
              ((? a) (? b)) => ((? b) (? a))
              ((? a) (? b) (? c)) => ((? c) (? b) (? a)))]
      (is (= '(4 3) (RS '(3 4))))
      (is (= '(8 7 6) (RS '(6 7 8))))
      (is (= '(* 2 5) (RS '(5 5))))
      (is (= '(* 2 f) (RS '(f f))))

      (testing "failure acts as ID for rulesets"
        (is (= '(4) (RS '(4))))
        (is (= '(5 6 7 8) (RS '(5 6 7 8)))))

      (is (= [8 10] (RS '(10 8))))
      (is (= [6 8 10] (RS '(10 8 6))))))

  (testing "algebra-1"
    (let [RS (r/ruleset
              (+ ?a (+ ?b ?c)) => (+ (+ ?a ?b ?c))
              (+ ?a) => ?a
              (* ?a (+ ?b ??c)) => (+ (* ?a ?b) (* ?a ??c)))
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
              (+ (?? as) (+ (?? bs)) (?? cs)) =>
              (+ (?? as) (?? bs) (?? cs)))
          R2 (r/ruleset
              (* (?? as) (* (?? bs)) (?? cs)) =>
              (* (?? as) (?? bs) (?? cs))

              (* (?? as) 1 (?? bs)) =>
              (* (?? as) (?? bs)))
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
          subtract-from (fn [sym amount]
                          #(- (% sym) amount))
          R (r/ruleset
             (a (? ?x integer?) ?y) => (b ?y ?x)
             (a (? ?x float?) ?y) => (c ?y ?x)
             (* (expt (cos (? x)) (? n more-than-two?))) => success
             (* (expt (tan (? x)) (? n #(> % 2)))) => (? n)
             (* (expt (sin (? x)) (? n #(> % 2)))) => (? #(- (% 'n) 2))
             (* (expt (bzz (? x)) (? n #(> % 2)))) => (? (subtract-from 'n -2))
             (expt (sin (? x)) (? n at-least-two?)) => (* (expt (sin (? x)) (? #(- (% 'n) 2)))
                                                          (- 1 (expt (cos (? x)) 2))))
          RS (r/term-rewriting R)]
      (is (= '(b 4 3) (R '(a 3 4))))
      (is (= '(c 4 3.1) (R '(a 3.1 4))))
      (is (= '(a "foo" 4) (R '(a "foo" 4))))
      (is (= 'success (R '(* (expt (cos y) 3)))))
      (is (= 4 (R '(* (expt (tan y) 4)))))
      (is (= 3 (R '(* (expt (sin z) 5)))))
      (is (= 6 (R '(* (expt (bzz t) 4)))))
      (is (= '(+ (expt (cos x) 2)
                 (* (expt (sin x) 0)
                    (- 1 (expt (cos x) 2))))
             (RS '(+ (expt (cos x) 2)
                     (expt (sin x) 2)))))))

  (testing "rearrangement"
    (let [R (r/rule (expt (?t ?x) ?n) => ((expt ?t ?n) ?x))]
      (is (= '((expt sin 2) t) (R '(expt (sin t) 2))))
      (is (= '((expt cos 2) t) (R '(expt (cos t) 2))))
      (is (r/failed? (R '(expt x 2)))))))
