#_"SPDX-License-Identifier: GPL-3.0"

(ns pattern.match-test
  (:require [clojure.test :refer [is deftest testing]]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.clojure-test
             :refer [checking] :include-macros true]
            [pattern.match :as m]
            [pattern.syntax :as ps]))

(def gen-frame
  (gen/map gen/keyword
           gen/any-equatable
           {:max-elements 4}))

(deftest basic-matcher-tests
  (checking "fail, always fails" 100 [x gen/any-equatable]
            (is (nil? (m/fail {} x identity))))

  (checking "pass always returns existing frame"
            100 [frame gen-frame
                 x gen/any]
            (is (= frame (m/pass frame x identity))))

  (checking "with-frame always subs new frame."
            100 [new-frame gen-frame
                 x gen/any]
            (is (= new-frame
                   ((m/with-frame new-frame) {} x identity))))

  (checking "update-frame always succeeds, merges frames"
            100 [ma gen-frame
                 mb gen-frame
                 x gen/any]
            (is (= (merge ma mb)
                   ((m/update-frame merge mb) ma x identity))))

  (checking "predicate gates based on a fn" 100 [x gen/nat]
            (if (even? x)
              (is (= {} ((m/predicate even?) {} x identity)))
              (is (false? ((m/predicate even?) {} x identity)))))

  (checking "frame-predicate gates based on a fn of a frame" 100
            [x gen/any
             v gen/nat]
            (let [match (m/frame-predicate (comp even? :k))]
              (if (even? v)
                (is (= {:k v} (match {:k v} x identity)))
                (is (false? (match {:k v} x identity))))))

  (testing "combinator eq"
    (is (= {} ((m/eq 'a) {} 'a identity))
        "A successful match returns the bindings in play, NOT something falsey.")

    (is (not ((m/eq 'a) {} '(e d c b a) identity))
        "'a does not match the sequence."))

  (testing "matcher version of eq"
    (is (m/failed? (m/match (m/eq 'a) nil)))
    (is (m/failed? (m/match (m/eq 'a) []))))

  (testing "m/bind"
    (is (= {:x 'a} ((m/bind :x) {} 'a identity)))
    (is (= {:x '(a b)} ((m/bind :x) {} '(a b) identity))))

  (testing "bind on nil, false"
    (is (false? ((m/bind :x) {:x false} nil identity))
        "false and nil don't match")

    (is (false? ((m/bind :x) {:x nil} false identity))
        "false and nil don't match")

    (is (= {:x false} ((m/bind :x) {} false identity))
        "false is bound if present")

    (is (= {:x false}
           ((m/bind :x) {:x false} false identity)
           ((m/bind :x) {} false identity))
        "false is bound if present and equal, or not present")

    (is (= {:x nil}
           ((m/bind :x) {:x nil} nil identity)
           ((m/bind :x) {} nil identity))
        "nil is bound if present and equal, or not present"))

  (testing "bind with constraint"
    (is (= {:x 6} ((m/bind :x integer?) {} 6 identity)))

    ;; ClojureScript treats floats with no mantissa as integers.
    (let [expected #?(:clj nil
                      :cljs {:x 6.0})]
      (is (= expected ((m/bind :x integer?) {} 6.0 identity))))

    ;; Both languages treat 6.1 as a float.
    (is (nil? ((m/bind :x integer?) {} 6.1 identity)))
    (is (= {:x 6.0} ((m/bind :x float?) {} 6.0 identity)))))

(deftest seq-matcher-tests
  (testing "segment"
    (is (= '[[{:x []} [a b c]]
             [{:x [a]} (b c)]
             [{:x [a b]} (c)]
             [{:x [a b c]} nil]]
           (m/all-results (m/segment :x) '[a b c])))

    (is (= '[[{} [a b c]]
             [{} (b c)]
             [{} (c)]
             [{} nil]]
           (m/all-results (m/segment '_) '[a b c]))
        "Segments respect wildcards by not binding.")

    (is (= '[{??x [a b c]}]
           (m/all-results ['??x] '[a b c]))
        "A final segment in a list matcher matches the entire list, no scanning.")

    (let [match (m/all-results-matcher
                 (m/sequence
                  (m/segment :x)
                  (m/segment :y)))]
      (is (= '[{:x [], :y [a b c]}
               {:x [a], :y [b c]}
               {:x [a b], :y [c]}
               {:x [a b c], :y []}]
             (match '[a b c]))
          "If you have TWO segment variables, they correctly scan."))

    (testing "segment matches empty segment for lists, vectors."
      (is (= [{}] ((m/all-results-matcher ()) ()))
          "empty dict!")

      (is (= [] ((m/all-results-matcher ()) '(foo)))
          "no match.")

      (is (= {:x []}
             ((m/segment :x) {} () (fn [frame _] frame))))

      (is (= {:x []}
             ((m/segment :x) {} [] (fn [frame _] frame))))))

  (testing "final segment with an ignored binding"
    (is (= '{?a 1, ?b 2}
           (m/match '[?a ?b (?? _)] [1 2 3 4]))))

  (is (= [{'stuff '(0 1 2 3 4 5 6 7 8 9)}]
         ((m/all-results-matcher '(and (?? stuff)))
          (cons 'and (range 10))))
      "segments at the end of a list run in constant time, except building the
        test list")

  (testing "segment-constraint"
    (let [find-two-ints (m/sequence
                         (m/segment :xs)
                         (m/bind :i integer?)
                         (m/segment :ys)
                         (m/bind :j integer?)
                         (m/segment :zs))]
      (is (= '[{:xs [1.1 [1 3] 2.3] :i 3 :ys [6.5 x [3 5]] :j 4 :zs [22]}
               {:xs [1.1 [1 3] 2.3] :i 3 :ys [6.5 x [3 5] 4] :j 22 :zs []}
               {:xs [1.1 [1 3] 2.3 3 6.5 x [3 5]] :i 4 :ys [] :j 22 :zs []}]
             (m/all-results
              find-two-ints
              '(1.1 [1 3] 2.3 3 6.5 x [3 5] 4 22)))
          "segments accomplish searches")))

  (testing "twin, equal-binding segments"
    (let [xs-xs (m/sequence
                 (m/segment :x)
                 (m/segment :x))
          xs-xs-etc (m/sequence
                     (m/segment :x)
                     (m/segment :x)
                     (m/segment :y))
          etc-xs-xs-etc (m/sequence
                         (m/segment :w)
                         (m/segment :x)
                         (m/segment :x)
                         (m/segment :y))]
      (is (= {:x '[a b c]}
             (m/match xs-xs '(a b c a b c)))
          "find two equal segments")

      (testing "equal segments don't exist"
        (is (m/failed? (m/match xs-xs '(a b c a b d))))
        (is (m/failed? (m/match xs-xs '(a b c a b c d e)))))

      (let [match (m/all-results-matcher xs-xs-etc)]
        (is (= [{:x [] :y '[a b c a b c d e]}
                {:x '[a b c] :y '[d e]}]
               (match '(a b c a b c d e))))

        (is (= [{:x [] :y '(a b a b a b a b)}
                {:x '[a b] :y '[a b a b]}
                {:x '[a b a b] :y '[]}]
               (match '(a b a b a b a b))))

        (is (= '[{:x [], :y [h k h k h k h k]}
                 {:x [h k], :y [h k h k]}
                 {:x [h k h k], :y []}]
               (match '[h k h k h k h k]))))

      (is (= '[{:y [a b a b a b a b], :x [], :w []}
               {:y [a b a b], :x [a b], :w []}
               {:y [], :x [a b a b], :w []}
               {:y [b a b a b a b], :x [], :w [a]}
               {:y [b a b], :x [b a], :w [a]}
               {:y [a b a b a b], :x [], :w [a b]}
               {:y [a b], :x [a b], :w [a b]}
               {:y [b a b a b], :x [], :w [a b a]}
               {:y [b], :x [b a], :w [a b a]}
               {:y [a b a b], :x [], :w [a b a b]}
               {:y [], :x [a b], :w [a b a b]}
               {:y [b a b], :x [], :w [a b a b a]}
               {:y [a b], :x [], :w [a b a b a b]}
               {:y [b], :x [], :w [a b a b a b a]}
               {:y [], :x [], :w [a b a b a b a b]}]
             (m/all-results
              etc-xs-xs-etc '(a b a b a b a b))))))

  (testing "example-from-6.945-notes"
    (is (= '[{y [b b b b b b] x []}
             {y [b b b b] x [b]}
             {y [b b] x [b b]}
             {y [] x [b b b]}]
           (m/all-results
            '(a (?? x) (?? y) (?? x) c)
            '(a b b b b b b c)))))

  (testing "an expression"
    (let [expr (m/sequence
                (m/sequence '*
                            (m/bind :a)
                            (m/bind :c))
                (m/sequence '*
                            (m/bind :b)
                            (m/bind :c)))]
      (is (= '{:a 3 :b 4 :c x}
             (m/match expr '((* 3 x) (* 4 x)))))

      (is (m/failed?
           (m/match expr '((* 3 x) (* 4 y))))))))

(deftest matcher-combinator-tests
  (testing "match-if branches"
    (let [match (m/matcher
                 (m/match-if odd? '?x '?y))]
      (is (= {'?x 9} (match 9)))
      (is (= {'?y 10} (match 10))))

    (let [m (m/match-if odd? '?odd '?even)]
      (is (= {'?odd 11} (m {} 11 identity)))
      (is (= {'?even 12} (m {} 12 identity)))))

  (testing "match-if without else matches match-when"
    (let [m-if (m/match-if odd? '?odd)
          m-when (m/match-when odd? '?odd)]
      (is (= {'?odd 11}
             (m-if {} 11 identity)
             (m-when {} 11 identity)))

      (is (nil? (m-if {} 12 identity)))
      (is (nil? (m-when {} 12 identity)))))

  (checking "(or) always fails" 100
            [m gen-frame
             x gen/any-equatable]
            (is (nil? ((m/or) m x identity))))

  (testing "m/or unit tests"
    (let [match (m/matcher (m/or ['? 'x #{11}]
                                 (m/not ['? 'x odd?])))]
      (is (= {'x 11} (match 11)))
      (is (= {} (match 12))))

    (let [match (m/or (m/predicate odd?)
                      (m/predicate #{12}))]
      (is (= {} (match {} 12 identity)))
      (is (= {} (match {} 11 identity)))
      (is (not (match {} 8 identity)))))

  (checking "(or r) always matches r, more args" 100
            [m gen-frame
             x gen/any-equatable]
            (is (= m ((m/or m/pass) m x identity)))
            (is (nil? ((m/or m/fail) m x identity)))

            (is (= m ((m/or m/fail m/fail m/fail m/pass) m x identity)))
            (is (nil? ((m/or m/fail m/fail m/fail m/fail) m x identity))))

  (checking "(and) always passes" 100
            [m gen-frame
             x gen/any-equatable]
            (is (= m ((m/and) m x identity))))

  (checking "(and r) always matches r, more args" 100
            [m gen-frame
             x gen/any-equatable]
            (is (= m ((m/and m/pass) m x identity)))
            (is (nil? ((m/and m/fail) m x identity)))

            (is (= m ((m/and m/pass m/pass m/pass) m x identity)))
            (is (nil? ((m/and m/fail m/pass m/pass) m x identity))))

  (testing "`and` must match all and returns the frame from the final."
    (let [match (m/and (m/predicate even?)
                       (m/bind :x)
                       (m/predicate #{12}))]
      (is (= {:x 12} (match {} 12 identity)))
      (is (not (match {} 8 identity)))))

  (checking "(not r) does the opposite of r" 100
            [m gen-frame
             x gen/any-equatable]
            (is (nil? ((m/not m/pass) m x identity)))
            (is (= m ((m/not m/fail) m x identity)))))

(deftest match-compiler
  (testing "simple"
    (let [match-x ['? :x]
          match-xx [['? :x] ['? :x]]
          match-xy [['? :x] ['? :y]]
          match-x-ys-x [['? :x] ['?? :ys] ['? :x]]]
      (is (= '{:x 3} (m/match match-x 3)))
      (is (= '{:x 2} (m/match match-xx [2 2])))
      (is (m/failed? (m/match match-xx [2 3])))
      (is (= '{:x 2 :y 3} (m/match match-xy [2 3])))
      (is (= '{:x 2 :ys [3 4 5]} (m/match match-x-ys-x [2 3 4 5 2])))
      (is (m/failed? (m/match match-x-ys-x [2 3 4 5 6])))
      (is (m/failed? (m/match match-xy [2]))))))

(deftest gjs-tests
  ;; these tests come from matcher.scm.
  (testing "element inside of list"
    (is (= {'b 1}
           (m/match '(a ((? b) 2 3) 1 c)
                    '(a (1 2 3) 1 c))))

    (is (= {'b 1}
           (m/match `(~'a ((~'? ~'b ~number?) 2 3) 1 ~'c)
                    '(a (1 2 3) 1 c)))
        "match element with predicate inside of list")

    (is (m/failed?
         (m/match `(~'a ((~'? ~'b ~symbol?) 2 3) 1 ~'c)
                  '(a (1 2 3) 1 c)))
        "match element inside list with failing predicate")

    (is (m/failed?
         (m/match '(a ((? b) 2 3) (? b) c)
                  '(a (1 2 3) 2 c)))
        "match element inside list, but fail the second match outside")

    (is (= {'b 1}
           (m/match '(a ((? b) 2 3) (? b) c)
                    '(a (1 2 3) 1 c)))
        "match element inside list, repeated match outside"))

  (testing "segment and reverse segment"
    (letfn [(palindrome? [x]
              ((m/pattern->combinators '((?? x) ($$ x)))
               {}
               x
               boolean))]
      (is (palindrome? '(a b c c b a)))
      (is (not (palindrome? '(a b c c a b)))))))

(deftest syntax-tests
  (testing "pattern-shaped inputs"
    (let [xx '[?x ?x]
          xy '[?x ?y]
          xs '[??x]
          xs-xs '[??x ??x]
          xs-ys '[??x ??y]]
      (is (= '{??x [1 2 3 4]}
             (m/match xs [1 2 3 4])))

      (is (= '{?x 2}
             (m/match xx [2 2])))

      (is (= '{?x 5 ?y 6}
             (m/match xy [5 6])))

      (is (ps/segment? '??x))
      (is (ps/segment? '??y))

      (is (= {'??x [1 2]}
             (m/match xs-xs [1 2 1 2])))

      (is (= '{??x [] ??y [1 2 3 4]}
             (m/match xs-ys [1 2 3 4])))

      (is (= '[{??x [], ??y [1 2 3 4]}
               {??x [1], ??y [2 3 4]}
               {??x [1 2], ??y [3 4]}
               {??x [1 2 3], ??y [4]}
               {??x [1 2 3 4], ??y []}]
             ((m/all-results-matcher xs-ys) '(1 2 3 4))))

      (is (= '[{??x [], ??y [1 2 3 4]}
               {??x [1], ??y [2 3 4]}
               {??x [1 2], ??y [3 4]}
               {??x [1 2 3], ??y [4]}
               {??x [1 2 3 4], ??y []}]
             ((m/all-results-matcher xs-ys) [1 2 3 4])))))

  (testing "predicates can return new bindings"
    (is (= {'x '+, :y 'z}
           (m/match (m/sequence (m/bind 'x))
                    (fn [_] {:y 'z})
                    ['+]))
        "We can add new bindings to the map."))

  (testing "wildcards are ignored"
    (let [match (m/matcher '(+ (? _ #{11}) ?b))]
      (is (= {'?b 12} (match '(+ 11 12))))
      (is (m/failed?
           (match '(+ 12 11))))))

  (testing "restrictions"
    (let [pattern ['? 'x odd? #(> % 12)]
          f (ps/restriction ['? 'x odd? #(> % 12)])]
      (is (ps/restricted? pattern)
          "Any extra entries in a binding pattern are treated as restriction
          predicates.")
      (is (f 13)
          "inputs pass if they match all predicates.")

      (is (not (f 11))
          "odd but not > 12.")))

  (testing "compile-pattern preserves container types"
    (is  (= `(list '~'?x :one '~'?y '~'?z)
            (ps/compile-pattern '(?x :one ?y ?z))))

    (is  (= ['(quote ?x) :one '(quote ?y) '(quote ?z)]
            (ps/compile-pattern '[?x :one ?y ?z])))

    (is  (= {:a '(quote ?a)
             :b ['(quote ??b)]
             :c "see"}
            (ps/compile-pattern {:a '?a :b ['??b] :c "see"})))))

(deftest matcher-tests
  (testing "using a new matcher as a predicate works"
    (is (= {'?x 10 '?y {'?x 10}}
           ((m/matcher '?x (m/matcher '?y)) 10))))

  (testing "foreach"
    (let [acc (atom [])]
      (m/foreach '(??pre ??mid $$pre)
                 (fn [x] (swap! acc conj x))
                 [1 2 3 4 5 4 3 2 1])
      (is (= '[{??pre [1], ??mid [2 3 4 5 4 3 2]}
               {??pre [1 2], ??mid [3 4 5 4 3]}
               {??pre [1 2 3], ??mid [4 5 4]}
               {??pre [1 2 3 4], ??mid [5]}]
             @acc)
          "foreach triggers a side effect for each possible match."))

    (let [acc (atom [])]
      (m/foreach (m/segment '??pre)
                 (fn [x tail] (swap! acc conj [x tail]))
                 [1 2 3])
      (is (= '[[{??pre []} [1 2 3]]
               [{??pre [1]} (2 3)]
               [{??pre [1 2]} (3)]
               [{??pre [1 2 3]} nil]]
             @acc)
          "foreach can handle segment matchers too."))))
