;;
;; Copyright © 2021 Sam Ritchie.
;; This work is based on the Scmutils system of MIT/GNU Scheme:
;; Copyright © 2002 Massachusetts Institute of Technology
;;
;; This is free software;  you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or (at
;; your option) any later version.
;;
;; This software is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this code; if not, see <http://www.gnu.org/licenses/>.
;;

(ns pattern.match-test
  (:require [clojure.test :refer [is deftest testing]]
            [pattern.match :as m]
            [pattern.syntax :as ps]))

(deftest matchers
  (testing "eq"
    (is (m/failed? (m/match (m/eq 'a) nil)))
    (is (m/failed? (m/match (m/eq 'a) [])))

    (is (= {} ((m/eq 'a) {} 'a identity))
        "A successful match returns the bindings in play, NOT something falsey.")

    (is (not ((m/eq 'a) {} '(e d c b a) identity))
        "This does not match a sequence."))

  (testing "element"
    (is (= {:x 'a} ((m/bind :x) {} 'a identity)))
    (is (= {:x '(a b)} ((m/bind :x) {} '(a b) identity))))

  (testing "element-constraint"
    (is (= {:x 6} ((m/bind :x integer?) {} 6 identity)))

    ;; Clojurescript treats floats with no mantissa as integers.
    (let [expected #?(:clj nil
                      :cljs {:x 6.0})]
      (is (= expected ((m/bind :x integer?) {} 6.0 identity))))

    ;; Both languages treat 6.1 as a float.
    (is (nil? ((m/bind :x integer?) {} 6.1 identity)))
    (is (= {:x 6.0} ((m/bind :x float?) {} 6.0 identity))))

  (testing "segment"
    (is (= '[[{:x []} [a b c]]
             [{:x [a]} (b c)]
             [{:x [a b]} (c)]
             [{:x [a b c]} nil]]
           (m/all-results (m/segment :x) '[a b c])))

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

    (is (= {:x []}
           ((m/segment :x) {} '() (fn [frame _] frame))))

    (is (= {:x []}
           ((m/segment :x) {} [] (fn [frame _] frame)))))

  (testing "segment-constraint"
    (let [find-two-ints (m/sequence
                         (m/segment :xs)
                         (m/bind :i integer?)
                         (m/segment :ys)
                         (m/bind :j integer?)
                         (m/segment :zs))]
      (is (= '[{:i 3 :xs [1.1 [1 3] 2.3] :ys [6.5 x [3 5]] :j 4 :zs [22]}
               {:i 3 :xs [1.1 [1 3] 2.3] :ys [6.5 x [3 5] 4] :j 22 :zs []}
               {:xs [1.1 [1 3] 2.3 3 6.5 x [3 5]] :i 4 :ys [] :j 22 :zs []}]
             (m/all-results
              find-two-ints
              '(1.1 [1 3] 2.3 3 6.5 x [3 5] 4 22))))))

  (testing "twin-segments"
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
      (is (= {:x '[a b c]} (m/match xs-xs '(a b c a b c))))
      (is (m/failed? (m/match xs-xs '(a b c a b d))))
      (is (m/failed? (m/match xs-xs '(a b c a b c d e))))
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
            '(a (:?? x) (:?? y) (:?? x) c)
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
  (testing "match-if"
    (let [m (m/match-if odd? '?odd '?even)]
      (is (= {'?odd 11} (m {} 11 identity)))
      (is (= {'?even 12} (m {} 12 identity))))))

(deftest match-compiler
  (testing "simple"
    (let [match-x [:? :x]
          match-xx [[:? :x] [:? :x]]
          match-xy [[:? :x] [:? :y]]
          match-x-ys-x [[:? :x] [:?? :ys] [:? :x]]]
      (is (= '{:x 3} (m/match match-x 3)))
      (is (= '{:x 2} (m/match match-xx [2 2])))
      (is (m/failed? (m/match match-xx [2 3])))
      (is (= '{:x 2 :y 3} (m/match match-xy [2 3])))
      (is (= '{:x 2 :ys [3 4 5]} (m/match match-x-ys-x [2 3 4 5 2])))
      (is (m/failed? (m/match match-x-ys-x [2 3 4 5 6])))
      (is (m/failed? (m/match match-xy [2]))))))

(deftest keyword-variables
  (testing "simple"
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
             ((m/all-results-matcher xs-ys) [1 2 3 4]))))))

(deftest gjs-tests
  ;; these tests come from matcher.scm.
  (testing "element inside of list"
    (is (= {'b 1}
           (m/match '(a ((:? b) 2 3) 1 c)
                    '(a (1 2 3) 1 c))))

    (is (= {'b 1}
           (m/match `(~'a ((:? ~'b ~number?) 2 3) 1 ~'c)
                    '(a (1 2 3) 1 c)))
        "match element with predicate inside of list")

    (is (m/failed?
         (m/match `(~'a ((:? ~'b ~symbol?) 2 3) 1 ~'c)
                  '(a (1 2 3) 1 c)))
        "match element inside list with failing predicate")

    (is (m/failed?
         (m/match '(a ((:? b) 2 3) (:? b) c)
                  '(a (1 2 3) 2 c)))
        "match element inside list, but fail the second match outside")

    (is (= {'b 1}
           (m/match '(a ((:? b) 2 3) (:? b) c)
                    '(a (1 2 3) 1 c)))
        "match element inside list, repeated match outside"))

  (testing "segment and reverse segment"
    (letfn [(palindrome? [x]
              ((m/pattern->combinators '((:?? x) (:$$ x)))
               {}
               x
               boolean))]
      (is (palindrome? '(a b c c b a)))
      (is (not (palindrome? '(a b c c a b)))))))

(deftest new-tests
  (testing "using a new matcher as a predicate works"
    (is (= {'?x 10 '?y {'?x 10}}
           ((m/matcher '?x (m/matcher '?y)) 10))))

  (let [match (m/matcher '(+ (:? _ #{11}) ?b))]
    (is (= {'?b 12} (match '(+ 11 12))))
    (is (m/failed?
         (match '(+ 12 11)))))

  (let [match (m/matcher (m/or [:? 'x #{11}]
                               (m/not [:? 'x odd?])))]
    (is (= {'x 11} (match 11)))
    (is (= {} (match 12))))

  (let [match (m/matcher
               (m/match-if odd? '?x '?y))]
    (is (= {'?x 9} (match 9)))
    (is (= {'?y 10} (match 10))))

  (testing "match/or"
    (let [match (m/or (m/predicate odd?)
                      (m/predicate #{12}))]
      (is (= {} (match {} 12 identity)))
      (is (= {} (match {} 11 identity)))
      (is (not (match {} 8 identity)))))

  (testing "`and` must match all and returns the frame from the final."
    (let [match (m/and (m/predicate even?)
                       (m/bind :x)
                       (m/predicate #{12}))]
      (is (= {:x 12} (match {} 12 identity)))
      (is (not (match {} 8 identity)))))

  (is (= {'x '+, :y 'z}
         (m/match (m/sequence (m/bind 'x))
                  (fn [m] {:y 'z})
                  ['+]))
      "We can add new bindings to the map.")

  (testing "match-empty-list"
    (is (= [{}]
           ((m/all-results-matcher ()) ()))
        "empty dict!")

    (is (= []
           ((m/all-results-matcher ())
            '(foo)))
        "no match."))

  (is (= [{'stuff '(0 1 2 3 4 5 6 7 8 9)}]
         ((m/all-results-matcher '(and (:?? stuff)))
          (cons 'and (range 10))))
      "segments at the end of a list run in constant time, except building the
        test list"))
