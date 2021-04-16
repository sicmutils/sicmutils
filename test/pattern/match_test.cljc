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

(ns pattern.match-test
  (:require [clojure.test :refer [is deftest testing]]
            [pattern.match :as m]))

(defn receive [frame xs] [frame xs])

(deftest matchers
  (testing "match-eq"
    (is (not (m/match (m/match-eq 'a) nil)))
    (is (not (m/match (m/match-eq 'a) [])))
    (is (= [{} nil] ((m/match-eq 'a) {} '(a) receive)))
    (is (= [{} '(b c d e)] ((m/match-eq 'a) {} '(a b c d e) receive)))
    (is (not ((m/match-eq 'a) {} '(e d c b a) receive))))

  (testing "match-element"
    (is (= [{:x 'a} nil] ((m/match-element :x) {} '(a) receive)))
    (is (= [{:x 'a} '(b)] ((m/match-element :x) {} '(a b) receive)))
    (is (= [{:x '(a b)} '(c)] ((m/match-element :x) {} '((a b) c) receive))))

  (testing "match-element-constraint"
    (is (= [{:x 6} nil] ((m/match-element :x integer?) {} '(6) receive)))

    ;; Clojurescript treats floats with no mantissa as integers.
    (let [expected #?(:clj nil
                      :cljs [{:x 6.0} nil])]
      (is (= expected ((m/match-element :x integer?) {} '(6.0) receive))))

    ;; Both languages treat 6.1 as a float.
    (is (= nil ((m/match-element :x integer?) {} '(6.1) receive)))
    (is (= [{:x 6.0} nil] ((m/match-element :x float?) {} '(6.0) receive)))
    (is (= [{:x 6.0} '(7.0)] ((m/match-element :x float?) {} '(6.0 7.0) receive))))

  (testing "match-segment"
    (is (= [[{:x []} '(a b c)]
            [{:x '[a]} '(b c)]
            [{:x '[a b]} '(c)]
            [{:x '[a b c]} nil]]
           (m/all-results-matcher (m/match-segment :x)
                                  '(a b c)
                                  :include-tails? true)))
    (is (= [[{:x []} '(a b c)]
            [{:x '[a]} '(b c)]
            [{:x '[a b]} '(c)]
            [{:x '[a b c]} nil]]
           (m/all-results-matcher (m/match-segment :x)
                                  '[a b c]
                                  :include-tails? true)))
    (is (= {:x []} ((m/match-segment :x) {} '() (fn [frame _] frame))))
    (is (= {:x []} ((m/match-segment :x) {} [] (fn [frame _] frame)))))

  (testing "match-segment-constraint"
    (let [find-two-ints (m/match-list [(m/match-segment :xs)
                                       (m/match-element :i integer?)
                                       (m/match-segment :ys)
                                       (m/match-element :j integer?)
                                       (m/match-segment :zs)])]
      (is (= '[{:i 3 :xs [1.1 [1 3] 2.3] :ys [6.5 x [3 5]] :j 4 :zs [22]}
               {:i 3 :xs [1.1 [1 3] 2.3] :ys [6.5 x [3 5] 4] :j 22 :zs []}
               {:xs [1.1 [1 3] 2.3 3 6.5 x [3 5]] :i 4 :ys [] :j 22 :zs []}]
             (m/all-results-matcher find-two-ints
                                    '((1.1 [1 3] 2.3 3 6.5 x [3 5] 4 22)))))))

  (testing "twin-segments"
    (let [xs-xs (m/match-list [(m/match-segment :x)
                               (m/match-segment :x)])
          xs-xs-etc (m/match-list [(m/match-segment :x)
                                   (m/match-segment :x)
                                   (m/match-segment :y)])
          etc-xs-xs-etc (m/match-list [(m/match-segment :w)
                                       (m/match-segment :x)
                                       (m/match-segment :x)
                                       (m/match-segment :y)])]
      (is (= {:x '[a b c]} (m/match xs-xs '(a b c a b c))))
      (is (not (m/match xs-xs '(a b c a b d))))
      (is (not (m/match xs-xs '(a b c a b c d e))))
      (is (= [{:x [] :y '[a b c a b c d e]}
              {:x '[a b c] :y '[d e]}]
             (m/all-results-matcher xs-xs-etc '((a b c a b c d e)))))
      (is (= [{:x [] :y '(a b a b a b a b)}
              {:x '[a b] :y '[a b a b]}
              {:x '[a b a b] :y '[]}]
             (m/all-results-matcher xs-xs-etc
                                    '((a b a b a b a b)))))
      (is (= '[{:x [], :y [h k h k h k h k]}
               {:x [h k], :y [h k h k]}
               {:x [h k h k], :y []}]
             (m/all-results-matcher xs-xs-etc
                                    '[[h k h k h k h k]])))
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
             (m/all-results-matcher etc-xs-xs-etc '((a b a b a b a b)))))))

  (testing "example-from-6.945-notes"
    (is (= '[{y [b b b b b b] x []}
             {y [b b b b] x [b]}
             {y [b b] x [b b]}
             {y [] x [b b b]}]
           (m/all-results-matcher
            (m/pattern->matcher '(a (:?? x) (:?? y) (:?? x) c))
            '((a b b b b b b c))))))

  (testing "an expression"
    (let [expr (m/match-list [(m/match-list [(m/match-eq '*)
                                             (m/match-element :a)
                                             (m/match-element :c)])
                              (m/match-list [(m/match-eq '*)
                                             (m/match-element :b)
                                             (m/match-element :c)])])]
      (is (= '{:a 3 :b 4 :c x} (m/match expr '((* 3 x) (* 4 x)))))
      (is (not (m/match expr '((* 3 x) (* 4 y))))))))

(deftest match-compiler
  (testing "simple"
    (let [match-x (m/pattern->matcher [:? :x])
          match-xx (m/pattern->matcher [[:? :x] [:? :x]])
          match-xy (m/pattern->matcher [[:? :x] [:? :y]])
          match-x-ys-x (m/pattern->matcher [[:? :x] [:?? :ys] [:? :x]])]
      (is (= '{:x 3} (m/match match-x 3)))
      (is (= '{:x 2} (m/match match-xx [2 2])))
      (is (not (m/match match-xx [2 3])))
      (is (= '{:x 2 :y 3} (m/match match-xy [2 3])))
      (is (= '{:x 2 :ys [3 4 5]} (m/match match-x-ys-x [2 3 4 5 2])))
      (is (not (m/match match-x-ys-x [2 3 4 5 6])))
      (is (not (m/match match-xy [2]))))))

(deftest keyword-variables
  (testing "simple"
    (let [xx (m/pattern->matcher [:x :x])
          xy (m/pattern->matcher [:x :y])
          xs (m/pattern->matcher [:x*])
          xs-xs (m/pattern->matcher [:x* :x*])
          xs-ys (m/pattern->matcher [:x* :y*])]
      (is (= {:x* [1 2 3 4]} (m/match xs [1 2 3 4])))
      (is (= {:x 2} (m/match xx [2 2])))
      (is (= {:x 5 :y 6} (m/match xy [5 6])))
      (is (m/segment? :x*))
      (is (m/segment? :y*))
      (is (= {:x* [1 2]} (m/match xs-xs [1 2 1 2])))
      (is (= {:x* [] :y* [1 2 3 4]} (m/match xs-ys [1 2 3 4])))
      (is (= '[{:x* [], :y* [1 2 3 4]}
               {:x* [1], :y* [2 3 4]}
               {:x* [1 2], :y* [3 4]}
               {:x* [1 2 3], :y* [4]}
               {:x* [1 2 3 4], :y* []}]
             (m/all-results-matcher xs-ys '((1 2 3 4)))))
      (is (= '[{:x* [], :y* [1 2 3 4]}
               {:x* [1], :y* [2 3 4]}
               {:x* [1 2], :y* [3 4]}
               {:x* [1 2 3], :y* [4]}
               {:x* [1 2 3 4], :y* []}]
             (m/all-results-matcher xs-ys [[1 2 3 4]]))))))

(deftest gjs-tests
  ;; these tests come from matcher.scm.
  (testing "element inside of list"
    (is (= [{'b 1} nil]
           ((m/pattern->matcher '(a ((:? b) 2 3) 1 c))
            {}
            '((a (1 2 3) 1 c))
            vector)))

    (is (= [{'b 1} nil]
           ((m/pattern->matcher `(~'a ((:? ~'b ~number?) 2 3) 1 ~'c))
            {}
            '((a (1 2 3) 1 c))
            vector))
        "match element with predicate inside of list")

    (is (not ((m/pattern->matcher `(~'a ((:? ~'b ~symbol?) 2 3) 1 ~'c))
              {}
              '((a (1 2 3) 1 c))
              vector))
        "match element inside list with failing predicate")

    (is (not ((m/pattern->matcher '(a ((:? b) 2 3) (:? b) c))
              {}
              '((a (1 2 3) 2 c))
              vector))
        "match element inside list, but fail the second match outside")

    (is (= [{'b 1} nil]
           ((m/pattern->matcher '(a ((:? b) 2 3) (:? b) c))
            {}
            '((a (1 2 3) 1 c))
            vector))
        "match element inside list, repeated match outside"))

  (testing "segment and reverse segment"
    (letfn [(palindrome? [x]
              ((m/pattern->matcher '((:?? x) (:$$ x)))
               {}
               (list x)
               (fn [x y]
                 (empty? y))))]
      (is (palindrome? '(a b c c b a)))
      (is (not (palindrome? '(a b c c a b)))))))

(deftest new-tests
  (is (= {'x '+, :y 'z}
         (m/match (m/match-list [(m/match-element 'x)])
                  ['+]
                  (fn [m] {:y 'z})))
      "We can add new bindings to the map."))

;; TODO note splice, test it in rules!
