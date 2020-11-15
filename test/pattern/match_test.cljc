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

(defn- receive [frame xs]
  [frame xs])

(defn- collect-all-results
  [matcher input & tails]
  (let [results (atom [])]
    (matcher {} input (fn [frame xs]
                        (let [elem (if tails [frame xs] frame)]
                          (swap! results conj elem))
                        false))
    @results))

(deftest matchers
  (testing "match-one"
    (is (not (m/match (m/match-one 'a) nil)))
    (is (not (m/match (m/match-one 'a) [])))
    (is (= [{} nil] ((m/match-one 'a) {} '(a) receive)))
    (is (= [{} '(b c d e)] ((m/match-one 'a) {} '(a b c d e) receive)))
    (is (not ((m/match-one 'a) {} '(e d c b a) receive))))

  (testing "match-var"
    (is (= [{:x 'a} nil] ((m/match-var :x) {} '(a) receive)))
    (is (= [{:x 'a} '(b)] ((m/match-var :x) {} '(a b) receive)))
    (is (= [{:x '(a b)} '(c)] ((m/match-var :x) {} '((a b) c) receive))))

  (testing "match-var-constraint"
    (is (= [{:x 6} nil] ((m/match-var :x integer?) {} '(6) receive)))

    ;; Clojurescript treats floats with no mantissa as integers.
    (let [expected #?(:clj nil
                      :cljs [{:x 6.0} nil])]
      (is (= expected ((m/match-var :x integer?) {} '(6.0) receive))))

    ;; Both languages treat 6.1 as a float.
    (is (= nil ((m/match-var :x integer?) {} '(6.1) receive)))
    (is (= [{:x 6.0} nil] ((m/match-var :x float?) {} '(6.0) receive)))
    (is (= [{:x 6.0} '(7.0)] ((m/match-var :x float?) {} '(6.0 7.0) receive))))

  (testing "match-segment"
    (is (= [[{:x []} '(a b c)]
            [{:x '[a]} '(b c)]
            [{:x '[a b]} '(c)]
            [{:x '[a b c]} nil]]
           (collect-all-results (m/match-segment :x) '(a b c) true)))
    (is (= [[{:x []} '(a b c)]
            [{:x '[a]} '(b c)]
            [{:x '[a b]} '(c)]
            [{:x '[a b c]} nil]]
           (collect-all-results (m/match-segment :x) '[a b c] true)))
    (is (= {:x []} ((m/match-segment :x) {} '() (fn [frame _] frame))))
    (is (= {:x []} ((m/match-segment :x) {} [] (fn [frame _] frame)))))

  (testing "match-segment-constraint"
    (let [find-two-ints (m/match-list [(m/match-segment :xs)
                                       (m/match-var :i integer?)
                                       (m/match-segment :ys)
                                       (m/match-var :j integer?)
                                       (m/match-segment :zs)])]
      (is (= '[{:i 3 :j 4 :xs [1.1 [1 3] 2.3] :ys [6.5 x [3 5]] :zs [22]}
               {:i 3 :j 22 :xs [1.1 [1 3] 2.3] :ys [6.5 x [3 5] 4] :zs []}
               {:i 4 :j 22 :xs [1.1 [1 3] 2.3 3 6.5 x [3 5]] :ys [] :zs []}]
             (collect-all-results find-two-ints
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
             (collect-all-results xs-xs-etc '((a b c a b c d e)))))
      (is (= [{:x [] :y '(a b a b a b a b)}
              {:x '[a b] :y '[a b a b]}
              {:x '[a b a b] :y '[]}]
             (collect-all-results xs-xs-etc
                                  '((a b a b a b a b)))))
      (is (= '[{:x [], :y [h k h k h k h k]}
               {:x [h k], :y [h k h k]}
               {:x [h k h k], :y []}]
             (collect-all-results xs-xs-etc
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
             (collect-all-results etc-xs-xs-etc '((a b a b a b a b)))))))

  (testing "example-from-6.945-notes"
    (is (= '[{y [b b b b b b] x []}
             {y [b b b b] x [b]}
             {y [b b] x [b b]}
             {y [] x [b b b]}]
           (collect-all-results
            (m/pattern->matcher '(a (:?? x) (:?? y) (:?? x) c))
            '((a b b b b b b c))))))

  (testing "an expression"
    (let [expr (m/match-list [(m/match-list [(m/match-one '*)
                                             (m/match-var :a)
                                             (m/match-var :c)])
                              (m/match-list [(m/match-one '*)
                                             (m/match-var :b)
                                             (m/match-var :c)])])]
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
      (is (m/segment-reference? :x*))
      (is (m/segment-reference? :y*))
      (is (= {:x* [1 2]} (m/match xs-xs [1 2 1 2])))
      (is (= {:x* [] :y* [1 2 3 4]} (m/match xs-ys [1 2 3 4])))
      (is (= '[{:x* [], :y* [1 2 3 4]}
               {:x* [1], :y* [2 3 4]}
               {:x* [1 2], :y* [3 4]}
               {:x* [1 2 3], :y* [4]}
               {:x* [1 2 3 4], :y* []}]
             (collect-all-results xs-ys '((1 2 3 4)))))
      (is (= '[{:x* [], :y* [1 2 3 4]}
               {:x* [1], :y* [2 3 4]}
               {:x* [1 2], :y* [3 4]}
               {:x* [1 2 3], :y* [4]}
               {:x* [1 2 3 4], :y* []}]
             (collect-all-results xs-ys [[1 2 3 4]]))))))

(comment
  ((match:->combinators '(a ((? b) 2 3) 1 c))
   '((a (1 2 3) 1 c))
   '()
   (lambda (x y) `(succeed ,x ,y)))
  ;;Value: (succeed ((b . 1)) ())

  ((match:->combinators `(a ((? b ,number?) 2 3) 1 c))
   '((a (1 2 3) 1 c))
   '()
   (lambda (x y) `(succeed ,x ,y)))
  ;;Value: (succeed ((b . 1)) ())

  ((match:->combinators `(a ((? b ,symbol?) 2 3) 1 c))
   '((a (1 2 3) 1 c))
   '()
   (lambda (x y) `(succeed ,x ,y)))
  ;;Value: #f

  ((match:->combinators '(a ((? b) 2 3) (? b) c))
   '((a (1 2 3) 2 c))
   '()
   (lambda (x y) `(succeed ,x ,y)))
  ;;Value: #f

  ((match:->combinators '(a ((? b) 2 3) (? b) c))
   '((a (1 2 3) 1 c))
   '()
   (lambda (x y) `(succeed ,x ,y)))
  ;;Value: (succeed ((b . 1)) ())

  ((match:->combinators '(a (?? x) (?? y) (?? x) c))
   '((a b b b b b b c))
   '()
   (lambda (x y)
           (pp `(succeed ,x ,y))
           false))

  (succeed ((y . #((b b b b b b c) (c))) (x . #((b b b b b b c) (b b b b b b c)))) ())
  (succeed ((y . #((b b b b b c) (b c))) (x . #((b b b b b b c) (b b b b b c)))) ())
  (succeed ((y . #((b b b b c) (b b c))) (x . #((b b b b b b c) (b b b b c)))) ())
  (succeed ((y . #((b b b c) (b b b c))) (x . #((b b b b b b c) (b b b c)))) ())
  ;;Value: #f

  (define (palindrome? x)
    ((match:->combinators '((?? x) ($$ x)))
     (list x) '() (lambda (x y) (null? y))))
  ;;Value: palindrome?

  (palindrome? '(a b c c b a))
  ;;Value: #t

  (palindrome? '(a b c c a b))
  ;;Value: #f
  )
