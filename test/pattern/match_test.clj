(ns pattern.match-test
  (:require [clojure.test :refer :all]
            [pattern.match :refer :all]))

(defn- test-match [matcher data]
  (let [receive (fn [frame data] (if (empty? data) frame))]
    (matcher {} (list data) receive)))

(defn- receive [frame xs] [frame xs])
(defn- collect-all-results [matcher input & tails] 
  (let [results (atom [])]
    (matcher (sorted-map) input (fn [frame xs]
                        (swap! results conj
                               (if tails [frame xs] frame))
                        false))
    @results))

(deftest matchers
  (testing "match-one"
    (is (not (test-match (match-one 'a) nil)))
    (is (not (test-match (match-one 'a) [])))
    (is (= [{} nil] ((match-one 'a) {} '(a) receive)))
    (is (= [{} '(b c d e)] ((match-one 'a) {} '(a b c d e) receive)))
    (is (not  ((match-one 'a) {} '(e d c b a) receive)))
    )
  (testing "match-var"
    (is (= [{:x 'a} nil] ((match-var :x) {} '(a) receive)))
    (is (= [{:x 'a} '(b)] ((match-var :x) {} '(a b) receive)))
    (is (= [{:x '(a b)} '(c)] ((match-var :x) {} '((a b) c) receive)))
    )
  (testing "match-segment"
    (is (= [[{:x []} '(a b c)]
            [{:x '[a]} '(b c)]
            [{:x '[a b]} '(c)]
            [{:x '[a b c]} ()]]
           (collect-all-results (match-segment :x) '(a b c) true)))
    )
  (testing "twin-segments"
    (let [twin-segments (match-list (match-segment :x)
                                    (match-segment :x))
          twin-segments-etc (match-list (match-segment :x)
                                        (match-segment :x)
                                        (match-segment :y))
          etc-twin-segments-etc (match-list (match-segment :w)
                                            (match-segment :x)
                                            (match-segment :x)
                                            (match-segment :y))]
      (is (= {:x '[a b c]} (test-match twin-segments '(a b c a b c))))
      (is (not (test-match twin-segments '(a b c a b d))))
      (is (not (test-match twin-segments '(a b c a b c d e))))
      (is (= [{:x [] :y '[a b c a b c d e]}
              {:x '[a b c] :y '[d e]} ]
             (collect-all-results twin-segments-etc '((a b c a b c d e)))))
      (is (= [{:x [] :y '[a b a b a b a b]}
              {:x '[a b] :y '[a b a b]}
              {:x '[a b a b] :y '[]}]
             (collect-all-results twin-segments-etc
                                  '((a b a b a b a b)))))
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
             (collect-all-results etc-twin-segments-etc '((a b a b a b a b)))))
      ))
  ;; XXX redo this one once we have the pattern compiler implemented.
  (testing "example-from-6.945-notes"
    (is (= '[{:y [b b b b b b] :x []} 
             {:y [b b b b] :x [b]} 
             {:y [b b] :x [b b]} 
             {:y [] :x [b b b]}]
           (collect-all-results (match-list (match-one 'a)
                                            (match-segment :x)
                                            (match-segment :y)
                                            (match-segment :x)
                                            (match-one 'c))
                                '((a b b b b b b c)))))
    )
  (testing "an expression"
    (let [expr (match-list (match-list (match-one '*)
                                       (match-var :a)
                                       (match-var :c))
                           (match-list (match-one '*)
                                       (match-var :b)
                                       (match-var :c)))]
      (is (= '{:a 3 :b 4 :c x} (test-match expr '((* 3 x) (* 4 x)))))
      (is (not (test-match expr '((* 3 x) (* 4 y)))))
      )
    )
  )

(deftest match-compiler
  (testing "simple"
    (let [match-x (pattern->matcher [:? :x])
          match-xx (pattern->matcher [[:? :x] [:? :x]])
          match-xy (pattern->matcher [[:? :x] [:? :y]])
          match-x-ys-x (pattern->matcher [[:? :x] [:?? :ys] [:? :x]])]
      (is (= '{:x 3} (test-match match-x 3)))
      (is (= '{:x 2} (test-match match-xx [2 2])))
      (is (not (test-match match-xx [2 3])))
      (is (= '{:x 2 :y 3} (test-match match-xy [2 3])))
      (is (= '{:x 2 :ys [3 4 5]} (test-match match-x-ys-x [2 3 4 5 2])))
      (is (not (test-match match-x-ys-x [2 3 4 5 6]))))
    ))

