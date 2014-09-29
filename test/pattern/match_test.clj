(ns pattern.match-test
  (:require [clojure.test :refer :all]
            [pattern.match :refer :all]))

(defn- receive [frame xs] [frame xs])
(defn- collect-all-results [matcher input & tails] 
  (let [results (atom [])]
    (matcher input {} (fn [frame xs]
                        (swap! results conj
                               (if tails [frame xs] frame))
                        false))
    @results))

(deftest matchers
  (testing "match-one"
    (is (= false ((match-one 'a) nil {} receive)))
    (is (= false ((match-one 'a) [] {} receive)))
    (is (= [{} nil] ((match-one 'a) '(a) {} receive)))
    (is (= [{} '(b c d e)] ((match-one 'a) '(a b c d e) {} receive)))
    (is (= false ((match-one 'a) '(e d c b a) {} receive)))
    )
  (testing "match-var"
    (is (= [{:x 'a} nil] ((match-var :x) '(a) {} receive)))
    (is (= [{:x 'a} '(b)] ((match-var :x) '(a b) {} receive)))
    (is (= [{:x '(a b)} '(c)] ((match-var :x) '((a b) c) {} receive)))
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
      (is (= [{:x '[a b c]} ()] (twin-segments '(a b c a b c) {} receive)))
      (is (= nil (twin-segments '(a b c a b d) {} receive)))
      (is (= nil (twin-segments '(a b c a b c d e) {} receive)))
      (is (= [{:x [] :y '[a b c a b c d e]}
              {:x '[a b c] :y '[d e]} ]
             (collect-all-results twin-segments-etc '(a b c a b c d e))))
      (is (= [{:x [] :y '[a b a b a b a b]}
              {:x '[a b] :y '[a b a b]}
              {:x '[a b a b] :y '[]}]
             (collect-all-results twin-segments-etc
                                  '(a b a b a b a b))))
      (is (= [{:y '[a b a b a b a b], :x [], :w []}
              {:y '[a b a b], :x '[a b], :w []}
              {:y [], :x '[a b a b], :w []}
              {:y '[b a b a b a b], :x [], :w '[a]}
              {:y '[b a b], :x '[b a], :w '[a]}
              {:y '[a b a b a b], :x [], :w '[a b]}
              {:y '[a b], :x '[a b], :w '[a b]}
              {:y '[b a b a b], :x [], :w '[a b a]}
              {:y '[b], :x '[b a], :w '[a b a]}
              {:y '[a b a b], :x [], :w '[a b a b]}
              {:y [], :x '[a b], :w '[a b a b]}
              {:y '[b a b], :x [], :w '[a b a b a]}
              {:y '[a b], :x [], :w '[a b a b a b]}
              {:y '[b], :x [], :w '[a b a b a b a]}
              {:y [], :x [], :w '[a b a b a b a b]}]
             (collect-all-results etc-twin-segments-etc '(a b a b a b a b))))
      ))
  ;; XXX redo this one once we have the pattern compiler implemented.
  ;; XXX interesting to note: difference between nil and () in this
  ;; example vs. the others.
  (testing "example-from-6.945-notes"
    (is (= [{:y '[b b b b b b] :x []} 
            {:y '[b b b b] :x '[b]} 
            {:y '[b b] :x '[b b]} 
            {:y [] :x '[b b b]}]
           (collect-all-results (match-list (match-one 'a)
                                            (match-segment :x)
                                            (match-segment :y)
                                            (match-segment :x)
                                            (match-one 'c))
                                '(a b b b b b b c))))
    )
  )

(deftest monadic-matchers
  (testing "match-one-monadic"
    (is (= [{} nil] ((match-one-monadic :a) {} [:a])))
    (is (= [{} [:b]] ((match-one-monadic :a) {} [:a :b])))
    (is (= [{} nil] ((match-one-monadic :a) {} [:c :b])))
    (is (= [{} nil] ((match-one-monadic :a) {} [])))
    (is (= [{} nil] ((match-one-monadic :a) {} nil)))
    )
  (testing "match-var-monadic"
    (is (= [{:x :a} nil] ((match-var-monadic :x) {} [:a])))
    (is (= [{:x :b} [:a]] ((match-var-monadic :x) {} [:b :a])))
    (is (= [{:x :b} [:a]] ((match-var-monadic :x) {:x :b} [:b :a])))
    (is (= [{} nil] ((match-var-monadic :x) {:x :a} [:b :a])))
    )
  (testing "match-segment-monadic"
    )
  )
