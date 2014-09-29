(ns pattern.match-test
  (:require [clojure.test :refer :all]
            [pattern.match :refer :all]))

(defn- receive [frame xs] [frame xs])
(defn- collect-all-results [matcher input]
  (let [results (atom [])]
    (matcher input {} (fn [frame xs]
                        (swap! results conj [frame xs])
                        false))
    @results))

(deftest matchers
  (testing "match-one"
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
           (collect-all-results (match-segment :x) '(a b c))))
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
      (is (= [[{:x [] :y '[a b c a b c d e]} ()]
              [{:x '[a b c] :y '[d e]} ()]]
             (collect-all-results twin-segments-etc '(a b c a b c d e))))
      (is (= [[{:x [] :y '[a b a b a b a b]} ()]
              [{:x '[a b] :y '[a b a b]} ()]
              [{:x '[a b a b] :y '[]} ()]]
             (collect-all-results twin-segments-etc
                                  '(a b a b a b a b))))
      (is (= [[{:y '[a b a b a b a b], :x [], :w []} ()]
              [{:y '[a b a b], :x '[a b], :w []} ()]
              [{:y [], :x '[a b a b], :w []} ()]
              [{:y '[b a b a b a b], :x [], :w '[a]} ()]
              [{:y '[b a b], :x '[b a], :w '[a]} ()]
              [{:y '[a b a b a b], :x [], :w '[a b]} ()]
              [{:y '[a b], :x '[a b], :w '[a b]} ()]
              [{:y '[b a b a b], :x [], :w '[a b a]} ()]
              [{:y '[b], :x '[b a], :w '[a b a]} ()]
              [{:y '[a b a b], :x [], :w '[a b a b]} ()]
              [{:y [], :x '[a b], :w '[a b a b]} ()]
              [{:y '[b a b], :x [], :w '[a b a b a]} ()]
              [{:y '[a b], :x [], :w '[a b a b a b]} ()]
              [{:y '[b], :x [], :w '[a b a b a b a]} ()]
              [{:y [], :x [], :w '[a b a b a b a b]} ()]]
             (collect-all-results etc-twin-segments-etc '(a b a b a b a b))))
      ))
  ;; XXX redo this one once we have the pattern compiler implemented.
  ;; XXX interesting to note: difference between nil and () in this
  ;; example vs. the others.
  (testing "example-from-6.945-notes"
    (is (= [[{:y '[b b b b b b] :x []} nil]
            [{:y '[b b b b] :x '[b]} nil]
            [{:y '[b b] :x '[b b]} nil]
            [{:y [] :x '[b b b]} nil]]
           (collect-all-results (match-list (match-one 'a)
                                            (match-segment :x)
                                            (match-segment :y)
                                            (match-segment :x)
                                            (match-one 'c))
                                '(a b b b b b b c))))
    )
  )

