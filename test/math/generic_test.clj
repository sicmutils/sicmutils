(ns math.generic-test
  (:require [clojure.test :refer :all]
            [math.generic :refer :all]))


(def T1 (dtree-insert
         (dtree-insert
          (dtree-insert empty-dtree :op1 [:p1 :p2])
          :op2 [:p2 :p1])
         :op3 [:p2 :p1 :p3]))

(deftest dtree-1
  (testing "lookup"
    (is (= :op1 (dtree-lookup T1 [#{:p1} #{:p2}])))
    (is (= :op2 (dtree-lookup T1 [#{:p2} #{:p1}])))
    (is (= :op3 (dtree-lookup T1 [#{:p2} #{:p1} #{:p3}])))
    (is (not (dtree-lookup T1 [#{:p2} #{:p1} #{:p3} #{:p1}])))
    (is (not (dtree-lookup T1 [])))
    (is (not (dtree-lookup T1 [#{:p2}#{:p2}])))))

(defhandler '+ [integer? integer?] :intint)
(defhandler '+ [integer? float?]   :intfloat)
(defhandler '+ [string? string?]   :strstr)
(defhandler 'z [number? string?]   :numstr)

(deftest handler-map
  (testing "lookup"
    (is (= :intint (findhandler '+ [1 1])))
    (is (not (findhandler '+ [1 "2"])))
    (is (= :intfloat (findhandler '+ [1 3.13])))
    (is (= :strstr (findhandler '+ ["foo" "bar"])))
    (is (= :numstr (findhandler 'z [1e10, "baz"])))
    (is (not (findhandler '+ [1e10, "baz"])))))

(defn multiply-string
  [n s]
  (let [sb (StringBuilder.)]
    (dotimes [_ n]
      (.append sb s))
    (.toString sb)))

(defhandler '* [number? string?] multiply-string)
(def star (make-operation '*))

(deftest handler-fn
  (testing "multiply-string"
    (is (= "foofoofoo") (multiply-string 3 "foo"))
    (is (= "") (multiply-string 0 "bar"))
    (is (= "") (multiply-string -2 "bar"))
    (is (= "barbarbar" (let [args [3 "bar"]
                             h (findhandler '* args)]
                         (apply h args)))))
  (testing "star"
    (is (= "bazbaz" (star 2 "baz")))
    (is (= "quxquxqux" (star 3 "qux")))
    (is (thrown? IllegalArgumentException
                 (star "qux" 3)))
    ))

