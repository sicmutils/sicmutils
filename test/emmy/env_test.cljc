#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.env-test
  (:refer-clojure :exclude [+ - * / zero? partial ref])
  (:require [clojure.test :refer [is deftest testing]]
            [same :refer [ish?] :include-macros true]
            [emmy.complex :as c]
            [emmy.env :as e :refer [+ * /  partial ref
                                    complex
                                    freeze
                                    simplify
                                    literal-function
                                    orientation up down
                                    ->infix
                                    cross-product
                                    cot csc sec]]
            [emmy.matrix :as matrix]
            [emmy.operator :as o]
            [emmy.value :as v]))

(deftest constant-tests
  (is (ish? e/euler (e/exp 1))
      "not quite equal, but close."))

(deftest partial-shim
  (testing "partial also works the way Clojure defines it"
    (is (= 10 ((partial + 9) 1)))
    (is (= 5 ((partial max 4) 5)))
    (is (= 4 ((partial max 4) 2)))))

(deftest ref-shim
  (testing "works for structures"
    (is (= 2 (ref (up 1 2 3) 1)))
    (is (= 3 (ref (down (up 1 2) (up 3 4)) 1 0))))

  (testing "works for functions"
    (let [f (fn [x] [[(dec x)] [x] [(inc x)]])]
      (is (= 2 ((ref f 2 0) 1)))
      (is (= 2 (((e/component 2 0) f) 1)))))

  (testing "works for operators"
    (let [op (-> (fn [x] [[(dec x)] [x] [(inc x)]])
                 (o/make-operator 'f))]
      (is (= 2 ((ref op 2 0) 1)))
      (is (= 2 (((e/component 2 0) op) 1)))))

  #?(:cljs
     (testing "ref acts as id in cljs"
       (is (= [] (ref []))))

     :clj
     ;; ClojureScript doesn't have refs.
     (testing "works clojure-style"
       (let [r (ref [])
             s (ref {} :meta {:a "apple"})]
         (is (= [] @r))
         (is (= [99] (dosync (alter r conj 99))))
         (is (= {:b 88} (dosync (alter s assoc :b 88))))))))

(deftest tex-tests
  (testing "inline and block tex environments"
    (is (= "$x + y$" (e/tex$ (+ 'x 'y))))
    (is (= "$$x + y$$" (e/tex$$ (+ 'x 'y)))))

  (testing "tex equation environment"
    (is (= (str "\\begin{equation}\n"
                "x + y\n"
                "\\end{equation}")
           (e/->tex-equation (+ 'x 'y))))

    (is (= (str "\\begin{equation}\n"
                "\\label{face}\n"
                "x + y\n"
                "\\end{equation}")
           (e/->tex-equation (+ 'x 'y) :label "face")))))

(deftest literal-function-shim
  (testing "works for signatures"
    (let [f1 (literal-function 'f)
          f2 (literal-function 'f (-> Real Real))
          f3 (literal-function 'f (-> Real (UP Real Real)))
          f4 (literal-function 'f [0] (up 1 2))
          f5 (literal-function 'f (-> (DOWN Real Real) (X Real Real)))]
      (is (v/= '(f x) (simplify (f1 'x))))
      (is (v/= '(f x) (simplify (f2 'x))))

      (is (= '(up (f↑0 x) (f↑1 x))
             (freeze
              (simplify (f3 'x)))))

      (is (= '(up (f↑0 x) (f↑1 x))
             (freeze
              (simplify (f4 'x)))))

      (is (= '(up (f↑0 (down p_x p_y)) (f↑1 (down p_x p_y)))
             (freeze
              (simplify (f5 (down 'p_x 'p_y)))))))))

(deftest shortcuts
  (testing "env aliases alias the actual object from the original namespace"
    (is (= matrix/by-rows
           e/matrix-by-rows)))

  #?(:clj
     (testing "aliases keep metadata from original var. Only works in Clojure."
       (let [ks [:doc :file :ns :line :column :arglists]]
         (is (= (select-keys (meta #'matrix/by-rows) ks)
                (select-keys (meta #'e/matrix-by-rows) ks))))))

  (testing "cot"
    (is (v/= '(/ (cos x) (sin x)) (simplify (cot 'x))))
    (is (v/= '(/ 1 (sin x)) (simplify (csc 'x))))
    (is (v/= '(/ 1 (cos x)) (simplify (sec 'x))))
    (is (= (c/complex 1 2) (complex 1 2)))
    (is (= :emmy.structure/up (orientation (up 1 2))))
    (is (= "up(b z - c y, - a z + c x, a y - b x)"
           (->infix (simplify (cross-product (up 'a 'b 'c) (up 'x 'y 'z))))))))

(deftest matrices
  (testing "qp-submatrix"
    (let [A (e/matrix-by-rows [1 2 3]
                              [4 5 6]
                              [7 8 9])]
      (is (= (e/matrix-by-rows [5 6]
                               [8 9])
             (e/qp-submatrix A)))
      (is (= 3 (e/dimension A))))))

(deftest pe
  (is (re-matches
       #"\(\* 2 x\)\r?\n"
       (with-out-str
         (e/print-expression (+ 'x 'x))))))

(deftest pv
  (let [π Math/PI
        zero-to-two-pi (e/principal-value (* 2 π))
        minus-pi-to-pi (e/principal-value π)]
    (is (= (* (/ 1 2) π) (zero-to-two-pi (* (/ 1 2) π))))
    (is (= 0.0 (zero-to-two-pi 0.0)))
    (is (= (* (/ 3 2) π) (zero-to-two-pi (* (/ 3 2) π))))
    (is (= (* (/ 3 2) π) (zero-to-two-pi (* (/ -1 2) π))))
    (is (= (* (/ 1 2) π) (zero-to-two-pi (* (/ -3 2) π))))
    (is (= 0.0 (zero-to-two-pi (* 2 π))))
    (is (= (* (/ 1 2) π) (minus-pi-to-pi (* (/ 1 2) π))))
    (is (= 0.0 (minus-pi-to-pi 0.0)))
    (is (= (* (/ -1 2) π) (minus-pi-to-pi (* (/ 3 2) π))))
    (is (= (* (/ -1 2) π) (minus-pi-to-pi (* (/ -1 2) π))))
    (is (= (* (/ 1 2) π) (minus-pi-to-pi (* (/ -3 2) π))))
    (is (= 0.0 (zero-to-two-pi (* 2 π))))))
