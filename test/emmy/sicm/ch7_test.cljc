#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.sicm.ch7-test
  (:refer-clojure :exclude [+ - * / = partial])
  (:require [clojure.test :refer [is deftest testing use-fixtures]]
            [emmy.env :as e
             :refer [+ - * = D I simplify compose
                     literal-function
                     up down
                     sin cos square cube exp]]
            [emmy.simplify :refer [hermetic-simplify-fixture]]
            [emmy.value :refer [within]]))

(use-fixtures :each hermetic-simplify-fixture)

(def ^:private near (within 1.0e-6))

(deftest section-1
  (let [h (compose cube sin)
        g (* cube sin)]
    (is (= (h 2) (cube (sin 2))))
    (is (near (h 2) 0.7518269))
    (is (= (g 2) (* (cube 2) (sin 2))))
    (is (near (g 2) 7.2743794))
    (is (= '(expt (sin a) 3) (simplify (h 'a))))
    (is (= 0 (simplify ((- (+ (square sin) (square cos)) 1) 'a))))
    (is (= '(f x) (simplify ((literal-function 'f) 'x))))
    (is (= '(f (g x))
           (simplify ((compose (literal-function 'f) (literal-function 'g)) 'x))))))

(deftest section-2
  (testing "literal functions"
    (is (= '(g x y) (simplify ((literal-function 'g [0 0] 0) 'x 'y)))))

  (testing "structured arguments"
    (let [s (up 't (up 'x 'y) (down 'p_x 'p_y))
          H (literal-function 'H (up 0 (up 0 0) (down 0 0)) 0)]
      (is (= '(H (up t (up x y) (down p_x p_y)))
             (simplify (H s))))
      (is (thrown? #?(:clj IllegalArgumentException :cljs js/Error)
                   (H (up 0 (up 1 2) (down 1 2 3)))))

      (is (thrown? #?(:clj IllegalArgumentException :cljs js/Error)
                   (H (up 0 (up 1) (down 1 2)))))

      (is (thrown? #?(:clj IllegalArgumentException :cljs js/Error)
                   (H (up (up 1 2) (up 1 2) (down 1 2)))))

      (is (= '(down (((partial 0) H) (up t (up x y) (down p_x p_y)))
                    (down (((partial 1 0) H) (up t (up x y) (down p_x p_y)))
                          (((partial 1 1) H) (up t (up x y) (down p_x p_y))))
                    (up (((partial 2 0) H) (up t (up x y) (down p_x p_y)))
                        (((partial 2 1) H) (up t (up x y) (down p_x p_y)))))
             (e/freeze
              (simplify ((D H) s))))))))

(deftest section-3
  (let [derivative-of-sine (D sin)]
    (is (= '(cos x) (simplify (derivative-of-sine 'x)))))

  (is (= '(+ (((expt D 2) f) x) (* -1 (f x)))
         (simplify (((* (- D I) (+ D I)) (literal-function 'f)) 'x)))))

(deftest section-4
  (let [g (fn [x y] (up (square (+ x y)) (cube (- y x)) (exp (+ x y))))]
    (is (= '(down
             (up (+ (* 2 x) (* 2 y))
                 (+ (* -3 (expt x 2)) (* 6 x y) (* -3 (expt y 2)))
                 (* (exp x) (exp y)))
             (up (+ (* 2 x) (* 2 y))
                 (+ (* 3 (expt x 2)) (* -6 x y) (* 3 (expt y 2)))
                 (* (exp x) (exp y))))
           (e/freeze
            (simplify ((D g) 'x 'y)))))))
