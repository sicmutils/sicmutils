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

(ns sicmutils.sicm.ch7-test
  (:refer-clojure :exclude [+ - * / = partial])
  (:require [clojure.test :refer [is deftest testing use-fixtures]]
            [sicmutils.env :as e
             :refer [+ - * / = D I simplify compose
                     literal-function
                     up down
                     sin cos square cube exp]
             #?@(:cljs [:include-macros true])]
            [sicmutils.simplify :refer [pe hermetic-simplify-fixture]]
            [sicmutils.value :refer [within]]))

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
  (let [g (literal-function 'g)]
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
                (simplify ((D H) s)))))))))

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
                 (exp (+ x y)))
             (up (+ (* 2 x) (* 2 y))
                 (+ (* 3 (expt x 2)) (* -6 x y) (* 3 (expt y 2)))
                 (exp (+ x y))))
           (e/freeze
            (simplify ((D g) 'x 'y)))))))
