;
; Copyright (C) 2016 Colin Smith.
; This work is based on the Scmutils system of MIT/GNU Scheme.
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

(ns net.littleredcomputer.math.calculus.derivative-test
  (:refer-clojure :exclude [+ - * / zero?])
  (:require [clojure.test :refer :all]
            [net.littleredcomputer.math
             [function :refer :all]
             [generic :refer :all]
             [complex :refer [complex]]
             [value :as v]
             [numbers]
             [simplify]
             [structure :refer :all]]
            [net.littleredcomputer.math.calculus.derivative :refer :all]))

(def ^:private q
  (up (literal-function 'x)
      (literal-function 'y)
      (literal-function 'z)))

(defn- δ
  [η]
  (fn [f]
    ;; Define g(ε) as in Eq. 1.22; then δ_η f[q] = Dg(0)
    (fn [q]
      (let [g (fn [ε]
                (f (+ q (* ε η))))]
        ((D g) 0)))))

(deftest differentials
  (testing "add, mul differentials"
    (let [zero-differential (make-differential [])
          dx (make-differential {[0] 1})
          -dx (make-differential {[0] -1})
          dy (make-differential {[1] 1})
          dz (make-differential {[2] 1})
          dx-plus-dx (make-differential {[0] 2})
          dxdy (make-differential {[0 1] 1})
          dxdydz (make-differential {[0 1 2] 1})
          dx-plus-dy (make-differential {[0] 1 [1] 1})
          dx-plus-dz (make-differential {[0] 1 [2] 1})
          ]
      (is (= dx-plus-dy (dx+dy dx dy)))
      (is (= dx-plus-dy (dx+dy dy dx)))
      (is (= dx-plus-dz (dx+dy dx dz)))
      (is (= dx-plus-dz (dx+dy dz dx)))
      (is (= dx-plus-dx (dx+dy dx dx)))
      (is (= (make-differential {[0] 3 [1] 2 [2] 3})
             (reduce dx+dy 0 [dx dy dz dy dz dx dz dx])))
      (is (= (make-differential {[] 1 [0] 1}) (dx+dy dx 1)))
      (is (= (make-differential {[] 'k [0] 1}) (dx+dy dx 'k)))
      (is (= zero-differential (dx+dy dx -dx)))
      (is (= zero-differential (dx+dy -dx dx)))
      (is (= zero-differential (dx*dy dx 0)))
      (let [b (dx+dy 0 (dx*dy dx 0))
            c (dx*dy 0 dx)]
        (is (= zero-differential b))
        (is (= zero-differential c))
        (is (= zero-differential (dx+dy b c))))
      (is (= dxdy (dx*dy dx dy)))
      (is (= dxdydz (dx*dy (dx*dy dx dy) dz)))
      (is (= dxdydz (dx*dy (dx*dy dz dx) dy)))
      (is (= dxdydz (dx*dy (dx*dy dy dz) dx)))
      (is (= zero-differential (dx*dy dx dx)))
      (is (= zero-differential (dx*dy dz (dx*dy dy dz))))
      (is (= 0 (* dx dx)))
      ))
  )

(deftest diff-test-1
  (testing "some simple functions"
    (is (= 2 ((D #(* 2 %)) 1)))
    (is (= 2 ((D #(* 2 %)) 'w)))
    (is (= (+ 'z 'z) ((D #(* % %)) 'z)))
    (is (= (* 3 (expt 'y 2))
           ((D #(expt % 3)) 'y)))
    (is (= (* (cos (* 2 'u)) 2) ((D #(sin (* 2 %))) 'u)))
    (is (= (/ 1 (expt (cos 'x) 2)) ((D tan) 'x)))
    (is (= (up 2 (+ 't 't)) ((D #(up (* 2 %) (* % %))) 't)))
    (is (= (up (- (sin 't)) (cos 't)) ((D #(up (cos %) (sin %))) 't)))
    (is (= '(/ 1 (sqrt (+ (* -1 (expt x 2)) 1))) (simplify ((D asin) 'x))))
    (is (= '(/ -1 (sqrt (+ (* -1 (expt x 2)) 1))) (simplify ((D acos) 'x)))))
  (testing "chain rule"
    (let [s (fn [t] (sqrt t))
          u (fn [t] (expt (- (* 3 (s t)) 1) 2/3))
          y (fn [t] (/ (+ (u t) 2) (- (u t) 1)))]
      (is ((v/within 1e-6) (/ -1 18.) ((D y) 9)))))
  (testing "structural-functions"
    (is (= '(up (cos t) (* -1 (sin t))) (simplify ((D (up sin cos)) 't))))))

(deftest partial-diff-test
  (testing "partial derivatives"
    (let [f (fn [x y] (+ (* x x) (* y y)))]
      (is (= 4 (((pd 0) f) 2 3)))
      (is (= 6 (((pd 1) f) 2 3))))
    (let [F (fn [a b]
              (fn [[x y]]
                (up (* a x) (* b y))))]
      (is (= (up 'x 'y) ((F 1 1) (up 'x 'y))))
      (is (= (up (* 2 'x) (* 3 'y)) ((F 2 3) (up 'x 'y))))
      (is (= (up 'x 0)  ((((pd 0) F) 1 1) (up 'x 'y))))
      (is (= (up 0 'y)  ((((pd 1) F) 1 1) (up 'x 'y))))
      (is (= (down (up 'x 0) (up 0 'y)) (((D F) 1 1) (up 'x 'y)))))))

(deftest amazing-bug
  (testing "1"
    (let [f (fn [x]
              (fn [g]
                (fn [y]
                  (g (+ x y)))))
          f-hat ((D f) 3)]
      (is ((v/within 1e-6) 2980.957987 ((f-hat exp) 5)))
      ;; TODO: this is the amazing bug: bbb == 0 is wrong.
      #_(is (= 'bbb ((f-hat (f-hat exp)) 5)))
      )))

(deftest diff-test-2
  (testing "delta-eta-test"
    (with-literal-functions [η q f g]
      (let [I (fn [q] (fn [t] (q t)))
            F (fn [q] (fn [t] (f (q t))))
            G (fn [q] (fn [t] (g (q t))))
            q+εη (+ q (* 'ε η))
            g (fn [ε] (+ q (* ε η)))
            δη (δ η)
            δηI (δη I)
            δηIq (δηI q)
            δηFq ((δη F) q)
            φ (fn [f] (fn [q] (fn [t] ((literal-function 'φ) ((f q) t)))))]
        (is (= '((D f) t) (simplify ((D f) 't))))
        (is (= '(+ (* (η t) ε) (q t)) (simplify (q+εη 't))))
        (is (= '(+ (* (η t) ε) (q t)) (simplify ((g 'ε) 't))))
        (is (= '(η a) (simplify (((D g) 'dt) 'a))))
        (is (= '(η t) (simplify (δηIq 't))))
        (is (= '(f (q t)) (simplify ((F q) 't))))
        (is (= '(* ((D f) (q t)) (η t)) (simplify (δηFq 't))))
        ;; sum rule for variation: δ(F+G) = δF + δG
        (is (= '(+ (* ((D f) (q t)) (η t)) (* (η t) ((D g) (q t)))) (simplify (((δη (+ F G)) q) 't))))
        ;; scalar product rule for variation: δ(cF) = cδF
        (is (= '(* ((D f) (q t)) (η t) c) (simplify (((δη (* 'c F)) q) 't))))
        ;; product rule for variation: δ(FG) = δF G + F δG
        (is (= (simplify (+ (* (((δη F) q) 't) ((G q) 't))
                            (* ((F q) 't) (((δη G) q) 't))))
               (simplify (((δη (* F G)) q) 't))))
        ;; path-independent chain rule for variation
        (is (= '(φ (f (q t))) (simplify (((φ F) q) 't))))
        (is (= '(* ((D φ) (f (q t))) ((D f) (q t)) (η t)) (simplify (((δη (φ F)) q) 't))))))))

(deftest derivatives-as-values
  (let [cs0 (fn [x] (sin (cos x)))
        cs1 (compose sin cos)
        cs2 (comp sin cos)
        y0 (D cs0)
        y1 (D cs1)
        y2 (D cs2)]
    (is (= '(sin (cos x)) (simplify (cs0 'x))))
    (is (= '(sin (cos x)) (simplify (cs1 'x))))
    (is (= '(sin (cos x)) (simplify (cs2 'x))))
    (is (= '(* -1 (cos (cos x)) (sin x)) (simplify ((D cs0) 'x))))
    (is (= '(* -1 (cos (cos x)) (sin x)) (simplify ((D cs1) 'x))))
    (is (= '(* -1 (cos (cos x)) (sin x)) (simplify ((D cs2) 'x))))
    (is (= '(* -1 (cos (cos x)) (sin x)) (simplify (y0 'x))))
    (is (= '(* -1 (cos (cos x)) (sin x)) (simplify (y1 'x))))
    (is (= '(* -1 (cos (cos x)) (sin x)) (simplify (y2 'x)))))
  (let [unity (reduce + (map square [sin cos]))
        dU (D unity)]
    (is (= 1 (simplify (unity 'x))))
    (is (= 0 (simplify (dU 'x)))))
  (let [odear (fn [z] ((D (compose sin cos)) z))]
    (is (= '(* -1 (cos (cos x)) (sin x)) (simplify (odear 'x))))))

(deftest exponentiation-and-composition
  (let [ff (fn [x y z] (+ (* x x y) (* y y z)(* z z x)))
        ]
    (is (= '(down
             (down (* 2 y) (* 2 x) (* 2 z))
             (down (* 2 x) (* 2 z) (* 2 y))
             (down (* 2 z) (* 2 y) (* 2 x)))
           (simplify (((expt D 2) ff) 'x 'y 'z))))
    (is (= (((* D D) ff) 'x 'y 'z) (((expt D 2) ff) 'x 'y 'z)))
    (is (= (((compose D D) ff) 'x 'y 'z) (((expt D 2) ff) 'x 'y 'z)))
    (is (= (((* D D D) ff) 'x 'y 'z) (((expt D 3) ff) 'x 'y 'z)))
    (is (= (((compose D D D) ff) 'x 'y 'z) (((expt D 3) ff) 'x 'y 'z)))
    ;; multiple partial derivatives seem to be broken at present.
    #_(is (= 'foo (simplify (((pd 1 0) ff) 'x 'y 'z))))
    #_(is (= (((pd 0 1) ff 'x 'y 'z))
           (((pd 1) ((pd 0) ff)) 'x 'y 'z)
           ))
    ))

(deftest literal-functions
  (with-literal-functions [f [g [0 0] 0]]
    (testing "R -> R"
      (is (= '((D f) x) (simplify ((D f) 'x))))
      (is (= '((D f) (+ x y)) (simplify ((D f) (+ 'x 'y))))))
    (testing "R^2 -> R"
      (is (= '(((∂ 0) g) x y) (simplify (((pd 0) g) 'x 'y))))
      (is (= '(((∂ 1) g) x y) (simplify (((pd 1) g) 'x 'y))))
      (is (= '(down (((∂ 0) g) x y) (((∂ 1) g) x y))
             (simplify ((D g) 'x 'y)))))))

(deftest complex-derivatives
  (let [i (complex 0 1)
        f (fn [z] (* i (sin (* i z))))]
    (is (= '(* -1 (cosh z))
           (simplify ((D f) 'z))))))
