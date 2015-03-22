;; Copyright (C) 2015 Colin Smith.
;; This work is based on the Scmutils system of MIT/GNU Scheme.
;;
;; This is free software;  you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or (at
;; your option) any later version.

;; This software is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this code; if not, see <http://www.gnu.org/licenses/>.

(ns math.calculus.derivative-test
  (:refer-clojure :exclude [+ - * / zero?])
  (:require [clojure.test :refer :all]
            [math.calculus.derivative :refer :all]
            [math.function :refer :all]
            [math.generic :refer :all]
            [math.value :as v]
            [math.numbers]
            [math.expression :refer [print-expression] :rename {print-expression pe}]
            [math.structure :refer :all]))

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
    )
  (testing "chain rule"
    (let [s (fn [t] (sqrt t))
          u (fn [t] (expt (- (* 3 (s t)) 1) 2/3))
          y (fn [t] (/ (+ (u t) 2) (- (u t) 1)))]
      (is ((v/within 1e-6) (/ -1 18.) ((D y) 9)))))
  (testing "structural-functions"
    (is (= '(up (cos t) (- (sin t))) (pe ((D (up sin cos)) 't))))))

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

(deftest diff-test-2
  (testing "delta-eta-test"
    (let [η (literal-function 'η)
          q (literal-function 'q)
          I (fn [q] (fn [t] (q t)))
          f (literal-function 'f)
          g (literal-function 'g)
          F (fn [q] (fn [t] (f (q t))))
          G (fn [q] (fn [t] (g (q t))))
          q+εη (+ q (* 'ε η))
          g (fn [ε] (+ q (* ε η)))
          δη (δ η)
          δηI (δη I)
          δηIq (δηI q)
          δηFq ((δη F) q)
          φ (fn [f] (fn [q] (fn [t] ((literal-function 'φ) ((f q) t)))))]
      (is (= '((D f) t) (pe ((D f) 't))))
      (is (= '(+ (q t) (* ε (η t))) (pe (q+εη 't))))
      (is (= '(+ (q t) (* ε (η t))) (pe ((g 'ε) 't))))
      (is (= '(η a) (pe (((D g) 'dt) 'a))))
      (is (= '(η t) (pe (δηIq 't))))
      (is (= '(f (q t)) (pe ((F q) 't))))
      (is (= '(* ((D f) (q t)) (η t)) (pe (δηFq 't))))
      ;; sum rule for variation: δ(F+G) = δF + δG
      (is (= '(+ (* ((D f) (q t)) (η t)) (* ((D g) (q t)) (η t))) (pe (((δη (+ F G)) q) 't))))
      ;; scalar product rule for variation: δ(cF) = cδF
      (is (= '(* c ((D f) (q t)) (η t)) (pe (((δη (* 'c F)) q) 't))))
      ;; product rule for variation: δ(FG) = δF G + F δG
      (is (= (pe (+ (* (((δη F) q) 't) ((G q) 't))
                                   (* ((F q) 't) (((δη G) q) 't))))
             (pe (((δη (* F G)) q) 't))))
      ;; path-independent chain rule for variation
      (is (= '(φ (f (q t))) (pe (((φ F) q) 't))))
      (is (= '(* ((D φ) (f (q t))) ((D f) (q t)) (η t)) (pe (((δη (φ F)) q) 't))))
      )
    ))
