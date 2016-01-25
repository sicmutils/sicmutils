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

(ns net.littleredcomputer.sicmutils.calculus.derivative-test
  (:refer-clojure :exclude [+ - * / zero? partial])
  (:require [clojure.test :refer :all]
            [net.littleredcomputer.sicmutils
             [function :refer :all]
             [generic :refer :all]
             [complex :refer [complex]]
             [operator :as o]
             [value :as v]
             [numbers]
             [simplify]
             [structure :refer :all]]
            [net.littleredcomputer.sicmutils.calculus.derivative :refer :all]))

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
      (is (= 0 (* dx dx)))))
  (testing "more terms"
    (let [d-expr #(-> % :terms (get (sorted-set 0)))
          d-simplify #(-> % d-expr simplify)]
      (is (= '(* 3 (expt x 2))
             (d-simplify (expt (+ 'x (make-differential {[0] 1})) 3))))
      (is (= '(* 4 (expt x 3))
             (d-simplify (expt (+ 'x (make-differential {[0] 1})) 4))))
      (let [dx (make-differential {[0] 1})
            x+dx (+ 'x dx)
            f (fn [x] (* x x x x))]
        (is (= '(* 4 (expt x 3))
               (d-simplify (* x+dx x+dx x+dx x+dx))))
        (is (= '(* 12 (expt x 2))
               (d-simplify (+ (* (+ (* (+ x+dx x+dx) x+dx) (* x+dx x+dx)) x+dx) (* x+dx x+dx x+dx)))))
        (is (= '(* 24 x) (d-simplify (+
                                      (* (+ (* 2 x+dx) x+dx x+dx x+dx x+dx) x+dx)
                                      (* (+ x+dx x+dx) x+dx)
                                      (* x+dx x+dx)
                                      (* (+ x+dx x+dx) x+dx)
                                      (* x+dx x+dx)))))
        (is (= 24 (d-expr (+ (* 6 x+dx)
                             (* 2 x+dx)
                             x+dx x+dx x+dx x+dx
                             (* 2 x+dx)
                             x+dx x+dx x+dx x+dx
                             (* 2 x+dx)
                             x+dx x+dx x+dx x+dx))))
        (is (= '(* 4 (expt x 3))
               (d-simplify (f x+dx))))))))

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
    (let [f (fn [x y] (+ (* 'a x x) (* 'b x y) (* 'c y y)))]
      (is (= '(+ (* 4 a) (* 3 b)) (simplify (((partial 0) f) 2 3))))
      (is (= '(+ (* 2 b) (* 6 c)) (simplify (((partial 1) f) 2 3))))
      (is (= '(+ (* 2 a x) (* b y)) (simplify (((partial 0) f) 'x 'y))))
      (is (= '(+ (* b x) (* 2 c y)) (simplify (((partial 1) f) 'x 'y))))
      ;; matrix of 2nd partials
      (is (= '[[(* 2 a) b]
               [b (* 2 c)]]
             (for [i (range 2)]
               (for [j (range 2)]
                 (simplify (((* (partial i) (partial j)) f) 'x 'y))))))
      (is (= '[[(* 2 a) b]
               [b (* 2 c)]]
             (for [i (range 2)]
               (for [j (range 2)]
                 (simplify (((compose (partial i) (partial j)) f) 'x 'y)))))))
    (let [F (fn [a b]
              (fn [[x y]]
                (up (* a x) (* b y))))]
      (is (= (up 'x 'y) ((F 1 1) (up 'x 'y))))
      (is (= (up (* 2 'x) (* 3 'y)) ((F 2 3) (up 'x 'y))))
      (is (= (up 'x 0)  ((((partial 0) F) 1 1) (up 'x 'y))))
      (is (= (up 0 'y)  ((((partial 1) F) 1 1) (up 'x 'y))))
      (is (= (down (up 'x 0) (up 0 'y)) (((D F) 1 1) (up 'x 'y)))))))

(deftest partial-shim
  (testing "partial also works the way Clojure defines it"
    (is (= 10 ((partial + 9) 1)))
    (is (= 5 ((partial max 4) 5)))
    (is (= 4 ((partial max 4) 2)))))

(deftest amazing-bug
  (testing "1"
    (let [f (fn [x]
              (fn [g]
                (fn [y]
                  (g (+ x y)))))
          f-hat ((D f) 3)]
      (is ((v/within 1e-6) 2980.957987 ((f-hat exp) 5)))
      ;; TODO: this is the amazing bug: bbb == 0 is wrong.
      #_(is (= 'bbb ((f-hat (f-hat exp)) 5))))))

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
    (is (= (((compose D D D) ff) 'x 'y 'z) (((expt D 3) ff) 'x 'y 'z))))
  (testing "issue #9 regression"
    (let [g (fn [z] (* z z z z))
          f4 (fn [x] (+ (* x x x) (* x x x)))]
      (is (= '(expt t 4) (simplify (g 't))))
      (is (= '(* 4 (expt t 3)) (simplify ((D g) 't))))
      (is (= '(* 12 (expt t 2)) (simplify ((D (D g)) 't))))
      (is (= '(* 24 t) (simplify ((D (D (D g))) 't))))
      (is (= '(* 24 z) (simplify (((expt D 3) g) 'z))))
      (is (= '(* 2 (expt s 3)) (simplify (f4 's))))
      (is (= '(* 6 (expt s 2)) (simplify ((D f4) 's))))
      (is (= '(* 12 s) (simplify ((D (D f4)) 's))))
      (is (= 12 (simplify ((D (D (D f4))) 's))))
      (is (= 12 (simplify (((* D D D) f4) 's))))
      (is (= 12 (simplify (((compose D D D) f4) 's))))
      (is (= 12 (simplify (((expt D 3) f4) 's)))))
    (let [fff (fn [x y z] (+ (* x x y)(* y y y z)(* z z z z x)))]
      (is (= '(+ (* x (expt z 4)) (* (expt y 3) z) (* (expt x 2) y))
             (simplify (((expt D 0) fff) 'x 'y 'z))))
      (is (= '(down
               (+ (expt z 4) (* 2 x y))
               (+ (* 3 (expt y 2) z) (expt x 2))
               (+ (* 4 x (expt z 3)) (expt y 3)))
             (simplify (((expt D 1) fff) 'x 'y 'z))))
      (is (= '(down
               (down (* 2 y) (* 2 x) (* 4 (expt z 3)))
               (down (* 2 x) (* 6 y z) (* 3 (expt y 2)))
               (down (* 4 (expt z 3)) (* 3 (expt y 2)) (* 12 x (expt z 2))))
             (simplify (((expt D 2) fff) 'x 'y 'z))))
      (is (= '(down
               (down (down 0 2 0) (down 2 0 0) (down 0 0 (* 12 (expt z 2))))
               (down (down 2 0 0) (down 0 (* 6 z) (* 6 y)) (down 0 (* 6 y) 0))
               (down
                (down 0 0 (* 12 (expt z 2)))
                (down 0 (* 6 y) 0)
                (down (* 12 (expt z 2)) 0 (* 24 x z))))
             (simplify (((expt D 3) fff) 'x 'y 'z)))))))

(deftest literal-functions
  (with-literal-functions [f [g [0 0] 0]]
    (testing "R -> R"
      (is (= '((D f) x) (simplify ((D f) 'x))))
      (is (= '((D f) (+ x y)) (simplify ((D f) (+ 'x 'y))))))
    (testing "R^2 -> R"
      (is (= '(((∂ 0) g) x y) (simplify (((partial 0) g) 'x 'y))))
      (is (= '(((∂ 1) g) x y) (simplify (((partial 1) g) 'x 'y))))
      (is (= '(down (((∂ 0) g) x y) (((∂ 1) g) x y))
             (simplify ((D g) 'x 'y)))))))

(deftest complex-derivatives
  (let [i (complex 0 1)
        f (fn [z] (* i (sin (* i z))))]
    (is (= '(* -1 (cosh z))
           (simplify ((D f) 'z))))))

(deftest fun-with-operators
  (let [f #(expt % 3)]
    (is (= '(+ (* (cos t) (expt t 3)) (* 3 (sin t) (expt t 2)))
           (simplify (((* D sin) f) 't))))
    (is (= '(* 3 (sin t) (expt t 2))
           (simplify (((* sin D) f) 't))))))

(deftest vector-calculus
  (let [f (up identity sin cos)
        divergence #(fn [t] (reduce + ((D %) t)))
        laplacian #(* (D %) ((transpose D) %))]
    (is (= '(up 1 (cos t) (* -1 (sin t))) (simplify ((D f) 't))))
    (is (= '(down 1 (cos t) (* -1 (sin t))) (simplify (((transpose D) f) 't))))
    (is (= 2 (simplify (* ((D f) 't) (((transpose D) f) 't)))))
    (is (= 2 (simplify ((laplacian (up identity sin cos)) 't))))
    (is (= '(+ (cos t) (* -1 (sin t)) 1) (simplify ((divergence f) 't))))))
