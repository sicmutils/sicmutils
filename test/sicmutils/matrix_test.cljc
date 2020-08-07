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

(ns sicmutils.matrix-test
  (:require #?(:clj  [clojure.test :refer :all]
               :cljs [cljs.test :as t :refer-macros [is deftest testing]])
            #?@(:clj [[clojure.test.check.generators :as gen]
                      [clojure.test.check.properties :as prop]
                      [clojure.test.check.clojure-test :refer [defspec]]])
            #?(:clj [sicmutils.calculus.derivative :as d])
            #?(:clj [sicmutils.function :refer [literal-function]])
            [sicmutils.generic :as g]
            [sicmutils.matrix :as m]
            [sicmutils.numsymb]
            #?(:clj [sicmutils.simplify])
            [sicmutils.structure :as s]
            [sicmutils.value :as v]))

(deftest matrix-value-implementation
  (testing "nullity?"
    (is (v/nullity? (m/by-rows [0])))
    (is (v/nullity?
         (m/by-rows [0 0 0]
                    [0 0 0]
                    [0 0 0])))
    (is (not (v/nullity? (m/by-rows [1 2 3])))))

  (testing "zero-like"
    (is (= (m/by-rows [0 0 0])
           (v/zero-like (m/by-rows [1 2 3]))))
    (is (= (m/by-rows [0])
           (v/zero-like (m/by-rows [1])))))

  (testing "exact?"
    (is (v/exact? (m/by-rows [1] [2])))
    (is (not (v/exact? (m/by-rows [1.2] [3] [4]))))
    #?(:clj (is (v/exact? (m/by-rows [0 1 3/2]))))
    (is (not (v/exact? (m/by-rows [0] [0] [0.00001])))))

  (testing "kind"
    (is (= ::m/matrix (v/kind (m/by-rows [1 2]))))))

(deftest matrix-interfaces
  (testing "count"
    (is (= 1 (count (m/by-rows [1 2 3]))))
    (is (= 3 (count (m/by-rows [1] [2] [3])))))

  (testing "support take"
    (is (= [[1 2 3] [4 5 6]] (take 2 (m/by-rows [1 2 3] [4 5 6] [7 8 9])))))

  (testing "support drop"
    (is (= [[4 5 6] [7 8 9]] (drop 1 (m/by-rows [1 2 3] [4 5 6] [7 8 9])))))

  (comment
    (testing "can be mapped"
      (is (= (s/up 1 4 9) (map square (s/up 1 2 3)))))

    (testing "a structure can produce a seq"
      (is (= [1 2 3] (seq (s/up 1 2 3))))
      (is (= [4 5 6] (seq (s/down 4 5 6))))
      (is (= [(s/up 1 2) (s/up 3 4)] (seq (s/down (s/up 1 2) (s/up 3 4)))))
      (is (= [1 2 3 4] (flatten (s/down (s/up 1 2) (s/up 3 4))))))

    (testing "seqable"
      (is (= [1 2 3] (into [] (s/up 1 2 3)))))

    (testing "a structure has a nth element (ILookup)"
      (is (= 14 (nth (s/up 10 12 14) 2)))
      (is (= 5 (nth (s/up 4 5 6) 1)))
      (is (thrown? #?(:clj IndexOutOfBoundsException :cljs js/Error) (nth (s/up 4 5 6) 4))))

    (testing "IFn"
      (is (= (s/up 6 9 1) ((s/up + * /) 3 3)))
      (is (= (s/up 22 2048 (g/expt 2 -9)) ((s/up + * /) 2 2 2 2 2 2 2 2 2 2 2))))

    (testing "print representation"
      (let [s (pr-str (s/up 1 2 3))]
        (is #?(:clj (clojure.string/includes? s "\"(up 1 2 3)\"")
               :cljs (= s "#object[sicmutils.structure.Structure \"(up 1 2 3)\"]"))))
      (is (= "(up 1 2 3)" (str (s/up 1 2 3)))))

    (testing "equality"
      (= (s/up 1 2 3) [1 2 3])
      (= (s/up 1 2 3) (s/up 1 2 3)))))

(deftest matrix-basics
  (let [M (m/by-rows (list 1 2 3)
                     (list 4 5 6))
        A (m/by-rows [11 12 13]
                     [21 22 23]
                     [31 32 33])
        v (m/column 7 8 9)]
    (is (= ::m/matrix (v/kind M)))
    (is (= '(matrix-by-rows [1 2 3] [4 5 6]) (v/freeze M)))
    (is (= (m/by-rows [1 4] [2 5] [3 6]) (m/transpose M)))
    (is (= (m/by-rows [0 0 0] [0 0 0]) (v/zero-like M)))
    (is (thrown? #?(:clj IllegalArgumentException :cljs js/Error) (v/one-like M)))
    (is (= (m/by-rows [1 0 0] [0 1 0] [0 0 1]) (v/one-like A)))
    (is (thrown? #?(:clj IllegalArgumentException :cljs js/Error) (m/by-rows [1 2 3] [4 5])))
    (is (thrown? #?(:clj AssertionError :cljs js/Error) (m/by-rows)))
    (is (= 5 (m/get-in M [1 1])))
    (is (= 3 (m/get-in M [0 2])))
    (is (= [4 5 6] (m/get-in M [1])))
    (is (= 8 (m/get-in v [1])))
    (is (= (m/by-rows [2 3 4]
                      [5 6 7]) (m/fmap inc M)))
    (is (= (m/by-rows [22 23] [32 33])
           (m/without A 0 0)))
    (is (= (m/by-rows [21 23] [31 33])
           (m/without A 0 1)))
    (is (= (m/by-rows [21 22] [31 32])
           (m/without A 0 2)))

    (is (= (m/by-rows [12 13] [32 33])
           (m/without A 1 0)))
    (is (= (m/by-rows [11 13] [31 33])
           (m/without A 1 1)))
    (is (= (m/by-rows [11 12] [31 32])
           (m/without A 1 2)))

    (is (= (m/by-rows [12 13] [22 23])
           (m/without A 2 0)))
    (is (= (m/by-rows [11 13] [21 23])
           (m/without A 2 1)))
    (is (= (m/by-rows [11 12] [21 22])
           (m/without A 2 2)))
    (is (= (s/up 11 21 31) (m/nth-col A 0)))
    (is (= (s/up 12 22 32) (m/nth-col A 1)))
    (is (= (s/up 13 23 33) (m/nth-col A 2)))
    (is (thrown? #?(:clj IndexOutOfBoundsException :cljs js/Error) (m/nth-col A 3)))
    (is (= 18 (m/determinant (m/by-rows [-2 2 -3]
                                        [-1 1 3]
                                        [2 0 -1]))))
    (is (= '(+
             (* a e i)
             (* -1 a f h)
             (* -1 b d i)
             (* b f g)
             (* c d h)
             (* -1 c e g))
           (g/simplify (m/determinant (m/by-rows '[a b c]
                                                 '[d e f]
                                                 '[g h i])))))
    #?(:clj (is (= (m/by-rows [1/2])
                   (m/invert (m/by-rows [2])))))
    (is (= (m/by-rows [7 -3 -3]
                      [-1 1 0]
                      [-1 0 1])
           (m/invert (m/by-rows [1 3 3]
                                [1 4 3]
                                [1 3 4]))))

    (is (= (m/by-rows [0 1 2]
                      [1 2 3]
                      [2 3 4])
           (m/generate 3 3 +)))

    (is (v/nullity? (m/by-rows [0 0]
                               [0 0])))
    (is (= '(matrix-by-rows [(f x) (g x)] [(h x) (k x)])
           (g/simplify
            ((m/by-rows (map literal-function '[f g])
                        (map literal-function '[h k])) 'x))))
    (let [R2f #(literal-function % [0 1] 0)]
      (is (= '(matrix-by-rows [(f x y) (g x y)] [(h x y) (k x y)])
             (g/simplify
              ((m/by-rows [(R2f 'f) (R2f 'g)]
                          [(R2f 'h) (R2f 'k)]) 'x 'y)))))))

(comment
  (deftest matrix-generic-operations
    (let [M (m/by-rows (list 1 2 3)
                       (list 4 5 6))]
      (is (= (m/by-rows (list -1 -2 -3)
                        (list -4 -5 -6)) (g/negate M)))
      (is (= (s/down (s/up -1 -2)
                     (s/up -3 -4))
             (m/square-structure-operation
              (s/down (s/up 1 2) (s/up 3 4))
              #(m/fmap - %))))))

  (deftest structure
    (let [A (s/up 1 2 'a (s/down 3 4) (s/up (s/down 'c 'd) 'e))]
      (is (= 8 (s/dimension A)))
      (let [vs (s/up (s/up 'vx1 'vy1) (s/up 'vx2 'vy2))
            L1 (fn [[v1 v2]]
                 (g/+ (g/* 1/2 'm1 (g/square v1))
                      (g/* 1/2 'm2 (g/square v2))))]
        (is (= '(matrix-by-rows [m1 0 0 0]
                                [0 m1 0 0]
                                [0 0 m2 0]
                                [0 0 0 m2])
               (g/simplify (m/s->m vs (((g/expt d/D 2) L1) vs) vs))))))
    (let [M (m/by-rows [1 2 3] [4 5 6])]
      (is (= (s/down (s/up 1 4)
                     (s/up 2 5)
                     (s/up 3 6))
             (m/->structure M ::s/down ::s/up true)))
      (is (= (s/up (s/down 1 2 3)
                   (s/down 4 5 6))
             (m/->structure M ::s/up ::s/down false)))
      (is (= (s/up (s/down 1 4)
                   (s/down 2 5)
                   (s/down 3 6))
             (m/->structure M ::s/up ::s/down true)))
      (is (= (s/up (s/down 1 2 3)
                   (s/down 4 5 6))
             (m/->structure M ::s/up ::s/down false))))
    (doseq [[S M] [[(s/down (s/up 11 12) (s/up 21 22))
                    (m/by-rows [11 21] [12 22])]
                   [(s/up (s/down 11 12) (s/down 21 22))
                    (m/by-rows [11 12] [21 22])]
                   [(s/up (s/up 11 12) (s/up 21 22))
                    (m/by-rows [11 21] [12 22])]
                   [(s/down (s/down 11 12) (s/down 21 22))
                    (m/by-rows [11 12] [21 22])]]]
      (m/square-structure-> S (fn [m ->s]
                                (is (= M m))
                                (is (= S (->s m))))))
    (testing "structure as matrix"
      (let [A (s/up (s/up 1 2) (s/up 3 4))
            B (s/down (s/up 1 2 3) (s/up 3 4 5))
            C (s/down (s/up 1 2 3) (s/up 0 4 5) (s/up 1 0 6))
            D (s/up (s/down 3))
            E (s/up 1)
            F (s/down (s/up 1 2) (s/up 3 4))
            G (s/down (s/up 4 0 0 0) (s/up 0 0 2 0) (s/up 0 1 2 0) (s/up 1 0 0 1))
            cof #(m/square-structure-operation % m/cofactors)
            det #(m/square-structure-> % (fn [m _] (m/determinant m)))]
        (testing "cofactors"
          (is (= (s/up (s/up 4 -3) (s/up -2 1)) (cof A)))
          (is (= (s/down (s/up 24 5 -4) (s/up -12 3 2) (s/up -2 -5 4)) (cof C)))
          (is (= (s/up (s/down 3)) (cof D))))
        (testing "determinant"
          (is (= -2 (det A)))
          (is (= 22 (det C)))
          (is (= 3 (det D)))
          (is (= -2 (det F)))
          (is (= -8 (det G))))))
    (testing "s->m->s"
      (let [as-matrix (fn [F]
                        (fn [s]
                          (let [v (F s)]
                            (m/s->m (s/compatible-shape (g/* v s)) v s))))
            C-general (literal-function 'C '(-> (UP Real
                                                    (UP Real Real)
                                                    (DOWN Real Real))
                                                (UP Real
                                                    (UP Real Real)
                                                    (DOWN Real Real))))
            s (s/up 't (s/up 'x 'y) (s/down 'px 'py))]
        (is (= '(matrix-by-rows
                 [(((∂ 0) C↑0) (up t (up x y) (down px py)))
                  (((∂ 1 0) C↑0) (up t (up x y) (down px py)))
                  (((∂ 1 1) C↑0) (up t (up x y) (down px py)))
                  (((∂ 2 0) C↑0) (up t (up x y) (down px py)))
                  (((∂ 2 1) C↑0) (up t (up x y) (down px py)))]
                 [(((∂ 0) C↑1↑0) (up t (up x y) (down px py)))
                  (((∂ 1 0) C↑1↑0) (up t (up x y) (down px py)))
                  (((∂ 1 1) C↑1↑0) (up t (up x y) (down px py)))
                  (((∂ 2 0) C↑1↑0) (up t (up x y) (down px py)))
                  (((∂ 2 1) C↑1↑0) (up t (up x y) (down px py)))]
                 [(((∂ 0) C↑1↑1) (up t (up x y) (down px py)))
                  (((∂ 1 0) C↑1↑1) (up t (up x y) (down px py)))
                  (((∂ 1 1) C↑1↑1) (up t (up x y) (down px py)))
                  (((∂ 2 0) C↑1↑1) (up t (up x y) (down px py)))
                  (((∂ 2 1) C↑1↑1) (up t (up x y) (down px py)))]
                 [(((∂ 0) C↑2_0) (up t (up x y) (down px py)))
                  (((∂ 1 0) C↑2_0) (up t (up x y) (down px py)))
                  (((∂ 1 1) C↑2_0) (up t (up x y) (down px py)))
                  (((∂ 2 0) C↑2_0) (up t (up x y) (down px py)))
                  (((∂ 2 1) C↑2_0) (up t (up x y) (down px py)))]
                 [(((∂ 0) C↑2_1) (up t (up x y) (down px py)))
                  (((∂ 1 0) C↑2_1) (up t (up x y) (down px py)))
                  (((∂ 1 1) C↑2_1) (up t (up x y) (down px py)))
                  (((∂ 2 0) C↑2_1) (up t (up x y) (down px py)))
                  (((∂ 2 1) C↑2_1) (up t (up x y) (down px py)))])
               (g/simplify ((as-matrix (d/D C-general)) s))))
        (is (= (m/by-rows [1 2] [2 3]) (m/s->m (s/down 'x 'y) (s/down (s/up 1 2) (s/up 2 3)) (s/up 'x 'y))))
        (is (= (m/by-rows [-3 2] [2 -1]) (m/invert (m/s->m (s/down 'x 'y) (s/down (s/up 1 2) (s/up 2 3)) (s/up 'x 'y)))))
        (is (= (s/up (s/down -3 2) (s/down 2 -1))
               (m/m->s
                (s/compatible-shape (s/down 1 2))
                (m/invert (m/s->m (s/down 'x 'y) (s/down (s/up 1 2) (s/up 2 3)) (s/up 'x 'y)))
                (s/compatible-shape (s/up 2 3)))))
        (is (= (s/down (s/up -3 2) (s/up 2 -1))
               (m/s:inverse (s/down 'x 'y) (s/down (s/up 1 2) (s/up 2 3)) (s/up 'x 'y)))))))

  (deftest matrix-mul-div
    (let [M (m/by-rows '[a b] '[c d])
          S (m/by-rows '[e f] '[g h])]

      (is (= '(matrix-by-rows [(+ (* a e) (* b g)) (+ (* a f) (* b h))]
                              [(+ (* c e) (* d g)) (+ (* c f) (* d h))])
             (g/simplify (g/* M S)))))
    (let [M (m/by-rows [1 2 3]
                       [2 3 4])
          S (m/by-rows [3 4]
                       [4 5]
                       [5 6])]
      (is (= (m/by-rows [26 32] [38 47]) (g/* M S))))
    (let [M (m/by-rows '[a b] '[c d])
          d (s/down 'x 'y)
          u (s/up 'x 'y)]
      (testing "mul"
        (is (= (s/up (g/+ (g/* 'a 'x) (g/* 'b 'y)) (g/+ (g/* 'c 'x) (g/* 'd 'y)))
               (g/* M u)))
        (is (= (s/down (g/+ (g/* 'x 'a) (g/* 'y 'b)) (g/+ (g/* 'x 'c) (g/* 'y 'd)))
               (g/* d M)))
        (is (= '(+ (* a (expt x 2)) (* b x y) (* c x y) (* d (expt y 2)))
               (g/simplify (g/* d M u))))
        (is (thrown? IllegalArgumentException 'foo (g/* u M)))
        (is (thrown? IllegalArgumentException (g/* M d))))
      (testing "div"
        (is (= '(up
                 (/ (+ (* -1 b y) (* d x)) (+ (* a d) (* -1 b c)))
                 (/ (+ (* a y) (* -1 c x)) (+ (* a d) (* -1 b c))))
               (g/simplify (g/divide u M)))))))

  #?(:clj
     ;; TODO convert to cljs
     (do (defn generate-square-matrix
           [n]
           (gen/fmap #(apply m/by-rows %) (gen/vector (gen/vector gen/ratio n) n)))

         (defspec p+q=q+p
           (gen/let [n (gen/choose 1 10)]
             (prop/for-all [p (generate-square-matrix n)
                            q (generate-square-matrix n)]
                           (= (g/+ p q) (g/+ q p)))))

         (defspec pq*r=p*qr
           (gen/let [n (gen/choose 1 10)]
             (prop/for-all [p (generate-square-matrix n)
                            q (generate-square-matrix n)
                            r (generate-square-matrix n)]
                           (= (g/* (g/* p q) r) (g/* p (g/* q r))))))

         (defspec a*ainv=i
           (gen/let [n (gen/choose 1 5)]
             (prop/for-all [A (generate-square-matrix n)]
                           (or (zero? (g/determinant A))
                               (= (m/I n) (g/* (g/invert A) A) (g/* A (g/invert A)))))))))

  (deftest matrices-from-structure
    (let [A (s/up (s/up 1 2) (s/up 3 4))
          C (s/down (s/up 1 2 3) (s/up 0 4 5) (s/up 1 0 6))
          D (s/up (s/down 3))
          F (s/down (s/up 1 2) (s/up 3 4))
          G (s/down (s/up 4 0 0 0) (s/up 0 0 2 0) (s/up 0 1 2 0) (s/up 1 0 0 1))]
      #?(:clj
         (testing "ratio literals"
           (is (= (s/down (s/down -2 1) (s/down 3/2 -1/2)) (g/divide A)))
           (is (= 5/2 (g/* A (g/divide A))))
           (is (= 5/2 (g/* (g/divide A) A)))
           (is (= (g/* 1/22 (s/down (s/up 24 -12 -2) (s/up 5 3 -5) (s/up -4 2 4))) (g/divide C)))
           (is (= (s/up (s/down 1/3)) (g/divide D)))
           (is (= (s/down (s/up 1/4 0 0 0) (s/up 0 -1 1 0) (s/up 0 1/2 0 0) (s/up -1/4 0 0 1)) (g/divide G)))
           (is (= (s/down (s/up 1/4 0 0 0) (s/up 0 -1 1 0) (s/up 0 1/2 0 0) (s/up -1/4 0 0 1)) (g/divide G)))
           (is (= (s/down (s/up 1/8)) (g/divide (s/down (s/up 8)))))))

      (testing "inverse"
        (is (= (s/up (s/down 1)) (g/* D (g/divide D))))
        (is (= (s/down (s/up 1 0) (s/up 0 1)) (g/* F (g/divide F))))
        (is (= (s/down (s/up 1 0) (s/up 0 1)) (g/divide F F)))
        (is (= (s/down (s/up 1 0) (s/up 0 1)) (g/* (g/divide F) F)))
        (is (= (s/down (s/up 1 0 0 0) (s/up 0 1 0 0) (s/up 0 0 1 0) (s/up 0 0 0 1)) (g/divide G G))))

      (testing "invert-hilbert-matrix"
        (let [N 3
              H (apply s/up (for [i (range 1 (inc N))]
                              (apply s/up (for [j (range 1 (inc N))] (g/divide 1 (g/+ i j -1))))))]
          (is (= (s/down (s/down 9 -36 30) (s/down -36 192 -180) (s/down 30 -180 180)) (g/divide H))))))))
