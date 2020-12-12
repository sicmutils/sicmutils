
;; Copyright © 2017 Colin Smith.
;; This work is based on the Scmutils system of MIT/GNU Scheme:
;; Copyright © 2002 Massachusetts Institute of Technology

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
;;

(ns sicmutils.matrix-test
  (:require [clojure.test :refer [is deftest testing]]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]
             #?@(:cljs [:include-macros true])]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :as ct :refer [defspec]]
            [same :refer [ish?]]
            [sicmutils.generators :as sg]
            [sicmutils.generic :as g]
            [sicmutils.matrix :as m]
            [sicmutils.numsymb]
            [sicmutils.structure :as s]
            [sicmutils.structure-test :refer [<l|:inner:|r>]]
            [sicmutils.value :as v]))

(deftest value-protocol-tests
  (testing "zero?"
    (is (v/zero? (m/by-rows [0])))
    (is (v/zero?
         (m/by-rows [0 0 0]
                    [0 0 0]
                    [0 0 0])))
    (is (not (v/zero? (m/by-rows [1 2 3])))))

  (testing "zero-like"
    (is (= (m/by-rows [0 0 0])
           (v/zero-like (m/by-rows [1 2 3]))))
    (is (= (m/by-rows [0])
           (v/zero-like (m/by-rows [1]))))

    (is (= (m/by-rows [0.0] [0.0])
           (v/zero-like (m/by-rows [1.5] [2.5])))
        "zero-like preserves types"))

  (testing "one?"
    (comment
      ;; TODO this is currently buggy. We want one to return true for the
      ;; identity matrix, false otherwise.
      (is (v/one? (m/I 10)))))

  (testing "one-like"
    (is (= (m/I 3)
           (v/one-like
            (m/by-rows [1 2 3] [4 5 6] [7 8 9]))))
    (is (thrown? #?(:clj IllegalArgumentException :cljs js/Error)
                 (v/one-like (m/by-rows [1 2 3 4])))
        "one-like is only supported on square matrices."))

  (testing "identity?"
    (comment
      ;; TODO this is currently buggy. We want one to return true for the
      ;; identity matrix, false otherwise.
      (is (v/identity? (m/I 10)))))

  (testing "identity-like"
    (is (= (m/I 3)
           (v/identity-like
            (m/by-rows [1 2 3] [4 5 6] [7 8 9]))))
    (is (thrown? #?(:clj IllegalArgumentException :cljs js/Error)
                 (v/identity-like (m/by-rows [1 2 3 4])))
        "identity-like is only supported on square matrices."))

  (testing "numerical? returns false, always"
    (is (not (v/numerical? (m/by-rows [1] [2]))))
    (is (not (v/numerical? (m/by-rows [1.2] [3] [4]))))
    (is (not (v/numerical? (m/by-rows [0] [0] [0.00001]))))
    (is (not (v/numerical? (m/by-rows [0 1 (g// 3 2)])))))

  (testing "exact?"
    (is (v/exact? (m/by-rows [1] [2])))
    (is (not (v/exact? (m/by-rows [1.2] [3] [4]))))
    (is (not (v/exact? (m/by-rows [0] [0] [0.00001]))))
    (is (v/exact? (m/by-rows [0 1 (g// 3 2)]))))

  (testing "kind"
    (is (= ::m/row-matrix (v/kind (m/by-rows [1 2]))))
    (is (= ::m/column-matrix (v/kind (m/by-rows [1] [2]))))
    (is (= ::m/square-matrix (v/kind (m/by-rows [1 2] [3 4]))))
    (is (= ::m/matrix (v/kind (m/by-rows [1 2 3] [3 4 5]))))))

(deftest matrix-interfaces
  (testing "count"
    (is (= 1 (count (m/by-rows [1 2 3]))))
    (is (= 3 (count (m/by-rows [1] [2] [3])))))

  (testing "support take"
    (is (= [[1 2 3] [4 5 6]] (take 2 (m/by-rows [1 2 3] [4 5 6] [7 8 9])))))

  (testing "support drop"
    (is (= [[4 5 6] [7 8 9]] (drop 1 (m/by-rows [1 2 3] [4 5 6] [7 8 9])))))

  (testing "can be mapped"
    (is (= (s/up 1 4 9) (map g/square (s/up 1 2 3)))))

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
    (= (s/up 1 2 3) (s/up 1 2 3))))

(deftest matrix-basics
  (checking "square? is false for numbers" 100
            [x sg/any-integral]
            (is (not (m/square? x))))

  (checking "square? is true for squares" 100
            [m (gen/let [n (gen/choose 1 10)]
                 (sg/square-matrix n))]
            (is (m/square? m)))

  (checking "by-rows == (comp transpose by-cols), vice versas" 100
            [vs (-> (gen/sized #(gen/vector sg/real %))
                    (gen/vector 1 20))]
            (is (= (m/by-rows vs)
                   (g/transpose (m/by-cols vs))))
            (is (= (m/by-cols vs)
                   (g/transpose (m/by-rows vs)))))

  (checking "row*==row, column*==column" 100
            [vs (gen/vector sg/real 1 20)]
            (is (= (m/row* vs)
                   (apply m/row vs)))

            (is (= (m/column* vs)
                   (apply m/column vs))))

  (checking "by-rows == row" 100
            [vs (gen/vector sg/real 1 20)]
            (let [row (m/row* vs)]
              (is (= (m/by-rows vs) row))
              (is (m/row? row))))

  (checking "by-cols == column" 100
            [vs (gen/vector sg/real 1 20)]
            (let [col (m/column* vs)]
              (is (= (m/by-cols vs) col))
              (is (m/column? col))))

  (checking "with-substituted-row works" 100
            [[m new-row] (gen/let [n (gen/choose 1 10)]
                           (gen/tuple (sg/square-matrix n)
                                      (gen/vector sg/real n)))]
            (doseq [i (range (m/num-rows m))]
              (is (= new-row
                     (-> (m/with-substituted-row m i new-row)
                         (m/nth-row i)))
                  "swapping in a row should swap the row!")))

  (checking "submatrix matches without" 100
            [M (gen/let [n (gen/choose 1 10)]
                 (sg/square-matrix n))]
            (is (= (m/submatrix
                    M
                    1 (dec (m/num-rows M))
                    1 (dec (m/num-cols M)))
                   (m/without M 0 0))))

  (testing "submatrix"
    (let [M (m/by-rows [1 2 3]
                       [4 5 6]
                       [7 8 9])]
      (is (= (m/by-rows [1 2]
                        [4 5])
             (m/submatrix M 0 1 0 1)))

      (is (= (m/by-rows [1 2]
                        [4 5])
             (m/submatrix M 0 1 0 1)))))

  (checking "make-zero" 100 [m (gen/choose 0 10)
                             n (gen/choose 0 10)]
            (let [M (m/make-zero m n)]
              (is (v/zero? M))
              (is (= m (m/num-rows M)))
              (is (= n (m/num-cols M)))))

  (checking "make-diagonal" 100
            [vs (gen/vector sg/real 1 20)]
            (let [M (m/make-diagonal vs)]
              (is (m/square? M))
              (is (= (g/dimension M)
                     (g/dimension vs)))
              (is (= vs (m/diagonal M)))
              (is (= vs (m/diagonal M)))))

  (checking "make-diagonal, v/identity? v/one?" 100
            [v (gen/vector (gen/return 1) 1 20)]
            (let [M (m/make-diagonal v)]
              (is (v/identity? M))
              (is (not (v/identity? (g/* 2 M))))

              (is (v/one? M))
              (is (not (v/one? (g/* 2 M))))))

  (let [M (m/by-rows (list 1 2 3)
                     (list 4 5 6))
        A (m/by-rows [11 12 13]
                     [21 22 23]
                     [31 32 33])
        v (m/column 7 8 9)]
    (is (= ::m/matrix (v/kind M)))
    (is (= '(matrix-by-rows [1 2 3] [4 5 6]) (v/freeze M)))
    (is (= (m/by-rows [1 4] [2 5] [3 6]) (g/transpose M)))
    (is (= (m/by-rows [0 0 0] [0 0 0]) (v/zero-like M)))
    (is (= (m/by-rows [1 0 0] [0 1 0] [0 0 1]) (v/one-like A)))
    (is (thrown? #?(:clj IllegalArgumentException :cljs js/Error) (v/one-like M)))
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
    (let [matrix (m/by-rows [-2 2 -3]
                            [-1 1 3]
                            [2 0 -1])]
      (is (= 18 (g/determinant matrix)))
      (is (= -2 (g/trace matrix))))
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
    (is (v/zero? (m/by-rows [0 0]
                            [0 0])))))

(deftest literal-matrix-creation
  (testing "literal-matrix"
    (let [M (m/literal-matrix 'x 3 3)]
      (is (m/matrix? M))
      (is (= M (m/by-rows ['x↑0_0 'x↑0_1 'x↑0_2]
                          ['x↑1_0 'x↑1_1 'x↑1_2]
                          ['x↑2_0 'x↑2_1 'x↑2_2]))
          "A literal matrix is a matrix populated by symbols with index refs
          baked in.")))

  (checking "structure prototype matches matrix literal" 100
            [[sym M] (gen/let [sym  sg/symbol
                               rows (gen/choose 1 10)
                               cols  (gen/choose 1 10)]
                       [sym (m/literal-matrix sym rows cols)])]
            (is (= M (s/structure->prototype
                      sym (m/->structure M)))
                "literal matrices contain symbols that store their orientation
                properly. [[s/structure->prototype]] matches this behavior.")))

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
  (checking "M^tM is always square" 100
            [M (gen/let [m (gen/choose 1 5)
                         n (gen/choose 1 5)]
                 (sg/matrix m n))]
            (is (m/square?
                 (g/* (g/transpose M) M))))

  (checking "(s:transpose <l|, inner, |r>)==(s/transpose-outer inner)"
            100 [[l inner r] (gen/let [rows (gen/choose 1 5)
                                       cols (gen/choose 1 5)]
                               (<l|:inner:|r> rows cols))]
            (is (v/zero?
                 (g/- (m/s:transpose l inner r)
                      (s/transpose-outer inner)))))

  ;; this binding is required because `s:transpose` is meant to return a scalar;
  ;; here, we're testing the relation in a case that returns an uncollapsed
  ;; empty structure.
  (binding [m/*careful-conversion* false]
    (checking "(s:transpose <l|, inner, |r>)==(s/transpose-outer inner) with
              either side empty returns an empty structure"
              100 [[l inner r] (gen/let [rows (gen/choose 0 5) cols (gen/choose 0 5)]
                                 (<l|:inner:|r> rows cols))]
              (is (v/zero?
                   (g/- (m/s:transpose l inner r)
                        (s/transpose-outer inner))))))

  (let [A (s/up 1 2 'a (s/down 3 4) (s/up (s/down 'c 'd) 'e))
        M (m/by-rows [1 2 3] [4 5 6])]
    (is (= 8 (g/dimension A)))
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
    (let [A (s/up (s/up 1 2)
                  (s/up 3 4))
          B (s/down (s/up 1 2 3)
                    (s/up 3 4 5))
          C (s/down (s/up 1 2 3)
                    (s/up 0 4 5)
                    (s/up 1 0 6))
          D (s/up (s/down 3))
          E (s/up 1)
          F (s/down (s/up 1 2)
                    (s/up 3 4))
          G (s/down (s/up 4 0 0 0)
                    (s/up 0 0 2 0)
                    (s/up 0 1 2 0)
                    (s/up 1 0 0 1))
          cof #(m/square-structure-operation % m/cofactors)
          det #(m/square-structure-> % (fn [m _] (g/determinant m)))]

      (testing "cofactors"
        (is (= (s/up (s/up 4 -3) (s/up -2 1)) (cof A)))
        (is (= (s/down (s/up 24 5 -4) (s/up -12 3 2) (s/up -2 -5 4)) (cof C)))
        (is (= (s/up (s/down 3)) (cof D))))

      (testing "determinant"
        (is (= -2 (det A)))
        (is (= 22 (det C)))
        (is (= 3 (det D)))
        (is (= -2 (det F)))
        (is (= -8 (det G))))

      (testing "trace"
        (is (= 5 (g/trace A)))
        (is (= 11 (g/trace C)))
        (is (= 3 (g/trace D)))
        (is (= 5 (g/trace F)))
        (is (= 7 (g/trace G))))))

  (testing "s->m->s"
    (is (= (m/by-rows [1 2] [2 3]) (m/s->m (s/down 'x 'y) (s/down (s/up 1 2) (s/up 2 3)) (s/up 'x 'y))))
    (is (= (m/by-rows [-3 2] [2 -1]) (m/invert (m/s->m (s/down 'x 'y) (s/down (s/up 1 2) (s/up 2 3)) (s/up 'x 'y)))))
    (is (= (s/up (s/down -3 2) (s/down 2 -1))
           (m/m->s
            (s/compatible-shape (s/down 1 2))
            (m/invert (m/s->m (s/down 'x 'y) (s/down (s/up 1 2) (s/up 2 3)) (s/up 'x 'y)))
            (s/compatible-shape (s/up 2 3)))))
    (is (= (s/down (s/up -3 2) (s/up 2 -1))
           (m/s:inverse (s/down 'x 'y) (s/down (s/up 1 2) (s/up 2 3)) (s/up 'x 'y))))))

(deftest matrix-mul-div
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
      (is (thrown? #?(:clj IllegalArgumentException :cljs js/Error) 'foo (g/* u M)))
      (is (thrown? #?(:clj IllegalArgumentException :cljs js/Error) (g/* M d))))))

(deftest square-tests
  (checking "square matrices return true for square?"
            100 [matrix (gen/bind (gen/choose 1 10)
                                  sg/square-matrix)]
            (is (m/square? matrix))

            (is (= matrix
                   (m/by-rows*
                    (for [i (range (g/dimension matrix))]
                      (seq (m/nth-row matrix i)))))
                "fusing rows back together should yield the original.")

            (is (= matrix
                   (g/transpose
                    (m/by-rows*
                     (for [i (range (g/dimension matrix))]
                       (seq (m/nth-col matrix i))))))
                "fusing columns back together, then transposing, should yield
                the original.")

            (is (= (reduce g/+ (m/diagonal matrix))
                   (g/trace matrix))
                "the trace is the sum of the diagonal.")

            (if (= 1 (g/dimension matrix))
              (do (is (m/column? matrix))
                  (is (m/row? matrix)))
              (do (is (not (m/column? matrix)))
                  (is (not (m/row? matrix)))))))

(deftest complex-entry-tests
  (checking "g/conjugate with real entries acts as id" 100
            [a sg/real b sg/real
             c sg/real d sg/real]
            (is (= (m/by-rows [a b] [c d])
                   (g/conjugate
                    (m/by-rows [a b] [c d])))))

  (checking "g/conjugate conjugates entries" 100
            [a sg/complex b sg/complex
             c sg/complex d sg/complex]
            (is (= (m/by-rows [(g/conjugate a)
                               (g/conjugate b)]
                              [(g/conjugate c)
                               (g/conjugate d)])
                   (g/conjugate
                    (m/by-rows [a b] [c d]))))))

(defspec p+q=q+p
  (gen/let [n (gen/choose 1 10)]
    (prop/for-all [p (sg/square-matrix n)
                   q (sg/square-matrix n)]
                  (= (g/+ p q) (g/+ q p)))))


(defspec pq*r=p*qr
  (gen/let [n (gen/choose 1 10)]
    (prop/for-all [p (sg/square-matrix n)
                   q (sg/square-matrix n)
                   r (sg/square-matrix n)]
                  (= (g/* (g/* p q) r) (g/* p (g/* q r))))))

(defspec a*ainv=i
  (gen/let [n (gen/choose 1 5)]
    (prop/for-all [A (sg/square-matrix n)]
                  (or (v/zero? (g/determinant A))
                      (= (m/I n)
                         (g/* (g/invert A) A)
                         (g/* A (g/invert A)))))))

(deftest product-tests
  ;; TODO - fix the case where we have a 1x1 matrix that can't reach these
  ;; methods, since it gets dispatched as square.
  (checking "dot-product" 100 [s (sg/up1 sg/real 2 10)]
            (let [col (m/up->column-matrix s)
                  row (m/down->row-matrix (g/transpose s))]
              (is (= (g/dot-product s s)
                     (g/dot-product col col)))

              (is (= (g/dot-product s s)
                     (g/dot-product row row)))))

  (checking "inner-product" 100 [s (sg/up1 sg/complex 2 10)]
            (let [col (m/up->column-matrix s)
                  row (m/down->row-matrix (g/transpose s))]
              (is (= (g/inner-product s s)
                     (g/inner-product col col)))

              (is (= (g/inner-product s s)
                     (g/inner-product row row)))))

  (checking "cross-product returns matrices" 100
            [s (sg/up1 sg/complex 3)]
            (let [col (m/up->column-matrix s)
                  row (m/down->row-matrix (g/transpose s))]
              (is (ish? (m/up->column-matrix
                         (g/cross-product s s))
                        (g/cross-product col col)))

              (is (ish? (g/cross-product col col)
                        (g/transpose
                         (g/cross-product row row))))))

  (checking "outer-product" 100 [s (sg/up1 sg/complex 2 10)]
            (let [col (m/up->column-matrix s)
                  row (m/down->row-matrix (g/transpose s))]
              (is (= (g/outer-product s s)
                     (g/outer-product col row))))))

(deftest matrices-from-structure
  (let [A (s/up (s/up 1 2) (s/up 3 4))
        C (s/down (s/up 1 2 3) (s/up 0 4 5) (s/up 1 0 6))
        D (s/up (s/down 3))
        F (s/down (s/up 1 2) (s/up 3 4))
        G (s/down (s/up 4 0 0 0) (s/up 0 0 2 0) (s/up 0 1 2 0) (s/up 1 0 0 1))]

    (testing "inverse"
      (is (= (s/up (s/down 1)) (g/* D (g/divide D))))
      (is (= (s/down (s/up 1 0) (s/up 0 1)) (g/* F (g/divide F))))
      (is (= (s/down (s/up 1 0) (s/up 0 1)) (g/divide F F)))
      (is (= (s/down (s/up 1 0) (s/up 0 1)) (g/* (g/divide F) F)))
      (is (= (s/down (s/up 1 0 0 0) (s/up 0 1 0 0) (s/up 0 0 1 0) (s/up 0 0 0 1)) (g/divide G G))))

    (testing "ratio literals"
      (is (= (s/down (s/down -2 1)
                     (s/down #sicm/ratio 3/2 #sicm/ratio -1/2))
             (g/divide A)))
      (is (= #sicm/ratio 5/2 (g/* A (g/divide A))))
      (is (= #sicm/ratio 5/2 (g/* (g/divide A) A)))
      (is (= (g/* #sicm/ratio 1/22
                  (s/down (s/up 24 -12 -2)
                          (s/up 5 3 -5)
                          (s/up -4 2 4)))
             (g/divide C)))

      (is (= (s/up (s/down #sicm/ratio 1/3))
             (g/divide D)))

      (is (= (s/down (s/up #sicm/ratio 1/4 0 0 0)
                     (s/up 0 -1 1 0)
                     (s/up 0 #sicm/ratio 1/2 0 0)
                     (s/up #sicm/ratio -1/4 0 0 1))
             (g/divide G)))

      (is (= (s/down (s/up #sicm/ratio 1/4 0 0 0)
                     (s/up 0 -1 1 0)
                     (s/up 0 #sicm/ratio 1/2 0 0)
                     (s/up #sicm/ratio -1/4 0 0 1))
             (g/divide G)))
      (is (= (s/down (s/up #sicm/ratio 1/8))
             (g/divide (s/down (s/up 8))))))

    (testing "matrix ops, ratio literals"
      (is (= (m/by-rows [#sicm/ratio 1/2])
             (m/invert (m/by-rows [2])))))

    (testing "invert-hilbert-matrix"
      (let [N 3
            H (apply s/up (for [i (range 1 (inc N))]
                            (apply s/up (for [j (range 1 (inc N))]
                                          (g/divide 1 (g/+ i j -1))))))]
        (is (= (s/down (s/down 9 -36 30)
                       (s/down -36 192 -180)
                       (s/down 30 -180 180))
               (g/divide H)))))))
