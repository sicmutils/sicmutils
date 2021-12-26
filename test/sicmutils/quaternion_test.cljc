;;
;; Copyright © 2021 Sam Ritchie.
;; This work is based on the Scmutils system of MIT/GNU Scheme:
;; Copyright © 2002 Massachusetts Institute of Technology
;;
;; This is free software;  you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or (at
;; your option) any later version.
;;
;; This software is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this code; if not, see <http://www.gnu.org/licenses/>.
;;

(ns sicmutils.quaternion-test
  (:require [clojure.test :refer [is deftest testing]]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]
             #?@(:cljs [:include-macros true])]
            [same :refer [ish? with-comparator]
             #?@(:cljs [:include-macros true])]
            [sicmutils.calculus.derivative :refer [D]]
            [sicmutils.complex :as sc]
            [sicmutils.function :as f]
            [sicmutils.generators :as sg]
            [sicmutils.laws :as sl]
            [sicmutils.generic :as g]
            [sicmutils.mechanics.rotation :as mr]
            [sicmutils.simplify]
            [sicmutils.structure :as s]
            [sicmutils.quaternion :as q]
            [sicmutils.util :as u]
            [sicmutils.value :as v]))

(defn v:make-unit
  "Normalizes the supplied vector. TODO move this to a collections, or vector
  namespace?"
  [v]
  (g/* (g/invert (g/abs v))
		   v))

(deftest interface-tests
  (checking "Clojure interface definitions" 100
            [q (sg/quaternion gen/nat)]
            (let [v (vec q)]
              (is (coll? q))
              (is (seqable? q))

              (is (not (seq? q))
                  "a quaternion is NOT a sequence...")
              (is (seq? (seq q))
                  "but it does respond to `seq` appropriately.")

              (is (sequential? q))

              (is (reversible? q))
              (is (= (rseq v) (rseq q))
                  "rseq matches vector impl")

              (is (counted? q))
              (is (= (count v) (count q))
                  "count matches vector impl")

              (is (associative? q))
              (is (indexed? q))
              (is (ifn? q))

              (is (= (reduce-kv + 0 v)
                     (reduce-kv + 0 q))
                  "reduce-kv matches vector impl")

              (is (= (reduce + 0 v)
                     (reduce + v)
                     (reduce + 0 q)
                     (reduce + q))
                  "reduce matches vector impl")))

  (checking "vector-like operations" 100
            [x (sg/quaternion)]
            (let [v (vec x)]
              (doseq [i (range 4)]
                (is (= (get x i) (get v i)))
                (is (= (nth x i) (nth v i))))

              (is (= "face" (get x 10 "face"))
                  "get can take a default arg")))

  (testing "quaternions can assoc new elements"
    (let [x #sicm/quaternion ['r 'i 'j 'k]]
      (is (= (q/make 10 'i 'j 'k)
             (assoc x 0 10))
          "assoc replaces indices")

      (is (thrown? IllegalArgumentException
                   (assoc x :face 10))
          "wrong type of key")

      (is (thrown? IndexOutOfBoundsException
                   (assoc x 10 10))
          "int, but wrong index")))

  (testing "find, valAt"
    (let [x (q/make 4 5 6 7)]
      (is (= [(find x 0)
              (find x 1)
              (find x 2)
              (find x 3)]
             (map-indexed vector x))
          "find works on quaternions, returning MapEntry instances.")))

  (testing "conj"
    (is (thrown? UnsupportedOperationException
                 (conj (q/make 4 5 6 7) 10))))

  (testing "f/arity"
    (is (= [:exactly 1]
           (f/arity #sicm/quaternion [g/abs g/negate g/exp g/cos]))
        "arity matches the arity of contained functions"))

  (testing "IFn"
    (is (= (q/make 6 0 9 1)
           ((q/make + - * /) 3 3)))
    (is (= (q/make 22 -18 2048 (g/expt 2 -9))
           ((q/make + - * /) 2 2 2 2 2 2 2 2 2 2 2))))

  (checking "quaternions hold metadata" 100
            [x (sg/quaternion)
             m (gen/map gen/keyword gen/string)]
            (let [with-m (with-meta x m)]
              (is (nil? (meta x))
                  "the original has no metadata")

              (is (= x with-m)
                  "equality isn't affected by metadata")

              (is (= m (meta with-m))
                  "metadata works")))

  (testing "calculus works! IPerturbed tests."
    (letfn [(f [x]
              (fn [y]
                #sicm/quaternion [(g/* x y)
                                  (g/* x x y)
                                  (g/* x x y y)
                                  x]))]
      (is (= #sicm/quaternion
             ['y '(* 2 x y) '(* 2 x (expt y 2)) 1]
             (g/simplify
              (((D f) 'x) 'y)))
          "derivatives are extracted for each component, allowing
          quaternion-valued functions to play well with D. (This tests the
          higher-order-function derivative ability too.)"))

    (let [fq (D #sicm/quaternion [g/square g/cube g/exp g/log])]
      (is (= #sicm/quaternion ['(* 2 x) '(* 3 (expt x 2)) '(exp x) '(/ 1 x)]
             (g/simplify
              (fq 'x)))
          "Derivatives of quaternions with functional coefficients works")))

  (testing "value protocol"
    (testing "zero?"
      (is (q/zero? q/ZERO))
      (is (v/zero? (q/make 0 0 0 0)))
      (is (not (v/zero? (q/make 1 0 0 0)))))

    (checking "zero-like" 100 [x (sg/quaternion)]
              (if (v/zero? x)
                (is (= x (q/make 0 0 0 0)))
                (do (is (v/zero? (v/zero-like x)))
                    (is (v/zero? (empty x))
                        "empty also returns the zero"))))

    (testing "one?"
      (is (q/one? q/ONE))
      (is (v/identity? q/ONE)))

    (checking "one-like, identity-like" 100 [x (sg/quaternion)]
              (if (v/one? x)
                (is (= x (q/make 1 0 0 0)))
                (is (v/one? (v/one-like x))))

              (if (v/identity? x)
                (is (= x (q/make 1 0 0 0)))
                (is (v/identity? (v/identity-like x)))))

    (testing "exact?"
      (is (v/exact? (q/make 1 2 3 4)))
      (is (not (v/exact? (q/make 1.2 3 4 5))))
      (is (v/exact? (q/make 1 2 3 #sicm/ratio 3/2)))
      (is (not (v/exact? (q/make 0 0 0 0.00001)))))

    (testing "numerical?"
      (is (not (v/numerical? (s/up 1 2 3 4)))
          "no structure is numerical."))

    (testing "freeze"
      (is (= '(quaternion (/ 1 2) 2 3 x)
             (v/freeze (q/make #sicm/ratio 1/2
                               2 3 'x)))))

    (checking "kind" 100 [x (sg/quaternion)]
              (is (= ::q/quaternion (v/kind x))))))

(deftest basic-tests
  (checking "accessors" 100 [x (sg/quaternion)]
            (let [[r i j k] x]
              (is (= r (q/get-r x)))
              (is (= i (q/get-i x)))
              (is (= j (q/get-j x)))
              (is (= k (q/get-k x)))

              (is (= (q/real-part x) (q/get-r x))
                  "`real-part` aliases `get-r`")

              (is (= (sc/complex r i)
                     (q/->complex x))
                  "The 'complex' part of a quaternion is the real part and the
                  coefficient attached to `i` turned into an imaginary
                  number.")

              (is (= (vec x) (q/->vector x))
                  "`->vector` is a more efficient version of `(vec x)`")

              (is (= [i j k] (q/three-vector x))
                  "`->vector` is a more efficient version of `(vec x)`")))

  (testing "predicate unit tests"
    (is (and (q/unit? q/ONE)
             (q/unit? q/I)
             (q/unit? q/J)
             (q/unit? q/K))
        "These bound unit quaternions all return true.")

    (is (and (not (q/pure? q/ONE))
             (q/pure? q/I)
             (q/pure? q/J)
             (q/pure? q/K))
        "of the nonzero unit quaternions, I, J and K are pure, ONE is not.")

    (is (and (not (q/unit? q/ZERO))
             (q/real? q/ZERO)
             (q/pure? q/ZERO))
        "the zero quaternion is both pure AND real... according to this API,
        anyway. (it is also not a unit quaternion.)"))

  (checking "predicates" 100 [x (sg/quaternion)]
            (is (= (q/real? (q/make (q/real-part x))))
                "a quaternion is 'real' if it contains ONLY the real part of the
                original quaternion.")

            (is (= (q/pure? (q/make 0 (q/three-vector x))))
                "a quaternion is 'pure' if it contains ONLY the imaginary part
                of the original quaternion."))

  (checking "eq, equality between types" 100
            [[r i j k :as v] (gen/vector sg/real 4)]
            (let [q (q/make v)]
              (is (v/= q q)
                  "quaternion equality is reflexive")

              (is (v/= r (q/make r)) "real == quaternion")
              (is (v/= (q/make r) r) "quaternion == real")

              (is (v/= #sicm/complex [r i] (q/make r i 0 0))
                  "complex == quaternion")
              (is (v/= (q/make r i 0 0) #sicm/complex [r i])
                  "quaternion == complex")

              (is (v/= v (q/make v)) "vector == quaternion")
              (is (v/= (q/make v) v) "quaternion == vector")))

  (testing "constructors"
    (is (thrown? IllegalArgumentException
                 (q/parse-quaternion [1 2]))
        "passing an ill-formed literal to parse-quaternion throws an error at
        read-time.")

    (checking "q/make" 100
              [[r i j k :as v] (gen/vector sg/real 4)]
              (is (= (q/make v)
                     (q/make (q/make v)))
                  "make is idempotent")

              (is (= (q/make v)
                     (q/make r i j k)
                     (q/make r [i j k]))
                  "make can take the full vector, individual components, or a
                  real component and imaginary vector.")

              (is (= (q/make [r i 0 0])
                     (q/make #sicm/complex [r i]))
                  "make can properly unpack complex numbers")

              (is (= (q/make v)
                     (q/make (concat v [10 11 12])))
                  "make ignores elements beyond the 4th"))))

(deftest arithmetic-tests
  (testing "Quaternions form a skew field, ie, a division ring."
    (with-comparator (v/within 5e-4)
      (sl/field 100
                (sg/quaternion (sg/reasonable-double))
                "quaternions" :skew? true)))

  (testing "zero-arg arithmetic"
    (is (= q/ZERO (q/add)) "additive identity")
    (is (= q/ZERO (q/sub)) "additive identity")
    (is (= q/ONE (q/mul)) "multiplicative identity")
    (is (= q/ONE (q/div)) "multiplicative identity"))

  (checking "scalar / quaternion arithmetic matches quaternion-only
            implementations." 100 [s sg/symbol
                                   x (sg/quaternion sg/symbol)]
            (is (= x (q/add x)) "single-arg add == identity")
            (is (= (q/negate x) (q/sub x)) "single-arg sub == negate")
            (is (= x (q/mul x)) "single-arg mul == identity")
            (is (= (q/invert x) (q/div x)) "single-arg div == identity")

            (is (= (q/add (q/make s) x)
                   (q/scalar+quaternion s x))
                "s+q matches quaternion addition")

            (is (= (q/add x (q/make s))
                   (q/quaternion+scalar x s))
                "q+s matches quaternion addition")

            (is (= (q/sub (q/make s) x)
                   (q/scalar-quaternion s x))
                "s-q matches quaternion subtraction")

            (is (= (q/sub x (q/make s))
                   (q/quaternion-scalar x s))
                "q-s matches quaternion subtraction")

            (is (= (q/mul (q/make s) x)
                   (q/scale-l s x))
                "s*q matches quaternion multiplication")

            (is (= (q/mul x (q/make s))
                   (q/scale x s))
                "q*s matches quaternion multiplication")

            (is (= (g/simplify (q/div x (q/make s)))
                   (g/simplify (q/q-div-scalar x s)))
                "q/s matches quaternion division"))

  (checking "multi-arg arithmetic" 100
            [xs (gen/vector (sg/quaternion) 0 20)]
            (is (= (apply g/+ xs)
                   (apply q/add xs))
                "q/add matches g/+ for all arities")

            (is (= (apply g/- xs)
                   (apply q/sub xs))
                "q/sub matches g/- for all arities")

            (is (= (apply g/* xs)
                   (apply q/mul xs))
                "q/mul matches g/* for all arities")

            (is (= (apply g// xs)
                   (apply q/div xs))
                "q/div matches g// for all arities"))

  (checking "q/conjugate" 100 [x (sg/quaternion sg/any-integral)]
            (let [sum (g/+ x (q/conjugate x))]
              (is (q/real? sum)
                  "adding x to its conjugate removes all imaginary coefficients")

              (is (= (q/make (q/real-part x))
                     (g// sum 2))
                  "and doubles the real coefficient"))

            (is (q/real?
                 (g/* x (q/conjugate x)))
                "x*conj(x) is real")

            (is (= (q/dot-product x x)
                   (q/real-part
                    (g/* x (q/conjugate x))))
                "x*conj(x) == xx*, the squared norm"))

  (checking "q/normalize, q/magnitude" 100
            [x (sg/quaternion sg/small-integral)]
            (is (ish? (q/dot-product x x)
                      (g/square
                       (q/magnitude x)))
                "the dot-product of a quaternion with itself equals its squared
                magnitude.")

            (let [m      (q/magnitude x)
                  normal (q/normalize x)]
              (if (v/zero? m)
                (is (q/zero? x)
                    "can't normalize if the quaternion is zero.")

                (is (q/unit? normal :epsilon 1e-8)
                    "normalizing a quaternion makes it (approximately) a unit
                      quaternion."))))

  (checking "exp, log match real and complex impls" 100 [x sg/complex]
            (let [quat (q/make x)]
              (is (ish? (g/log x) (g/log quat))
                  "complex log matches quat log when j==k==0")

              (is (ish? (g/exp x) (g/exp quat))
                  "complex exp matches quat exp when j==k==0")

              (is (ish? (g/log (q/get-r quat))
                        (g/log (q/make (q/get-r quat))))
                  "real log matches quat log when i==j==k==0")

              (is (ish? (g/exp (q/get-r quat))
                        (g/exp (q/make (q/get-r quat))))
                  "real exp matches quat exp when i==j==k==0")))

  ;; TODO notate these
  (is (= '(quaternion (log y) (* (/ 1 2) pi) 0 0)
         (v/freeze
          (g/simplify
           (q/log (q/make 0 'y 0 0))))))

  (is (= '(quaternion (log y) 0 0 0)
         (v/freeze
          (g/simplify
           (q/log (q/make 'y 0 0 0))))))

  (let [gen (sg/quaternion
             (sg/reasonable-double
              {:min -1e3
               :max 1e3
               :excluded-lower -1
               :excluded-upper 1}))]
    (with-comparator (v/within 1e-4)
      (checking "exp/log" 100 [x gen]
                (is (ish? x (q/exp (q/log x)))
                    "exp(log(q)) acts as identity"))

      (checking "expt/sqrt" 100 [x gen]
                (is (ish? x (q/mul
                             (q/sqrt x)
                             (q/sqrt x)))
                    "q == sqrt(q)*sqrt(q)")

                (is (ish? (q/sqrt x)
                          (q/expt x 0.5))
                    "sqrt(q) == q^0.5")

                (is (ish? (q/mul x x)
                          (q/expt x 2))
                    "q*q == q^2, expt impl matches manual exponentiation")))))

;; TODO test dot-product between complex, scalar, quaternion combos
;;
;; TODO look at https://www.3dgep.com/understanding-quaternions/#Quaternion_Dot_Product
(deftest rotation-tests
  (is (= '(up theta
              (up x y (sqrt (+ (* -1 (expt x 2))
                               (* -1 (expt y 2))
                               1))))
         (v/freeze
          (g/simplify
           (q/->angle-axis
            (q/angle-axis->
             'theta
             ['x 'y (g/sqrt (g/- 1 (g/square 'x) (g/square 'y)))]))))))


  (is (= (s/up 0 (s/up 0 0 0))
         (g/simplify
          (let [theta 'theta
                v (s/up 'x 'y 'z)
                axis (v:make-unit v)
                [theta' axis'] (-> (q/angle-axis-> theta axis)
                                   (q/->rotation-matrix)
                                   (q/rotation-matrix->)
                                   (q/->angle-axis))]
            (s/up (g/- theta' theta)
                  (g/- axis' axis))))))

  ;; But look at (show-notes) to see the assumptions.
  ;;
  ;; Indeed:
  (is (= (s/up 2.0
               (s/up -0.5345224838248488
                     -1.0690449676496976
                     -1.6035674514745464))
         (let [theta -1
               v (s/up 1 2 3)
               axis (v:make-unit v)
               [theta' axis'] (-> (q/angle-axis-> theta axis)
                                  (q/->rotation-matrix)
                                  (q/rotation-matrix->)
                                  (q/->angle-axis))]
           (s/up (g/- theta' theta)
                 (g/- axis' axis))))))

(defn rotation-matrix->quaternion-mason [M]
  (let [r11 (get-in M [0 0]) r12 (get-in M [0 1]) r13 (get-in M [0 2])
        r21 (get-in M [1 0]) r22 (get-in M [1 1]) r23 (get-in M [1 2])
        r31 (get-in M [2 0]) r32 (get-in M [2 1]) r33 (get-in M [2 2])
        quarter (g// 1 4)

        q0-2 (g/* quarter (g/+ 1 r11 r22 r33))

        q0q1 (g/* quarter (g/- r32 r23))
        q0q2 (g/* quarter (g/- r13 r31))
        q0q3 (g/* quarter (g/- r21 r12))
        q1q2 (g/* quarter (g/+ r12 r21))
        q1q3 (g/* quarter (g/+ r13 r31))
        q2q3 (g/* quarter (g/+ r23 r32))]
    ;; If numerical, choose largest of squares.
    ;; If symbolic, choose nonzero square.
    (let [q0 (g/sqrt q0-2)
          q1 (g// q0q1 q0)
          q2 (g// q0q2 q0)
          q3 (g// q0q3 q0)]
      (q/make q0 q1 q2 q3))))

(deftest new-tests
  (let [M (g/* (mr/rotate-z-matrix 0.1)
               (mr/rotate-x-matrix 0.2)
               (mr/rotate-z-matrix 0.3))]
    (is (v/zero?
         (g/- (rotation-matrix->quaternion-mason M)
              (q/rotation-matrix-> M))))))
