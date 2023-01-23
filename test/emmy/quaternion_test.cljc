#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.quaternion-test
  (:require [clojure.test :refer [is deftest testing use-fixtures]]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]]
            [same :refer [ish? with-comparator] :include-macros true]
            [emmy.calculus.derivative :refer [D]]
            [emmy.complex :as sc]
            [emmy.function :as f]
            [emmy.generators :as sg]
            [emmy.generic :as g]
            [emmy.laws :as sl]
            [emmy.matrix :as m]
            [emmy.quaternion :as q]
            [emmy.simplify]
            [emmy.structure :as s]
            [emmy.util.logic :as ul]
            [emmy.value :as v]))

(use-fixtures :each
  (fn [thunk]
    (binding [ul/*log-assumptions?* false]
      (thunk))))

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

              (is (= x (assoc q/ZERO
                              0 (q/get-r x)
                              1 (q/get-i x)
                              2 (q/get-j x)
                              3 (q/get-k x))))

              (is (= "face" (get x 10 "face"))
                  "get can take a default arg")

              (is (nil? (get x "face")))))

  (testing "quaternions can assoc new elements"
    (let [x #sicm/quaternion ['r 'i 'j 'k]]
      (is (= (q/make 10 'i 'j 'k)
             (assoc x 0 10))
          "assoc replaces indices")

      (is (thrown? #?(:clj IllegalArgumentException :cljs js/Error)
                   (assoc x :face 10))
          "wrong type of key")

      (is (thrown? #?(:clj IndexOutOfBoundsException :cljs js/Error)
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
    (is (thrown? #?(:clj UnsupportedOperationException :cljs js/Error)
                 (conj (q/make 4 5 6 7) 10))))

  (testing "f/arity"
    (is (= [:exactly 1]
           (f/arity #sicm/quaternion [g/abs g/negate g/exp g/cos]))
        "arity matches the arity of contained functions"))

  (testing "IFn"
    (is (= (q/make 6 0 9 1)
           ((q/make + - * g//) 3 3)))
    (is (= (q/make 22 -18 2048 (g/expt 2 -9))
           ((q/make + - * g//) 2 2 2 2 2 2 2 2 2 2 2))))

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
                (q/make
                 (g/* x y)
                 (g/* x x y)
                 (g/* x x y y)
                 x)))]
      (is (= #sicm/quaternion
             ['y '(* 2 x y) '(* 2 x (expt y 2)) 1]
             (g/simplify
              (((D f) 'x) 'y)))
          "derivatives are extracted for each component, allowing
          quaternion-valued functions to play well with D. (This tests the
          higher-order-function derivative ability too.)"))

    (let [fq (D (q/make g/square g/cube g/exp g/log))]
      (is (= (q/make '(* 2 x) '(* 3 (expt x 2)) '(exp x) '(/ 1 x))
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

              (is (= [(sc/complex r i)
                      (sc/complex j k)]
                     (q/->complex-pair x))
                  "The 'complex' version of a quaternion is a pair of complex
                  numbers built from r,i and j,k respectively.")

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
            (is (q/real? (q/make (q/real-part x)))
                "a quaternion is 'real' if it contains ONLY the real part of the
                original quaternion.")

            (is (q/pure? (q/make 0 (q/three-vector x)))
                "a quaternion is 'pure' if it contains ONLY the imaginary part
                of the original quaternion."))

  (checking "eq, equality between types" 100
            [[r i :as v] (gen/vector sg/real 4)]
            (let [q (q/make v)]
              (is (v/= q q)
                  "quaternion equality is reflexive")

              (is (v/= r (q/make r)) "real == quaternion")
              (is (v/= (q/make r) r) "quaternion == real")

              (is (ish? #sicm/complex [r i] (q/make r i 0 0))
                  "complex == quaternion")
              (is (ish? (q/make r i 0 0) #sicm/complex [r i])
                  "quaternion == complex")

              (is (v/= v (q/make v)) "vector == quaternion")
              (is (v/= (q/make v) v) "quaternion == vector")))

  (testing "constructors"
    (is (thrown? #?(:clj IllegalArgumentException :cljs js/Error)
                 (q/parse-quaternion [1 2]))
        "passing an ill-formed literal to parse-quaternion throws an error at
        read-time.")

    (is (= `(q/make 10) (q/parse-quaternion 10)))
    (is (= `(q/make 10 11 12 13)
           (q/parse-quaternion [10 11 12 13])))

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

              (is (ish? (q/make [r i 0 0])
                        (q/make #sicm/complex [r i]))
                  "make can properly unpack complex numbers")

              (is (= (q/make v)
                     (q/make (concat v [10 11 12])))
                  "make ignores elements beyond the 4th"))

    (checking "to, from complex" 100 [[r i j k] (gen/vector sg/real 4)]
              (let [c1 (sc/complex r i)
                    c2 (sc/complex j k)
                    q  (q/from-complex c1 c2)]
                (is (= c1 (q/complex-1 q))
                    "complex-1 returns the input first complex part")

                (is (= c2 (q/complex-2 q))
                    "complex-2 returns the input second complex part")

                (is (= [c1 c2] (q/->complex-pair q))
                    "->complex-pair gets both in one shot")))

    (with-comparator (v/within 1e-10)
      (checking "spherical" 100 [[r theta colat lon]
                                 (gen/vector (sg/reasonable-real 1e3) 4)]
                (let [q (q/spherical r theta colat lon)]
                  (is (ish? (g/abs r) (g/abs q))
                      "magnitude equals r")

                  (let [axis [(* (g/sin colat) (g/cos lon))
                              (* (g/sin colat) (g/sin lon))
                              (* (g/cos colat))]]
                    (is (ish? (q/spherical r theta colat lon)
                              (-> (q/from-angle-axis theta axis)
                                  (q/scale r)))
                        "spherical is identical to building a spherical-coordinate
                      axis and using from-angle-axis.")))))

    (checking "semipolar" 100 [[r alpha theta1 theta2]
                               (gen/vector (sg/reasonable-real 1e3) 4)]
              (let [q       (q/semipolar r alpha theta1 theta2)
                    [c1 c2] (q/->complex-pair q)
                    c1-mag  (g/* r (g/cos alpha))
                    c2-mag  (g/* r (g/sin alpha))]
                (is (ish? (g/abs r) (g/abs q))
                    "magnitude equals r")

                (testing "magnitudes match"
                  (is (ish? (g/abs c1-mag) (g/magnitude c1)))
                  (is (ish? (g/abs c2-mag) (g/magnitude c2))))

                (testing "angles match"
                  (is (ish? (g/angle (g/make-polar c1-mag theta1))
                            (g/angle c1)))
                  (is (ish? (g/angle (g/make-polar c2-mag theta2))
                            (g/angle c2))))))

    (checking "multipolar" 100 [[r1 theta1 r2 theta2]
                                (gen/vector (sg/reasonable-real 1e3) 4)]
              (let [q (q/multipolar r1 theta1 r2 theta2)
                    [c1 c2] (q/->complex-pair q)]
                (is (ish? (g/make-polar r1 theta1) c1))
                (is (ish? (g/make-polar r2 theta2) c2))))

    (with-comparator (v/within 1e-12)
      (checking "cylindrospherical" 100 [[t r theta phi]
                                         (gen/vector (sg/reasonable-real 1e3) 4)]
                (is (ish? (g/+ t (g/* r (q/spherical 1 Math/PI theta phi)))
                          (q/cylindrospherical t r theta phi))
                    "building cylindrospherical matches building spherical,
                    scaling by `r` and ADDING the real component.")))

    (checking "cylindrical" 100 [[mag angle j k]
                                 (gen/vector (sg/reasonable-real 1e3) 4)]
              (let [q (q/cylindrical mag angle j k)]
                (is (ish? (g/make-polar mag angle)
                          (q/complex-1 q)))

                (is (ish? (g/make-rectangular j k)
                          (q/complex-2 q)))))))

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

  (testing "generic handles zeros"
    (is (= q/ONE (g/+ q/ZERO 1))
        "addition handles zero structures on the left")

    (is (= q/ZERO (g/* q/ONE q/ZERO))
        "mul handles zero structures on the left")

    (is (= (g/negate q/ONE) (g/- q/ZERO 1))
        "sub handles zero structures on the left"))

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
                "q/s matches quaternion division")

            (is (= (g/simplify (g/div s x))
                   (g/simplify (g/* s (g/invert x))))
                "s/q matches quaternion division")

            (is (= (g/simplify (g/solve-linear (q/make s) x))
                   (g/simplify  (g/solve-linear s x)))
                "solve-linear scalar matches q*q")

            (is (= (g/simplify (g/solve-linear-right x (q/make s)))
                   (g/simplify  (g/solve-linear-right x s)))
                "solve-linear-right scalar matches q*q"))

  (testing "unit tests from failed generative"
    (let [c #sicm/complex [-1.0 -2.0]
          x #sicm/quaternion ['a 'b 'c 'd]]
      (is (= (g/simplify (g/solve-linear (q/make c) x))
             (g/simplify (g/solve-linear c x)))
          "solve-linear complex matches q*q unit")))

  (checking "complex + quaternion arithmetic matches quaternion-only
            implementations." 100
            [c sg/complex x (sg/quaternion sg/symbol)]
            (is (= (g/simplify (g/solve-linear (q/make c) x))
                   (g/simplify  (g/solve-linear c x)))
                "solve-linear complex matches q*q")

            (is (= (g/simplify (g/solve-linear x (q/make c)))
                   (g/simplify  (g/solve-linear x c)))
                "solve-linear complex matches q*q, c right")

            (is (= (g/simplify (g/solve-linear-right (q/make c) x))
                   (g/simplify  (g/solve-linear-right c x)))
                "solve-linear-right complex matches q*q")

            (is (= (g/simplify (g/solve-linear-right x (q/make c)))
                   (g/simplify  (g/solve-linear-right x c)))
                "solve-linear-right complex matches q*q, c right")

            (is (= (g/simplify (g/dot-product (q/make c) x))
                   (g/simplify  (g/dot-product c x)))
                "dot-product complex matches q*q")

            (is (= (g/simplify (g/dot-product x (q/make c)))
                   (g/simplify  (g/dot-product x c)))
                "dot-product complex matches q*q, c right"))

  (checking "multi-arg arithmetic" 100
            [xs (gen/vector (sg/quaternion) 0 20)]
            (is (v/= (apply g/+ xs)
                     (apply q/add xs))
                "q/add matches g/+ for all arities")

            (is (v/= (apply g/- xs)
                     (apply q/sub xs))
                "q/sub matches g/- for all arities")

            (is (v/= (apply g/* xs)
                     (apply q/mul xs))
                "q/mul matches g/* for all arities")

            (is (v/= (apply g// xs)
                     (apply q/div xs))
                "q/div matches g// for all arities"))

  (checking "q/conjugate" 100 [x (sg/quaternion
                                  #?(:clj sg/any-integral
                                     :cljs sg/native-integral))]
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

  (testing "normalize"
    (doseq [quat [q/ZERO q/ONE q/I q/J q/K]]
      (is (= quat (q/normalize quat)))))

  (checking "q/dot-product, q/normalize, q/magnitude" 100
            [x (sg/quaternion #?(:clj sg/small-integral
                                 :cljs sg/native-integral))]
            (is (= (q/magnitude-sq x)
                   (q/dot-product x x))
                "dot product of a quaternion with itself == the square of its
                magnitude")

            (is (ish? (q/dot-product x x)
                      (g/square
                       (q/magnitude x)))
                "the dot-product of a quaternion with itself equals its squared
                magnitude.")

            (let [[x-complex _] (q/->complex-pair x)
                  x-real        (q/real-part x)]
              (is (v/= (g/dot-product x x-complex)
                       (g/dot-product x-complex x))
                  "quaternion dots with complex")

              (is (= (g/dot-product x x-complex)
                     (g/dot-product x-complex x-complex))
                  "quaternion dots with complex")


              (is (= (g/dot-product x x-real)
                     (g/dot-product x-real x)
                     (g/dot-product x-real x-real))
                  "quaternion dots with real"))

            (let [m      (q/magnitude x)
                  normal (q/normalize x)]
              (is (ish? normal (q/normalize normal)))
              (if (v/zero? m)
                (is (q/zero? x)
                    "can't normalize if the quaternion is zero.")

                (is (q/unit? normal :epsilon 1e-8)
                    "normalizing a quaternion makes it (approximately) a unit
                      quaternion."))))

  (let [gen-q #?(:cljs (sg/quaternion sg/native-integral)
                 :clj (sg/quaternion sg/any-integral))]
    (checking "cross-product" 100
              [q1 gen-q
               q2 gen-q]
              (let [q1xq2 (g/cross-product q1 q2)]
                (is (q/pure? q1xq2)
                    "quaternion cross product has no real component")

                (is (v/zero? (g/dot-product q1 q1xq2))
                    "dot of quaternion with an orthogonal quaternion == 0")

                (testing "cross with scalar"
                  (is (= q/ZERO (g/cross-product (q/get-r q1) q2))
                      "zero result")

                  (is (ish? (g/cross-product (q/get-r q1) q2)
                            (g/cross-product (q/make (q/get-r q1)) q2))
                      "real on left vs q on both sides, real-only left")

                  (is (= q/ZERO (g/cross-product q1 (q/get-r q2)))
                      "real on right")
                  (is (= (g/cross-product q1 (q/make (q/get-r q2)))
                         (g/cross-product q1 (q/get-r q2)))
                      "real on right vs q on both sides, real-only right"))

                (testing "cross with complex"
                  (is (ish? (g/cross-product (q/complex-1 q1) q2)
                            (g/cross-product (q/make (q/complex-1 q1)) q2))
                      "complex on left vs q on both sides, complex-1-only left")

                  (is (ish? (g/cross-product q1 (q/complex-1 q2))
                            (g/cross-product q1 (q/make (q/complex-1 q2))))
                      "complex on right vs q on both sides, complex-1-only right")))))

  (testing "commutator"
    (let [p (q/make 'r1 'i1 'j1 'k1)
          q (q/make 'r2 'i2 'j2 'k2)]
      (is (q/zero?
           (g/simplify
            (q/commutator p p)))
          "the commutator of a vector with itself is zero")

      (is (q/zero?
           (g/simplify
            (g/- (q/commutator p q)
                 (g/cross-product  (g/* 2 p) q))))
          "commutator identity")

      (is (= #sicm/quaternion
             [0
              '(+ (* 2 j1 k2) (* -2 j2 k1))
              '(+ (* -2 i1 k2) (* 2 i2 k1))
              '(+ (* 2 i1 j2) (* -2 i2 j1))]
             (g/simplify
              (q/commutator
               (q/make 'r1 'i1 'j1 'k1)
               (q/make 'r2 'i2 'j2 'k2))))
          "commutator is only 0 when
           j_1 k_2 == j_2 k_1,
           i_1 k_2 == i_2 k_1,
           i_1 j_2 == i_2 j_1,

           which is of course true for any form of 'complex numbers' built from
           quaternions, where 2 components are 0.")))

  (checking "q/commutator" 100
            [q1 (sg/quaternion sg/small-integral)
             q2 (sg/quaternion sg/small-integral)]
            (is (v/zero?
                 (q/commutator
                  (q/make (first (q/->complex-pair q1)))
                  (q/make (first (q/->complex-pair q2)))))
                "complex multiplication commutes, so the commutator of the
                complex part (r,i) is always zero.")

            (is (v/zero?
                 (q/commutator
                  (q/make (q/real-part q1))
                  q2))
                "real quaternions commute with all other quaternions")

            (is (v/zero?
                 (q/commutator
                  q1
                  (q/make (q/real-part q2))))
                "real quaternions commute with all other quaternions")))

(deftest transcendental-tests
  (testing "exp, log, expt"
    (with-comparator (v/within 1e-4)
      (checking "x^(a+b) == x^a x^b holds for all b when a is a real
                      quaternion."
                20 [r  (gen/choose 1 10)
                    q2 (sg/quaternion (gen/choose 1 10))]
                (let [q1 (q/make r)]
                  (is (ish? (g/* (g/exp q1)
                                 (g/exp q2))
                            (g/exp (g/+ q1 q2)))))))

    (checking
     "exp, log match real and complex impls" 100 [x sg/complex]
     (let [quat (q/make x)]
       (with-comparator (v/within 1e-6)
         (is (ish? (g/log x) (g/log quat))
             "complex log matches quat log when j==k==0"))

       (is (ish? (g/exp x) (g/exp quat))
           "complex exp matches quat exp when j==k==0")

       (is (ish? (g/log (q/get-r quat))
                 (g/log (q/make (q/get-r quat))))
           "real log matches quat log when i==j==k==0")

       (is (ish? (g/exp (q/get-r quat))
                 (g/exp (q/make (q/get-r quat))))
           "real exp matches quat exp when i==j==k==0")))

    (testing "q/log unit tests"
      (is (= '(quaternion (log y) (* (/ 1 2) pi) 0 0)
             (v/freeze
              (g/simplify
               (q/log (q/make 0 'y 0 0)))))
          "this test failed before a fix in `emmy.numsymb` forced atan to
        return an exact value of `pi/2` instead of a floating point number.")

      (is (= '(quaternion (log y) 0 0 0)
             (v/freeze
              (g/simplify
               (q/log (q/make 'y 0 0 0)))))
          "note that symbolic log on a real quaternion generates a symbolic real
      entry in the real position."))

    (let [double-gen (sg/reasonable-double
                      {:min -1e3
                       :max 1e3
                       :excluded-lower -1
                       :excluded-upper 1})
          gen (sg/quaternion double-gen)]
      (with-comparator (v/within 1e-4)
        (checking "exp/log" 100 [x gen]
                  (is (ish? x (q/exp (q/log x)))
                      "exp(log(q)) acts as identity"))

        (checking "expt/sqrt" 100 [x gen]
                  (let [r      (q/get-r x)
                        pos-r  (g/abs r)
                        sqrt-r (g/sqrt pos-r)]
                    (if (g/negative? r)
                      (is (= (q/make 0 sqrt-r 0 0)
                             (g/sqrt (q/make r)))
                          "sqrt of a negative real quaternion populates the i
                        slot only")

                      (is (= (q/make sqrt-r 0 0 0)
                             (g/sqrt (q/make r)))
                          "sqrt of a positive real quaternion populates the r
                        slot only")))

                  (is (ish? x (q/mul
                               (q/sqrt x)
                               (q/sqrt x)))
                      "q == sqrt(q)*sqrt(q)")

                  (is (ish? (q/sqrt x)
                            (q/expt x 0.5))
                      "sqrt(q) == q^0.5")

                  (is (ish? (q/mul x x)
                            (q/expt x 2))
                      "q*q == q^2, expt impl matches manual exponentiation")

                  (is (q/one? (g/expt x q/ZERO))
                      "x to the quaternion 0 power == 1")

                  (is (ish? x (g/expt x q/ONE))
                      "x to the quaternion 1 power is approx= x"))))

    (testing "q/expt unit tests"
      (let [i**i (g/expt sc/I sc/I)]
        (is (= i**i
               (g/expt q/I q/I)
               (g/expt q/J q/J)
               (g/expt q/K q/K))
            "i^i matches quaternion implementation for each of the components.")

        (is (ish? q/K (g/expt q/I q/J)))
        (is (ish? q/I (g/expt q/J q/K)))
        (is (ish? q/J (g/expt q/K q/I))))))

  (testing "cos^2(x) + sin^2(x) ~== 1"
    (with-comparator (v/within 1e-10)
      (doseq [x [(q/make 2 0 0 0)
                 (q/make 2 2 0 0)
                 (q/make 2 2 2 0)
                 (q/make 1 1 1 1 )]]
        (is (ish? q/ONE
                  (g/+ (g/square (g/cos x))
                       (g/square (g/sin x))))))))

  (testing "transcendental functions match complex implementation"
    (doseq [f [g/log g/exp g/sin g/cos g/tan g/sinh g/cosh g/tanh]]
      (testing "tan matches complex"
        (is (ish? (f sc/I)
                  (f q/I)))

        (is (= (q/get-i (f q/I))
               (q/get-j (f q/J))
               (q/get-k (f q/K)))))))

  (testing "transcendental unit"
    (is (= q/ONE (q/cos q/ZERO)))
    (is (= q/ZERO (q/sin q/ZERO)))
    (is (= q/ZERO (q/tan q/ZERO))))

  (testing "unit tests ported from Boost's quaternion test suite"
    (let [q4 (q/make 34 56 20 2)]
      (is (= (q/make -321776 -4032, -1440, -144)
             (g/expt q4 3))
          "exponents with native integral powers stay exact")

      (is (ish? #sicm/quaternion
                [-730823.7637667366
                 -156449.1960650097
                 -55874.71288036061
                 -5587.471288036061]
                (g/expt q4 3.2))
          "not so much for fractional exponents")

      (testing "transcendental functions"
        (is (ish? #sicm/quaternion
                  [-5.727001093501774E14
                   1.0498682596332112E14
                   3.749529498690041E13
                   3.7495294986900405E12]
                  (g/exp q4)))

        (is (ish? #sicm/quaternion
                  [1.8285331065398575E25
                   -2.7602822237164246E25
                   -9.85815079898723E24
                   -9.85815079898723E23]
                  (g/sin q4)))

        (is (ish? #sicm/quaternion
                  [-2.932696308866326E25
                   -1.7210331032912269E25
                   -6.146546797468668E24
                   -6.146546797468668E23]
                  (g/cos q4)))

        (is (ish? #sicm/quaternion
                  [0.0
                   0.9412097036339402
                   0.3361463227264072
                   0.03361463227264068]
                  (g/tan q4)))

        (is (ish? #sicm/quaternion
                  [-2.863500546750887E14
                   5.249341298166056E13
                   1.8747647493450203E13
                   1.8747647493450203E12]
                  (q/sinh q4)))

        (is (ish? #sicm/quaternion
                  [-2.863500546750887E14
                   5.249341298166056E13
                   1.8747647493450203E13
                   1.8747647493450203E12]
                  (q/cosh q4)))

        (is (ish? #sicm/quaternion
                  [1.0
                   0.0
                   -6.288372600415926E-18
                   1.734723475976807E-18]
                  (q/tanh q4)))

        (is (ish? #sicm/quaternion
                  [-2.391804589431832E23
                   -4.179034275395878E23
                   -1.4925122412128137E23
                   -1.4925122412128143E22]
                  (g/sinc q4)))

        (is (ish? #sicm/quaternion
                  [-1.3666031202326084E12
                   3.794799638667254E12
                   1.3552855852383052E12
                   1.3552855852383054E11]
                  (g/sinhc q4)))))))

(deftest generic-tests
  (testing "any infinite? entry triggers infinite?"
    (is (g/infinite? (q/make ##Inf 0 0 0)))
    (is (g/infinite? (q/make 0 ##-Inf 0 0)))
    (is (g/infinite? (q/make 0 0 ##Inf 0)))
    (is (g/infinite? (q/make 0 0 0 ##Inf))))

  (checking "infinite?" 100 [q (sg/quaternion gen/nat)]
            (is (not (g/infinite? q)))))

(defn v:make-unit
  "Normalizes the supplied vector."
  [v]
  (g/* (g/invert (g/abs v))
		   v))

(deftest rotation-tests
  (is (= '(up theta
              (up x y (sqrt (+ (* -1 (expt x 2))
                               (* -1 (expt y 2))
                               1))))
         (v/freeze
          (g/simplify
           (q/->angle-axis
            (q/from-angle-axis
             'theta
             ['x 'y (g/sqrt (g/- 1 (g/square 'x) (g/square 'y)))])))))))

(deftest complex-matrix-rep-tests
  (checking "to and from complex matrices" 100
            [x (sg/quaternion)]
            (is (= x (q/from-complex-matrix
                      (q/->complex-matrix x)))))

  (checking "to and from complex matrices" 100
            [x (sg/quaternion)]
            (is (= (q/->complex-pair x)
                   (first
                    (q/->complex-matrix x)))
                "The first row of the associated complex matrix matches
                ->complex-pair.")) )

(deftest four-by-four-matrix-tests
  (checking "to and from 4x4 matrices" 100 [x (sg/quaternion)]
            (is (= x (q/from-4x4-matrix
                      (q/->4x4-matrix x)))))

  (checking "Building quaternions out of 4x4 matrices" 100 [x (sg/quaternion)]
            (let [[r i j k] x
                  M (q/->4x4-matrix x)]
              (is (= M (g/+
                        (g/* r q/ONE-matrix)
                        (g/* i q/I-matrix)
                        (g/* j q/J-matrix)
                        (g/* k q/K-matrix)))
                  "Fine, this is the definition, BUT it's an important
                  relationship!"))))

(deftest tensor-tests
  (checking "Building quaternions out of tensors" 100 [x (sg/quaternion)]
            (let [[r i j k] x]
              (is (= (m/->structure
                      (q/->4x4-matrix x))
                     (g/+
                      (g/* r q/ONE-tensor)
                      (g/* i q/I-tensor)
                      (g/* j q/J-tensor)
                      (g/* k q/K-tensor)))
                  "Build up the tensor, check that it matches the matrix
                  version."))))

(deftest rotation-matrix-tests
  (checking "to and from 3x3 rotation matrices" 100
            [x (sg/quaternion)]
            (let [x (q/normalize x)]
              (is (ish? (q/->rotation-matrix x)
                        (q/->rotation-matrix
                         (q/from-rotation-matrix
                          (q/->rotation-matrix x))))
                  "Ending in matrix land gets rid of ambiguities about which
                direction to rotate when there's a tie."))))
