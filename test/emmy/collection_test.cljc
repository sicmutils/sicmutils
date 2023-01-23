#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.collection-test
  (:require [clojure.test :refer [is deftest testing]]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]]
            [same :refer [ish?]]
            [emmy.calculus.derivative :refer [D]]
            [emmy.collection :as collection]
            [emmy.complex :refer [complex I]]
            [emmy.differential :as d]
            [emmy.function :as f]
            [emmy.generators :as sg]
            [emmy.generic :as g]
            [emmy.laws :as laws]
            [emmy.util :as u]
            [emmy.value :as v]))

(deftest vector-tests
  (testing "Vector protocol implementations"
    (checking "f/arity" 100 [v (gen/vector sg/any-integral)]
              (is (= [:between 1 2] (f/arity v))
                  "vectors respond to f/arity correctly"))

    (checking "v/zero-like" 100
              [v (gen/vector sg/number)]
              (let [zero-v (v/zero-like v)]
                (is (vector? zero-v)
                    "still a vector!")

                (is (v/zero? zero-v)
                    "zero? works")

                (is (every? v/zero? zero-v)
                    "zero-like zeros out all values.")))

    (checking "v/kind, one?, identity?" 100 [v (gen/vector sg/any-integral)]
              (is (not (v/one? v))
                  "no vector is a multiplicative identity.")

              (is (not (v/identity? v))
                  "no vector is a multiplicative identity!")

              (is (= (v/kind v) (type v))
                  "Kind reflects type back out."))

    (testing "v/one-like, v/identity-like return 1, the multiplicative identity for vectors"
      (is (= 1 (v/one-like [1 2 3])))
      (is (= 1 (v/identity-like [1 2 3])))

      (is (thrown? #?(:clj UnsupportedOperationException :cljs js/Error)
                   (v/identity-like {:k "v"}))))

    (checking "v/exact?" 100
              [v (gen/vector sg/any-integral)]
              (is (v/exact? v)
                  "all integral values == exact vector")

              (is (not (v/exact? (conj v 1.5)))
                  "conj-ing an inexact value removes the exact? designation"))

    (testing "v/freeze"
      (is (= '(up (/ 1 2))
             (v/freeze [#sicm/ratio 1/2]))
          "v/freeze freezes entries"))))

(deftest sequence-tests
  (testing "sequence protocol impls"
    (let [zeros (v/zero-like (range 10))]
      (is (seq? zeros)
          "The output is indeed a seq, not a vector.")

      (is (every? v/zero? zeros)
          "v/zero-like lazily zeroes all entries")

      (is (not (v/zero? zeros))
          "to return true, this predicate would have to realize the full
          sequence... so instead it returns false.")

      (is (every? v/zero? (v/zero-like (map inc (range 10))))
          "works with a non-Range type")

      (is (every? v/zero? (v/zero-like (list 1 2 3)))
          "works with lists"))))

(deftest map-tests
  (let [map-gen (gen/map gen/keyword sg/real {:max-elements 5})]
    (laws/additive-group 100 map-gen "Map"
                         :commutative? true))

  (checking "map addition merges keys" 100
            [m1 (gen/map gen/keyword sg/real {:max-elements 5})
             m2 (gen/map gen/keyword sg/real {:max-elements 5})]
            (is (= (u/keyset (g/add m1 m2))
                   (g/add (u/keyset m1)
                          (u/keyset m2)))))

  (checking "map is a sparse vector space over complex" 100
            [m1 (gen/map gen/keyword sg/real {:max-elements 5})
             m2 (gen/map gen/keyword sg/real {:max-elements 5})
             x sg/real]
            (is (ish? (u/map-vals #(g/* x %) m1)
                      (g/* x m1))
                "mult pushes into values")

            (when-not (v/zero? x)
              (is (ish? (u/map-vals #(g// % x) m1)
                        (g/divide m1 x))
                  "division by scalar pushes into values"))

            (is (v/= (g/* m1 x)
                     (g/* x m1))
                "multiplication by field element is commutative")

            (is (ish? (g/add (g/* x m1)
                             (g/* x m2))
                      (g/* x (g/add m1 m2)))
                "multiplication distributes over group addition"))

  (checking "g/make-{rectangular,polar} on maps" 100
            [m (gen/map gen/keyword sg/real {:max-elements 5})]
            (is (= m (g/make-rectangular m {}))
                "make-rectangular with no imaginary parts is identity.")

            (is (ish? (g/* m I)
                      (g/make-rectangular {} m))
                "every entry turns turns imaginary!")

            (is (= m (g/make-polar m {}))
                "make-polar with no angles is identity.")

            (is (ish? (v/zero-like m)
                      (g/make-polar {} m))
                "if all angles comes from m, but every radius is 0, then the
                resulting entries will be zero.")

            (is (= m (g/real-part m))
                "real-part on all real is id.")

            (is (ish? (v/zero-like m)
                      (g/imag-part m))
                "imag-part on all real is zeor-like.")

            (is (ish? m (g/imag-part
                         (g/make-rectangular m m)))
                "imag-part recovers all imaginary pieces")

            (is (ish? m (g/real-part
                         (g/make-rectangular m m)))
                "real-part"))

  (checking "g/make-rectangular round trip on maps" 100
            [m (gen/map gen/keyword sg/complex {:max-elements 5})]
            (is (= m (g/make-rectangular
                      (g/real-part m)
                      (g/imag-part m)))))

  (testing "make-rectangular etc for maps, unit"
    (is (v/= {:a (g/make-rectangular 1 2)
              :b 1
              :c (g/make-rectangular 0 1)}
             (g/make-rectangular {:a 1 :b 1}
                                 {:a 2 :c 1})))

    (is (v/= {:a (g/make-polar 1 2)
              :b 1
              :c (g/make-polar 0 1)}
             (g/make-polar {:a 1 :b 1}
                           {:a 2 :c 1}))))

  (testing "Map protocol implementations"
    (testing "sorted map"
      (let [m (sorted-map 1 2 3 4)]
        (is (= (type m) (v/kind m))
            "This would fail without special handling when the `Value`
            implementation attempts to pass a `:type` keyword to a sorted map
            containing numbers, since keywords and numbers don't compare.")))

    (checking "f/arity" 100 [m (gen/map gen/keyword sg/any-integral)]
              (is (= [:between 1 2] (f/arity m))
                  "maps respond to f/arity correctly"))

    (checking "v/zero-like" 100
              [m (gen/map gen/keyword sg/number)]
              (let [zero-m (v/zero-like m)]
                (is (v/zero? zero-m)
                    "zero? works")

                (is (every? v/zero? (vals zero-m))
                    "zero-like zeros out all values.")

                (is (= (u/keyset m) (u/keyset zero-m))
                    "The keyset is identical after zeroing.")))

    (checking "v/kind, one?, identity?" 100 [m (gen/map gen/keyword sg/any-integral)]
              (is (not (v/one? m))
                  "no map is a multiplicative identity.")

              (is (not (v/identity? m))
                  "no map is a multiplicative identity.")

              (is (isa? (v/kind m) ::collection/map)
                  "All maps inherit from this new keyword.
                   TODO should this in value, with ::v/function and friends?"))

    (testing "v/one-like, v/identity-like throw"
      (is (thrown? #?(:clj UnsupportedOperationException :cljs js/Error)
                   (v/one-like {:k "v"})))

      (is (thrown? #?(:clj UnsupportedOperationException :cljs js/Error)
                   (v/identity-like {:k "v"}))))

    (checking "v/exact?" 100
              [m (gen/map gen/keyword sg/any-integral)]
              (is (v/exact? m)
                  "all integral values == exact map")

              (is (not (v/exact? (assoc m :key 1.5)))
                  "adding an inexact key removes the exact? designation"))

    (testing "v/freeze"
      (is (= {:ratio '(/ 1 2)}
             (v/freeze {:ratio #sicm/ratio 1/2}))
          "v/freeze freezes values"))

    (testing "v/= on collections"
      #?(:cljs
         (testing "in cljs, clojure.core/= can do the right thing for nested
             values, since we've overridden equality of numbers."
           (is (= {:one 1
                   :two 2}
                  {:one (complex 1 0)
                   :two (complex 2 0)}))
           (is (= [1 2] [(complex 1 0)
                         (complex 2 0)]))))

      (testing "Both jvm and js work with v/=."
        (is (v/= {:one 1
                  :two 2}
                 {:one (complex 1 0)
                  :two (complex 2 0)}))

        (is (v/= [1 2] [(complex 1 0)
                        (complex 2 0)])))

      (is (not (v/= {:one 1 :two 2}
                    {:one (complex 1 0)
                     :two-prime (complex 2 0)}))
          " with unequal keys, we fail.")))

  (checking "d/perturbed?" 100
            [m (gen/map gen/keyword sg/any-integral)]
            (is (not (d/perturbed? m))
                "maps with no [[Differential]] aren't perturbed.")

            (let [diff (d/bundle-element 1 1 0)]
              (is (d/perturbed? (assoc m :key diff))
                  "adding a perturbed entry perturbs the map.")

              (is (d/perturbed?
                   {:outer-key
                    (assoc m :key diff)})
                  "d/perturbed? descends into keys")))

  (let [m {:sin g/sin :cos g/cos}
        {D-sin :sin D-cos :cos} (D m)]
    (is (= {:sin ((D g/sin) 'x)
            :cos ((D g/cos) 'x)}
           {:sin (D-sin 'x)
            :cos (D-cos 'x)})
        "derivatives get pushed inside maps.")))

(deftest set-tests
  (let [set-gen (gen/set gen/any-equatable {:max-elements 20})]
    (laws/additive-monoid 100 set-gen "Set"
                          :commutative? true))

  (testing "Set protocol implementations"
    (checking "f/arity" 100 [s (gen/set gen/any-equatable)]
              (is (= [:between 1 2] (f/arity s))
                  "sets respond to f/arity correctly"))

    (checking "v/zero-like works" 100
              [s (gen/set sg/number)]
              (let [zero-s (v/zero-like s)]
                (is (v/zero? zero-s))))

    (checking "v/kind, v/one?, v/identity?" 100 [s (gen/set sg/any-integral)]
              (is (not (v/one? s))
                  "no map is a multiplicative identity.")

              (is (not (v/identity? s))
                  "no map is a multiplicative identity.")

              (is (isa? (v/kind s) ::collection/set)
                  "All sets inherit from this new keyword."))

    (testing "v/one-like, v/identity-like throw"
      (is (thrown? #?(:clj UnsupportedOperationException :cljs js/Error)
                   (v/one-like #{"v"})))

      (is (thrown? #?(:clj UnsupportedOperationException :cljs js/Error)
                   (v/identity-like #{"V"}))))

    (checking "v/exact?" 100
              [m (gen/set sg/any-integral)]
              (is (not (v/exact? m))
                  "sets aren't exact."))

    (testing "v/freeze currently throws, since we don't have a way of rendering
    it or simplifying."
      (is (thrown? #?(:clj UnsupportedOperationException :cljs js/Error)
                   (v/freeze #{"v"}))))))
