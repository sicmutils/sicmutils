#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.differential-test
  (:require [clojure.test :refer [is deftest testing use-fixtures]]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]]
            [same :refer [ish? with-comparator] :include-macros true]
            [emmy.differential :as d]
            [emmy.generators :as sg]
            [emmy.generic :as g]
            [emmy.numerical.derivative :refer [D-numeric]]
            [emmy.simplify :refer [hermetic-simplify-fixture]]
            #?(:cljs [emmy.util :as u])
            [emmy.value :as v]))

(use-fixtures :each hermetic-simplify-fixture)

(defn- derivative
  "A bare-bones derivative implementation, used for testing the functionality made
  available by [[Differential]]. The real version lives
  at [[emmy.calculus.derivative/derivative]]"
  [f]
  (let [tag (d/fresh-tag)]
    (fn [x]
      (-> (f (d/bundle-element x 1 tag))
          (d/extract-tangent tag)))))

(defn nonzero [gen]
  (gen/fmap (fn [x]
              (if (= x 0)
                (v/one-like x)
                x))
            gen))

(def real-diff-gen
  (sg/differential))

(def integral-diff-gen
  (sg/differential gen/nat))

(def terms-gen
  (gen/fmap #'d/bare-terms real-diff-gen))

(deftest term-arithmetic-tests
  (checking "term accessors work" 100 [tags (sg/vector-set)
                                       coef sg/real]
            (let [term (d/make-term tags coef)]
              (is (= tags (#'d/tags term)))
              (is (= coef (#'d/coefficient term)))
              (doseq [tag tags]
                (is (#'d/tag-in-term? term tag)))))

  (checking "terms:+ each term in random order returns original list" 100
            [terms terms-gen]
            (let [rebuilt (transduce (map vector)
                                     #'d/terms:+
                                     (shuffle terms))]
              (is (vector? rebuilt))
              (is (= terms rebuilt))))

  (checking "terms:+ returns sorted, well-formed terms" 500
            [l terms-gen, r terms-gen]
            (let [sum (#'d/terms:+ l r)]
              (is (= sum (sort-by #'d/tags sum)))))

  (testing "terms:+ removes zero coefs"
    (let [l [(d/make-term [1] 5)]
          r[(d/make-term [1] -5)]]
      (is (= [] (#'d/terms:+ l r)))))

  (checking "terms:* returns sorted, well-formed terms" 500
            [l terms-gen, r terms-gen]
            (let [product (#'d/terms:* l r)]
              (is (= product (sort-by #'d/tags product)))))

  (testing "terms:* removes zeros"
    (let [l [(d/make-term [1] 0)]
          r [(d/make-term [1] -5)]]
      (is (= [] (#'d/terms:* l r))))))

(deftest differential-type-tests
  (testing "v/numerical? special cases"
    (is (not (v/numerical? (d/from-terms {[] "face"}))))
    (is (v/numerical? (d/->Differential []))
        "An empty term list is interpreted as a 0-valued [[Differential]]."))

  (checking "native comparison operators work with differential" 100
            [l sg/real, r sg/real]
            (is (= (v/compare l r)
                   (v/compare (d/bundle-element l 1 0) r)
                   (v/compare l (d/bundle-element r 1 0)))))

  #?(:cljs
     (testing "comparison unit tests"
       (is (= 0 (d/bundle-element 0 1 0)))
       (is (= (u/bigint 0) (d/bundle-element 0 1 0)))
       (is (= (u/long 0) (d/bundle-element 0 1 0)))
       (is (= (u/int 0) (d/bundle-element 0 1 0)))))

  #?(:cljs
     ;; NOTE: JVM Clojure won't allow non-numbers on either side of < and
     ;; friends. Once we implement `v/<` we can duplicate this test for those
     ;; overridden versions, which should piggyback on compare.
     (let [real-minus-rationals (gen/one-of [sg/any-integral (sg/reasonable-double)])]
       (checking "[[Differential]] is transparent to native comparison operators" 100
                 [[l-num r-num] (gen/vector real-minus-rationals 2)]
                 (let [compare-bit (v/compare l-num r-num)]
                   (doseq [l [l-num (d/bundle-element l-num 1 0)]
                           r [r-num (d/bundle-element r-num 1 0)]]
                     (cond (neg? compare-bit)  (is (< l r))
                           (zero? compare-bit) (is (and (<= l r) (= l r) (>= l r)))
                           :else (is (> l r))))))))

  (checking "v/numerical?" 100 [diff (sg/differential sg/real)]
            (is (v/numerical? diff)
                "True for all differentials populated by v/numerical? things"))

  (testing "value protocol implementation"
    (let [zero (d/->Differential [])
          dy (d/from-terms {[] 0, [1] 1})]
      (is (v/zero? zero)
          "zero? returns true for an empty term list")

      (is (v/zero? (d/from-terms [(d/make-term [] 0)]))
          "zero? returns true for an explicit zero")

      (is (not (v/zero? dy))
          "the finite term is 0, but `v/zero?` fails if any perturbation is
          non-zero.")

      (is (= dy 0)
          "subtly, `dy` IS in fact equal to zero; this can be used for control
      flow.")

      (testing "v/one? only responds true to a one primal if all tangents are zero."
        (is (v/one? (d/from-terms {[] 1})))
        (is (v/one? (d/from-terms {[] 1 [1] 0})))
        (is (not (v/one? (d/from-terms {[] 1 [1] 1})))))

      (testing "v/identity? only responds true to an `identity` primal if all
      tangents are zero."
        (is (v/identity? (d/from-terms {[] 1})))
        (is (v/identity? (d/from-terms {[] 1 [1] 0})))
        (is (not (v/identity? (d/from-terms {[] 1 [1] 1})))))


      (checking "*-like works" 100 [diff real-diff-gen]
                (is (v/zero? (v/zero-like diff)))
                (is (v/one? (v/one-like diff)))
                (is (v/identity? (v/identity-like diff))))

      (testing "equality, comparison"
        (checking "g/negative?, g/infinite?" 100 [x sg/real]
                  (let [elem (d/bundle-element x 1 0)]
                    (is (= (g/negative? x)
                           (g/negative? elem))
                        "negative? operates on finite-part only.")

                    (is (not (g/infinite? elem))
                        "infinite? is always false for real finite parts.")))

        (testing "g/infinite?"
          (is (not (g/infinite? (d/bundle-element 10 ##Inf 0)))
              "g/infinite? only looks at the finite part right now. Not sure how
              we would get into an infinite derivative with non-infinite finite
              part, but marking this test here as documentation.")

          (is (every?
               g/infinite?
               [(d/bundle-element ##-Inf 1 0)
                (d/bundle-element ##Inf 1 0)])
              "an infinite or negative infinite value in the finite part slot
               makes the differential `g/infinite?`"))

        (checking "=, equiv ignore tangent parts" 100
                  [n sg/real-without-ratio]
                  (is (= (d/bundle-element n 1 0) n)
                      "differential on the left works.")

                  #?(:cljs
                     (is (= n (d/bundle-element n 1 0))
                         "differential on the right works in CLJS."))

                  (is (d/equiv (d/bundle-element n 1 0) n n (d/bundle-element n 1 0) n)
                      "d/equiv matches = behavior, varargs"))

        (checking "eq uses tangent parts" 100 [n sg/real]
                  (is (d/eq (d/bundle-element n 0 0) n n (d/bundle-element n 0 0) n)
                      "eq is true with no tangent, varargs")

                  (is (not (d/eq (d/bundle-element n 1 0) n))
                      "eq is FALSE with a tangent, bundle on left")

                  (is (not (d/eq n (d/bundle-element n 1 0)))
                      "eq is FALSE with a tangent, bundle on right")

                  (is (d/eq (d/bundle-element n 1 0) (d/bundle-element n 1 0))
                      "d/eq handles equality")

                  (is (not (d/eq (d/bundle-element n 1 0) (d/bundle-element n 2 0)))
                      "d/eq is false for [[Differential]]s with diff tags"))

        (checking "compare ignores tangent parts" 100
                  [l sg/real, r sg/real]
                  (is (= (v/compare l r)
                         (v/compare (d/bundle-element l 1 0) r)
                         (v/compare l (d/bundle-element r 1 0)))
                      "differential works on either side.")

                  (is (= (d/compare l r)
                         (d/compare (d/bundle-element l r 0) r)
                         (d/compare l (d/bundle-element r l 0)))
                      "d/compare can handle non-differential on either side, also
                    ignores tangents.")

                  (testing "{d,v}/compare l, l matches equals behavior, ignores tangents"
                    (is (zero? (d/compare (d/bundle-element l r 0) l)))
                    (is (zero? (d/compare l (d/bundle-element l r 0))))
                    (is (zero? (v/compare (d/bundle-element l r 0) l)))
                    (is (zero? (v/compare l (d/bundle-element l r 0))))))

        (checking "compare-full takes tags into account" 100
                  [l sg/real]
                  (is (pos? (d/compare-full (d/bundle-element l 1 0) l))
                      "a [[Differential]] with a positive tangent is ALWAYS
                    greater than a non-[[Differential]] whatever the tangent.")

                  (is (neg? (d/compare-full l (d/bundle-element l 1 0)))
                      "a [[Differential]] with a positive tangent is ALWAYS
                    greater than a non-[[Differential]] whatever the tangent."))

        (testing "freeze, simplify, str"
          (let [not-simple (g/square
                            (g/square (d/bundle-element 'x 1 0)))]
            (is (= '[Differential
                     [[]  (expt x 4)]
                     [[0] (* 2 (expt x 2) 2 x)]]
                   (v/freeze not-simple))
                "A frozen differential freezes each entry")

            (is (= '[Differential
                     [[]  (expt x 4)]
                     [[0] (* 4 (expt x 3))]]
                   (v/freeze
                    (g/simplify not-simple)))
                "simplify simplifies each tangent term")

            (is (v/= "D[[] → (expt x 4) [0] → (* 4 (expt x 3))]"
                     (str (g/simplify not-simple)))
                "str representation properly simplifies.")))))))

(deftest differential-fn-tests
  (testing "differentials can take branches inside functions, PROVIDED (with
            clojure.core/=) the perturbed variable is on the
            left! (ClojureScript can handle equals on either side.)"
    (let [f (fn [x]
              (let [g (if #?(:clj (= x 10) :cljs (= 10 x))
                        (g/* x g/square)
                        (g/* x g/cube))]
                (g x)))
          Df (derivative f)]

      (is (= ((derivative (g/* identity g/square)) 10)
             (Df 10))
          "providing 10 takes the x*g/square branch")
      (is (= ((derivative (g/* identity g/cube)) 9)
             (Df 9))
          "providing 9 takes the x*g/cube branch"))))

(deftest active-tag-tests
  (testing "with-active-tag works"
    (is (not (d/tag-active? 'tag))
        "this tag is not active if it's not bound.")

    (is (= 6 (d/with-active-tag 'tag
               (fn [& xs]
                 (is (d/tag-active? 'tag)
                     "the supplied tag is active inside the scope of `f`.")
                 (reduce + xs))
               [1 2 3]))
        "d/with-active-tag calls its fn with the supplied args.")))

(deftest differential-arithmetic-tests
  (checking "(d:+ diff 0) == (d:+ 0 diff) == diff" 100 [diff real-diff-gen]
            (is (d/eq diff
                      (d/d:+ diff 0)
                      (d/d:+ 0 diff))))

  (checking "d:+ is associative given associative coefs"
            100 [[a b c] (gen/vector integral-diff-gen 3)]
            (is (d/eq (d/d:+ a (d/d:+ b c))
                      (d/d:+ (d/d:+ a b) c))
                "associative law"))

  (checking "d:+ is commutative given commutative coefs"
            100 [a real-diff-gen, b real-diff-gen]
            (is (d/eq (d/d:+ a b) (d/d:+ b a))
                "commutative law"))

  (checking "(d:+ diff diff) == (d:* 2 diff)" 100
            [diff integral-diff-gen]
            (is (d/eq (d/d:+ diff diff)
                      (d/d:* diff 2)
                      (d/d:* 2 diff))))

  (checking "(d:* diff 1) == (d:* 1 diff) == diff" 100
            [diff integral-diff-gen]
            (is (d/eq diff
                      (d/d:* diff 1)
                      (d/d:* 1 diff))
                "1 is a multiplicative identity"))

  (checking "(d:* diff 0) == (d:* 0 diff) == 0" 100 [diff real-diff-gen]
            (is (d/eq 0
                      (d/d:* diff 0)
                      (d/d:* 0 diff))
                "multiplying by 0 erases any diff"))

  (checking "d:* is associative given associative coefs"
            100 [[a b c] (gen/vector integral-diff-gen 3)]
            (is (d/eq (d/d:* a (d/d:* b c))
                      (d/d:* (d/d:* a b) c))
                "associative law"))

  (checking "d:* is commutative given commutative coefs"
            100 [[a b] (gen/vector integral-diff-gen 2)]
            (is (d/eq (d/d:* a b) (d/d:* b a))
                "commutative law"))

  (checking "adding primal and tangent separately matches adding full term" 100
            [[l r] (gen/vector real-diff-gen 2)]
            (let [[pl tl] (d/primal-tangent-pair l)
                  [pr tr] (d/primal-tangent-pair r)]
              (is (d/eq (d/d:+ l r)
                        (reduce
                         d/d:+ (shuffle [pl pr tl tr])))
                  "adding the pieces in any order works!")))

  (testing "map-coefficients"
    (let [diff (d/from-terms {[0] 1 [1] 2 [2] 3})]
      (is (= (d/from-terms {[1] 1 [2] 2})
             (d/map-coefficients dec diff))
          "the 0 coefficient term is filtered out.")))

  (testing "differential +, - unit tests"
    (let [dx     (d/from-terms {[0] 1})
          -dx    (d/from-terms {[0] -1})
          dy     (d/from-terms {[1] 1})
          dz     (d/from-terms {[2] 1})
          dxdy   (d/from-terms {[0 1] 1})
          dxdydz (d/from-terms {[0 1 2] 1})
          dx+dy  (d/from-terms {[0] 1, [1] 1})
          dx+dz  (d/from-terms {[0] 1, [2] 1})]
      (testing "d:+ is commutative"
        (is (d/eq dx+dy
                  (d/d:+ dx dy)
                  (d/d:+ dy dx)))
        (is (d/eq dx+dz
                  (d/d:+ dx dz)
                  (d/d:+ dz dx))))

      (is (d/eq (d/from-terms {[0] 3, [1] 2, [2] 3})
                (reduce d/d:+ [dx dy dz dy dz dx dz dx]))
          "d:+ groups terms appropriately")

      (testing "addition preserves primal"
        (is (d/eq (d/from-terms {[] 1, [0] 1}) (d/d:+ dx 1)))
        (is (d/eq (d/from-terms {[] 'k [0] 1}) (d/d:+ dx 'k))))

      (testing "various ways to get to zero"
        (is (v/zero? (d/d:+ dx -dx)))
        (is (v/zero? (d/d:+ -dx dx)))
        (is (v/zero? (d/d:* dx 0)))
        (is (v/zero? (d/d:* 0 dx)))
        (is (v/zero? (g/* dx dx))))

      (testing "associative, commutative multiplication"
        (is (d/eq dxdy (d/d:* dx dy)))
        (is (d/eq dxdydz (d/d:* (d/d:* dx dy) dz)))
        (is (d/eq dxdydz (d/d:* (d/d:* dz dx) dy)))
        (is (d/eq dxdydz (d/d:* (d/d:* dy dz) dx))))

      (testing "infinitesimals go to zero when multiplied!"
        (is (v/zero? (d/d:* dx dx))
            "dx^2==0")
        (is (v/zero? (d/d:* dz (d/d:* dy dz)))
            "dy*dz^2==0"))))

  (checking "(a/b)*b == a, (a*b)/b == a" 100
            [x integral-diff-gen
             y (nonzero integral-diff-gen)]
            (is (d/eq x (g/* (g// x y) y)))
            (is (d/eq x (g// (g/* x y) y))))

  (checking "solve-linear, div relationships" 100
            [x  real-diff-gen
             y (nonzero sg/real)]
            (let [y (d/bundle-element y 1 0)]
              (is (d/eq (g/solve-linear-left y x)
                        (g// x y)))

              (is (d/eq (g/solve-linear-left y x)
                        (g/solve-linear-right x y)))

              (is (d/eq (g/solve-linear-left y x)
                        (g/solve-linear y x)))))

  (testing "various unit tests with more terms"
    (let [tangent  (fn [dx] (d/extract-tangent dx 0))
          simplify (comp g/simplify tangent)]
      (is (v/= '(* 3 (expt x 2))
               (simplify
                (g/expt (g/+ 'x (d/bundle-element 0 1 0)) 3))))

      (is (v/= '(* 4 (expt x 3))
               (simplify
                (g/expt (g/+ 'x (d/bundle-element 0 1 0)) 4))))

      (let [dx   (d/bundle-element 0 1 0)
            x+dx (g/+ 'x dx)
            f    (fn [x] (g/* x x x x))]
        (is (v/= '(* 4 (expt x 3))
                 (simplify (g/* x+dx x+dx x+dx x+dx))))
        (is (v/= '(* 12 (expt x 2))
                 (simplify
                  (g/+ (g/* (g/+ (g/* (g/+ x+dx x+dx) x+dx)
                                 (g/* x+dx x+dx))
                            x+dx)
                       (g/* x+dx x+dx x+dx)))))

        (is (v/= '(* 24 x)
                 (simplify
                  (g/+
                   (g/* (g/+ (g/* 2 x+dx)
                             x+dx x+dx x+dx x+dx) x+dx)
                   (g/* (g/+ x+dx x+dx) x+dx)
                   (g/* x+dx x+dx)
                   (g/* (g/+ x+dx x+dx) x+dx)
                   (g/* x+dx x+dx)))))

        (is (= 24 (tangent
                   (g/+ (g/* 6 x+dx)
                        (g/* 2 x+dx)
                        x+dx x+dx x+dx x+dx
                        (g/* 2 x+dx)
                        x+dx x+dx x+dx x+dx
                        (g/* 2 x+dx)
                        x+dx x+dx x+dx x+dx))))

        (is (v/= '(* 4 (expt x 3))
                 (simplify (f x+dx))))))))

(deftest differential-api-tests
  (testing "bundle-element can flatten bundle-element primal, tangent"
    (is (d/eq (g/+ (g/sin (d/bundle-element 1 1 0))
                   (g/* (g/cos (d/bundle-element 1 1 0))
                        (d/bundle-element 0 1 1)))
              (d/bundle-element (g/sin (d/bundle-element 1 1 0))
                                (g/cos (d/bundle-element 1 1 0))
                                1))))

  (checking "to, from terms == id" 100
            [diff real-diff-gen]
            (is (d/eq diff
                      (-> (#'d/->terms diff)
                          (#'d/terms->differential)))))

  (checking "round-trip to terms and back" 100
            [n sg/real]
            (let [rt (-> (#'d/->terms n)
                         (#'d/terms->differential))]
              (is (d/eq n rt)
                  "round tripping doesn't change equality")
              (is (not (d/differential? rt))
                  "round trip keeps non-differentials as non-diff")))

  (letfn [(all-tags [diff]
            (into #{}
                  (mapcat #'d/tags)
                  (#'d/->terms diff)))]

    (checking "max-order-tag picks from the greatest of SOME term's tags" 100
              [diff real-diff-gen]
              (if (seq (all-tags diff))
                (let [top-tags (into #{} (map (comp peek #'d/tags)
                                              (#'d/->terms diff)))]
                  (is (contains?
                       top-tags (d/max-order-tag diff))
                      "the max order tag comes from the set of top tags,
                      gathered across all terms.")
                  (is (= (d/max-order-tag diff)
                         (peek (#'d/tags (last (#'d/->terms diff)))))
                      "the max tag of the LAST term matches the max order
                      tag."))

                (is (nil? (d/max-order-tag diff))
                    "if the tag set is totally empty, `max-order-tag` returns
                    nil.")))

    (checking "(extract-tangent diff) == dx*(tangent-part diff)" 100
              [diff real-diff-gen]
              (doseq [tag (all-tags diff)
                      :let [dx (d/bundle-element 0 1 tag)]]
                (is (d/eq (d/tangent-part diff tag)
                          (d/d:* dx (d/extract-tangent diff tag))))))

    (checking "(primal-tangent-pair diff) == [(primal-part diff) (tangent-part diff)]" 100
              [diff real-diff-gen]
              (let [[primal tangent] (d/primal-tangent-pair diff)]
                (is (d/eq primal (d/primal-part diff)))
                (is (d/eq tangent (d/tangent-part diff))))

              ;; with specified tag
              (doseq [tag (all-tags diff)]
                (let [[primal tangent] (d/primal-tangent-pair diff tag)]
                  (is (d/eq primal (d/primal-part diff tag)))
                  (is (d/eq tangent (d/tangent-part diff tag))))))

    (checking "finite-term matches primal-part for 1-tag differentials" 100
              [primal  (sg/reasonable-double)
               tangent (sg/reasonable-double)
               tag     gen/nat]
              (let [diff (d/bundle-element primal tangent tag)]
                (is (= primal (d/finite-term diff)))
                (is (= (d/finite-term diff)
                       (d/primal-part diff)))))

    (checking "finite-term fetches bottom primal-part" 100
              [diff real-diff-gen]
              (loop [diff diff]
                (let [primal (d/primal-part diff)]
                  (if (d/differential? primal)
                    (recur primal)
                    (is (= primal (d/finite-term diff))
                        "recursing to the bottom with primal-part gives the same
                         result as jumping straight there with
                         finite-term")))))))

(deftest lifted-fn-tests
  (letfn [(breaks? [f x]
            (is (thrown? #?(:clj IllegalArgumentException :cljs js/Error)
                         ((derivative f) x))))]
    (checking "integer-discontinuous derivatives work" 100 [x sg/real]
              (if (v/integral? x)
                (do (breaks? g/floor x)
                    (breaks? g/ceiling x)
                    (breaks? g/integer-part x)
                    (breaks? g/fractional-part x))

                (do (is (zero? ((derivative g/floor) x)))
                    (is (zero? ((derivative g/ceiling) x)))
                    (is (zero? ((derivative g/integer-part) x)))
                    (is (v/one? ((derivative g/fractional-part) x)))))))

  (testing "lift-n"
    (let [*   (d/lift-n g/* (fn [_] 1) (fn [_ y] y) (fn [x _] x))
          Df7 (derivative
               (fn x**7 [x] (* x x x x x x x)))
          Df1 (derivative *)
          Df0 (derivative (fn [_] (*)))]
      (is (v/= '(* 7 (expt x 6))
               (g/simplify (Df7 'x)))
          "functions created with lift-n can take many args (they reduce via the
          binary case!)")

      (is (= 1 (Df1 'x))
          "single-arity acts as id.")

      (is (zero? (Df0 'x))
          "zero-arity acts as constant")))

  (testing "exercise some of the lifted fns by comparing them to numeric
  derivatives."
    (let [f (fn [x]
              (g/+ (g/* (g/cos x) (g/sin x))
                   (g/+ (g/sin x) (g/expt x 2))
                   (g/+ (g/sin x) x)
                   (g/log (g/abs x))))
          Df         (derivative f)
          Df-numeric (D-numeric f)]
      (with-comparator (v/within 1e-6)
        (checking "exercise some lifted fns" 100
                  [n (gen/double*
                      {:infinite? false
                       :NaN? false
                       :min 1
                       :max 100})]
                  (is (ish? (Df-numeric n)
                            (Df n))
                      "Does numeric match autodiff?"))))))

(deftest sinc-etc-tests
  (is (zero? ((derivative g/sinc) 0)))
  (is (zero? ((derivative g/tanc) 0)))
  (is (zero? ((derivative g/sinhc) 0)))
  (is (zero? ((derivative g/tanhc) 0)))

  (letfn [(gen-double [min max]
            (gen/double*
             {:infinite? false
              :NaN? false
              :min min
              :max max}))]
    (with-comparator (v/within 1e-4)
      (checking "sinc" 100 [n (gen-double 1 50)]
                (is (ish? ((D-numeric g/sinc) n)
                          ((derivative g/sinc) n))))

      ;; attempting to limit to a region where we avoid the infinities at
      ;; multiples of pi/2 (other than 0).
      (checking "tanc" 100 [n (gen-double 0.01 (- (/ Math/PI 2) 0.01))]
                (is (ish? ((D-numeric g/tanc) n)
                          ((derivative g/tanc) n))))

      (checking "tanhc" 100 [n (gen-double 1 50)]
                (is (ish? ((D-numeric g/tanhc) n)
                          ((derivative g/tanhc) n)))))

    (with-comparator (v/within 1e-4)
      (checking "sinhc" 100 [n (gen-double 1 10)]
                (is (ish? ((D-numeric g/sinhc) n)
                          ((derivative g/sinhc) n)))))

    (with-comparator (v/within 1e-8)
      (checking "acot" 100 [n (gen-double 0.01 (- (/ Math/PI 2) 0.01))]
                (is (ish? ((D-numeric g/acot) n)
                          ((derivative g/acot) n))))

      (checking "asec" 100 [n (gen-double 3 100)]
                (is (ish? ((D-numeric g/asec) n)
                          ((derivative g/asec) n))))

      (checking "acsc" 100 [n (gen-double 3 100)]
                (is (ish? ((D-numeric g/acsc) n)
                          ((derivative g/acsc) n))))

      (checking "sech" 100 [n (gen-double 3 100)]
                (is (ish? ((D-numeric g/sech) n)
                          ((derivative g/sech) n))))

      (checking "coth" 100 [n (gen-double 1 3)]
                (is (ish? ((D-numeric g/coth) n)
                          ((derivative g/coth) n))))

      (checking "csch" 100 [n (gen-double 0.5 10)]
                (is (ish? ((D-numeric g/csch) n)
                          ((derivative g/csch) n))))

      (checking "acosh" 100 [n (gen-double 2 10)]
                (is (ish? ((D-numeric g/acosh) n)
                          ((derivative g/acosh) n))))

      (checking "asinh" 100 [n (gen-double 2 10)]
                (is (ish? ((D-numeric g/asinh) n)
                          ((derivative g/asinh) n))))

      (checking "atanh" 100 [n (gen-double 0.1 0.9)]
                (is (ish? ((D-numeric g/atanh) n)
                          ((derivative g/atanh) n))))

      (checking "acoth" 100 [n (gen-double 2 10)]
                (is (ish? ((D-numeric g/acoth) n)
                          ((derivative g/acoth) n))))

      (checking "asech" 100 [n (gen-double 0.1 0.9)]
                (is (ish? ((D-numeric g/asech) n)
                          ((derivative g/asech) n))))

      (checking "acsch" 100 [n (gen-double 2 10)]
                (is (ish? ((D-numeric g/acsch) n)
                          ((derivative g/acsch) n)))))))
