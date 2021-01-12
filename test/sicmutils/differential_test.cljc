;;
;; Copyright © 2020 Sam Ritchie.
;; This work is based on the Scmutils system of MIT/GNU Scheme:
;; Copyright © 2002 Massachusetts Institute of Technology
;;
;; This is free software;  you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or (at
;; your option) any later version.
;;
;;; This software is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this code; if not, see <http://www.gnu.org/licenses/>.
;;

(ns sicmutils.differential-test
  (:require [clojure.test :refer [is deftest testing use-fixtures]]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]
             #?@(:cljs [:include-macros true])]
            [same :refer [ish?]]
            [sicmutils.differential :as d]
            [sicmutils.generators :as sg]
            [sicmutils.generic :as g]
            [sicmutils.simplify :refer [hermetic-simplify-fixture]]
            [sicmutils.util :as u]
            [sicmutils.value :as v]))

(use-fixtures :each hermetic-simplify-fixture)

(defn- derivative
  "A bare-bones derivative implementation, used for testing the functionality made
  available by [[Differential]]. The real version lives
  at [[sicmutils.calculus.derivative/derivative]]"
  [f]
  (let [tag (d/fresh-tag)]
    (fn [x]
      (-> (f (d/bundle x 1 tag))
          (d/extract-tangent tag)))))

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
        (checking "=, equiv ignore tangent parts" 100 [n sg/real]
                  (is (= (d/bundle n 1 0) n)
                      "differential on the left works.")

                  (is (d/equiv (d/bundle n 1 0) n n (d/bundle n 1 0) n)
                      "d/equiv matches = behavior, varargs"))

        (checking "eq uses tangent parts" 100 [n sg/real]
                  (is (d/eq (d/bundle n 0 0) n n (d/bundle n 0 0) n)
                      "eq is true with no tangent, varargs")

                  (is (not (d/eq (d/bundle n 1 0) n))
                      "eq is FALSE with a tangent, bundle on left")

                  (is (not (d/eq n (d/bundle n 1 0)))
                      "eq is FALSE with a tangent, bundle on right")

                  (is (= (d/eq (d/bundle n 1 0) (d/bundle n 1 0)))
                      "d/eq handles equality")

                  (is (= (d/eq (d/bundle n 1 0) (d/bundle n 2 0)))
                      "d/eq is false for [[Differential]]s with diff tags"))

        (checking "compare ignores tangent parts" 100
                  [l sg/real, r sg/real]
                  (is (= (compare l r)
                         (compare (d/bundle l 1 0) r))
                      "differential on the left works.")

                  (is (= (d/compare l r)
                         (d/compare (d/bundle l r 0) r)
                         (d/compare l (d/bundle r l 0)))
                      "d/compare can handle non-differential on either side, also
                    ignores tangents.")

                  (is (zero? (d/compare (d/bundle l r 0) l))
                      "d/compare matches equals behavior, ignores tangents.")

                  (is (zero? (d/compare l (d/bundle l r 0)))
                      "d/compare matches equals behavior, ignores tangents."))

        (checking "compare-full takes tags into account" 100
                  [l sg/real, r sg/real]
                  (is (= 1 (d/compare-full (d/bundle l 1 0) l))
                      "a [[Differential]] with a positive tangent is ALWAYS
                    greater than a non-[[Differential]] whatever the tangent.")

                  (is (= -1 (d/compare-full l (d/bundle l 1 0)))
                      "a [[Differential]] with a positive tangent is ALWAYS
                    greater than a non-[[Differential]] whatever the tangent."))))))

(deftest differential-fn-tests
  (testing "differentials can take branches inside functions, PROVIDED (with
            clojure.core/=) the perturbed variable is on the left!"
    (let [f (fn [x]
              (let [g (if (= x 10)
                        (g/* x g/square)
                        (g/* x g/cube))]
                (g x)))
          Df (derivative f)]

      (is (= ((derivative (g/* identity g/square)) 10)
             (Df 10))
          "providing 10 takes the x*g/square branch")
      (is (= ((derivative (g/* identity g/cube)) 9)
             (Df 9))
          "providing 9 takes the x*g/cube branch")))

  (checking "applying a Differential with fn coefs works!" 200
            [xs (gen/vector (sg/reasonable-double) 1 20)]
            (let [dx      (d/bundle 0 1 0)
                  diff    (apply (d/bundle g/+ g/- 0) xs)
                  [ps ts] (d/primal-tangent-pair diff)]
              (is (= (apply g/+ xs) ps)
                  "primal passes through")
              (is (= (g/* (apply g/- xs) dx) ts)
                  "tangent part keeps its dx, but applies fn")

              (is (= (d/extract-tangent diff 0)
                     (d/extract-tangent ts 0))
                  "the tangent extracted from the tangent-part is identical to
                   the `extract-tangent` of the full diff")))

  (testing "partial-derivative of a differential"
    (let [D #(g/partial-derivative % [])]
      (is (= ((d/bundle (D g/sin) (D g/cos) 0) 't)
             ((D (d/bundle g/sin g/cos 0)) 't))
          "partial-derivative is linear through a differential"))))

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

  (checking "(d:* diff 0) == (d:* 0 diff) == 0" 100 [diff diff-gen]
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

  (checking "adding primal and tangent sepraately matches adding full term" 100
            [l diff-gen, r diff-gen]
            (let [[pl tl] (d/primal-tangent-pair l)
                  [pr tr] (d/primal-tangent-pair r)]
              (is (d/eq (d/d:+ l r)
                        (reduce
                         d/d:+ (shuffle [pl pr tl tr])))
                  "adding the pieces in any order works!")))

  (testing "differential +, - unit tests"
    (let [dx     (d/from-terms {[0] 1})
          -dx    (d/from-terms {[0] -1})
          dy     (d/from-terms {[1] 1})
          dz     (d/from-terms {[2] 1})
          dx+dx  (d/from-terms {[0] 2})
          dxdy   (d/from-terms {[0 1] 1})
          dxdydz (d/from-terms {[0 1 2] 1})
          dx+dy  (d/from-terms {[0] 1, [1] 1})
          dx+dz  (d/from-terms {[0] 1, [2] 1})
          =      d/eq]
      (testing "d:+ is commutative"
        (is (= dx+dy
               (d/d:+ dx dy)
               (d/d:+ dy dx)))
        (is (= dx+dz
               (d/d:+ dx dz)
               (d/d:+ dz dx))))

      (is (= (d/from-terms {[0] 3, [1] 2, [2] 3})
             (reduce d/d:+ [dx dy dz dy dz dx dz dx]))
          "d:+ groups terms appropriately")

      (testing "addition preserves primal"
        (is (= (d/from-terms {[] 1, [0] 1}) (d/d:+ dx 1)))
        (is (= (d/from-terms {[] 'k [0] 1}) (d/d:+ dx 'k))))

      (testing "various ways to get to zero"
        (is (v/zero? (d/d:+ dx -dx)))
        (is (v/zero? (d/d:+ -dx dx)))
        (is (v/zero? (d/d:* dx 0)))
        (is (v/zero? (d/d:* 0 dx)))
        (is (v/zero? (g/* dx dx))))

      (testing "associative, commutative multiplication"
        (is (= dxdy (d/d:* dx dy)))
        (is (= dxdydz (d/d:* (d/d:* dx dy) dz)))
        (is (= dxdydz (d/d:* (d/d:* dz dx) dy)))
        (is (= dxdydz (d/d:* (d/d:* dy dz) dx))))

      (testing "infinitesimals go to zero when multiplied!"
        (is (v/zero? (d/d:* dx dx))
            "dx^2==0")
        (is (v/zero? (d/d:* dz (d/d:* dy dz)))
            "dy*dz^2==0"))))

  (testing "various unit tests with more terms"
    (let [tangent  (fn [dx] (d/extract-tangent dx 0))
          simplify (comp g/simplify tangent)]
      (is (= '(* 3 (expt x 2))
             (simplify
              (g/expt (g/+ 'x (d/bundle 0 1 0)) 3))))

      (is (= '(* 4 (expt x 3))
             (simplify
              (g/expt (g/+ 'x (d/bundle 0 1 0)) 4))))

      (let [dx   (d/bundle 0 1 0)
            x+dx (g/+ 'x dx)
            f    (fn [x] (g/* x x x x))]
        (is (= '(* 4 (expt x 3))
               (simplify (g/* x+dx x+dx x+dx x+dx))))
        (is (= '(* 12 (expt x 2))
               (simplify
                (g/+ (g/* (g/+ (g/* (g/+ x+dx x+dx) x+dx)
                               (g/* x+dx x+dx))
                          x+dx)
                     (g/* x+dx x+dx x+dx)))))

        (is (= '(* 24 x)
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

        (is (= '(* 4 (expt x 3))
               (simplify (f x+dx))))))))

(deftest differential-api-tests
  (checking "to, from terms == id" 100
            [diff real-diff-gen]
            (is (= diff
                   (-> (#'d/->terms diff)
                       (#'d/terms->differential)))))

  (checking "round-trip to terms and back" 100
            [n sg/real]
            (let [rt (-> (#'d/->terms n)
                         (#'d/terms->differential))]
              (is (= n rt)
                  "round tripping doesn't change equality")
              (is (not (d/differential? rt))
                  "round trip keeps non-differentials as non-diff")))

  (letfn [(all-tags [diff]
            (into #{}
                  (mapcat #'d/tags)
                  (#'d/->terms diff)))]

    (checking "max-order-tag picks from the greatest of SOME term's tags" 100
              [diff real-diff-gen]
              (if-let [tags (seq (all-tags diff))]
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
                      :let [dx (d/bundle 0 1 tag)]]
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
              (let [diff (d/bundle primal tangent tag)]
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
  )
