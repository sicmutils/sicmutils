#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.calculus.indexed-test
  (:refer-clojure :exclude [+ - * /])
  (:require [clojure.test :refer [is deftest testing use-fixtures]]
            [emmy.calculus.basis :as b]
            [emmy.calculus.coordinate :refer [let-coordinates]]
            [emmy.calculus.form-field :as ff]
            [emmy.calculus.indexed :as ci]
            [emmy.calculus.manifold :as m]
            [emmy.calculus.vector-field :as vf]
            [emmy.function :as f]
            [emmy.generic :as g :refer [+ *]]
            [emmy.simplify :refer [hermetic-simplify-fixture]]
            [emmy.structure :as s :refer [up]]
            [emmy.value :as v]))

(use-fixtures :each hermetic-simplify-fixture)

(def simplify
  (comp v/freeze g/simplify))

(deftest indexed-tests
  (let-coordinates [[x y] m/R2-rect]
    (let [T (-> (fn [w1 w2 v1]
                  (+ (* 'a (dx v1) (w1 d:dx) (w2 d:dy))
                     (* 'b (dy v1) (w1 d:dy) (w2 d:dx))
                     (* 'c (dy v1) (w1 d:dy) (w2 d:dy))))
                (ci/with-argument-types [::ff/oneform-field
                                         ::ff/oneform-field
                                         ::vf/vector-field]))]
      (is (ci/has-argument-types? T))
      (is (= [:exactly 3] (f/arity T))
          "with-argument-types sets the arity correctly")

      (is (= '(+ (* c (v1↑1 (up x y)) (w2_1 (up x y)) (w1_1 (up x y)))
                 (* b (v1↑1 (up x y)) (w2_0 (up x y)) (w1_1 (up x y)))
                 (* a (v1↑0 (up x y)) (w2_1 (up x y)) (w1_0 (up x y))))
             (v/freeze
              (((ci/indexed->typed
                 (ci/typed->indexed T (b/coordinate-system->basis R2-rect))
                 (b/coordinate-system->basis R2-rect))
                (ff/literal-oneform-field 'w1 R2-rect)
                (ff/literal-oneform-field 'w2 R2-rect)
                (vf/literal-vector-field 'v1 R2-rect))
               ((m/point R2-rect) (up 'x 'y))))))))

  (testing "outer-product and contract"
    (let-coordinates [[x y] m/R2-rect]
      (let [T1 (-> (fn [w1 w2 v1]
                     (+ (* 'a (dx v1) (w1 d:dx) (w2 d:dy))
                        (* 'b (dy v1) (w1 d:dy) (w2 d:dx))
                        (* 'c (dy v1) (w1 d:dy) (w2 d:dy))))
                   (ci/with-argument-types [::ff/oneform-field
                                            ::ff/oneform-field
                                            ::vf/vector-field]))
            iT1 (ci/typed->indexed T1 (b/coordinate-system->basis R2-rect))
            T2 (-> (fn [w1 w2]
                     (+ (* (w1 d:dx) (w2 d:dx))
                        (* (w1 d:dy) (w2 d:dy))
                        (* (w1 d:dy) (w2 d:dx))))
                   (ci/with-argument-types [::ff/oneform-field
                                            ::ff/oneform-field]))
            iT2 (ci/typed->indexed T2 (b/coordinate-system->basis R2-rect))
            iT3 (ci/outer-product iT1 iT2)]
        (is (not (ci/has-index-types? T1)))
        (is (ci/has-index-types? iT1))

        (is (= [:exactly 3]
               (f/arity T1)
               (f/arity iT1))
            "Arity is preserved")

        (is (= [:exactly 2] (f/arity iT2)))
        (is (= [:exactly 5] (f/arity iT3)))

        (testing "outer-product"
          (is (= '(+ (* a (w4_1 (up x y)) (w3_1 (up x y)) (w2_1 (up x y)) (v1↑0 (up x y)) (w1_0 (up x y)))
                     (* a (w3_1 (up x y)) (w2_1 (up x y)) (v1↑0 (up x y)) (w1_0 (up x y)) (w4_0 (up x y)))
                     (* a (w2_1 (up x y)) (v1↑0 (up x y)) (w1_0 (up x y)) (w4_0 (up x y)) (w3_0 (up x y)))
                     (* b (w4_1 (up x y)) (w3_1 (up x y)) (v1↑1 (up x y)) (w1_1 (up x y)) (w2_0 (up x y)))
                     (* b (w3_1 (up x y)) (w4_0 (up x y)) (v1↑1 (up x y)) (w1_1 (up x y)) (w2_0 (up x y)))
                     (* b (w4_0 (up x y)) (w3_0 (up x y)) (v1↑1 (up x y)) (w1_1 (up x y)) (w2_0 (up x y)))
                     (* c (w4_1 (up x y)) (w3_1 (up x y)) (w2_1 (up x y)) (v1↑1 (up x y)) (w1_1 (up x y)))
                     (* c (w3_1 (up x y)) (w2_1 (up x y)) (w4_0 (up x y)) (v1↑1 (up x y)) (w1_1 (up x y)))
                     (* c (w2_1 (up x y)) (w4_0 (up x y)) (w3_0 (up x y)) (v1↑1 (up x y)) (w1_1 (up x y))))
                 (simplify
                  (((ci/indexed->typed iT3 (b/coordinate-system->basis R2-rect))
                    (ff/literal-oneform-field 'w1 R2-rect)
                    (ff/literal-oneform-field 'w2 R2-rect)
                    (ff/literal-oneform-field 'w3 R2-rect)
                    (ff/literal-oneform-field 'w4 R2-rect)
                    (vf/literal-vector-field 'v1 R2-rect))
                   ((m/point R2-rect) (up 'x 'y)))))))

        (testing "contract"
          (is (= '(+ (* a (w1_1 (up x y)))
                     (* b (w1_0 (up x y)))
                     (* c (w1_1 (up x y))))
                 (simplify
                  (((ci/indexed->typed (ci/contract iT1 0 0 2)
                                       (b/coordinate-system->basis R2-rect))
                    (ff/literal-oneform-field 'w1 R2-rect))
                   ((m/point R2-rect) (up 'x 'y))))))

          (is (= '(* c (w1_1 (up x y)))
                 (simplify
                  (((ci/indexed->typed (ci/contract iT1 1 0 2)
                                       (b/coordinate-system->basis R2-rect))
                    (ff/literal-oneform-field 'w1 R2-rect))
                   ((m/point R2-rect) (up 'x 'y))))))

          (is (= 0 (simplify
                    (((ci/indexed->typed (ci/contract iT3 1 0 0)
                                         (b/coordinate-system->basis R2-rect))
                      (ff/literal-oneform-field 'w1 R2-rect)
                      (ff/literal-oneform-field 'w2 R2-rect)
                      (ff/literal-oneform-field 'w3 R2-rect))
                     ((m/point R2-rect) (up 'x 'y))))))))))

  (testing "typed->structure"
    (let-coordinates [[x y] m/R2-rect]
      (let [T (-> (fn [v1 w1 w2]
                    (+ (* 'a (dx v1) (w1 d:dx) (w2 d:dy))
                       (* 'b (dy v1) (w1 d:dy) (w2 d:dx))
                       (* 'c (dy v1) (w1 d:dy) (w2 d:dy))))
                  (ci/with-argument-types [::vf/vector-field
                                           ::ff/oneform-field
                                           ::ff/oneform-field]))]
        (is (= '(down (up (up 0 a) (up 0 0))
                      (up (up 0 0) (up b c)))
               (simplify
                ((ci/typed->structure T (b/coordinate-system->basis R2-rect))
                 ((m/point R2-rect) (up 'x 'y))))))

        (testing "Outer index is first argument.  Inner index is last argument."
          (is (= '(+ (* a (w2_1 (up x y)) (v1↑0 (up x y)) (w1_0 (up x y)))
                     (* b (v1↑1 (up x y)) (w1_1 (up x y)) (w2_0 (up x y)))
                     (* c (w2_1 (up x y)) (v1↑1 (up x y)) (w1_1 (up x y))))
                 (simplify
                  (((ci/structure->typed
                     (ci/typed->structure T (b/coordinate-system->basis R2-rect))
                     (b/coordinate-system->basis R2-rect))
                    (vf/literal-vector-field 'v1 R2-rect)
                    (ff/literal-oneform-field 'w1 R2-rect)
                    (ff/literal-oneform-field 'w2 R2-rect))
                   ((m/point R2-rect) (up 'x 'y))))))

          (is (= '(down (up (up 0 a) (up 0 0))
                        (up (up 0 0) (up b c)))
                 (simplify
                  ((ci/typed->structure
                    (ci/structure->typed
                     (ci/typed->structure T (b/coordinate-system->basis R2-rect))
                     (b/coordinate-system->basis R2-rect))
                    (b/coordinate-system->basis R2-rect))
                   ((m/point R2-rect) (up 'x 'y)))))))))))
