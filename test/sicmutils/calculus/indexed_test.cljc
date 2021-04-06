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

(ns sicmutils.calculus.indexed-test
  (:refer-clojure :exclude [+ - * /])
  (:require [clojure.test :refer [is deftest testing use-fixtures]]
            [sicmutils.calculus.basis :as b]
            [sicmutils.calculus.coordinate :refer [let-coordinates]
             #?@(:cljs [:include-macros true])]
            [sicmutils.calculus.form-field :as ff]
            [sicmutils.calculus.indexed :as ci]
            [sicmutils.calculus.manifold :as m]
            [sicmutils.calculus.vector-field :as vf]
            [sicmutils.generic :as g :refer [+ - * /]]
            [sicmutils.simplify :refer [hermetic-simplify-fixture]]
            [sicmutils.structure :as s :refer [up down]]
            [sicmutils.value :as v]))

(use-fixtures :each hermetic-simplify-fixture)

(def simplify
  (comp v/freeze g/simplify))

(deftest indexed-tests
  (let-coordinates [[x y] m/R2-rect]
    (let [T (-> (fn [w1 w2 v1]
                  (+ (* 'a (dx v1) (w1 d:dx) (w2 d:dy))
                     (* 'b (dy v1) (w1 d:dy) (w2 d:dx))
                     (* 'c (dy v1) (w1 d:dy) (w2 d:dy))))
                (with-meta {:arguments
                            [::ff/oneform-field
                             ::ff/oneform-field
                             ::vf/vector-field]}))]
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
                   (with-meta {:arguments
                               [::ff/oneform-field
                                ::ff/oneform-field
                                ::vf/vector-field]}))
            iT1 (ci/typed->indexed T1 (b/coordinate-system->basis R2-rect))
            T2 (-> (fn [w1 w2]
                     (+ (* (w1 d:dx) (w2 d:dx))
                        (* (w1 d:dy) (w2 d:dy))
                        (* (w1 d:dy) (w2 d:dx))))
                   (with-meta {:arguments [::ff/oneform-field
                                           ::ff/oneform-field]}))
            iT2 (ci/typed->indexed T2 (b/coordinate-system->basis R2-rect))
            iT3 (ci/outer-product iT1 iT2)]
        (testing "outer-product"
          (is (= '(+ (* a (w4_1 (up x y)) (w3_1 (up x y)) (w2_1 (up x y)) (v1↑0 (up x y)) (w1_0 (up x y)))
                     (* a (w3_1 (up x y)) (w2_1 (up x y)) (w4_0 (up x y)) (v1↑0 (up x y)) (w1_0 (up x y)))
                     (* a (w2_1 (up x y)) (w4_0 (up x y)) (w3_0 (up x y)) (v1↑0 (up x y)) (w1_0 (up x y)))
                     (* b (v1↑1 (up x y)) (w4_1 (up x y)) (w3_1 (up x y)) (w1_1 (up x y)) (w2_0 (up x y)))
                     (* b (v1↑1 (up x y)) (w3_1 (up x y)) (w1_1 (up x y)) (w2_0 (up x y)) (w4_0 (up x y)))
                     (* b (v1↑1 (up x y)) (w1_1 (up x y)) (w2_0 (up x y)) (w4_0 (up x y)) (w3_0 (up x y)))
                     (* c (v1↑1 (up x y)) (w4_1 (up x y)) (w3_1 (up x y)) (w2_1 (up x y)) (w1_1 (up x y)))
                     (* c (v1↑1 (up x y)) (w3_1 (up x y)) (w2_1 (up x y)) (w1_1 (up x y)) (w4_0 (up x y)))
                     (* c (v1↑1 (up x y)) (w2_1 (up x y)) (w1_1 (up x y)) (w4_0 (up x y)) (w3_0 (up x y))))
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
                  (with-meta {:arguments [::vf/vector-field
                                          ::ff/oneform-field
                                          ::ff/oneform-field]}))]
        (is (= '(down (up (up 0 a) (up 0 0))
                      (up (up 0 0) (up b c)))
               (simplify
                ((ci/typed->structure T (b/coordinate-system->basis R2-rect))
                 ((m/point R2-rect) (up 'x 'y))))))

        (testing "Outer index is first argument.  Inner index is last argument."
          (is (= '(+ (* a (w2_1 (up x y)) (v1↑0 (up x y)) (w1_0 (up x y)))
                     (* b (v1↑1 (up x y)) (w1_1 (up x y)) (w2_0 (up x y)))
                     (* c (v1↑1 (up x y)) (w2_1 (up x y)) (w1_1 (up x y))))
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
