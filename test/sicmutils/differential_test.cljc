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
            [same :refer [ish?]]
            [sicmutils.differential :as d]
            [sicmutils.generic :as g]
            [sicmutils.simplify :refer [hermetic-simplify-fixture]]
            [sicmutils.util :as u]
            [sicmutils.value :as v]))

(use-fixtures :each hermetic-simplify-fixture)

;; TODO - one law - extract-tangent * tag == tangent-part
;;
;; TODO make a vector-set generator
;;
;; TODO use that to make a differential generator, given a coefs generator.

(deftest guess-tests
  (testing "branching"
    (let [f (fn [x] (if (= x 10)
                     (g/square x)
                     (g/cube x)))]
      (is (= 20 (d/extract-tangent (f (d/bundle 10 1 0)) 0)))
      (is (= (* 3 (g/square 9))
             (d/extract-tangent (f (d/bundle 9 1 0)) 0)))))

  (testing "differentials can take fn values"
    (let [f (fn [x]
              (let [g (if (= x 10)
                        (g/* x g/square)
                        (g/* x g/cube))]
                (g x)))]
      (is (= (+ (g/square 10)
                (* 10 (* 2 10)))
             (d/extract-tangent (f (d/bundle 10 1 0)) 0)))
      (is (= (+ (g/cube 9)
                (* 9 (* 3 (g/square 9))))
             (d/extract-tangent (f (d/bundle 9 1 0)) 0))))))

(deftest value-protocol-tests
  (let [zero-diff (d/from-terms [])
        dy        (d/from-terms {[1] 1})]
    (is (v/zero? zero-diff))
    (is (not (v/zero? dy)))
    (is (= dy 0) "subtly, it IS in fact equal to zero.")
    (is (not (v/one? dy)))
    (is (not (v/identity? dy)))

    (is (v/zero? (v/zero-like dy)))
    (is (v/one? (v/one-like dy)))
    (is (v/identity? (v/identity-like dy)))))

(deftest differentials
  (testing "add, mul differentials"
    (let [zero-differential (d/->Differential [])
          dx (d/from-terms {[0] 1})
          -dx (d/from-terms {[0] -1})
          dy (d/from-terms {[1] 1})
          dz (d/from-terms {[2] 1})
          dx-plus-dx (d/from-terms {[0] 2})
          dxdy (d/from-terms {[0 1] 1})
          dxdydz (d/from-terms {[0 1 2] 1})
          dx-plus-dy (d/from-terms {[0] 1 [1] 1})
          dx-plus-dz (d/from-terms {[0] 1 [2] 1})]
      (is (= dx-plus-dy (d/d:+ dx dy)))
      (is (= dx-plus-dy (d/d:+ dy dx)))
      (is (= dx-plus-dz (d/d:+ dx dz)))
      (is (= dx-plus-dz (d/d:+ dz dx)))
      (is (= dx-plus-dx (d/d:+ dx dx)))
      (is (= (d/from-terms {[0] 3 [1] 2 [2] 3})
             (reduce d/d:+ 0 [dx dy dz dy dz dx dz dx])))
      (is (= (d/from-terms {[] 1 [0] 1}) (d/d:+ dx 1)))
      (is (= (d/from-terms {[] 'k [0] 1}) (d/d:+ dx 'k)))
      (is (= zero-differential (d/d:+ dx -dx)))
      (is (= zero-differential (d/d:+ -dx dx)))
      (is (= zero-differential (d/d:* dx 0)))
      (let [b (d/d:+ 0 (d/d:* dx 0))
            c (d/d:* 0 dx)]
        (is (= zero-differential b))
        (is (= zero-differential c))
        (is (= zero-differential (d/d:+ b c))))
      (is (= dxdy (d/d:* dx dy)))
      (is (= dxdydz (d/d:* (d/d:* dx dy) dz)))
      (is (= dxdydz (d/d:* (d/d:* dz dx) dy)))
      (is (= dxdydz (d/d:* (d/d:* dy dz) dx)))
      (is (= zero-differential (d/d:* dx dx)))
      (is (= zero-differential (d/d:* dz (d/d:* dy dz))))
      (is (= 0 (g/* dx dx)))))

  (testing "more terms"
    (let [tangent  (fn [dx] (d/extract-tangent dx 0))
          simplify (comp g/simplify tangent)]
      (is (= '(* 3 (expt x 2))
             (simplify
              (g/expt (g/+ 'x (d/from-terms {[0] 1})) 3))))

      (is (= '(* 4 (expt x 3))
             (simplify
              (g/expt (g/+ 'x (d/from-terms {[0] 1})) 4))))

      (let [dx   (d/bundle 0 0)
            x+dx (g/+ 'x dx)
            f    (fn [x] (g/* x x x x))]
        (is (= '(* 4 (expt x 3))
               (simplify (g/* x+dx x+dx x+dx x+dx))))
        (is (= '(* 12 (expt x 2))
               (simplify (g/+ (g/* (g/+ (g/* (g/+ x+dx x+dx) x+dx)
                                        (g/* x+dx x+dx))
                                   x+dx)
                              (g/* x+dx x+dx x+dx)))))
        (is (= '(* 24 x)
               (simplify
                (g/+
                 (g/* (g/+ (g/* 2 x+dx) x+dx x+dx x+dx x+dx) x+dx)
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
