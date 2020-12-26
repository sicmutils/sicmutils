;;
;; Copyright © 2017 Colin Smith.
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

(deftest value-protocol-tests
  (let [zero-diff (d/sum->differential [])
        dy        (d/sum->differential {[1] 1})]
    (is (v/zero? zero-diff))
    (is (not (v/zero? dy)))
    (is (not (v/one? dy)))
    (is (not (v/identity? dy)))

    (is (v/zero? (v/zero-like dy)))
    (is (v/one? (v/one-like dy)))
    (is (v/identity? (v/identity-like dy)))))

(deftest differentials
  (testing "add, mul differentials"
    (let [zero-differential (d/terms->differential [])
          dx (d/sum->differential {[0] 1})
          -dx (d/sum->differential {[0] -1})
          dy (d/sum->differential {[1] 1})
          dz (d/sum->differential {[2] 1})
          dx-plus-dx (d/sum->differential {[0] 2})
          dxdy (d/sum->differential {[0 1] 1})
          dxdydz (d/sum->differential {[0 1 2] 1})
          dx-plus-dy (d/sum->differential {[0] 1 [1] 1})
          dx-plus-dz (d/sum->differential {[0] 1 [2] 1})]
      (is (= dx-plus-dy (d/d:+ dx dy)))
      (is (= dx-plus-dy (d/d:+ dy dx)))
      (is (= dx-plus-dz (d/d:+ dx dz)))
      (is (= dx-plus-dz (d/d:+ dz dx)))
      (is (= dx-plus-dx (d/d:+ dx dx)))
      (is (= (d/sum->differential {[0] 3 [1] 2 [2] 3})
             (reduce d/d:+ 0 [dx dy dz dy dz dx dz dx])))
      (is (= (d/sum->differential {[] 1 [0] 1}) (d/d:+ dx 1)))
      (is (= (d/sum->differential {[] 'k [0] 1}) (d/d:+ dx 'k)))
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
    (let [d-expr (fn [dx]
                   (->> (d/terms dx)
                        (filter (fn [[tags coef]] (= tags [0])))
                        (first)
                        (second)))
          d-simplify #(g/simplify (d-expr %))]
      (is (= '(* 3 (expt x 2))
             (d-simplify (g/expt (g/+ 'x (d/sum->differential {[0] 1})) 3))))
      (is (= '(* 4 (expt x 3))
             (d-simplify (g/expt (g/+ 'x (d/sum->differential {[0] 1})) 4))))
      (let [dx (d/sum->differential {[0] 1})
            x+dx (g/+ 'x dx)
            f (fn [x] (g/* x x x x))]
        (is (= '(* 4 (expt x 3))
               (d-simplify (g/* x+dx x+dx x+dx x+dx))))
        (is (= '(* 12 (expt x 2))
               (d-simplify (g/+ (g/* (g/+ (g/* (g/+ x+dx x+dx) x+dx) (g/* x+dx x+dx)) x+dx)
                                (g/* x+dx x+dx x+dx)))))
        (is (= '(* 24 x) (d-simplify (g/+
                                      (g/* (g/+ (g/* 2 x+dx) x+dx x+dx x+dx x+dx) x+dx)
                                      (g/* (g/+ x+dx x+dx) x+dx)
                                      (g/* x+dx x+dx)
                                      (g/* (g/+ x+dx x+dx) x+dx)
                                      (g/* x+dx x+dx)))))
        (is (= 24 (d-expr (g/+ (g/* 6 x+dx)
                               (g/* 2 x+dx)
                               x+dx x+dx x+dx x+dx
                               (g/* 2 x+dx)
                               x+dx x+dx x+dx x+dx
                               (g/* 2 x+dx)
                               x+dx x+dx x+dx x+dx))))
        (is (= '(* 4 (expt x 3))
               (d-simplify (f x+dx))))))))
