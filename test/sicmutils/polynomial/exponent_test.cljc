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

(ns sicmutils.polynomial.exponent-test
  (:require [clojure.test :refer [is deftest testing]]
            [sicmutils.polynomial.exponent :as xpt]))

(deftest monomial-ordering-tests
  (testing "monomial orderings"
    (let [x3 (xpt/dense->exponents [3 0 0])
          x2z2 (xpt/dense->exponents [2 0 2])
          xy2z (xpt/dense->exponents [1 2 1])
          z2   (xpt/dense->exponents [0 0 2])
          monomials [x3 x2z2 xy2z z2]
          sort-with #(sort % monomials)]
      (is (= [z2 xy2z x2z2 x3]
             (sort-with xpt/lex-order)))

      (is (= [z2 x3 xy2z x2z2]
             (sort-with xpt/graded-lex-order)))

      (is (= [z2 x3 x2z2 xy2z]
             (sort-with xpt/graded-reverse-lex-order))))

    (testing "monomial ordering example from wikipedia"
      (let [x2 (xpt/make 0 2)
            xy (xpt/make 0 1 1 1)
            xz (xpt/make 0 1 2 1)
            y2 (xpt/make 1 2)
            yz (xpt/make 1 1 2 1)
            z2 (xpt/make 2 2)
            monomials [x2 xy xz y2 yz z2]
            sort-with #(sort % monomials)]
        (is (= [z2 yz y2 xz xy x2]
               (sort-with xpt/lex-order)
               (sort-with xpt/graded-lex-order))
            "grlex and lex match when all orders are the same")

        (is (= [z2 yz xz y2 xy x2]
               (sort-with xpt/graded-reverse-lex-order)))))))

(comment
  (defn tester [m]
    (let [[sort-m unsort-m] (->sort-fns m)]
      (and (= (vals (sort-m m))
              (sort (vals m)))
           (= m (unsort-m (sort-m m))))))

  (tester (xpt/make 1 2, 3 1, 5 4))
  (tester (xpt/make 1 3, 3 1, 5 4))
  (tester (xpt/make 1 8, 3 1, 5 4)))
