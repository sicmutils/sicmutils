;;
;; Copyright © 2020 Sam Ritchie.
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

(ns sicmutils.numerical.quadrature.romberg-test
  (:require [clojure.test :refer [is deftest testing]]
            [same :refer [ish?]]
            [sicmutils.numerical.quadrature.boole :as qb]
            [sicmutils.numerical.quadrature.romberg :as qr]
            [sicmutils.numerical.quadrature.simpson :as qs]
            [sicmutils.numerical.quadrature.trapezoid :as qt]
            [sicmutils.generic :as g]
            [sicmutils.numsymb]
            [sicmutils.util :as u]))

(defn gaussian [x]
  (* (/ 1 (Math/sqrt Math/PI))
     (Math/exp (- (* x x)))))

(defn exploding
  "Returns a wrapped version of `f` that will throw if its endpoints are evaluated."
  [f a b]
  (fn [x]
    (condp = x
      a (u/illegal (str a " is explosive!"))
      b (u/illegal (str b " is explosive!"))
      (f x))))

(deftest open-romberg-tests
  (testing "Romberg integration over an open interval converges."
    (let [expected {:converged? true
                    :terms-checked 5
                    :result 0.4213503964748162}
          terms (qr/open-sequence gaussian 0 1)]
      (is (ish? expected (qr/open-integral gaussian 0 1))
          "The sequence converges.")

      (is (= (:result expected)
             (nth terms 4))
          "The sequence converged on the 5 term (index 4).")

      (is (ish? [0.4393912894677224
                 0.4212712195749088
                 0.42134999562630593
                 0.4213503969053587
                 0.4213503964748162
                 0.4213503964748573]
                (take 6 terms))
          "here's a look at the first six terms.")))

  (testing "open romberg won't touch the endpoints."
    (is (= (qr/open-integral gaussian 0 1)
           (-> (exploding gaussian 0 1)
               (qr/open-integral 0 1)))))

  (testing "open romberg passes options through."
    (is (ish? {:converged? false
               :terms-checked 3
               :result 0.42134999562630593}
              (qr/open-integral gaussian 0 1 {:maxterms 3})))))

(deftest closed-romberg-tests
  (testing "Romberg integration over a closed interval converges."
    (is (ish? {:converged? true
               :terms-checked 6
               :result 0.421350396474754}
              (qr/closed-integral gaussian 0.0 1.0))))

  (testing "Romberg integration over a closed interval DOES evaluate the
  endpoints."
    (is (thrown? #?(:clj Exception :cljs js/Error)
                 (-> (exploding gaussian 0 1)
                     (qr/closed-integral 0 1)))))

  (testing "closed romberg passes options through."
    (is (ish? {:converged? false
               :terms-checked 3
               :result 0.42135579973955767}
              (qr/closed-integral gaussian 0 1 {:maxterms 3}))))

  (testing "Romberg integration matches certain Newton-Cotes methods"
    (let [[a b c] (qr/closed-sequence gaussian 0 1)
          [trap]  (qt/trapezoid-sequence gaussian 0 1)
          [simps] (qs/simpson-sequence gaussian 0 1)
          [boole] (qb/boole-sequence gaussian 0 1)]
      (is (ish? a trap) "The first step of closed Romberg quadrature is equivalent to the Trapezoid method.")
      (is (ish? b simps) "The second step = the first step of Simpson's method.")
      (is (ish? c boole) "The third step = the first step of Boole's method."))))
