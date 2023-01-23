#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.numerical.quadrature.romberg-test
  (:require [clojure.test :refer [is deftest testing]]
            [same :refer [ish?]]
            [emmy.numerical.quadrature.boole :as qb]
            [emmy.numerical.quadrature.common :as qc]
            [emmy.numerical.quadrature.romberg :as qr]
            [emmy.numerical.quadrature.simpson :as qs]
            [emmy.numerical.quadrature.trapezoid :as qt]
            [emmy.numsymb]
            [emmy.util :as u]))

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

(deftest romberg-sequence-tests
  (testing "romberg-sequence dispatches appropriately"
    (is (ish? (take 5 (qr/open-sequence gaussian 0 1))
              (take 5 (qr/romberg-sequence gaussian 0 1 {:interval qc/open}))
              (take 5 (-> (exploding gaussian 0 1)
                          (qr/romberg-sequence 0 1 {:interval qc/open-closed})))
              (take 5 (qr/romberg-sequence gaussian 0 1 {:interval qc/closed-open})))
        "romberg-sequence returns an open sequence if either endpoint is open.")

    (is (ish? (take 5 (qr/closed-sequence gaussian 0 1))
              (take 5 (qr/romberg-sequence gaussian 0 1 {:interval qc/closed})))
        "romberg-sequence returns a CLOSED sequence if both endpoints are closed.")))

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
