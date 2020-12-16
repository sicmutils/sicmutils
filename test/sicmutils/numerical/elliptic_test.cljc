;;
;; Copyright © 2017 Colin Smith.
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

(ns sicmutils.numerical.elliptic-test
  (:require [clojure.test :refer [is deftest testing]]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]
             #?@(:cljs [:include-macros true])]
            [same :refer [zeroish? ish? with-comparator]
             #?@(:cljs [:include-macros true])]
            [sicmutils.generators :as sg]
            [sicmutils.generic :as g]
            [sicmutils.numerical.derivative :refer [D-numeric]]
            [sicmutils.numerical.unimin.golden :refer [phi]]
            [sicmutils.numerical.elliptic :as e]
            [sicmutils.numerical.quadrature :as q]
            [sicmutils.value :as v]))

(def ^:private near (v/within 1e-6))

(def gen-k
  (gen/double* {:infinite? false :NaN? false
                :min 0
                :max (- 1 v/machine-epsilon)}))

(defn gen-phi-k
  ([] (gen-phi-k {}))
  ([phi-opts]
   (gen/tuple (sg/reasonable-double phi-opts)
              gen-k)))

(deftest elliptic-tests
  (checking "complete-elliptic-K matches first" 100
            [k gen-k]
            (is (ish? (e/complete-elliptic-integral-K k)
                      (e/first-elliptic-integral k))))

  (checking "complete-elliptic-E matches second" 100
            [k gen-k]
            (is (ish? (e/complete-elliptic-integral-E k)
                      (e/second-elliptic-integral k))))

  (checking "complete-elliptic-E matches second" 100
            [k gen-k]
            (is (ish? (e/elliptic-integrals k)
                      [(e/first-elliptic-integral k)
                       (e/second-elliptic-integral k)])))

  (checking "first-elliptic-integral-and-deriv" 100
            [k (gen/double* {:infinite? false :NaN? false
                             :min 0.1
                             :max 0.8})]
            (let [[K DK] (e/first-elliptic-integral-and-deriv k vector)]
              (is (ish? K (e/first-elliptic-integral k))
                  "the returned elliptic integral is correct")

              (with-comparator (v/within 1e-8)
                (is (ish? DK ((D-numeric e/first-elliptic-integral) k))
                    "the returned derivative matches the numerical estimate"))))

  (checking "elliptic-integral-E" 100
            [[phi k] (gen-phi-k {:min (- (/ Math/PI 2))
                                 :max (/ Math/PI 2)})]
            (is (ish? (e/elliptic-integral-E phi k)
                      (q/definite-integral
                        (fn [theta]
                          (g/sqrt
                           (- 1.0 (g/square (* k (g/sin theta))))))
                        0.0
                        phi {:tolerance 1.e-13}))))

  (with-comparator (v/within 1e-10)
    (checking "elliptic-f" 100
              [[phi k] (gen-phi-k {:min (- (/ Math/PI 2))
                                   :max (/ Math/PI 2)})]
              (is (ish? (e/elliptic-f phi k)
                        (q/definite-integral
                          (fn [theta]
                            (/ 1.0 (g/sqrt
                                    (- 1.0 (g/square
                                            (* k (g/sin theta)))))))
                          0.0 phi {:tolerance 1.e-13})))))

  (testing "direct elliptic"
    (is (near 0.200212 (e/elliptic-f 0.2 0.4)))
    (is (near 0.841935 (e/elliptic-f 0.8 0.7)))
    (is (near 1.30567  (e/elliptic-f 1.2 (Math/sqrt 0.4))))
    (is (near 0.303652 (e/elliptic-f 0.3 (Math/sqrt 0.8))))
    (is (near 0.300712 (e/elliptic-f 0.3 0.4)))
    (is (near 0.738059 (e/elliptic-f 0.7 0.8))))

  (testing "general pendulum periods"
    (let [period (fn [theta_0]
                   (/ (* 8 (e/elliptic-f (/ theta_0 2) (/ (Math/sin (/ theta_0 2)))))
                      (* (Math/sqrt (* 2 9.8))
                         (Math/sqrt (- 1 (Math/cos theta_0))))))]
      (is (near 2.009916 (period 0.15)))
      (is (near 2.018438 (period 0.30)))
      (is (near 2.032791 (period 0.45)))
      (is (near 2.053204 (period 0.60)))
      (is (near 2.080013 (period 0.75)))
      (is (near 2.113680 (period 0.90)))
      (is (near 2.154814 (period 1.05)))
      (is (near 2.204206 (period 1.20)))
      (is (near 2.262882 (period 1.35)))
      (is (near 2.332176 (period 1.50)))
      (is (near 2.413836 (period 1.65)))
      (is (near 2.510197 (period 1.80)))
      (is (near 2.624447 (period 1.95))))))

(deftest jacobi-elliptic-tests
  (checking "jacobi-elliptic-functions" 100
            [[phi k] (gen-phi-k)]
            (let [u          (e/elliptic-f phi k)
                  [sn cn dn] (e/jacobi-elliptic-functions u k vector)]
              (is (ish? 1 (+ (g/square sn)
                             (g/square cn)))
                  "Press 6.11.26, {sn}^2 + {cn}^2 = 1, defining relation")

              (with-comparator (v/within 1e-6)
                (is (ish? (g/sin phi) sn)
                    "Press 6.11.24, sn = \\sin \\phi")

                (is (ish? 1 (+ (g/square dn)
                               (* (g/square sn) (g/square k))))
                    "Press 6.11.26, k^2 {sn}^2 + {dn}^2 = 1, defining relation")

                (is (ish? dn (g/sqrt (- 1 (* (g/square k) (g/square sn)))))
                    "equivalent expression of 6.11.26 relation above"))))

  (checking "special-case test for k = 0.0" 100
            [[phi k] (gen-phi-k)]
            (let [u (e/elliptic-f phi k)
                  [sn cn dn] (e/jacobi-elliptic-functions u 0.0 vector)]
              (is (ish? [(g/sin u)
                         (- 1 (g/square (g/sin u)))
                         1.0]
                        [sn (g/square cn) dn])
                  "special case for k == u")))

  (with-comparator (v/within 1e-8)
    (checking "jacobi-elliptic phi == asin(sn)" 100
              [[phi k] (gen-phi-k {:min (- (/ Math/PI 2))
                                   :max (/ Math/PI 2)})]
              (let [u    (e/elliptic-f phi k)
                    [sn] (e/jacobi-elliptic-functions u k vector)]
                (is (ish? phi (Math/asin sn)))))))
