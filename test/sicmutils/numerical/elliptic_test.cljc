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
            [same :refer [ish? with-comparator]
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

(def gen-phi-k
  (gen/tuple (sg/reasonable-double)
             gen-k))

(def ^{:doc "valid arguments for the Legendre form of the elliptic functions."}
  legendre-phi-k
  (gen/bind
   (gen/double* {:infinite? false
                 :NaN? false
                 :min 0.0001 :max (/ Math/PI 2)})
   (fn [phi]
     (let [s (Math/sin phi)]
       (gen/tuple
        (gen/return phi)
        (if (zero? s)
          (gen/return 0)
          (gen/double* {:infinite? false
                        :NaN? false
                        :min (/ 0 (Math/sin phi))
                        :max (/ 1 (Math/sin phi))})))))))

(def gen-double
  (gen/double* {:infinite? false
                :NaN? false
                :min 0.5
                :max 100}))

(def gen-xyz
  (gen/tuple gen-double
             gen-double
             gen-double))

(deftest carlson-elliptic-tests
  (with-comparator (v/within 1e-5)
    (checking "carlson-rf definition" 100
              [[x y z] gen-xyz]
              (is (ish? (e/carlson-rf x y z)
                        (q/definite-integral
                          (fn [t]
                            (/ 1.0 (* 2.0 (g/sqrt
                                           (* (+ t x)
                                              (+ t y)
                                              (+ t z))))))
                          0.0 ##Inf {:tolerance 1e-8}))))

    (checking "carlson-rj definition" 100
              [[x y z] gen-xyz
               p (gen/double* {:infinite? false
                               :NaN? false
                               :min 0.5
                               :max 10})]
              (is (ish? (e/carlson-rj x y z p)
                        (q/definite-integral
                          (fn [t]
                            (/ 3.0 (* 2.0
                                      (+ t p)
                                      (g/sqrt
                                       (* (+ t x)
                                          (+ t y)
                                          (+ t z))))))
                          0.0 ##Inf {:tolerance 1e-9}))))

    (checking "carlson-rc definition" 100
              [[x y] gen-xyz]
              (is (ish? (e/carlson-rc x y)
                        (q/definite-integral
                          (fn [t]
                            (/ 1.0 (* 2.0
                                      (+ t y)
                                      (g/sqrt (+ t x)))))
                          0.0 ##Inf {:tolerance 1e-8}))))

    (checking "carlson-rd definition" 100
              [[x y z] gen-xyz]
              (is (ish? (e/carlson-rd x y z)
                        (q/definite-integral
                          (fn [t]
                            (/ 3.0 (* 2.0
                                      (+ t z)
                                      (g/sqrt
                                       (* (+ t x)
                                          (+ t y)
                                          (+ t z))))))
                          0.0 ##Inf {:tolerance 1e-10})))))

  (checking "carlson-rf can permute all arguments" 100
            [x gen-double
             y gen-double
             z gen-double]
            (let [expected (e/carlson-rf x y z)]
              (is (ish? expected (e/carlson-rf x z y)))
              (is (ish? expected (e/carlson-rf y x z)))
              (is (ish? expected (e/carlson-rf y z x)))
              (is (ish? expected (e/carlson-rf z x y)))
              (is (ish? expected (e/carlson-rf z y x)))))

  (checking "carlson-rf is homogeneous degree -1/2" 100
            [x      gen-double
             y      gen-double
             z      gen-double
             factor gen-double]
            (is (ish? (* (Math/pow factor -0.5)
                         (e/carlson-rf x y z))
                      (e/carlson-rf (* factor x)
                                    (* factor y)
                                    (* factor z)))))

  (checking "rf degenerates to rc when y == z" 100
            [x gen-double
             y gen-double]
            (is (ish? (e/carlson-rc x y)
                      (e/carlson-rf x y y))))

  (checking "carlson-rj can permute its first 3 args" 100
            [x      gen-double
             y      gen-double
             z      gen-double
             p      gen-double]
            (let [expected (e/carlson-rj x y z p)]
              (is (ish? expected (e/carlson-rj x z y p)))
              (is (ish? expected (e/carlson-rj y x z p)))
              (is (ish? expected (e/carlson-rj y z x p)))
              (is (ish? expected (e/carlson-rj z x y p)))
              (is (ish? expected (e/carlson-rj z y x p)))))

  (with-comparator (v/within 1e-10)
    (checking "carlson-rj is homogeneous degree -3/2" 100
              [x      gen-double
               y      gen-double
               z      gen-double
               p      gen-double
               factor gen-double]
              (is (ish? (* (Math/pow factor -1.5)
                           (e/carlson-rj x y z p))
                        (e/carlson-rj (* factor x)
                                      (* factor y)
                                      (* factor z)
                                      (* factor p))))))

  (checking "rj degenerates to rd when y == z" 100
            [x gen-double
             y gen-double
             z gen-double]
            (is (ish? (e/carlson-rd x y z)
                      (e/carlson-rj x y z z))))

  (is (ish? -0.09324045243867665
            (e/carlson-rj 1 2 3 -1))
      "carlson-rj: negative p returns the Cauchy principal value")

  (is (ish? 0.38017299815047323
            (e/carlson-rc 1 -2))
      "carlson-rc: negative y returns the Cauchy principal value"))

(deftest incomplete-elliptic-tests
  (with-comparator (v/within 1e-6)
    (checking "elliptic-f" 100
              [[phi k] legendre-phi-k]
              (is (ish? (e/elliptic-f phi k)
                        (q/definite-integral
                          (fn [theta]
                            (/ 1.0
                               (g/sqrt
                                (- 1.0 (g/square (* k (g/sin theta)))))))
                          0.0 phi))))

    (checking "elliptic-e" 100
              [[phi k] legendre-phi-k]
              (is (ish? (e/elliptic-e phi k)
                        (q/definite-integral
                          (fn [theta]
                            (g/sqrt
                             (- 1.0 (g/square (* k (g/sin theta))))))
                          0.0
                          phi))))

    (checking "elliptic-pi" 100
              [[phi k] legendre-phi-k
               n (gen/choose -10 -1)]
              (is (ish? (e/elliptic-pi phi n k)
                        (q/definite-integral
                          (fn [theta]
                            (/ (* (- 1 (* n (g/square (g/sin theta))))
                                  (g/sqrt
                                   (- 1.0 (g/square (* k (g/sin theta))))))))
                          0.0
                          phi)))))

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

;; Note from `scmutils` to accompany the following ports: "older definition of
;; the complete elliptic integrals, probably from A&Stegun"

(defn elliptic-integrals
  "Computes the first and second complete elliptic integrals at once, and passes
  them to the supplied continuation as args `K` and `E`."
  [k continue]
  (if (= k 1)
    (continue ##Inf 1.0)
    (loop [a        1.0
           b        (Math/sqrt (- 1.0 (* k k)))
           c        k
           d        0.0
           powers-2 1.0]
      (if (< (Math/abs c) v/machine-epsilon)
        (let [first-elliptic-integral (/ (/ Math/PI 2) a)]
          (continue first-elliptic-integral
                    (* first-elliptic-integral
                       (- 1.0 (/ d 2.0)))))
        (recur (/ (+ a b) 2.0)
               (Math/sqrt (* a b))
               (/ (- a b) 2.0)
               (+ d (* (* c c) powers-2))
               (* powers-2 2.0))))))

(defn first-elliptic-integral
  "Complete elliptic integral of the first kind - see Press, 6.11.18."
  [k]
  (elliptic-integrals k (fn [K _] K)))


(defn second-elliptic-integral
  "Complete elliptic integral of the second kind - see Press, 6.11.18."
  [k]
  (elliptic-integrals k (fn [_ E] E)))

(deftest elliptic-deriv-tests
  (checking "first-elliptic-integral-and-deriv" 100
            [k (gen/double* {:infinite? false :NaN? false
                             :min 0.1
                             :max 0.8})]
            (let [[Kk DKk] (e/k-and-deriv k)]
              (is (ish? Kk (first-elliptic-integral k))
                  "the returned elliptic integral is correct")

              (with-comparator (v/within 1e-8)
                (is (ish? DKk ((D-numeric e/elliptic-k) k))
                    "the returned derivative matches the numerical estimate")))))

(deftest complete-elliptic-integral-tests
  (checking "complete elliptic-k as a special case of elliptic-f" 100
            [k gen-k]
            (is (ish? (e/elliptic-f (/ Math/PI 2) k)
                      (e/elliptic-k k))))

  (checking "complete elliptic-e as a special case of elliptic-e" 100
            [k gen-k]
            (is (ish? (e/elliptic-e (/ Math/PI 2) k)
                      (e/elliptic-e k))))

  (checking "complete elliptic-pi as a special case of elliptic-pi" 100
            [k gen-k
             n (gen/choose -10 -1)]
            (is (ish? (e/elliptic-pi (/ Math/PI 2) n k)
                      (e/elliptic-pi n k))))

  (checking "complete elliptic-k matches alternate impl" 100
            [k gen-k]
            (is (ish? (e/elliptic-k k)
                      (first-elliptic-integral k))))

  (checking "complete elliptic-e matches alternate impl" 100
            [k gen-k]
            (is (ish? (e/elliptic-e k)
                      (second-elliptic-integral k)))))

(deftest jacobi-elliptic-tests
  (checking "jacobi-elliptic-functions" 100
            [[phi k] gen-phi-k]
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
            [[phi k] gen-phi-k]
            (let [u (e/elliptic-f phi k)
                  [sn cn dn] (e/jacobi-elliptic-functions u 0.0 vector)]
              (is (ish? [(g/sin u)
                         (- 1 (g/square (g/sin u)))
                         1.0]
                        [sn (g/square cn) dn])
                  "special case for k == u")))

  (with-comparator (v/within 1e-8)
    (checking "jacobi-elliptic phi == asin(sn)" 100
              [[phi k] legendre-phi-k]
              (let [u    (e/elliptic-f phi k)
                    [sn] (e/jacobi-elliptic-functions u k vector)]
                (is (ish? phi (Math/asin sn)))))))
