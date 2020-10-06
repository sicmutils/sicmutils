;
; Copyright © 2017 Colin Smith.
; This work is based on the Scmutils system of MIT/GNU Scheme:
; Copyright © 2002 Massachusetts Institute of Technology
;
; This is free software;  you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3 of the License, or (at
; your option) any later version.
;
; This software is distributed in the hope that it will be useful, but
; WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
; General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this code; if not, see <http://www.gnu.org/licenses/>.
;

(ns sicmutils.numerical.integrate-test
  (:require [clojure.test :refer [is deftest testing]]
            [sicmutils.numerical.integrate :as i]
            [sicmutils.value :as v]
            [same :refer [ish?  with-comparator]
             #?@(:cljs [:include-macros true])]))

(def ^:private near (v/within 1e-6))

(def ^:private natural-log (partial i/definite-integral / 1.))

(def ^:private sine (partial i/definite-integral #(Math/cos %) 0.))

(defn bessel-j0 [x]
  (/ (i/definite-integral #(Math/cos (- (* x (Math/sin %)))) 0. Math/PI) Math/PI))

(deftest integrals
  (testing "easy"
    (is (near 0.333333 (i/definite-integral #(* % %) 0. 1.)))
    (is (near 0.5 (i/definite-integral identity 0. 1.)))
    (is (near 3 (i/definite-integral (constantly 1.0) 0. 3.)))
    (is (near 0 (i/definite-integral (constantly 0.0) 0. 1000.)))
    (is (near 1.0 (natural-log (Math/exp 1.))))
    (is (near 0 (sine Math/PI)))
    (is (near 1 (sine (/ Math/PI 2))))
    (is (near 0.7651976 (bessel-j0 1)))
    (is (near -0.2459358 (bessel-j0 10))))
  (testing "harder"
    ;; Apache commons math doesn't seem to have an integrator handy that works
    ;; well on integrals with a singularity at the edge
    (let [near (v/within 5e-3)
          g 9.8
          integrand (fn [theta0]
                      (fn [theta]
                        (/ (Math/sqrt (* 2 g (- (Math/cos theta) (Math/cos theta0)))))))
          ;; the integrand is improper at the right endpoint: the function is not
          ;; defined there. Delta is the "indent" from the right endpoint used to
          ;; avoid the singularity
          L (fn [epsilon delta a] (* 4 (i/definite-integral
                                         (integrand a)
                                         0 (- a delta)
                                         :epsilon epsilon)))]
      (let [delta 1e-6
            epsilon 1e-9
            f (partial L epsilon delta)]
        (is (near 2.00992 (f 0.15)))
        (is (near 2.01844 (f 0.30)))
        (is (near 2.03279 (f 0.45)))
        (is (near 2.0532 (f 0.60)))
        (is (near 2.08001 (f 0.75)))
        (is (near 2.11368 (f 0.9))))
      ;; experiment with "squeezing" the end of the numerical integration
      ;; interval closer to the singularity on the right. How does that
      ;; affect convergence? We can see that it's much more important than
      ;; the epsilon value of the integrator; we can relax that to 1e-6 and
      ;; yet get more accurate answers by reducing delta to 1e-10. Mathematica
      ;; reports that the correct answer is 2.03279.
      (with-comparator (v/within 1e-6)
        (is (ish? [1.1505131
                   1.7583257
                   1.9461327
                   2.0053914
                   2.0241265
                   2.0300509
                   2.0319244
                   2.0325168
                   2.0327042]
                 (map #(L 1e-6 % 0.45) (map #(Math/pow 10 (- %)) (range 1 10))))))))

  (testing "elliptic integral"
    (let [F (fn [phi k]
              (i/definite-integral #(/ (Math/sqrt (- 1 (* k k (Math/pow (Math/sin %) 2))))) 0 phi
                :relative-accuracy 0.01
                :absolute-accuracy 0.001
                :max-evaluations 1000000))]
      (is (near 0.303652 (F 0.3 (Math/sqrt 0.8))))
      (is (near 1.30567 (F 1.2 (Math/sqrt 0.4)))))))
