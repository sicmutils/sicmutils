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

(ns sicmutils.numerical.quadrature.adaptive-test
  (:require [clojure.test :refer [is deftest testing]]
            [same :refer [ish? with-comparator]
             #?@(:cljs [:include-macros true])]
            [sicmutils.numerical.quadrature.adaptive :as qa]
            [sicmutils.numerical.quadrature.common :as qc]
            [sicmutils.numerical.quadrature.bulirsch-stoer :as bs]
            [sicmutils.value :as v]))

(def ^:private adaptive-integrate
  (qa/adaptive
   bs/open-integral
   bs/closed-integral))

(deftest adaptive-tests
  (with-comparator (v/within 1e-10)
    (let [inverse (fn [x] (/ x))]
      (testing "inverse function"
        (let [real-answer (- (Math/log 100)
                             (Math/log 0.01))
              actual (adaptive-integrate inverse 0.01 100 {:tolerance 1e-10})]
          (is (ish? 9.210340371976189
                    (:result actual))
              "convergence happens, even at high precision")

          (is (ish? real-answer (:result actual))
              "The estimate comes close to the correct answer")

          (is (ish? {:converged? true
                     :iterations 23
                     :result 9.210340371904028}
                    (adaptive-integrate inverse 0.01 100 {:adaptive-neighborhood-width 0}))
              "faster at lower precision.")))

      (testing "forcing splits slows down convergence."
        (is (ish? {:converged? true
                   :iterations 267
                   :result 9.210340372036757}
                  (adaptive-integrate inverse 0.01 100
                                      {:maxterms 3
                                       :adaptive-neighborhood-width 0})))))

    (testing "adaptive respects intervals"
      (let [f (fn [x] (condp = x
                       0 (u/illegal "Zero!!")
                       1 (u/illegal "One!")
                       (/ 4 (+ 1 (* x x)))))
            opts (fn [interval]
                   (-> {:adaptive-neighborhood-width 0

                        ;; To force internal splits:
                        :maxterms 3}
                       (qc/with-interval interval)))]
        (testing "both endpoints blow up, so any of the following should throw
        on subdivision."
          (is (thrown? #?(:clj Exception :cljs js/Error)
                       (adaptive-integrate f 0 1 (opts qc/closed))))

          (is (thrown? #?(:clj Exception :cljs js/Error)
                       (adaptive-integrate f 0 1 (opts qc/closed-open))))

          (is (thrown? #?(:clj Exception :cljs js/Error)
                       (adaptive-integrate f 0 1 (opts qc/open-closed)))))

        (is (ish? {:converged? true
                   :iterations 31
                   :result 3.1415926535899246}
                  (adaptive-integrate f 0 1 (opts qc/open)))
            "Even with splits, open endpoints stay respected.")

        (is (ish? {:converged? true
                   :iterations 1
                   :result 3.1415926532781384}
                  (adaptive-integrate f 0 1))
            "Interval is open by default.")))))
