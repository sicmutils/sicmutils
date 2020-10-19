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

(ns sicmutils.numerical.quadrature.common-test
  (:require [clojure.test :refer [is deftest testing]]
            [sicmutils.numerical.quadrature.common :as qc]
            [sicmutils.value :as v]))

(deftest interval-tests
  (testing "an interval is open unless it's fully closed"
    (is (qc/closed? qc/closed))
    (is (qc/open? qc/open))
    (is (qc/open? qc/open-closed))
    (is (qc/open? qc/closed-open))
    (is (not (qc/open? qc/closed)))
    (is (qc/open? "face") "Anything not closed is open."))

  (testing "infinities"
    (is (qc/infinite? ##Inf))
    (is (qc/infinite? ##-Inf))
    (is (not (qc/infinite? 10))))

  (testing "interval changing functions"
    (is (qc/closed? (qc/close-l qc/open-closed)))
    (is (qc/closed? (qc/close-r qc/closed-open)))
    (is (qc/open? (qc/close-r qc/open)))
    (is (= qc/closed-open (qc/flip qc/open-closed)))))

(deftest interval-opt-tests
  (testing "update-interval updates properly."
    (is (= qc/open (qc/interval {}))
        "defaults to open")

    (is (= qc/closed-open
           (qc/interval
            (qc/update-interval {} qc/close-l)))
        "update-interval updates the default")

    (is (= qc/closed
           (-> (qc/with-interval {} qc/closed-open)
               (qc/update-interval qc/close-r)
               (qc/interval)))
        "with-interval sets the interval in options.")))

(deftest make-integrator-tests
  (testing "make-integrator-fn builds integrators"
    (let [f (fn [x] (* x x))
          estimates [1000 100 10 10]
          slim-estimate 3.14
          integrate (qc/make-integrator-fn
                     (fn [f a b] slim-estimate)
                     (fn [f a b opts] estimates))]
      (is (= {:converged? true
              :terms-checked 4
              :result 10}
             (integrate f 0 10))
          "our fake sequence converges after four steps!")

      (let [tiny-r (fn [l] (+ l (* 10 v/machine-epsilon)))]
        (is (= {:converged? true
                :terms-checked 1
                :result slim-estimate}
               (integrate f 0 (tiny-r 0))
               (integrate f 10 (tiny-r 10)))
            "A tiny integration window calls the supplied `area-fn`."))

      (is (= {:converged? true
              :terms-checked 1
              :result slim-estimate}
             (integrate f 12 25 {:roundoff-cutoff 20}))
          "`:roundoff-cutoff` widens the window."))))
