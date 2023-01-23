#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.numerical.quadrature.common-test
  (:require [clojure.test :refer [is deftest testing]]
            [emmy.numerical.quadrature.common :as qc]
            [emmy.value :as v]))

(deftest interval-tests
  (testing "an interval is open unless it's fully closed"
    (is (qc/closed? qc/closed))
    (is (qc/open? qc/open))
    (is (qc/open? qc/open-closed))
    (is (qc/open? qc/closed-open))
    (is (not (qc/open? qc/closed)))
    (is (qc/open? "face") "Anything not closed is open."))

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
                     (fn [_ _ _] slim-estimate)
                     (fn [_ _ _ _] estimates))]
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
