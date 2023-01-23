#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.numerical.derivative-test
  (:require [clojure.test :refer [is deftest testing]]
            [same :refer [ish? with-comparator] :include-macros true]
            [emmy.calculus.derivative :as cd]
            [emmy.generic :as g]
            [emmy.numerical.derivative :as d]
            [emmy.polynomial.richardson :as r]
            [emmy.util :as u]
            [emmy.util.stream :as us]
            [emmy.value :as v]))

(deftest derivative-tests
  (let [f         g/sqrt
        x         1
        h         0.1
        tolerance 1e-13
        ratio     (/ (f x)
                     (- (f (+ x h))
                        (f (- x h))))]
    (is (= 6.0 (@#'d/terms-before-roundoff ratio tolerance))
        "It would take 5 halvings of h for roundoff to overwhelm machine
        precision."))

  (testing "eventually the sequence converges"
    (is (= (-> (@#'d/central-diff-stream g/sqrt 1 0.1)
               (us/seq-limit {:tolerance 1e-13}))

           {:converged? true
            :terms-checked 15
            :result 0.5000000000109139})))

  (testing "richardson-sequence makes it converge much faster."
    (is (= {:converged? true
            :terms-checked 5
            :result 0.5000000000000159}

           (let [h 0.1, p 2, q 2]
             (-> (@#'d/central-diff-stream g/sqrt 1 h)
                 (r/richardson-sequence 2 p q)
                 (us/seq-limit {:tolerance 1e-13})))))))

(deftest D-numeric-tests
  (with-comparator (v/within (g/sqrt v/machine-epsilon))
    (testing "D-numeric packages all of this up."
      (let [f (d/D-numeric g/sqrt)]
        (is (ish? (u/double
                   ((cd/D g/sqrt) 1))
                  (f 1))
            "Matches the true derivative."))

      (let [f (d/D-numeric g/sqrt {:tolerance 1e-6})]
        (is (ish? 0.5000000002540212 (f 1)))

        (is (not= (f 1) (f 1 {:tolerance 1e-13}))
            "the internal options override."))))
  (testing "info? returns a full dict of info."
    (let [expected {:converged? true
                    :terms-checked 4
                    :result 0.4999999999999701}]
      (is (= expected ((d/D-numeric g/sqrt {:info? true}) 1)))
      (is (= expected ((d/D-numeric g/sqrt) 1  {:info? true})))))

  (testing "method customizes"
    (let [expected {:converged? true
                    :terms-checked 4
                    :result 0.4999999999999701}]
      (is (= expected ((d/D-numeric g/sqrt {:info? true}) 1)))
      (is (= expected ((d/D-numeric g/sqrt) 1  {:info? true})))))

  (testing "initial-h overrides"
    (is (ish? {:converged? true
               :terms-checked 4
               :result 0.022360679774996683}
              ((d/D-numeric g/sqrt) 500 {:info? true}))
        "Without an initial-h it starts small, at 0.1 times the absolute value
        of x..")

    (is (=  ((d/D-numeric g/sqrt) 500 {:initial-h 50 :info? true})
            ((d/D-numeric g/sqrt) 500 {:info? true}))
        "confirming the default of 0.1 times the abs.")

    (is (ish? {:converged? true
               :terms-checked 6
               :result 0.022360679774952784}
              ((d/D-numeric g/sqrt {:initial-h 400}) 500 {:info? true}))
        "if we set the initial-h way up it takes longer to converge."))

  (testing "maxterms can override, or autocalc"
    (let [f (d/D-numeric g/sqrt {:info? true
                                 :method :forward
                                 :tolerance 1e-13})]
      (is (ish? {:converged? false
                 :terms-checked 5
                 :result 0.49999999982731064}
                (f 1))
          "forward mode can't converge before underflow wins..")

      (is (ish? {:converged? false
                 :terms-checked 5
                 :result 0.5000000002343629}
                (f 1 {:method :backward}))
          "backward mode can't converge before underflow wins..")

      (is (ish? {:converged? true
                 :terms-checked 8
                 :result 0.4999999999999164}
                (f 1 {:maxterms 20
                      :method :backward}))
          "We can override the maxterms calculation by supplying our own.")

      (is (ish? {:converged? false
                 :terms-checked 22
                 :result 0.500000004418671}
                (f 1 {:minterms 22
                      :method :backward}))
          "If we override minterms we can force 20 evaluations, and force it
          into underflow territory where it starts diverging again.")

      (is (ish? {:converged? true
                 :terms-checked 80
                 :result 0.0}
                (f 1 {:minterms 80}))
          "If we force it far enough under any method it converges BACK to
          zero."))

    (let [expected {:converged? true
                    :terms-checked 4
                    :result 0.4999999999999701}]
      (is (= expected ((d/D-numeric g/sqrt {:info? true}) 1)))
      (is (= expected ((d/D-numeric g/sqrt) 1  {:info? true}))))))

(deftest central-d2-tests
  (testing "central-d2 mode generates a second derivative."
    (with-comparator (v/within (g/sqrt v/machine-epsilon))
      (let [f   (fn [x] (g/* (g// (g/expt x 3) 3)))
            f'' (d/D-numeric f {:method :central-d2})]

        (is (ish? 10.0 (f'' 5))
            "The second derivative is 2x.")

        (is (ish? (f'' 12.8)
                  (((g/expt cd/D 2) f) 12.8))
            "Numeric second derivative matches the symbolic one.")))))
