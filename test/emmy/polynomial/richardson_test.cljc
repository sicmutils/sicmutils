#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.polynomial.richardson-test
  (:require [clojure.test :refer [is deftest testing]]
            [same :refer [ish?]]
            [emmy.numbers]
            [emmy.polynomial.interpolate :as pi]
            [emmy.polynomial.richardson :as pr]
            [emmy.util.stream :as us]
            [emmy.value :as v]))

(deftest richardson-limit-tests
  (let [pi-seq pr/archimedean-pi-sequence]

    (testing "without richardson extrapolation, the sequence takes a long time."
      (is (ish? {:converged? true
                 :terms-checked 26
                 :result 3.1415926535897944}
                (us/seq-limit pi-seq {:tolerance v/machine-epsilon}))))

    (testing "with richardson, we go faster."
      (is (ish? {:converged? true
                 :terms-checked 7
                 :result 3.1415926535897936}
                (-> (pr/richardson-sequence pi-seq 2 2 2)
                    (us/seq-limit {:tolerance v/machine-epsilon}))))

      (is (ish? {:converged? false
                 :terms-checked 3
                 :result 3.1415903931299374}
                (-> (take 3 pi-seq)
                    (pr/richardson-sequence 2 2 2)
                    (us/seq-limit {:tolerance v/machine-epsilon})))
          "richardson-sequence bails if the input sequence runs out of terms.")

      (is (ish? [2.8284271247461903
                 3.1391475703122276
                 3.1415903931299374
                 3.141592653286045
                 3.1415926535897865]
                (take 5 (pr/richardson-sequence pi-seq 4)))))

    (testing "richardson extrapolation is equivalent to polynomial extrapolation
    to 0"
      (let [h**2 (fn [i]
                   ;; (1/t^{i + 1})^2
                   (-> (/ 1 (Math/pow 2 (inc i)))
                       (Math/pow 2)))
            xs (map-indexed (fn [i fx] [(h**2 i) fx]) pi-seq)]
        (is (ish? (take 7 (us/seq-limit
                           (pr/richardson-sequence pi-seq 4 1 1)))
                  (take 7 (us/seq-limit
                           (pi/modified-neville xs 0.0)))))))))

(deftest fold-tests
  (let [pi-seq pr/archimedean-pi-sequence]
    (let [sum  (pr/richardson-sum 2 2 #(+ % 2))
          scan (pr/richardson-scan 2 2 #(+ % 2))]
      (is (= (take 10 (scan pi-seq))
             (take 10 (pr/richardson-sequence pi-seq 2 2 2)))
          "Scanning via richardson-fold is equivalent to richardson-sequence")

      (is (= (sum (take 10 pi-seq))
             (last (pr/richardson-sequence (take 10 pi-seq) 2 2 2)))
          "Summing via richardson-fold is equivalent to taking the final estimate
         from richardson-sequence"))

    (testing "fold's error terms start at expt == 1 and increase by 1 each time
              by default"
      (let [sum  (pr/richardson-sum 2)
            scan (pr/richardson-scan 2)]
        (is (= (take 10 (scan pi-seq))
               (take 10 (pr/richardson-sequence pi-seq 2 1 1)))
            "Scanning via richardson-fold is equivalent to richardson-sequence")

        (is (= (sum (take 10 pi-seq))
               (last (pr/richardson-sequence (take 10 pi-seq) 2 1 1)))
            "Summing via richardson-fold is equivalent to taking the final estimate
         from richardson-sequence")))))
