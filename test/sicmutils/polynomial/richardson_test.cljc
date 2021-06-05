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

(ns sicmutils.polynomial.richardson-test
  (:require [clojure.test :refer [is deftest testing]]
            [same :refer [ish?]]
            [sicmutils.numbers]
            [sicmutils.polynomial.interpolate :as pi]
            [sicmutils.polynomial.richardson :as pr]
            [sicmutils.util.stream :as us]
            [sicmutils.value :as v]))

(deftest richardson-limit-tests
  (let [pi-seq @#'pr/archimedean-pi-sequence]

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
