#_
"Copyright © 2020 Sam Ritchie.
This work is based on the Scmutils system of MIT/GNU Scheme:
Copyright © 2002 Massachusetts Institute of Technology

This is free software;  you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or (at
your option) any later version.

This software is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this code; if not, see <http://www.gnu.org/licenses/>."

(ns sicmutils.numerical.quadrature.midpoint-test
  (:require [clojure.test :refer [is deftest testing]]
            [same :refer [ish?]]
            [sicmutils.numerical.quadrature.midpoint :as qm]
            [sicmutils.numerical.quadrature.riemann :as qr]
            [sicmutils.numsymb]
            [sicmutils.util :as u]))

(deftest midpoint-tests
  (testing "midpoint-sum does the job"
    (is (= (* 0.5 10 10)
           ((qr/midpoint-sum identity 0.0 10.0) 10))))

  (testing "incremental midpoint is more efficient, even with arbitrary
  sequences"
    (let [f (fn [x] (/ 4 (+ 1 (* x x))))
          [counter1 f1] (u/counted f)
          [counter2 f2] (u/counted f)
          n-seq (interleave
                 (iterate (fn [x] (* 2 x)) 2)
                 (iterate (fn [x] (* 2 x)) 3))]
      (is
       (ish? (take 12 (qm/midpoint-sequence f1 0 1 {:n n-seq}))
             (take 12 (map (qr/midpoint-sum f2 0 1) n-seq)))
       "The incremental and non-incremental versions of the midpoint method
       produce ~identical results.")

      (is (= [253 315]
             [@counter1 @counter2])
          "The incremental method requires many fewer evaluations."))))

(deftest api-tests
  (testing "midpoint integrator never evaluates the endpoints"
    (let [f (fn [x] (condp = x
                     0 (u/illegal "Zero!!")
                     1 (u/illegal "One!")
                     (/ 4 (+ 1 (* x x)))))]
      (is (ish? {:converged? true
                 :terms-checked 9
                 :result 3.141592655525674}
                (qm/integral f 0 1))
          "convergence happens fairly fast!")

      (is (ish? {:converged? true
                 :terms-checked 5
                 :result 3.141592653582428}
                (qm/integral f 0 1 {:accelerate? true}))
          "convergence happens faster when accelerated."))))
