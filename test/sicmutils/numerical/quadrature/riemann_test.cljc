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

(ns sicmutils.numerical.quadrature.riemann-test
  (:require [clojure.test :refer [is deftest testing use-fixtures]]
            [same :refer [zeroish? ish? with-comparator]
             #?@(:cljs [:include-macros true])]
            [sicmutils.numerical.interpolate.richardson :as ir]
            [sicmutils.numerical.quadrature.riemann :as qr]
            [sicmutils.numsymb]
            [sicmutils.util.stream :as us]))

(deftest riemann-tests
  (let [f              (fn [x] (* x x))
        left-estimates  (map (@#'qr/left-sum f 0 10)
                             (us/powers 2))
        right-estimates (map (@#'qr/right-sum f 0 10)
                             (us/powers 2))]
    (is (ish? [0.0 125.0 218.75 273.4375 302.734375]
              (take 5 left-estimates))
        "left estimates...")

    (is (ish? {:converged? false
               :terms-checked 16
               :result 333.31807469949126}
              (let [f (fn [x] (* x x))]
                (-> (map (@#'qr/left-sum f 0 10)
                         (us/powers 2))
                    (us/seq-limit {:maxterms 16}))))
        "convergence sucks.")

    (is (ish? {:converged? true
               :terms-checked 4
               :result 333.3333333333333}

              (let [f (fn [x] (* x x))]
                (-> (map (@#'qr/left-sum f 0 10)
                         (us/powers 2))
                    (ir/richardson-sequence 2)
                    (us/seq-limit))))
        "Better with richardson.")

    (let [f (fn [x] (* x x))]
      (is (ish? (take 10 (qr/left-sequence f 0 10 1))
                (take 10 (map (@#'qr/left-sum f 0 10)
                              (us/powers 2 1))))
          "Incremental works."))

    (is (ish? [1000.0 625.0 468.75 398.4375 365.234375]
              (take 5 right-estimates))
        "right estimates..."))
  )
