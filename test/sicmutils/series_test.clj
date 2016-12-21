;
; Copyright (C) 2016 Colin Smith.
; This work is based on the Scmutils system of MIT/GNU Scheme.
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

(ns sicmutils.series-test
  (:require [clojure.test :refer :all]
            [sicmutils
             [numsymb]
             [simplify]
             [generic :as g]
             [series :as series]
             [value :as v]
             ]))


(deftest series-test
  (let [Q (series/starting-with 4)
        R (series/starting-with 4 3)
        S (series/starting-with 4 3 2)]
    (is (= '(4 0 0 0 0 0 0 0) (take 8 (series/->seq Q))))
    (is (= '(4 3 0 0 0 0 0 0) (take 8 (series/->seq R))))
    (is (= '(4 3 2 0 0 0 0 0) (take 8 (series/->seq S))))
    (is (= 4 (series/sum S 1)))
    (is (= 7 (series/sum S 2)))
    (is (= 7 (series/sum R 8)))
    (is (= 4 (series/sum Q 20)))
    (is (= 9 (series/sum S 3)))
    (is (= 9 (series/sum S 4)))
    (is (= '(0 1 2 3) (take 4 (series/->seq (series/generate identity)))))
    (is (= '(0 1 4 9) (take 4 (series/->seq (series/generate g/square)))))
    (is (= '(0 2 6 12) (take 4 (series/->seq
                                (g/+
                                 (series/generate identity)
                                 (series/generate g/square)
                                 )))))
    (is (= '(0 m (* 2 m) (* 3 m))
           (->> (series/generate identity)
                (g/* 'm)
                series/->seq
                (take 4)
                g/simplify)))
    #_(is (= '(0 m (* 2 m) (* 3 m))
           (->> (series/generate identity)
                g/square
                (g/* 'm)
                series/->seq
                (take 4)
                g/simplify)))))
