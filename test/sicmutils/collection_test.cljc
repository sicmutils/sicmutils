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

(ns sicmutils.collection-test
  (:require [clojure.test :refer [is deftest testing]]
            [sicmutils.calculus.derivative :refer [D]]
            [sicmutils.collection]
            [sicmutils.generic :as g]))

(deftest coll-test
  (let [m {:sin g/sin :cos g/cos}
        {D-sin :sin D-cos :cos} (D m)]
    (is (= {:sin ((D g/sin) 'x)
            :cos ((D g/cos) 'x)}
           {:sin (D-sin 'x)
            :cos (D-cos 'x)})
        "derivatives get pushed inside maps.")))
