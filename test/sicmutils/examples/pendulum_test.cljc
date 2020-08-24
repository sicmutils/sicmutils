;;
;; Copyright © 2017 Colin Smith.
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

(ns sicmutils.examples.pendulum-test
  (:require [clojure.test :refer [is deftest testing use-fixtures]]
            [sicmutils.env :refer [up simplify]]
            [sicmutils.examples.pendulum :as p]
            [sicmutils.simplify :refer [hermetic-simplify-fixture]]))

(use-fixtures :once hermetic-simplify-fixture)

(deftest simple-pendulum
  (is (= '(+ (* #?(:clj 1/2 :cljs 0.5) (expt l 2) m (expt thetadot 2))
             (* g l m (cos theta)))
         (simplify ((p/L 'm 'l 'g (fn [_t] (up 0 0))) (up 't 'theta 'thetadot))))))
