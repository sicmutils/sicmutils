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

(ns sicmutils.numerical.quadrature.adaptive-test
  (:require [clojure.test :refer [is deftest testing]]
            [same :refer [ish?]]
            [sicmutils.numerical.quadrature.adaptive :as qa]
            [sicmutils.numerical.quadrature.bulirsch-stoer :as bs]
            [sicmutils.numsymb]
            [sicmutils.util :as u]))

;; Example:
#_
(let [f (fn [x] (/ 4 (+ 1 (* x x))))
      integrate (adaptive
                 bs/open-integral
                 bs/closed-integral)]
  (time (integrate f 0 1)))
