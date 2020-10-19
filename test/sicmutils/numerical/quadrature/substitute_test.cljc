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

(ns sicmutils.numerical.quadrature.substitute-test
  (:refer-clojure :exclude [+ - * /])
  (:require [clojure.test :refer [is deftest testing]]
            [same :refer [ish?]]
            [sicmutils.numerical.quadrature.boole :as qb]
            [sicmutils.numerical.quadrature.trapezoid :as qt]
            [sicmutils.function :as f #?@(:cljs [:include-macros true])]
            [sicmutils.generic :as g :refer [+ - * /]]
            [sicmutils.numsymb]
            [sicmutils.simplify :as s :refer [hermetic-simplify-fixture]]))

#_
((power-law sicmutils.numerical.quadrature.simpson/integral 0.8 true)
 (fn  [x] (Math/sin x)) 0 10 {})
