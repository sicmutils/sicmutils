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

(ns sicmutils.abstract.number
  (:require [sicmutils.expression :as x]
            [sicmutils.util :as u]
            [sicmutils.value :as v]))

(defn literal-number [x]
  (x/->Literal ::x/numeric x #{}))

(defn literal-number? [x]
  (and (x/literal? x)
       (= (:type x) ::x/numeric)))

(defn abstract-number? [x]
  (or (literal-number? x)
      (symbol? x)))
