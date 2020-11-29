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
  (x/->Literal ::x/numeric x {}))

(defn literal-number? [x]
  (and (x/literal? x)
       (= (x/literal-type x) ::x/numeric)))

(defn abstract-number? [x]
  (or (literal-number? x)
      (symbol? x)))

(defn- literal=num [l n]
  (and (= (x/literal-type l) ::x/numeric)
       (= (x/expression-of l) n)))

(defmethod v/eq [::x/numeric ::v/number] [l r] (literal=num l r))
(defmethod v/eq [::v/number ::x/numeric] [l r] (literal=num r l))
(prefer-method v/eq [::x/numeric ::v/number] [::v/number ::x/numeric])
