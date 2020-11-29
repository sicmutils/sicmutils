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
  (:require [sicmutils.complex :as c]
            [sicmutils.expression :as x]
            [sicmutils.generic :as g]
            [sicmutils.numsymb :as ns]
            [sicmutils.util :as u]
            [sicmutils.value :as v])
  #?(:clj
     (:import (clojure.lang Symbol))))

(defn literal-number [x]
  (x/make-literal ::x/numeric x))

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


;; ## Generic Installation TODO move these to abstract number...

(defn- numerical-expression [expr]
  (cond (v/number? expr)        expr
        (c/complex? expr)       expr
        (abstract-number? expr) (x/expression-of expr)
        :else expr))

(defn- defbinary [generic-op op-sym]
  (let [pairs [[::x/numeric ::x/numeric]
               [::v/number ::x/numeric]
               [::x/numeric ::v/number]]]
    ;; TODO fix this once we figure out
    (prefer-method generic-op
                   [::x/numeric ::v/number]
                   [::v/number ::x/numeric])
    (if-let [op (ns/symbolic-operator op-sym)]
      (doseq [[l r] pairs]
        (defmethod generic-op [l r] [a b]
          (literal-number
           (op (numerical-expression a)
               (numerical-expression b)))))

      (doseq [[l r] pairs]
        (defmethod generic-op [l r] [a b]
          (x/make-combination op-sym [a b]))))))

(defn- defunary [generic-op op-sym]
  (if-let [op (ns/symbolic-operator op-sym)]
    (defmethod generic-op [::x/numeric] [a]
      (literal-number (op (numerical-expression a))))
    (defmethod generic-op [::x/numeric] [a]
      (x/make-combination op-sym [a]))))

(derive Symbol ::x/numeric)
(derive ::v/number ::x/numeric)

(defbinary g/add '+)
(defbinary g/sub '-)
(defbinary g/mul '*)
(defbinary g/div '/)
(defbinary g/expt 'expt)
(defunary g/negate 'negate)
(defunary g/invert 'invert)

(defunary g/sin 'sin)
(defunary g/cos 'cos)
(defunary g/tan 'tan)

(defunary g/asin 'asin)
(defunary g/acos 'acos)
(defunary g/atan 'atan)
(defbinary g/atan 'atan)

(defunary g/sinh 'sinh)
(defunary g/cosh 'cosh)
(defunary g/sec 'sec)
(defunary g/csc 'csc)

(defunary g/abs 'abs)
(defunary g/sqrt 'sqrt)
(defunary g/log 'log)
(defunary g/exp 'exp)
