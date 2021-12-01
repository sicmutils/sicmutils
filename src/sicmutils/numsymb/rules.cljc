;;
;; Copyright © 2021 Adam Haber.
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

(ns sicmutils.numsymb.rules
  "Implementations of the generic operations for numeric types that have
   optimizations available, and for the general symbolic case.
   The implementations in this namespace are pattern-matching-based versions
   of the implementations in the sicmutils.numsymb namespace."
  (:require [sicmutils.complex :as c]
            [sicmutils.euclid]
            [sicmutils.generic :as g]
            [sicmutils.numbers]
            [sicmutils.ratio]
            [sicmutils.value :as v]
            [sicmutils.util :as u]
            [sicmutils.util.aggregate :as ua]
            [pattern.rule :as r :refer [=>]]
            [sicmutils.numsymb :as sym]))

(defn sym:not
  "For symbolic `x`, returns a symbolic expression representing the logical
  negation of `x`. For boolean `x`, returns the negation of `x`."
  [x]
  (let [RS (r/ruleset
            (? ?x boolean?) => (? #(not (% '?x)))
            ?x => (not ?x))]
    (RS x)))

(defn sym:gcd [x y]
  (let [gcd (r/ruleset
             ((? ?a v/number?) (? ?b v/number?)) => (? #(g/gcd (% '?a) (% '?b)))
             (0 ?b) => ?b
             (1 _) => 1
             ((? ?a v/number?) ?b) => (gcd ?a ?b)
             (?a 0) => ?a
             (_ 1) => 1
             (?a ?a) => ?a
             (?a ?b) => (gcd ?a ?b))]
    (gcd [x y])))


(defn tan
  [x]
  (let [tan (r/ruleset
             (? _ v/number? v/zero?) => 0
             (? ?x v/number? v/exact?) => (tan ?x)
             (? _ v/number? sym/n:zero-mod-pi?) => 0
             (? _ v/number? sym/n:pi-over-4-mod-pi?) => 1
             (? _ v/number? sym/n:-pi-over-4-mod-pi?) => -1
             (? _ v/number? sym/n:pi-over-2-mod-pi?) => (u/illegal "Undefined: tan")
             (? ?x v/number?) => (? #(Math/tan (% '?x)))
             (? _ symbol? sym/zero-mod-pi?) => 0
             (? _ symbol? sym/pi-over-4-mod-pi?) => 1
             (? _ symbol? sym/-pi-over-4-mod-pi?) => -1
             (? _ symbol? sym/pi-over-2-mod-pi?) => (u/illegal "Undefined: tan")
             ?x => (tan ?x))]
    (tan x)))


(defn atan
  ([y]
   (let [not-exact? (fn [x] (not (v/exact? x)))
         atan (r/ruleset
               (? ?y v/number? not-exact?) => (? #(g/atan (% '?y)))
               (? _ v/number? v/zero?) => 0
               ?y => (atan ?y))]
     (atan y)))
  ([y x]
   (let [atan (r/ruleset
               (?y (? ?x v/one?)) => (? #(atan (% '?y))) ;;(atan ?y)
               ((? _ v/number? v/exact? v/zero?) ?x) => 0
               ((? ?y v/number? v/exact?) (? ?x v/number? v/exact? v/zero?)) => (? #(g/atan (% '?y) (% '?x)))
               ((? ?y v/number? v/exact?) (? ?x v/number? v/exact?)) => (atan ?y ?x)
               ((? ?y v/number? v/exact?) (? ?x v/number?)) => (? #(g/atan (% '?y) (% '?x)))
               ((? ?y v/number? v/exact?) ?x) => (atan ?y ?x)
               ((? ?y v/number?) (? ?x v/number?)) => (? #(g/atan (% '?y) (% '?x)))
               (?y ?x) => (atan ?y ?x))]
     (atan [y x]))))