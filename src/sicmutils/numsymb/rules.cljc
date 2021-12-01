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

;; basic tests - these should probavbly live elsewhere?
(let [v [true false 1 0 'a '() ()]]
  (= (map sym:not v)
     (map (sym/symbolic-operator 'not) v)))

(defn sym:gcd [x y]
  (let [gcd (r/ruleset
             ((? ?a v/number?) (? ?b v/number?)) => (?? #(g/gcd (% '?a) (% '?b)))
             (0 ?b) => ?b
             (1 _) => 1
             (?a 0) => ?a
             (_ 1) => 1
             (?a ?b) => (gcd ?a ?b))]
    (gcd [x y])))


(let [v [0 1 2 3 4 'a true +]]
  (= (map #((ua/monoid sym:gcd 0) 2 %) v)
     (map #((sym/symbolic-operator 'gcd) 2 %) v)))

(defn tan
  [x]
  (let [tan (r/ruleset
             (? _ v/zero?) => 0
             (? ?x v/exact?) => (tan ?x)
             (? _ sym/n:zero-mod-pi?) => 0
             (? _ sym/n:pi-over-4-mod-pi?) => 1
             (? _ sym/n:-pi-over-4-mod-pi?) => -1
             (? _ sym/n:pi-over-2-mod-pi?) => (u/illegal "Undefined: tan")
             (? ?x v/number?) => (? #(Math/tan (% '?x)))
             (? _ sym/zero-mod-pi?) => 0
             (? _ sym/pi-over-4-mod-pi?) => 1
             (? _ sym/-pi-over-4-mod-pi?) => -1
             (? _ sym/pi-over-2-mod-pi?) => (u/illegal "Undefined: tan")
             (? ?x v/number?) => (? #(Math/tan (% '?x)))
             ?x => (tan ?x))]
    (tan x)))

(def v [0 0.1 (/ sym/pi 4) (/ (* 3 sym/pi) 4) 0/1 1/3 -1.2])
(map tan v)
(= (map tan v)
   (map (sym/symbolic-operator 'tan) v))


(defn atan
  ([y]
   (let [not-exact? (fn [x] (not (v/exact? x)))
         atan (r/ruleset
               (? ?y v/number? not-exact?) => (?? #(g/atan (% '?y)))
               (? _ v/number? v/zero?) => 0
               ?y => (atan ?y))]
     (atan y)))
  ([y x]
   (let [atan (r/ruleset
               (?y (? ?x v/one?)) => (?? #(atan (% '?y))) ;;(atan ?y)
               ((? _ v/number? v/exact? v/zero?) ?x) => 0
               ((? ?y v/number? v/exact?) (? ?x v/number? v/exact? v/zero?)) => (?? #(g/atan (% '?y) (% '?x)))
               ((? ?y v/number? v/exact?) (? ?x v/number? v/exact?)) => (atan ?y ?x)
               ((? ?y v/number? v/exact?) (? ?x v/number?)) => (?? #(g/atan (% '?y) (% '?x)))
               ((? ?y v/number? v/exact?) ?x) => (atan ?y ?x)
               ((? ?y v/number?) (? ?x v/number?)) => (?? #(g/atan (% '?y) (% '?x)))
               (?y ?x) => (atan ?y ?x))]
     (atan [y x]))))

(def v [0 0.1 (/ sym/pi 4) (/ (* 3 sym/pi) 4) 0/1 1/3 -1.2])
(= (map atan v)
   (map (sym/symbolic-operator 'atan) v))

(= (map #(atan 0 %) v)
   (map #((sym/symbolic-operator 'atan) 0 %) v))

(= (map #(atan 1 %) v)
   (map #((sym/symbolic-operator 'atan) 1 %) v))

(= (map #(atan % 1) v)
   (map #((sym/symbolic-operator 'atan) % 1) v))