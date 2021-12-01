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
            [pattern.rule :as r]
            [sicmutils.numsymb :as sym]))


;; Q - what would be the best way to reuse these?
;; ## Trig Functions

(def ^:private pi Math/PI)
(def ^:private pi-over-4 (/ pi 4))
(def ^:private two-pi (* 2 pi))
(def ^:private pi-over-2 (* 2 pi-over-4))

(defn ^:private n:zero-mod-pi? [x]
  (v/almost-integral? (/ x pi)))

(defn ^:private n:pi-over-2-mod-2pi? [x]
  (v/almost-integral? (/ (- x pi-over-2 two-pi))))

(defn ^:private n:-pi-over-2-mod-2pi? [x]
  (v/almost-integral? (/ (+ x pi-over-2) two-pi)))

(defn ^:private n:pi-mod-2pi? [x]
  (v/almost-integral? (/ (- x pi) two-pi)))

(defn ^:private n:pi-over-2-mod-pi? [x]
  (v/almost-integral? (/ (- x pi-over-2) pi)))

(defn ^:private n:zero-mod-2pi? [x]
  (v/almost-integral? (/ x two-pi)))

(defn ^:private n:-pi-over-4-mod-pi? [x]
  (v/almost-integral? (/ (+ x pi-over-4) pi)))

(defn ^:private n:pi-over-4-mod-pi? [x]
  (v/almost-integral? (/ (- x pi-over-4) pi)))

(def ^:no-doc zero-mod-pi? #{'-pi 'pi '-two-pi 'two-pi})
(def ^:no-doc pi-over-2-mod-2pi? #{'pi-over-2})
(def ^:no-doc -pi-over-2-mod-2pi? #{'-pi-over-2})
(def ^:no-doc pi-mod-2pi? #{'-pi 'pi})
(def ^:no-doc pi-over-2-mod-pi? #{'-pi-over-2 'pi-over-2})
(def ^:no-doc zero-mod-2pi? #{'-two-pi 'two-pi})
(def ^:no-doc -pi-over-4-mod-pi? #{'-pi-over-4})
(def ^:no-doc pi-over-4-mod-pi? #{'pi-over-4 '+pi-over-4})


(defn sym:not
  "For symbolic `x`, returns a symbolic expression representing the logical
  negation of `x`. For boolean `x`, returns the negation of `x`."
  [x]
  (let [RS (r/ruleset
            (? ?x true?) r/=> false
            (? ?x false?) r/=> true
            ?x r/=> (not ?x))]
    (RS x)))

;; basic tests - these should probavbly live elsewhere?
(let [v [true false 1 0 'a '() ()]]
  (= (map sym:not v)
     (map (sym/symbolic-operator 'not) v)))

(defn sym:gcd [x y]
  (let [gcd (r/ruleset
             ((? ?a v/number?) (? ?b v/number?)) r/=> (?? #(g/gcd (% '?a) (% '?b)))
             (0 ?b) r/=> ?b
             (1 ?b) r/=> 1
             (?a 0) r/=> ?a
             (?a 1) r/=> 1
             (?a ?b) r/=> (gcd ?a ?b))]
    (gcd [x y])))


(let [v [0 1 2 3 4 'a true +]]
  (= (map #((ua/monoid sym:gcd 0) 2 %) v)
     (map #((sym/symbolic-operator 'gcd) 2 %) v)))

(defn tan
  [x]
  (let [tan (r/ruleset
             (? ?x v/zero?) r/=> 0
             (? ?x v/exact?) r/=> (tan ?x)
             (? ?x n:zero-mod-pi?) r/=> 0
             (? ?x n:pi-over-4-mod-pi?) r/=> 1
             (? ?x n:-pi-over-4-mod-pi?) r/=> -1
             (? ?x n:pi-over-2-mod-pi?) r/=> (u/illegal "Undefined: tan")
             (? ?x v/number?) r/=> (?? #(Math/tan (% '?x)))
             (? ?x zero-mod-pi?) r/=> 0
             (? ?x pi-over-4-mod-pi?) r/=> 1
             (? ?x -pi-over-4-mod-pi?) r/=> -1
             (? ?x pi-over-2-mod-pi?) r/=> (u/illegal "Undefined: tan")
             (? ?x v/number?) r/=> (?? #(Math/tan (% '?x)))
             ?x r/=> (tan ?x))]
    (tan x)))

(def v [0 0.1 (/ pi 4) (/ (* 3 pi) 4) 0/1 1/3 -1.2])
(map tan v)
(= (map tan v)
   (map (sym/symbolic-operator 'tan) v))


(defn atan
  ([y]
   (let [not-exact? (fn [x] (not (v/exact? x)))
         atan (r/ruleset
               (? ?y v/number? not-exact?) r/=> (?? #(g/atan (% '?y)))
               (? ?y v/number? v/zero?) r/=> 0
               ?y r/=> (atan ?y))]
     (atan y)))
  ([y x]
   (let [atan (r/ruleset
               (?y (? ?x v/one?)) r/=> (?? #(atan (% '?y))) ;;(atan ?y)
               ((? ?y v/number? v/exact? v/zero?) ?x) r/=> 0
               ((? ?y v/number? v/exact?) (? ?x v/number? v/exact? v/zero?)) r/=> (?? #(g/atan (% '?y) (% '?x)))
               ((? ?y v/number? v/exact?) (? ?x v/number? v/exact?)) r/=> (atan ?y ?x)
               ((? ?y v/number? v/exact?) (? ?x v/number?)) r/=> (?? #(g/atan (% '?y) (% '?x)))
               ((? ?y v/number? v/exact?) ?x) r/=> (atan ?y ?x)
               ((? ?y v/number?) (? ?x v/number?)) r/=> (?? #(g/atan (% '?y) (% '?x)))
               (?y ?x) r/=> (atan ?y ?x))]
     (atan [y x]))))

(def v [0 0.1 (/ pi 4) (/ (* 3 pi) 4) 0/1 1/3 -1.2])
(= (map atan v)
   (map (sym/symbolic-operator 'atan) v))

(= (map #(atan 0 %) v)
   (map #((sym/symbolic-operator 'atan) 0 %) v))

(= (map #(atan 1 %) v)
   (map #((sym/symbolic-operator 'atan) 1 %) v))

(= (map #(atan % 1) v)
   (map #((sym/symbolic-operator 'atan) % 1) v))