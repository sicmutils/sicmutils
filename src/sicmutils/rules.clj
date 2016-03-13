;
; Copyright (C) 2016 Colin Smith.
; This work is based on the Scmutils system of MIT/GNU Scheme.
;
; This is free software;  you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3 of the License, or (at
; your option) any later version.
;
; This software is distributed in the hope that it will be useful, but
; WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
; General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this code; if not, see <http://www.gnu.org/licenses/>.
;

(ns sicmutils.rules
  (:require [pattern.rule :refer [ruleset rule-simplifier]]))

(def ^:private => (constantly true))

(defn- more-than-two? [x] (and (number? x) (> x 2)))
(defn- at-least-two? [x] (and (number? x) (>= x 2)))
(defn- even-integer? [x] (and (number? x) (even? x)))

(def sin-sq->cos-sq
  (rule-simplifier
   (ruleset
    (expt (sin :x) (:? n at-least-two?))
    => (* (expt (sin :x) (:? #(- (% 'n) 2)))
          (- 1 (expt (cos :x) 2))))))

(def ^:private split-high-degree-cosines
  (ruleset
   (* :f1* (expt (cos :x) (:? n more-than-two?)) :f2*)
   => (* (expt (cos :x) 2)
         (expt (cos :x) (:? #(- (% 'n) 2)))
         :f1*
         :f2*)

   (+ :a1* (expt (cos :x) (:? n more-than-two?)) :a2*)
   => (+ (* (expt (cos :x) 2)
            (expt (cos :x) (:? #(- (% 'n) 2))))
         :a1*
         :a2*)))

(def ^:private split-high-degree-sines
  (ruleset
   (* :f1* (expt (sin :x) (:? n more-than-two?)) :f2*)
   => (* (expt (sin :x) 2)
         (expt (sin :x) (:? #(- (% 'n) 2)))
         :f1*
         :f2*)

   (+ :a1* (expt (sin :x) (:? n more-than-two?)) :a2*)
   => (+ (* (expt (sin :x) 2)
            (expt (sin :x) (:? #(- (% 'n) 2))))
         :a1*
         :a2*)))

(def simplify-square-roots
  (rule-simplifier
   (ruleset
    (expt (sqrt :x) (:? n even-integer?))
    => (expt :x (:? #(/ (% 'n) 2)))

    (sqrt (expt :x (:? n even-integer?)))
    => (expt :x (:? #(/ (% 'n) 2)))

    ;; others to follow
    )))

(def complex-trig
  ;; TODO: clearly more of these are needed.
  (rule-simplifier
   (ruleset
    (cos (* (complex 0.0 1.0) :z))
    => (cosh :z)

    (sin (* (complex 0.0 1.0) :z))
    => (* (complex 0.0 1.0) (sinh :z))

    ;; Does this really belong here?
    ;; It works by reducing n mod 4 and then indexing into [1 i -1 -i].
    (expt (complex 0.0 1.0) (:? n integer?))
    => (:? #([1 '(complex 0 1) -1 '(complex 0 -1)] (mod (% 'n) 4))))))

(def divide-numbers-through
  (ruleset
   (* 1 :factor)
   => :factor

   (* 1 :factors*)
   => (* :factors*)

   (/ (:? n number?) (:? d number?))
   => (:? #(/ (% 'n) (% 'd)))

   (/ (+ :terms*) (:? d number?))
   => (+ (:?? #(map (fn [n] `(~'/ ~n ~(% 'd))) (% :terms*))))))

(def ^:private flush-obvious-ones
  (ruleset
   (+ :a1* (expt (sin :x) 2) :a2* (expt (cos :x) 2) :a3*)
   => (+ 1 :a1* :a2* :a3*))
  ;; are sines always before cosines after we poly simplify?
  ;; they are in scmutils, so we should be alert for this.
  ;; in scmutils, there are a couple of others that involve rcf:simplify,
  ;; which we dont' have, and we don't know if pcf:simplify is an
  ;; acceptable substitute here; and we don't have a method for
  ;; pasting the value of a predicate into a rule, so this is far from
  ;; complete.
  )

(def trig->sincos
  (rule-simplifier
   (ruleset
    ;; GJS has other rules: to map cot, sec and csc to sin/cos, but
    ;; I don't think we need those since we transform those to sin/cos
    ;; in the env namespace.
    (tan :x) => (/ (sin :x) (cos :x)))))

(def sincos->trig
  (ruleset
   ;; undoes the effect of trig->sincos
   (/ (sin :x) (cos :x))
   => (tan :x)

   (/ (sin :x) (* :d1* (cos :x) :d2*))
   => (/ (tan :x) (* :d1* :d2*))

   (/ (* :n1* (sin :x) :n1*)
      (* :d1* (cos :x) :d2*))
   => (/ (* :n1* (tan :x) :n2*)
         (* :d1* :d2*))))

(def sincos-flush-ones (rule-simplifier split-high-degree-cosines
                                        split-high-degree-sines
                                        flush-obvious-ones))
