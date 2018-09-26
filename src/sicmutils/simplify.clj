;
; Copyright © 2017 Colin Smith.
; This work is based on the Scmutils system of MIT/GNU Scheme:
; Copyright © 2002 Massachusetts Institute of Technology
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

(ns sicmutils.simplify
  (:import (java.util.concurrent TimeoutException)
           (clojure.lang Sequential Var LazySeq Symbol PersistentVector)
           (java.io StringWriter))
  (:require [clojure.walk :refer [postwalk]]
            [clojure.tools.logging :as log]
            [clojure.set :as set]
            [clojure.pprint :as pp]
            [sicmutils
             [analyze :as a]
             [polynomial :as poly]
             [polynomial-factor :as factor]
             [rational-function :as rf]
             [value :as v]
             [numsymb :as nsy]
             [expression :as x]
             [generic :as g]
             [rules :as rules]]
            [pattern.rule :as rule]))


(defn ^:private unless-timeout
  "Returns a function that invokes f, but catches TimeoutException;
  if that exception is caught, then x is returned in lieu of (f x)."
  [f]
  (fn [x]
    (try (f x)
         (catch TimeoutException _
           (log/warn (str "simplifier timed out: must have been a complicated expression"))
           x))))

(defn ^:private poly-analyzer
  "An analyzer capable of simplifying sums and products, but unable to
  cancel across the fraction bar"
  []
  (a/make-analyzer (poly/->PolynomialAnalyzer) (a/monotonic-symbol-generator "-s-")))

(defn ^:private rational-function-analyzer
  "An analyzer capable of simplifying expressions built out of rational
  functions."
  []
  (a/make-analyzer (rf/->RationalFunctionAnalyzer (poly/->PolynomialAnalyzer)) (a/monotonic-symbol-generator "-r-")))

(def ^:dynamic *rf-analyzer* (memoize (unless-timeout (rational-function-analyzer))))
(def ^:dynamic *poly-analyzer* (memoize (poly-analyzer)))

(defn hermetic-simplify-fixture
  [f]
  (binding [*rf-analyzer* (rational-function-analyzer)
            *poly-analyzer* (poly-analyzer)]
    (f)))

(def ^:private simplify-and-flatten #'*rf-analyzer*)

(defn ^:private simplify-until-stable
  [rule-simplify canonicalize]
  (fn [expression]
    (let [new-expression (rule-simplify expression)]
      (if (= expression new-expression)
        expression
        (let [canonicalized-expression (canonicalize new-expression)]
          (cond (= canonicalized-expression expression) expression
                (g/zero? (*poly-analyzer* `(~'- ~expression ~canonicalized-expression))) canonicalized-expression
                :else (recur canonicalized-expression)))))))

(defn ^:private simplify-and-canonicalize
  [rule-simplify canonicalize]
  (fn simplify [expression]
    (let [new-expression (rule-simplify expression)]
      (if (= expression new-expression)
        expression
        (canonicalize new-expression)))))

(def ^:private sin-sq->cos-sq-simplifier
  (simplify-and-canonicalize rules/sin-sq->cos-sq simplify-and-flatten))
(def ^:private sincos-simplifier
  (simplify-and-canonicalize rules/sincos-flush-ones simplify-and-flatten))
(def ^:private square-root-simplifier
  (simplify-and-canonicalize rules/simplify-square-roots simplify-and-flatten))

;; looks like we might have the modules inverted: rulesets will need some functions from the
;; simplification library, so this one has to go here. Not ideal the way we have split things
;; up, but at least things are beginning to simplify adequately.

(def ^:private simplifies-to-zero?
  #(-> % *poly-analyzer* g/zero?))

(def ^:private simplifies-to-unity?
  #(-> % *rf-analyzer* v/unity?))

(def trig-cleanup
  "This finds things like a - a cos^2 x and replaces them with a sin^2 x"
  (let [at-least-two? #(and (number? %) (>= % 2))]
    (simplify-until-stable
     (rule/rule-simplifier
      (rule/ruleset
       ;;  ... + a + ... + cos^n x + ...   if a + cos^(n-2) x = 0: a sin^2 x
       (+ :a1* :a :a2* (expt (cos :x) (:? n at-least-two?)) :a3*)
       #(simplifies-to-zero? `(~'+ (~'expt (~'cos ~(% :x)) ~(- (% 'n) 2)) ~(% :a)))
       (+ :a1* :a2* :a3* (* :a (expt (sin :x) 2)))

       (+ :a1* (expt (cos :x) (:? n at-least-two?)) :a2* :a :a3*)
       #(simplifies-to-zero? `(~'+ (~'expt (~'cos ~(% :x)) ~(- (% 'n) 2)) ~(% :a)))
       (+ :a1* :a2* :a3* (* :a (expt (sin :x) 2)))

       (+ :a1* :a :a2* (* :b1* (expt (cos :x) (:? n at-least-two?)) :b2*) :a3*)
       #(simplifies-to-zero? `(~'+ (~'* ~@(% :b1*) ~@(% :b2*) (~'expt (~'cos ~(% :x)) ~(- (% 'n) 2))) ~(% :a)))
       (+ :a1* :a2* :a3* (* :a (expt (sin :x) 2)))

       (+ :a1* (* :b1* (expt (cos :x) (:? n at-least-two?)) :b2*) :a2* :a :a3*)
       #(simplifies-to-zero? `(~'+ (~'* ~@(% :b1*) ~@(% :b2*) (~'expt (~'cos ~(% :x)) ~(- (% 'n) 2))) ~(% :a)))
       (+ :a1* :a2* :a3* (* :a (expt (sin :x) 2)))

       ;; since computing GCDs of rational functions is expensive, it would be nice if the
       ;; result of the computation done in simplifies-to-unity could be captured and reused
       ;; in the substitution. Idea: provide a binding for the *return value* of the predicate
       ;; in the scope of the substitution.
       (atan :y :x)
       #(not (simplifies-to-unity? `(~'gcd ~(% :x) ~(% :y))))
       (atan (/ :y (gcd :x :y)) (/ :x (gcd :x :y)))

       ))
     simplify-and-flatten)))

;; (defn ^:private spy [x a] (println a x) x)

(def clear-square-roots-of-perfect-squares
  (simplify-and-canonicalize
   (comp rules/universal-reductions factor/root-out-squares)
   simplify-and-flatten))

(defn ^:private simplify-expression-1
  "this is a chain of rule-simplifiers (i.e., each entry in the chain
  passes the expression through after the simplification of the step
  stabilizes.)"
  [x]
  (-> x
      rules/canonicalize-partials
      rules/trig->sincos
      simplify-and-flatten
      rules/complex-trig
      sincos-simplifier
      sin-sq->cos-sq-simplifier
      trig-cleanup
      rules/sincos->trig
      square-root-simplifier
      clear-square-roots-of-perfect-squares
      simplify-and-flatten))

(def simplify-expression (simplify-until-stable simplify-expression-1 simplify-and-flatten))

(defn ^:private abstract-quantity? [x] (= (:type x) ::x/numerical-expression))

;; TODO: it is structurally irritating to have an if here.
(defn simplify-numerical-expression
  "Runs the content of the Expression e through the simplifier, but leaves the result in
  Expression form."
  [e]
  (if (g/abstract-quantity? e)
    (x/fmap simplify-expression e)
    e))

(defn ^:private haz
  "Returns a function which checks whether its argument, a set, has a nonempty
  intersection with thing-set."
  [thing-set]
  #(-> % x/variables-in (set/intersection thing-set) not-empty))

(defn only-if
  "returns a function that will apply f to its argument x if (p x), else returns x."
  [p f]
  (fn [x]
    (if (p x) (f x) x)))

#_(defn ^:private new-simplify
  [x]
  (let [sqrt? (haz #{'sqrt})
        full-sqrt? (haz #{'sqrt}) ;; normally, (and sqrt? sqrt-factor-simplify?)
        logexp? (haz #{'log 'exp})
        sincos? (haz #{'sin 'cos})
        partials? (haz #{'partial})
        simplified-exp ((comp (only-if (fn [x] true #_"divide-numbers-through-simplify?")
                                       rules/divide-numbers-through)
                              (only-if sqrt? rules/clear-square-roots-of-perfect-squares)
                              (only-if full-sqrt?
                                       (comp
                                        (simplify-until-stable (comp rules/universal-reductions
                                                                        sqrt-expand)
                                                               simplify-and-flatten)
                                        clear-square-roots-of-perfect-squares
                                        (simplify-until-stable sqrt-contract
                                                               simplify-and-flatten)))
                              (only-if sincos?
                                       (comp (simplify-and-canonicalize
                                                 (comp rules/universal-reductions sincos->trig)
                                                 simplify-and-flatten)
                                                (simplify-and-canonicalize angular-parity
                                                                           simplify-and-flatten)
                                                (simplify-until-stable sincos-random
                                                                       simplify-and-flatten)
                                                (simplify-and-canonicalize sin-sq->cos-sq
                                                                           simplify-and-flatten)
                                                (simplify-and-canonicalize sincos-flush-ones
                                                                           simplify-and-flatten)
                                                (if trig-product-to-sum-simplify?
                                                  (simplify-and-canonicalize trig-product-to-sum
                                                                             simplify-and-flatten)
                                                  (lambda (x) x))
                                                (simplify-and-canonicalize rules/universal-reductions
                                                                           simplify-and-flatten)
                                                (simplify-until-stable sincos-random
                                                                       simplify-and-flatten)
                                                (simplify-and-canonicalize sin-sq->cos-sq
                                                                           simplify-and-flatten)
                                                (simplify-and-canonicalize sincos-flush-ones
                                                                           simplify-and-flatten)))


                              (only-if logexp?
                                       (comp
                                        (simplify-and-canonicalize rules/universal-reductions
                                                                   simplify-and-flatten)
                                        (simplify-until-stable (comp log-expand exp-expand)
                                                               simplify-and-flatten)
                                        (simplify-until-stable (comp log-contract exp-contract)
                                                               simplify-and-flatten)))


                              (simplify-until-stable (comp rules/universal-reductions
                                                              (only-if logexp?
                                                                       (comp log-expand
                                                                                exp-expand))
                                                              (only-if sqrt? sqrt-expand))
                                                     simplify-and-flatten)

                              (only-if sincos?
                                       (simplify-and-canonicalize angular-parity
                                                                  simplify-and-flatten))
                              (simplify-and-canonicalize trig->sincos simplify-and-flatten)
                              (only-if partials?
                                       (simplify-and-canonicalize canonicalize-partials
                                                                  simplify-and-flatten))
                              simplify-and-flatten)
                        exp)]
    simplified-exp))

(defmethod g/simplify [::x/numerical-expression]
  [a]
  (-> a g/freeze simplify-expression))

(defmethod g/simplify :default [a] (g/freeze a))
(defmethod g/simplify [Var] [a] (-> a meta :name))
(defmethod g/simplify [Sequential] [a] (map g/simplify a))
(defmethod g/simplify [PersistentVector] [a] (mapv g/simplify a))
(defmethod g/simplify [LazySeq] [a] (map g/simplify a))
(defmethod g/simplify [Symbol] [a] a)
(prefer-method g/simplify [:sicmutils.structure/structure] [Sequential])

;; Freezing an expression means removing wrappers and other metadata
;; from subexpressions, so that the result is basically a pure
;; S-expression with the same structure as the input. Doing this will
;; rob an expression of useful information fur further computation; so
;; this is intended to be done just before simplification and printing, to
;; simplify those processes.
;;
;; TODO: is it really necessary to maintain the distinction between freeze and simplify
;; at this point? It is probably an artifact of the development process we went through
;; (in which the simplifier was slow to mature, but useful results could still be got
;; through the expedient of freeze)

(def ^:private object-name-map {g/+ '+ g/- '- g/* '* g/divide (symbol "/")})
(defmethod g/freeze :default [a] (or (object-name-map a) a))
;; TODO dodgy. the right thing to do is sever the dependence of this module on expression,
;; i.e. invert that dependence, and move the following implementation there.
(defmethod g/freeze [::x/numerical-expression] [a] (g/freeze (:expression a)))
(defmethod g/freeze [clojure.lang.PersistentVector] [a] (mapv g/freeze a))
(defmethod g/freeze [clojure.lang.Cons] [a] (map g/freeze a))

(defn expression->string
  "Renders an expression through the simplifier and into a string,
  which is returned."
  [expr]
  (let [w (StringWriter.)]
    (-> expr g/simplify (pp/write :stream w))
    (.toString w)))
(def print-expression #(-> % g/simplify pp/pprint))
(def pe print-expression)
