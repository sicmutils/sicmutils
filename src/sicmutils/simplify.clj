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

(ns sicmutils.simplify
  (:import (java.util.concurrent TimeoutException)
           (clojure.lang Sequential Var))
  (:require [clojure.walk :refer [postwalk]]
            [clojure.tools.logging :as log]
            [clojure.pprint :as pp]
            [sicmutils
             [numsymb :as sym]
             [polynomial :as poly]
             [rational-function :as rf]
             [value :as v]
             [generic :as g]
             [rules :as rules]]
            [pattern.rule :as rule]))

(defn analyzer
  [symbol-generator expr-> ->expr known-operations]
  ;; TODO: we haven't recorded variable order, so expressions can get scrambled
  ;; as a result of sorting by generated-symbol-name. The solution is to communicate
  ;; the existing order of subexpressions to the polynomial simplifier.
  ;; TODO: the atom-transient might be better represented as a volatile in a newer
  ;; version of Clojure
  ;; TODO: on closer examination of scmutils, it looks like analyzers are reused
  ;; statefully. We should consider doing that.
  (fn [expr]
    (let [expr-map (atom (transient {}))]
      (letfn [(analyze [expr]
                (if (and (sequential? expr)
                         (not (= (first expr) 'quote)))
                  (let [analyzed-expr (map analyze expr)]
                    (if (and (known-operations (sym/operator analyzed-expr))
                             (or (not (= 'expt (sym/operator analyzed-expr)))
                                 (integer? (second (sym/operands analyzed-expr)))))
                      analyzed-expr
                      (if-let [existing-expr (@expr-map analyzed-expr)]
                        existing-expr
                        (new-kernels analyzed-expr))))
                  expr))
              (new-kernels [expr]
                (let [simplified-expr (map base-simplify expr)]
                  (if-let [v (sym/symbolic-operator (sym/operator simplified-expr))]
                    (let [w (apply v (sym/operands simplified-expr))]
                      (if (and (sequential? w)
                               (= (sym/operator w) (sym/operator simplified-expr)))
                        (add-symbols! w)
                        (analyze w)))
                    (add-symbols! simplified-expr))))
              (add-symbols! [expr]
                ;; doall is needed here because we need to have all the effects of
                ;; add-symbol! accounted for strictly before the transient expr-map
                ;; is made persistent in backsubstitute.
                (->> expr (map add-symbol!) doall add-symbol!))
              (add-symbol! [expr]
                (if (and (sequential? expr)
                         (not (= (first expr) 'quote)))
                  (if-let [existing-expr (@expr-map expr)]
                    existing-expr
                    (let [newvar (symbol-generator)]
                      (swap! expr-map assoc! expr newvar)
                      newvar))
                  expr))
              (backsubstitute [expr]
                ;; Finalize the expression map, invert it, and use it to perform the backsubstitution.
                (let [mapx (-> expr-map (swap! persistent!) invert-map)
                      bsub (fn bsub [v]
                             (cond (sequential? v) (map bsub v)
                                   (symbol? v) (let [w (mapx v)]
                                                 (if w (bsub w) v))
                                   :else v))]
                  (bsub expr)))
              (base-simplify [expr] (expr-> expr ->expr))
              (invert-map [m] (into {} (for [[k v] m] [v k])))]
        (-> expr analyze base-simplify backsubstitute)))))

(defn ^:private monotonic-symbol-generator
  "Returns a function which generates a sequence of symbols with the given
  prefix with the property that later symbols will sort after earlier symbols.
  This is important for the stability of the simplifier. (If we just used
  gensym, then a temporary symbol like G__1000 will sort earlier than G__999,
  and this will happen at unpredictable times.)"
  [prefix]
  (let [count (atom -1)]
    (fn [] (symbol (format "%s%016x" prefix (swap! count inc))))))

(def ^:private poly-analyzer
  "An analyzer capable of simplifying sums and products, but unable to
  cancel across the fraction bar"
  (analyzer (monotonic-symbol-generator "-s-")
            poly/expression-> poly/->expression poly/operators-known))

(def rational-function-analyzer
  "The heart of sicmutils simplification. Simplifies quotients of polynomials."
  (let [A (analyzer (monotonic-symbol-generator "-r-")
                    rf/expression-> rf/->expression rf/operators-known)]
    (fn [x]
      (try (A x)
           (catch TimeoutException _
             (log/warn (str "simplifier timed out: must have been a complicated expression"))
             x)))))

(def ^:private twin-analyzer
  "kick off poly analyzer in a thread (the rational function analyzer
  should typically take longer, so by the time that's done the future
  should be fulfilled). We could decide to accept the poly analyzer's
  result in that case."
  (fn [x]
    (let [pf (future (poly-analyzer x))
          r (rational-function-analyzer x)
          p @pf
          lp (count (flatten p))
          lr (count (flatten r))]
      (when (< lp lr)
        (log/warn (format "the poly analyzer did a better job %d %d" lp lr))
        (log/warn (format "poly: %s" (seq p)))
        (log/warn (format "rf: %s" (seq r))))
      r)))

(def ^:private simplify-and-flatten rational-function-analyzer)

(defn- simplify-until-stable
  [rule-simplify canonicalize]
  (fn [expression]
    (let [new-expression (rule-simplify expression)]
      (if (= expression new-expression)
        expression
        (let [canonicalized-expression (canonicalize new-expression)]
          (cond (= canonicalized-expression expression) expression
                (g/zero? (poly-analyzer `(~'- ~expression ~canonicalized-expression))) canonicalized-expression
                :else (recur canonicalized-expression)))))))

(defn- simplify-and-canonicalize
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

(def ^:private simplify-zero
  #(-> % poly-analyzer g/zero?))

(def ^:private sincos-cleanup
  "This finds things like a - a cos^2 x and replaces them with a sin^2 x"
  (let [at-least-two? #(and (number? %) (>= % 2))]
    (simplify-until-stable
     (rule/rule-simplifier
      (rule/ruleset
       ;;  ... + a + ... + cos^n x + ...   if a + cos^(n-2) x = 0: a sin^2 x
       (+ :a1* :a :a2* (expt (cos :x) (:? n at-least-two?)) :a3*)
       #(simplify-zero `(~'+ (~'expt (~'cos ~(% :x)) ~(- (% 'n) 2)) ~(% :a)))
       (+ :a1* :a2* :a3* (* :a (expt (sin :x) 2)))

       (+ :a1* (expt (cos :x) (:? n at-least-two?)) :a2* :a :a3*)
       #(simplify-zero `(~'+ (~'expt (~'cos ~(% :x)) ~(- (% 'n) 2)) ~(% :a)))
       (+ :a1* :a2* :a3* (* :a (expt (sin :x) 2)))

       (+ :a1* :a :a2* (* :b1* (expt (cos :x) (:? n at-least-two?)) :b2*) :a3*)
       #(simplify-zero `(~'+ (~'* ~@(% :b1*) ~@(% :b2*) (~'expt (~'cos ~(% :x)) ~(- (% 'n) 2))) ~(% :a)))
       (+ :a1* :a2* :a3* (* :a (expt (sin :x) 2)))

       (+ :a1* (* :b1* (expt (cos :x) (:? n at-least-two?)) :b2*) :a2* :a :a3*)
       #(simplify-zero `(~'+ (~'* ~@(% :b1*) ~@(% :b2*) (~'expt (~'cos ~(% :x)) ~(- (% 'n) 2))) ~(% :a)))
       (+ :a1* :a2* :a3* (* :a (expt (sin :x) 2)))))
     simplify-and-flatten)))

;; (defn ^:private spy [x a]
;;   (println a x)
;;   x)

(def ^:private simplify-expression-1
  #(-> %
       rules/trig->sincos
       simplify-and-flatten
       rules/complex-trig
       sincos-simplifier
       sin-sq->cos-sq-simplifier
       sincos-cleanup
       rules/sincos->trig
       square-root-simplifier
       ;;rules/divide-numbers-through
       simplify-and-flatten))

(def simplify-expression (simplify-until-stable simplify-expression-1 simplify-and-flatten))

(defmethod g/simplify :sicmutils.expression/numerical-expression
  [a]
  (->> a v/freeze simplify-expression))

(defmethod g/simplify :default [a] (v/freeze a))
(defmethod g/simplify Var [a] (-> a meta :name))
(defmethod g/simplify Sequential [a] (map g/simplify a))
(prefer-method g/simplify :sicmutils.structure/structure Sequential)

(defn expression->string
  "Renders an expression through the simplifier and into a string,
  which is returned."
  [expr]
  (let [w (java.io.StringWriter.)]
    (-> expr g/simplify (pp/write :stream w))
    (str w)))
(def print-expression #(-> % g/simplify pp/pprint))
(def pe print-expression)
