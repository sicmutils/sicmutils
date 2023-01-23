#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.simplify
  (:require [emmy.expression :as x]
            [emmy.expression.analyze :as a]
            [emmy.polynomial :as poly]
            [emmy.polynomial.factor :as factor]
            [emmy.rational-function :as rf]
            [emmy.simplify.rules :as rules]
            [emmy.value :as v]
            [taoensso.timbre :as log])
  #?(:clj
     (:import (java.util.concurrent TimeoutException))))

(defn- unless-timeout
  "Returns a function that invokes f, but catches TimeoutException;
  if that exception is caught, then x is returned in lieu of (f x)."
  [f]
  (fn [x]
    (try (f x)
         (catch #?(:clj TimeoutException :cljs js/Error) _
           (log/warn
            (str "simplifier timed out: must have been a complicated expression"))
           x))))

(defn ^:no-doc poly-analyzer
  "An analyzer capable of simplifying sums and products, but unable to cancel
  across the fraction bar.
  NOTE: I think this is fpf:analyzer in the scheme code."
  []
  (let [gensym (a/monotonic-symbol-generator "-s-")]
    (a/make-analyzer poly/analyzer gensym)))

(defn ^:no-doc rational-function-analyzer
  "An analyzer capable of simplifying expressions built out of rational
  functions.
  NOTE: This is rcf:analyzer."
  []
  (let [gensym (a/monotonic-symbol-generator "-r-")]
    (a/make-analyzer rf/analyzer gensym)))

(def ^:dynamic *poly-simplify*
  (memoize
   (a/expression-simplifier
    (poly-analyzer))))

(def ^:dynamic *rf-simplify*
  (unless-timeout
   (memoize
    (a/expression-simplifier
     (rational-function-analyzer)))))

(defn hermetic-simplify-fixture
  "Returns the result of executing the supplied `thunk` in an environment where
  the [[*rf-simplify*]] and [[*poly-simplify*]] are not memoized."
  [thunk]
  (binding [*rf-simplify* (unless-timeout
                           (a/expression-simplifier
                            (rational-function-analyzer)))
            *poly-simplify* (unless-timeout
                             (a/expression-simplifier
                              (poly-analyzer)))]
    (thunk)))

(defn- simplify-and-flatten [expr]
  (*poly-simplify*
   (*rf-simplify* expr)))

(defn- simplify-until-stable
  [rule-simplify canonicalize]
  (fn [expr]
    (let [new-expr (rule-simplify expr)]
      (if (= expr new-expr)
        expr
        (let [canonicalized-expr (canonicalize new-expr)]
          (cond (= canonicalized-expr expr) expr
                (v/zero?
                 (*poly-simplify*
                  (list '- expr canonicalized-expr)))
                canonicalized-expr
                :else (recur canonicalized-expr)))))))

(defn- simplify-and-canonicalize
  [rule-simplify canonicalize]
  (fn [expr]
    (let [new-expr (rule-simplify expr)]
      (if (= expr new-expr)
        expr
        (canonicalize new-expr)))))

(def ^:private clear-square-roots-of-perfect-squares
  (-> (comp (rules/universal-reductions #'*rf-simplify*)
            factor/root-out-squares)
      (simplify-and-canonicalize simplify-and-flatten)))

(defn- only-if
  "If the supplied `bool` is true, returns `f`, else returns `identity`."
  [bool f]
  (if bool
    f
    identity))

(let [universal-reductions (rules/universal-reductions #'*rf-simplify*)
      sqrt-contract (rules/sqrt-contract #'*rf-simplify*)
      sqrt-expand (rules/sqrt-expand #'*rf-simplify*)
      log-contract (rules/log-contract #'*rf-simplify*)
      sincos-random (rules/sincos-random #'*rf-simplify*)
      sincos-flush-ones (rules/sincos-flush-ones #'*rf-simplify*)]

  (defn simplify-expression
    "Simplifies an expression representing a complex number. TODO say more!"
    [expr]
    (let [syms (x/variables-in expr)
          sqrt? (rules/occurs-in? #{'sqrt} syms)
          full-sqrt? (and rules/*sqrt-factor-simplify?*
                          (rules/occurs-in? #{'sqrt} syms))

          logexp? (rules/occurs-in? #{'log 'exp} syms)
          trig? (rules/occurs-in? #{'sin 'cos 'tan 'cot 'sec 'csc} syms)
          partials? (rules/occurs-in? #{'partial} syms)
          simple
          (comp (only-if rules/*divide-numbers-through-simplify?*
                         rules/divide-numbers-through)

                (only-if sqrt? clear-square-roots-of-perfect-squares)

                (only-if full-sqrt?
                         (comp (-> (comp universal-reductions sqrt-expand)
                                   (simplify-until-stable simplify-and-flatten))
                               clear-square-roots-of-perfect-squares
                               (-> sqrt-contract
                                   (simplify-until-stable simplify-and-flatten))))

                (only-if trig?
                         (comp (-> (comp universal-reductions rules/sincos->trig)
                                   (simplify-and-canonicalize simplify-and-flatten))
                               (-> rules/complex-trig
                                   (simplify-and-canonicalize simplify-and-flatten))
                               (-> rules/angular-parity
                                   (simplify-and-canonicalize simplify-and-flatten))
                               (-> sincos-random
                                   (simplify-until-stable simplify-and-flatten))
                               (-> rules/sin-sq->cos-sq
                                   (simplify-and-canonicalize simplify-and-flatten))
                               (-> sincos-flush-ones
                                   (simplify-and-canonicalize simplify-and-flatten))

                               (only-if rules/*trig-product-to-sum-simplify?*
                                        (-> rules/trig:product->sum
                                            (simplify-and-canonicalize simplify-and-flatten)))

                               (-> universal-reductions
                                   (simplify-and-canonicalize simplify-and-flatten))
                               (-> sincos-random
                                   (simplify-until-stable simplify-and-flatten))
                               (-> rules/sin-sq->cos-sq
                                   (simplify-and-canonicalize simplify-and-flatten))
                               (-> sincos-flush-ones
                                   (simplify-and-canonicalize simplify-and-flatten))))

                (only-if logexp?
                         (comp (-> universal-reductions
                                   (simplify-and-canonicalize simplify-and-flatten))
                               (-> (comp rules/log-expand
                                         rules/exp-expand)
                                   (simplify-until-stable simplify-and-flatten))
                               (-> (comp log-contract
                                         rules/exp-contract)
                                   (simplify-until-stable simplify-and-flatten))))

                (-> (comp universal-reductions
                          (only-if logexp?
                                   (comp rules/log-expand
                                         rules/exp-expand))
                          (only-if sqrt?
                                   sqrt-expand))
                    (simplify-until-stable simplify-and-flatten))

                (only-if trig?
                         (-> rules/angular-parity
                             (simplify-and-canonicalize simplify-and-flatten)))

                (-> rules/trig->sincos
                    (simplify-and-canonicalize simplify-and-flatten))

                ;; TODO this should happen at the END, only a single time, after
                ;; everything else is done. It's not right to get operator
                ;; multiplication going and then attempt to canonicalize the
                ;; expression, even if it sort of works.
                (only-if partials?
                         (-> rules/canonicalize-partials
                             (simplify-and-canonicalize simplify-and-flatten)))
                simplify-and-flatten)]
      (simple expr))))
