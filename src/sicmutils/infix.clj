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

(ns sicmutils.infix
  (:require [clojure.zip :as z]
            [clojure.set :as set]
            [clojure.string :as s]
            [clojure.walk :as walk]
            [pattern.rule :as R]
            [sicmutils.expression :as x]
            [sicmutils.numerical.compile :as compile])
  (:import (java.io StringWriter)))

(defn ^:private make-symbol-generator
  [p]
  (let [i (atom 0)]
    (fn [] (format "%s%d" p (swap! i inc)))))

(def ^:private rewrite-trig-powers
  "Historical preference is to write sin^2(x) rather than (sin(x))^2."
  (let [ok? #(and ('#{sin cos tan} (% :T))
                  (= 2 (% :N)))]
    (R/rule (expt (:T :X) :N) ok? ((expt :T :N) :X))))

(def ^:private rewrite-negation
  "The simplifier returns sums of products; for negative summands the
  simplifier negates by wrapping with (* -1 ...). For rendering, we
  prefer to use a unary minus"
  (R/ruleset
   (* -1 :X) => (u- :X)
   (* -1 :X*) => (u- (* :X*))))

(defn ^:private make-infix-renderer
  "Base function for infix renderers. This is meant to be specialized via
  options for the treatment desired. Returns a rendering function. The options are:
  - `precedence-map`: a map from (symbol or keyword) to numbers. Higher numbers
    mean higher precedence. This guides parenthesization.
  - `juxtapose-multiply`: a string that will be placed between factors in a
    product. Defaults to `*`.
  - `infix?` A function mapping symbols to boolean, used to decide if a function
    application should be written as `x f y` or `f(x, y)`.
  - `render-primitive` is a function used to render symbols, numeeric constants
    etc. into string form.
  - `parenthesize` is a function used to wrap parens around objects when
    needed. It defaults to the obvious thing.
  - `special-handlers` is a map from symbol to a function of operator and
    arguments, used to provide custom rendering for things like exponentiation
    which might not be rendered either as infix or prefix.
  - `rename-functions` is a map supplying replacement function names to be used
    just before the expression is written.
  "
  [& {:keys [juxtapose-multiply special-handlers infix? render-primitive
             rename-functions parenthesize precedence-map rewrite-trig-squares]
      :or {special-handlers {}
           parenthesize #(str "(" % ")")
           juxtapose-multiply " * "
           rewrite-trig-squares false
           rename-functions {}
           infix? {}}}]
  (letfn [(precedence [op] (or (precedence-map op)
                               (cond (seq? op) (precedence-map :apply)
                                     (symbol? op) (precedence-map :apply)
                                     :else 0)))
          ;; TODO: the names are wack; reverse them one way or the other
          (precedence> [a b] (> (precedence a) (precedence b)))
          (precedence<= [a b] (not (precedence> a b)))
          (parenthesize-if [b x]
            (if b (parenthesize x) x))
          (maybe-rename-function [f]
            (or (rename-functions f) f))
          (maybe-rewrite-negation [loc]
            (or (rewrite-negation (z/node loc) #(z/replace loc %))
                loc))
          (maybe-rewrite-trig-squares [loc]
            (or (and rewrite-trig-squares
                     (rewrite-trig-powers (z/node loc) #(z/replace loc %)))
                loc))
          (render-unary-node [op args]
            (let [a (first args)]
              (case op
                (+ *) (str a)
                u- (str "- " a)
                / (str "1 / " a)
                (str op " " a))))
          (render-loc [loc]
            (if (z/branch? loc)
              ;; then the first child is the function and the rest are the
              ;; arguments.
              (let [fn-loc (-> loc maybe-rewrite-negation maybe-rewrite-trig-squares z/next)
                    arg-loc (loop [a (-> fn-loc z/right)]
                              (let [a' (z/replace a (render-loc a))]
                                (if-let [r (z/right a')]
                                  (recur r)
                                  (z/up a'))))
                    [op & args] (z/node arg-loc)
                    upper-op (and (z/up arg-loc)
                                  (-> arg-loc z/leftmost z/node))]
                (if (infix? op)
                  (parenthesize-if
                   (and (infix? upper-op)
                        (and (precedence<= op upper-op)
                             ;; respect precedence, except in the special case
                             ;;   (- (* a b c...))
                             ;; which we would prefer to write as "- a b c..." rather than "- (a b c...)"
                             ;; as strict precedence rules would require.
                             (not (and (= op '*) (= upper-op 'u-)))))
                   (or (and (special-handlers op)
                            ((special-handlers op) args))
                       (and (= (count args) 1)
                            (render-unary-node op args))
                       (let [w (StringWriter.)
                             ^String sep (case op
                                           * (or juxtapose-multiply " * ")
                                           expt "^"
                                           (str " " op " "))]
                         (loop [a args]
                           (.write w (str (first a)))
                           (if-let [a' (next a)]
                             (do
                               (if-not (and (string? (first a')) (= (first (first a')) \-))
                                 (.write w sep)
                                 (.write w " "))
                               (recur a'))
                             (str w))))))
                  ;; case: op is not infix.
                  ;; The _whole_ result may need to be parenthesized, though, if it
                  ;; is part of an infix expression with an operator with very high
                  ;; precedence (in practice, this means exponentiation)
                  (parenthesize-if
                   (and upper-op
                        (infix? upper-op)
                        (precedence<= op upper-op))
                   (or (and (special-handlers op)
                            ((special-handlers op) args))
                       (str (parenthesize-if (and (z/branch? fn-loc)
                                                  (precedence> :apply (z/node (z/next fn-loc))))
                                             (maybe-rename-function (render-loc (z/next arg-loc))))
                            (parenthesize-if (or (precedence<= op :apply)
                                                 (> (count args) 1)
                                                 (z/branch? (z/right fn-loc)))
                                             (s/join ", " args)))))))

              ;; primitive case
              (let [n (z/node loc)]
                (or (and render-primitive (render-primitive n))
                    n))))]
    #(-> % z/seq-zip render-loc)))

(def ^:private decimal-superscripts
  [\⁰ \¹ \² \³ \⁴ \⁵ \⁶ \⁷ \⁸ \⁹])
(def ^:private decimal-subscripts
  [\₀ \₁ \₂ \₃ \₄ \₅ \₆ \₇ \₈ \₉])

(defn ^:private digit->int
  [^Character d]
  (Character/digit d 10))

(defn ^:private n->script
  "Given an integer, returns a string where each digit of the
  integer is used as the index into the replacement map scripts,
  which is expected to be indexable by integers in the range [0..9]."
  [n scripts]
  (apply str (map #(-> % digit->int scripts)
                  (str n))))

(def ^:private n->subscript #(n->script % decimal-subscripts))
(def ^:private n->superscript #(n->script % decimal-superscripts))

(def ->infix
  "Converts an S-expression to printable infix form. Numeric exponents are
  written as superscripts. Partial derivatives get subscripts."
  (make-infix-renderer
   :precedence-map '{∂ 9, D 9, expt 7, :apply 5, u- 4, / 3, * 3, + 1, - 1}
   :infix? '#{* + - / expt u-}
   :juxtapose-multiply " "
   :rewrite-trig-squares true
   :special-handlers
   {'expt (fn [[x e]]
            (if (and (integer? e) ((complement neg?) e))
              (str x (n->superscript e))))
    '∂ (fn [ds]
         (when (and (= (count ds) 1) (integer? (first ds)))
           (str "∂" (n->subscript (first ds)))))}
   :render-primitive (fn r [v]
                       (let [s (str v)
                             [_ stem subscript] (re-find #"(.+)_(\d+)$" s)]
                         (when stem
                           (str stem (n->subscript subscript)))))))

(def ^:private TeX-letters
  "The set of names of TeX letters (e.g., the Greek letters). Symbols
  whose names match this set are prefixed with \\, as in alpha -> \\alpha."
  #{"alpha" "beta" "gamma" "delta" "epsilon" "varepsilon" "zeta" "eta"
    "theta" "vartheta" "kappa" "lambda" "mu" "nu" "xi" "pi" "varpi"
    "rho" "varrho" "sigma" "varsigma" "tau" "upsilon" "phi" "varphi"
    "chi" "psi" "omega" "Gamma" "Delta" "Theta" "Lambda" "Xi" "Pi" "Sigma"
    "Upsilon" "Phi" "Psi" "Omega" "ell"})

(def ^:private TeX-map
  "Direct mapping of symbols to TeX."
  {"α" "\\alpha",
   "ω" "\\omega",
   "θ" "\\theta",
   "φ" "\\varphi",
   "sin" "\\sin",
   "cos" "\\cos",
   "tan" "\\tan",
   "asin" "\\arcsin",
   "acos" "\\arccos",
   "atan" "\\arctan",
   })

(defn ^:private brace
  "Wrap the argument, as a string, in braces"
  [s]
  (str "{" s "}"))

(defn ^:private maybe-brace
  "Wrap the argument in braces, as a string, unless it's just a single character"
  [s]
  (if (and (string? s) (= (count s) 1))
    s
    (brace s)))

(def ->TeX
  "Convert the given (simplified) expression to TeX format, as a string."
  (let [TeX-accent (fn [accent]
                     (fn [[_ stem]]
                       (str "\\" accent " " (maybe-brace (->TeX stem)))))
        dot (TeX-accent "dot")
        ddot (TeX-accent "ddot")
        hat (TeX-accent "hat")
        bar (TeX-accent "bar")
        vec (TeX-accent "vec")
        tilde (TeX-accent "tilde")]
    (make-infix-renderer
     ;; here we set / to a very low precedence because the fraction bar we will
     ;; use in the rendering groups things very strongly.
     :precedence-map '{∂ 9, D 9, :apply 7, expt 7, u- 6, * 5, + 3, - 3, / 1}
     :parenthesize #(str "\\left(" % "\\right)")
     :infix? '#{* + - / expt u-}
     :juxtapose-multiply "\\,"
     :rewrite-trig-squares true
     :special-handlers
     {'expt (fn [[x e]] (str (maybe-brace x) "^" (maybe-brace e)))
      '∂ (fn [ds] (str "\\partial_" (maybe-brace (s/join "," ds))))
      '/ (fn [xs]
           (when (= (count xs) 2)
             (str "\\dfrac" (brace (first xs)) (brace (second xs)))))
      'up #(str "\\begin{pmatrix}" (s/join "\\\\" %) "\\end{pmatrix}")
      'down #(str "\\begin{bmatrix}" (s/join "&" %) "\\end{bmatrix}")
      'sqrt #(str "\\sqrt " (maybe-brace (first %)))}
     :render-primitive
     (fn r [v]
       (cond (ratio? v)
             (str "\\dfrac" (brace (numerator v)) (brace (denominator v)))

             :else
             (let [s (str v)]
               (cond (TeX-letters s) (str "\\" s)
                     (TeX-map s) (TeX-map s)
                     :else (condp re-find s
                             #"(.+)_([0-9a-zA-Z]+)$"
                             :>> (fn [[_ stem subscript]]
                                   (str (maybe-brace (r stem)) "_" (maybe-brace subscript)))
                             ;; KaTeX doesn't do \dddot.
                             #"(.+)dotdot$" :>> ddot
                             #"(.+)dot$" :>> dot
                             #"(.+)hat$" :>> hat
                             #"(.+)bar$" :>> bar
                             #"(.+)vec$" :>> vec
                             #"(.+)tilde$" :>> tilde

                             ;; otherwise do nothing
                             v))))))))

(def ->JavaScript
  "Convert the given (simplified) expression to a JavaScript function.
  Parameters to the function will be extracted from the symbols in the
  expression. Common subexpression elimination will be performed and
  auxiliary variables will be bound in the body of the function; the
  names of these symbols are obtained from the nullary function
  option :symbol-generator, which defaults to a function yielding `_1,
  ...`. If `:parameter-order is specified, it is used to determine
  function parameter order in one of two ways: if it is set to a
  function, that function will be called on the sequence of parameters
  and is expected to return the parameters in the desired
  sequence. Otherwise, it is interpreted as the sequence of parameters
  itself. If not specified, the default behavior is `sort`."
  (let [operators-known '#{sin cos tan asin acos atan sqrt abs pow + - * /
                           expt exp log up down}
        make-js-vector #(str \[ (s/join ", " %) \])
        R (make-infix-renderer
           :precedence-map '{:apply 8, * 5, / 5, - 3, + 3}
           :infix? '#{* + - / u-}
           :rename-functions {'sin "Math.sin",
                              'cos "Math.cos",
                              'tan "Math.tan",
                              'asin "Math.asin",
                              'acos "Math.acos",
                              'atan "Math.atan",
                              'sqrt "Math.sqrt",
                              'abs "Math.abs",
                              'expt "Math.pow",
                              'log "Math.log",
                              'exp "Math.exp"}
           :special-handlers {'up make-js-vector
                              'down make-js-vector})]
    (fn [x & {:keys [symbol-generator parameter-order]
              :or {symbol-generator (make-symbol-generator "_")
                   parameter-order sort}}]
      (let [params (set/difference (x/variables-in x) operators-known)
            cs (compile/extract-common-subexpressions
                x
                :symbol-generator symbol-generator)
            w (StringWriter.)
            ordered-params (if (fn? parameter-order)
                             (parameter-order params)
                             parameter-order)]
        (doto w
          (.write "function(")
          (.write (s/join ", " ordered-params))
          (.write ") {\n"))
        (doseq [[val var] cs]
          (doto w
            (.write "  var ")
            (.write (str var " = "))
            (.write ^String (R val))
            (.write ";\n")))
        (.toString (doto w
                     (.write "  return ")
                     (.write ^String (R (walk/postwalk-replace cs x)))
                     (.write ";\n}")))))))
