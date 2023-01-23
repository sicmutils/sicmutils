#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.expression.render
  "Functions and utilities for rendering symbolic expressions to various backends
  like LaTeX, infix or Javascript."
  (:require [clojure.set :as set]
            [clojure.string :as s]
            [clojure.zip :as z]
            [pattern.rule :as R :refer [=>]]
            [emmy.expression :as x]
            [emmy.expression.compile :as compile]
            [emmy.ratio :as r]
            [emmy.util :as u]
            [emmy.value :as v]))

(defn- make-symbol-generator [p]
  (let [i (atom 0)]
    (fn [] (symbol
           #?(:clj
              (format "%s%04x" p (swap! i inc))

              :cljs
              (let [suffix (-> (swap! i inc)
                               (.toString 16)
                               (.padStart 4 "0"))]
                (str p suffix)))))))

(def ^{:private true
       :doc "Historical preference is to write `sin^2(x)` rather
       than `(sin(x))^2`."}
  rewrite-trig-powers
  (R/choice
   (R/rule (expt ((? f #{'sin 'cos 'tan}) ?x) 2) => ((expt (? f) 2) ?x))
   (R/return nil)))

(def ^{:private true
       :doc "The simplifier returns sums of products; for negative summands the
  simplifier negates by wrapping with `(* -1 ...)`. For rendering, we prefer to
  use a unary minus."}
  rewrite-negation
  (R/ruleset
   (* -1 ?x) => (u- ?x)
   (* -1 ??x) => (u- (* ??x))))

(defn- render-infix-ratio
  "renders a pair of the form `[numerator denominator]` as a infix ratio of the
  form `num/denom`.

  If the pair contains only one entry `x`, it's coerced to `[1 x]` (and treated
  as a denominator)."
  [[num denom :as xs]]
  (let [n (count xs)]
    (cond (and (= n 1) (v/integral? num))
          (str "1/" num)

          (and (= n 2)
               (v/integral? num)
               (v/integral? denom))
          (str num "/" denom))))

(defn- make-infix-renderer
  "Base function for infix renderers. This is meant to be specialized via
  options for the treatment desired. Returns a rendering function. The options are:

  - `precedence-map`: a map from (symbol or keyword) to numbers. Higher numbers
    mean higher precedence. This guides parenthesization.
  - `juxtapose-multiply`: a string that will be placed between factors in a
    product. Defaults to `*`.
  - `infix?` A function mapping symbols to boolean, used to decide if a function
    application should be written as `x f y` or `f(x, y)`.
  - `render-primitive` is a function used to render symbols, numeric constants
    etc. into string form.
  - `parenthesize` is a function used to wrap parens around objects when
    needed. It defaults to the obvious thing.
  - `special-handlers` is a map from symbol to a function of operator and
    arguments, used to provide custom rendering for things like exponentiation
    which might not be rendered either as infix or prefix.
  - `rename-functions` is a map supplying replacement function names to be used
    just before the expression is written."
  [& {:keys [juxtapose-multiply special-handlers infix? render-primitive
             rename-functions parenthesize precedence-map rewrite-trig-squares]
      :or {special-handlers {}
           parenthesize #(str "(" % ")")
           juxtapose-multiply " * "
           rewrite-trig-squares false
           rename-functions {}
           infix? {}}}]
  (letfn [(ratio-expr? [op [num denom]]
            (and (= '/ op)
                 (v/integral? num)
                 (or (nil? denom)
                     (v/integral? denom))))
          (precedence [op] (or (precedence-map op)
                               (cond (seq? op)
                                     ;; Some special cases:
                                     ;; - give (expt X n) the precedence of X
                                     ;; - give (partial ...) the precedence of D
                                     ;; - otherwise (...) has the precedence of application
                                     (cond (and (= 3 (count op))
                                                (= 'expt (first op))) (recur (second op))
                                           (= 'partial (first op)) (precedence-map 'D)
                                           :else (precedence-map :apply))
                                     (symbol? op) (precedence-map :apply)
                                     :else 0)))
          (precedence> [a b] (> (precedence a) (precedence b)))
          (precedence<= [a b] (not (precedence> a b)))
          (parenthesize-if [b x]
            (if b (parenthesize x) x))

          (maybe-rename-function [f]
            (or (rename-functions f) f))

          (maybe-rewrite-negation [loc]
            (let [result (rewrite-negation (z/node loc))]
              (if (identical? loc result)
                loc
                (z/replace loc result))))

          (maybe-rewrite-trig-squares [loc]
            (if-let [result (and rewrite-trig-squares
                                 (rewrite-trig-powers
                                  (z/node loc)))]
              (z/replace loc result)
              loc))
          (render-unary-node [op args]
            (let [a (first args)]
              (case op
                (+ *) (str a)
                u- (str "- " a)
                / (if (v/integral? a)
                    (str "1/" a)
                    (str "1 / " a))
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
                             ;; respect precedence, except in the special cases
                             ;; of ratios rendered as calls to `/`:
                             ;;
                             ;; (/ x), (/ x y)
                             ;;
                             ;; which should render as 1/x or x/y, or
                             ;;
                             ;; (- (* a b c...))
                             ;;
                             ;; which we would prefer to write as "- a b c..."
                             ;; rather than "- (a b c...)" as strict precedence
                             ;; rules would require.
                             (not (or (and (= op '*) (= upper-op 'u-))
                                      (ratio-expr? op args)))))
                   (or (and (special-handlers op)
                            ((special-handlers op) args))
                       (and (= (count args) 1)
                            (render-unary-node op args))
                       (let [sep (case op
                                   * (or juxtapose-multiply " * ")
                                   expt "^"
                                   (str " " op " "))]
                         (with-out-str
                           (loop [a args]
                             (print (str (first a)))
                             (when-let [a' (next a)]
                               (if-not (and (string? (first a')) (= (first (first a')) \-))
                                 (print sep)
                                 (print " "))
                               (recur a')))))))
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
    (fn [expr]
      (let [result (-> (v/freeze expr)
                       (z/seq-zip)
                       (render-loc))]
        (if (string? result)
          result
          (str result))))))

(def ^:private decimal-superscripts
  [\⁰ \¹ \² \³ \⁴ \⁵ \⁶ \⁷ \⁸ \⁹])

(def ^:private decimal-subscripts
  [\₀ \₁ \₂ \₃ \₄ \₅ \₆ \₇ \₈ \₉])

(def ^:private subscript-pattern
  #"(.+)_([0-9a-zA-ZϖγηΦνΩδυσιΔρϵωϱςψΠπϑΞκφχζΨτΓΛΘΥμθαℓβΣξλφε]+)$")

(def ^:private superscript-pattern
  #"(.+)↑([0-9a-zA-ZϖγηΦνΩδυσιΔρϵωϱςψΠπϑΞκφχζΨτΓΛΘΥμθαℓβΣξλφε]+)$")

(def ^{:private true
       :doc "Greek letter names we want to recognize that aren't supported by
  TeX, mapped to their unicode characters."}
  non-TeX-greek
  {"Alpha" "Α"
   "Beta" "Β"
   "Epsilon" "Ε"
   "Zeta" "Ζ"
   "Eta" "Η"
   "Iota" "Ι"
   "Kappa" "Κ"
   "Mu" "Μ"
   "Nu" "Ν"
   "omicron" "ο" "Omicron" "O"
   "Rho" "Ρ"
   "Tau" "Τ"
   "Chi" "Χ"})

(def  ^{:private true
        :doc "Mapping of TeX-supported characters (Greek letter names and a few
  others) to their corresponding unicode characters."}
  sym->unicode
  {"alpha" "α"
   "beta" "β"
   "gamma" "γ" "Gamma" "Γ"
   "delta" "δ" "Delta" "Δ"
   "epsilon" "ε" "varepsilon" "ϵ"
   "zeta" "ζ"
   "eta" "η"
   "theta" "θ" "Theta" "Θ" "vartheta" "ϑ"
   "iota" "ι"
   "kappa" "κ"
   "lambda" "λ" "Lambda" "Λ"
   "mu" "μ"
   "nu" "ν"
   "xi" "ξ" "Xi" "Ξ"
   "pi" "π" "Pi" "Π" "varpi" "ϖ"
   "rho" "ρ" "varrho" "ϱ"
   "sigma" "σ"  "Sigma" "Σ" "varsigma" "ς"
   "tau" "τ"
   "upsilon" "υ" "Upsilon" "Υ"
   "phi" "φ" "Phi" "Φ" "varphi" "φ"
   "chi" "χ"
   "psi" "ψ" "Psi" "Ψ"
   "omega" "ω" "Omega" "Ω"
   "ell" "ℓ"
   "ldots" "..."})

(def ^{:private true
       :doc "Map of of TeX-compatible greek letter names to their \\-prefixed
  LaTeX code versions. alpha -> \\alpha, for example."}
  TeX-letters
  (into {} (map (fn [[k]]
                  [k (str "\\" k)]))
        sym->unicode))

(def ^{:private true
       :doc "Full mapping of special-cased TeX symbols to their TeX codes. This
  includes all greek letters in both english ('alpha') and unicode ('α')
  versions, plus a few more special-cased symbols."}
  TeX-map
  (let [sym->tex (->> (set/map-invert sym->unicode)
                      (u/map-vals #(str "\\" %)))]
    (merge TeX-letters
           sym->tex
           {"sin" "\\sin"
            "cos" "\\cos"
            "tan" "\\tan"
            "asin" "\\arcsin"
            "acos" "\\arccos"
            "atan" "\\arctan"
            "sinh" "\\sinh"
            "cosh" "\\sinh"
            "tanh" "\\sinh"
            "cot" "\\cot"
            "sec" "\\sec"
            "csc" "\\csc"
            "_" "\\_"})))

(defn- digit->int
  [^Character d]
  #?(:clj (Character/digit d 10)
     :cljs (js/parseInt d)))

(defn- n->script
  "Given an integer, returns a string where each digit of the
  integer is used as the index into the replacement map scripts,
  which is expected to be indexable by integers in the range [0..9]."
  [n scripts]
  (apply str (map #(-> % digit->int scripts)
                  (str n))))

(def ^:private n->subscript #(n->script % decimal-subscripts))
(def ^:private n->superscript #(n->script % decimal-superscripts))

(def infix-sym->unicode
  (merge non-TeX-greek
         sym->unicode))

(defn- infinity->infix
  "Given some infinite value, returns a string representation of ##Inf or ##-Inf
  appropriate for infix rendering, else returns `nil`."
  [x]
  (case x
    ##Inf "∞"
    ##-Inf "-∞"
    nil))

(def ^{:doc "Converts an S-expression to printable infix form. Numeric exponents
  are written as superscripts. Partial derivatives get subscripts."}
  ->infix
  (make-infix-renderer
   :precedence-map '{D 9, partial 9,
                     expt 8,
                     :apply 7,
                     u- 6,
                     * 5, modulo 5, remainder 5, / 5,
                     + 4, - 4, not 4,
                     = 3, > 3, < 3, >= 3, <= 3,
                     and 2, or 1}
   :infix? '#{* + - / modulo remainder expt u- = > < >= <= and or}
   :juxtapose-multiply " "
   :rewrite-trig-squares true
   :rename-functions
   {'fractional-part "frac"
    'integer-part "int"
    'not "¬"}
   :special-handlers
   {'floor (fn [[x]] (str "⌊" x "⌋"))
    'ceiling (fn [[x]] (str "⌈" x "⌉"))
    'modulo (fn [[x y]] (str x " mod " y))
    'remainder (fn [[x y]] (str x " % " y))
    'and (fn [[x y]] (str x " ∧ " y))
    'or  (fn [[x y]] (str x " ∨ " y))
    'expt (fn [[x e]]
            (when (and (integer? e) ((complement neg?) e))
              (str x (n->superscript e))))
    'partial (fn [ds]
               (when (and (= (count ds) 1) (integer? (first ds)))
                 (str "∂" (n->subscript (first ds)))))
    '/ render-infix-ratio}
   :render-primitive
   (fn r [v]
     (or (infinity->infix v)
         (let [s (str v)]
           (or (infix-sym->unicode s)
               (condp re-find s
                 superscript-pattern
                 :>> (fn [[_ stem superscript]]
                       (if-let [n (re-matches #"[0-9]+" superscript)]
                         (str (r stem) (n->superscript n))
                         (str (r stem) "↑" (r superscript))))

                 subscript-pattern
                 :>> (fn [[_ stem subscript]]
                       (if-let [n (re-matches #"[0-9]+" subscript)]
                         (str (r stem) (n->subscript n))
                         (str (r stem) "_" (r subscript))))
                 v)))))))

(defn- brace
  "Wrap the argument, as a string, in braces"
  [s]
  (str "{" s "}"))

(defn- maybe-brace
  "Wrap the argument in braces, as a string, unless it's just a single character"
  [s]
  (if (and (string? s) (= (count s) 1))
    s
    (brace s)))

(defn- infinity->tex
  "Given some infinite value, returns a (string) representation of the LaTeX
  commands required to render ##Inf or ##-Inf.

  Returns `nil` for all other inputs."
  [x]
  (case x
    ##Inf "\\infty"
    ##-Inf "-\\infty"
    nil))

(def ^{:dynamic true
       :doc "If true, [[->TeX]] will render down tuples as vertical matrices
  with square braces. Defaults to false."}
  *TeX-vertical-down-tuples*
  false)

(def ^{:dynamic true
       :doc "If true, [[->TeX]] will render symbols with more than 1 character
  using a sans-serif typestyle via `\\mathsf`. Defaults to true."}
  *TeX-sans-serif-symbols* true)

(defn- displaystyle [s]
  (str "\\displaystyle{" s "}"))

(def ^:no-doc ->TeX*
  (let [TeX-accent (fn [accent]
                     (fn [[_ stem]]
                       (str "\\" accent " " (maybe-brace
                                             (->TeX* stem)))))
        dot   (TeX-accent "dot")
        ddot  (TeX-accent "ddot")
        hat   (TeX-accent "hat")
        bar   (TeX-accent "bar")
        vec   (TeX-accent "vec")
        tilde (TeX-accent "tilde")
        prime (fn [[_ stem]]
                (let [x (maybe-brace (->TeX* stem))]
                  (str x "^\\prime")))
        primeprime
        (fn [[_ stem]]
          (let [x (maybe-brace (->TeX* stem))]
            (str x "^{\\prime\\prime}")))
        parenthesize
        #(str "\\left(" % "\\right)")]
    (make-infix-renderer
     ;; here we set / to a very low precedence because the fraction bar we will
     ;; use in the rendering groups things very strongly.
     :precedence-map '{D 9, partial 9,
                       expt 8,
                       :apply 7,
                       u- 6,
                       * 5, modulo 5, remainder 5,
                       + 4, - 4,
                       = 3, > 3, < 3, >= 3, <= 3,
                       and 2, or 1, not 1, / 0}
     :parenthesize parenthesize
     :infix? '#{* + - / modulo remainder and or expt u- = > < >= <=}
     :juxtapose-multiply "\\,"
     :rewrite-trig-squares true
     :special-handlers
     {'floor
      (fn [[x]]
        (str "\\left\\lfloor " x " \\right\\rfloor"))

      'ceiling
      (fn [[x]]
        (str "\\left\\lceil " x " \\right\\rceil"))

      'integer-part
      (fn [[x]]
        (str "\\mathsf{int} " (parenthesize x)))

      'fractional-part
      (fn [[x]]
        (str "\\mathsf{frac} " (parenthesize x)))

      'modulo
      (fn [[x y]]
        (str (maybe-brace x) " \\bmod " (maybe-brace y)))

      'remainder
      (fn [[x y]]
        (str (maybe-brace x) " \\mathbin{\\%} " (maybe-brace y)))

      'and
      (fn [[x y]]
        (str x " \\land " y))

      'or
      (fn [[x y]]
        (str x " \\lor " y))

      'not
      (fn [[x]]
        (str "\\lnot" (parenthesize x)))

      'expt (fn [[x e]]
              (str (maybe-brace x) "^" (maybe-brace e)))

      'partial (fn [ds] (str "\\partial_" (maybe-brace (s/join "," ds))))
      '/ (fn [xs]
           (let [n (count xs)]
             (cond (= n 1)
                   (str "\\frac" (brace 1) (brace (first xs)))
                   (= n 2)
                   (str "\\frac" (brace (first xs)) (brace (second xs))))))
      'up (fn [x]
            (let [body (->> (map displaystyle x)
                            (s/join " \\cr \\cr "))]
              (str "\\begin{pmatrix}"
                   body
                   "\\end{pmatrix}")))
      'down (fn [x]
              (let [sep  (if *TeX-vertical-down-tuples*
                           " \\cr \\cr "
                           "&")
                    body (->> (map displaystyle x)
                              (s/join sep))]
                (str "\\begin{bmatrix}"
                     body
                     "\\end{bmatrix}")))
      'sqrt #(str "\\sqrt " (maybe-brace (first %)))
      '<= #(s/join " \\leq " %)
      '>= #(s/join " \\geq " %)}
     :render-primitive
     (fn r [v]
       (if (r/ratio? v)
         (str "\\frac" (brace (r/numerator v)) (brace (r/denominator v)))
         (or (infinity->tex v)
             (let [s (str v)]
               (or (TeX-map s)
                   (condp re-find s
                     superscript-pattern
                     :>> (fn [[_ stem superscript]]
                           (str (maybe-brace (r stem))
                                "^" (maybe-brace (r superscript))))

                     subscript-pattern
                     :>> (fn [[_ stem subscript]]
                           (str (maybe-brace (r stem))
                                "_" (maybe-brace (r subscript))))

                     ;; KaTeX doesn't do \dddot.
                     #"(.+)dotdot$" :>> ddot
                     #"(.+)dot$" :>> dot
                     #"(.+)hat$" :>> hat
                     #"(.+)primeprime$" :>> primeprime
                     #"(.+)prime$" :>> prime
                     #"(.+)bar$" :>> bar
                     #"(.+)vec$" :>> vec
                     #"(.+)tilde$" :>> tilde
                     ;; wrap it if it's a multiletter variable... unless it looks
                     ;; like a differential. (Too hacky?)
                     (if (and (symbol? v)
                              (> (count s) 1)
                              (not (re-matches #"^d[a-zαωθφ]" s)))
                       (if *TeX-sans-serif-symbols*
                         (str "\\mathsf" (brace s))
                         (brace s))
                       v))))))))))

(defn ->TeX
  "Convert the given expression to TeX format, as a string.

  If you set the `:equation` keyword argument to a truthy value, the result will
  be wrapped in an equation environment. `:equation <string>` will insert a
  `\\label{<string>}` entry inside the equation environment.

  For example:

  ```clojure
  (let [expr (+ 'x 'xy)]
    (println
      (->TeX expr :equation \"label!\")))

  \\begin{equation}
  \\label{label!}
  x + y
  \\end{equation}
  ```
  "
  [expr & {:keys [equation]}]
  (let [tex-string (->TeX* expr)]
    (if equation
      (let [label (if (and (string? equation)
                           (seq equation))
                    (str "\\label{" equation "}\n")
                    "")]
        (str "\\begin{equation}\n"
             label tex-string
             "\n\\end{equation}"))
      tex-string)))

(def ^{:doc "Convert the given expression to a string representation of a
  JavaScript function.

  Parameters to the function will be extracted from the symbols in the expression.
  Common subexpression elimination will be performed and auxiliary variables
  will be bound in the body of the function; the names of these symbols are
  obtained from the nullary function option :symbol-generator, which defaults to
  a function yielding `_1, ...`.

  If `:parameter-order` is specified, it is used to determine function parameter
  order in one of two ways:

  If it is set to a function, that function will be called on the sequence of
  parameters and is expected to return the parameters in the desired sequence.

  Otherwise, it is interpreted as the sequence of parameters itself. If not
  specified, the default behavior is `sort`.

  If `:deterministic? true` is supplied, the function will assign variables by
  sorting the string representations of each term before assignment. Otherwise,
  the nondeterministic order of hash maps inside this function won't guarantee a
  consistent variable naming convention in the returned function. For tests, set
  `:deterministic? true`."}
  ->JavaScript
  (let [operators-known '#{+ - * /
                           sin cos tan
                           asin acos atan
                           cosh sinh tanh
                           asinh acosh atanh
                           sqrt abs expt
                           exp log
                           up down}
        make-js-vector #(str \[ (s/join ", " %) \])
        R (make-infix-renderer
           :precedence-map '{not 9, D 8, :apply 8, * 5, / 5, - 3, + 3,
                             = 2, > 2, < 2, >= 2, <= 2,
                             and 1, or 1}
           :infix? '#{* + - / u- =}
           :rename-functions {'sin "Math.sin"
                              'cos "Math.cos"
                              'tan "Math.tan"
                              'asin "Math.asin"
                              'acos "Math.acos"
                              'atan "Math.atan"
                              'cosh "Math.cosh"
                              'sinh "Math.sinh"
                              'tanh "Math.tanh"
                              'asinh "Math.asinh"
                              'acosh "Math.acosh"
                              'atanh "Math.atanh"
                              'sqrt "Math.sqrt"
                              'abs "Math.abs"
                              'expt "Math.pow"
                              'log "Math.log"
                              'exp "Math.exp"
                              'floor "Math.floor"
                              'ceiling "Math.ceil"
                              'integer-part "Math.trunc"
                              'not "!"}
           :special-handlers (let [parens (fn [x]
                                            (str "(" x ")"))]
                               {'up make-js-vector
                                'down make-js-vector
                                'modulo (fn [[a b]]
                                          (-> (str a " % " b)
                                              (parens)
                                              (str " + " b)
                                              (parens)
                                              (str " % " b)
                                              (parens)))
                                'remainder (fn [[a b]]
                                             (str a " % " b))
                                'and (fn [[a b]] (str a " && " b))
                                'or (fn [[a b]] (str a " || " b))
                                '/ render-infix-ratio}))]
    (fn [x & {:keys [symbol-generator parameter-order deterministic?]
             :or {symbol-generator (make-symbol-generator "_")
                  parameter-order sort}}]
      (let [x      (v/freeze x)
            params (set/difference (x/variables-in x) operators-known)
            ordered-params (if (fn? parameter-order)
                             (parameter-order params)
                             parameter-order)
            callback (fn [new-expression new-vars]
                       (doseq [[var val] new-vars]
                         (print "  var ")
                         (print (str var " = "))
                         (print (R val))
                         (print ";\n"))
                       (print "  return ")
                       (print (R new-expression))
                       (print ";\n}"))
            opts {:deterministic?   deterministic?
                  :symbol-generator symbol-generator}]
        (with-out-str
          (print "function(")
          (print (s/join ", " ordered-params))
          (print ") {\n")
          (compile/extract-common-subexpressions x callback opts))))))
