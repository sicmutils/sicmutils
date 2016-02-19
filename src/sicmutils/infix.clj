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
            [clojure.string :as s]))

(defn ^:private make-infix-renderer
  [& {:keys [juxtapose-multiply special-handlers infix? render-primitive
             parenthesize precedence-map]
      :or {special-handlers {}
           infix? (constantly false)}}]
  (letfn [(precedence [op] (or (precedence-map op)
                               (cond (seq? op) (precedence-map :apply)
                                     (symbol? op) (precedence-map :apply)
                                     :else 99)))
          ;; TODO: the names are wack; reverse them one way or the other
          (precedence> [a b] (< (precedence a) (precedence b)))
          (precedence<= [a b] (not (precedence> a b)))
          (parenthesize-if [b x]
            (if b (parenthesize x) x))
          (render-node [n]
            (if (z/branch? n)
              ;; then the first child is the function and the rest are the
              ;; arguments.
              (let [fn-loc (-> n z/next)
                    arg-loc (loop [a (-> n z/next z/right)]
                              (let [a' (z/replace a (render-node a))]
                                (if-let [r (z/right a')]
                                  (recur r)
                                  (z/up a'))))
                    [op & args] (z/node arg-loc)
                    upper-op (and (z/up arg-loc)
                                  (-> arg-loc z/leftmost z/node))]
                (if (infix? op)
                  (parenthesize-if
                   (and (infix? upper-op)
                        (precedence> upper-op op))
                   (or (and (special-handlers op)
                            ((special-handlers op) args))
                       (s/join (cond
                                 (= op '*) (or juxtapose-multiply " * ")
                                 (= op 'expt) "^"
                                 :else (str " " op " "))
                               args)))
                  (or (and (special-handlers op)
                           ((special-handlers op) args))
                      (str (parenthesize-if (and (z/branch? fn-loc)
                                                 (precedence> :apply (z/node (z/next fn-loc))))
                                            (render-node (z/next arg-loc)))
                           (parenthesize-if (or (precedence<= op :apply)
                                                (> (count args) 1)
                                                (z/branch? (z/right fn-loc)))
                                            (s/join ", " args))))))

              ;; primitive case
              (let [n (z/node n)]
                (or (and render-primitive (render-primitive n))
                    n))))]
    #(-> % z/seq-zip render-node)))

(def ^:private decimal-superscripts
  [\⁰ \¹ \² \³ \⁴ \⁵ \⁶ \⁷ \⁸ \⁹])
(def ^:private decimal-subscripts
  [\₀ \₁ \₂ \₃ \₄ \₅ \₆ \₇ \₈ \₉])

(defn ^:private n->script
  "Given an integer, returns a string where each digit of the
  integer is used as the index into the replacement map scripts,
  which is expected to be indexable by integers in the range [0..9]."
  [n scripts]
  (apply str (map #(-> % (Character/digit 10) scripts)
                  (str n))))

(def ^:private n->subscript #(n->script % decimal-subscripts))
(def ^:private n->superscript #(n->script % decimal-superscripts))

(def ->infix
  "Converts an S-expression to printable infix form. Numeric exponents are
  written as superscripts. Partial derivatives get subscripts."
  (make-infix-renderer
   :precedence-map {'∂ 1, 'D 1, 'expt 2, :apply 3, '/ 5, '* 5, '+ 6, '- 6}
   :parenthesize #(str "(" % ")")
   :infix? #{'* '+ '- '/ 'expt}
   :juxtapose-multiply " "
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
   })

(defn ^:private brace
  "Wrap the argument, as a string, in braces"
  [s]
  (str "{" s "}"))

(defn ^:private maybe-brace
  "Wrap the argument in braces, as a string, unless it's just a single character"
  [s]
  (if (and (string? s) (> (count s) 1))
    (brace s)
    s))

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
     :precedence-map {'∂ 1, 'D 1, :apply 2, 'expt 2, '* 5, '+ 6, '- 6, '/ 9}
     :parenthesize #(str "\\left(" % "\\right)")
     :infix? #{'* '+ '- '/ 'expt}
     :juxtapose-multiply "\\,"
     :special-handlers
     {'expt (fn [[x e]] (str (maybe-brace x) "^" (maybe-brace e)))
      '∂ (fn [ds] (str "\\partial_" (maybe-brace (s/join "," ds))))
      '/ (fn [xs]
           (when (= (count xs) 2)
             (str "\\dfrac" (maybe-brace (first xs)) (maybe-brace (second xs)))))
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
