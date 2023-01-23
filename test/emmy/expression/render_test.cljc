#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.expression.render-test
  (:refer-clojure :exclude [+ - * /])
  (:require [clojure.test :refer [is deftest testing use-fixtures]]
            #?(:cljs [goog.string :refer [format]])
            [emmy.abstract.function :as af]
            [emmy.calculus.derivative :refer [D taylor-series]]
            [emmy.expression.render :as r :refer [->infix ->TeX ->JavaScript]]
            [emmy.function :as f]
            [emmy.generic :as g :refer [expt sin cos + - * /]]
            [emmy.numsymb :as sym]
            [emmy.series :as series]
            [emmy.simplify :refer [hermetic-simplify-fixture]]
            [emmy.structure :refer [up down]]))

(use-fixtures :each hermetic-simplify-fixture)

(def ^:private s->infix
  (f/compose ->infix g/simplify))

(def ^:private s->TeX
  (f/compose ->TeX g/simplify))

(defn ^:private s->JS [x & options]
  (apply ->JavaScript (g/simplify x) options))

(deftest basic
  (testing "raw expressions"
    (is (= "function(remainder, x, y) {\n  return x % y;\n}"
           (->JavaScript (g/remainder 'x 'y)))
        "remainder works")

    (is (= "Df(x, y)" (->infix '((D f) x y))))
    (is (= "D(f + g)(x, y)" (->infix '((D (+ f g)) x y))))
    (is (= "D(f g)(x, y)" (->infix '((D (* f g)) x y))))
    (is (= "(sin cos)(t)" (->infix '((* sin cos) t))))
    (is (= "- cos(t ω + φ) a m ω² + cos(t ω + φ) a k"
           (->infix '(+ (* -1 (cos (+ (* t ω) φ)) a m (expt ω 2))
                        (* (cos (+ (* t ω) φ)) a k)))))
    (is (= "(a + b c)(x, y, z)" (->infix '((+ a (* b c)) x y z))))
    (is (= "(a (b + c))(u, v)" (->infix '((* a (+ b c)) u v))))
    (is (= "f(g)(h)(k)(r, s)" (->infix '((((f g) h) k) r s))))
    (is (= "f(r, s)(x, y)" (->infix '((f r s) x y))))
    (is (= "a + b + c f(u)(v)" (->infix '(+ a b (* c ((f u) v))))))
    (is (= "a b (c + (f / g)(v))" (->infix '(* a b (+ c ((/ f g) v))))))
    (is (= "foo bar" (->infix '(* foo bar))))
    (is (= "a b (f / g)(v)" (->infix '(* a b ((/ f g) v)))))
    (is (= "a" (->infix '(* a))))
    (is (= "(- x)³" (->infix '(expt (- x) 3))))
    (is (= "(- x)⁴" (->infix '(expt (- x) 4))))
    (is (= "a" (->infix '(+ a))))
    (is (= "1 / a" (->infix '(/ a))))
    (is (= "- a" (->infix '(- a)))))

  (testing "with-simplifier"
    (af/with-literal-functions [[f [0 0] 0]]
      (is (= "a + b + c" (s->infix (+ 'a 'b 'c))))
      (is (= "a b c" (s->infix (* 'a 'b 'c))))
      (is (= "a b + a c" (s->infix (* 'a (+ 'b 'c)))))
      (is (= "b c + a" (s->infix (+ 'a (* 'b 'c)))))
      (is (= "- a + b - c + d" (s->infix (+ (* -1 'a) 'b (* -1 'c) 'd))))
      (is (= "a - b + c - d" (s->infix (+ 'a (* -1 'b) 'c (* -1 'd)))))
      (is (= "- a" (s->infix (- 'a))))
      (is (= "- a" (s->infix (- 0 'a))))
      (is (= "- a / b" (s->infix (/ (- 'a) 'b))))
      (is (= "1 / (a + b)" (s->infix (/ (+ 'a 'b)))))
      (is (= "x⁴" (s->infix (expt (- 'x) 4))))
      (is (= "- x³" (s->infix (expt (- 'x) 3))))
      (is (= "a - b - c" (s->infix (- 'a 'b 'c))))
      (is (= "a f(b, c)" (s->infix (* 'a (f 'b 'c)))))
      (is (= "a f(2 h + 2 k, c)" (s->infix (* 'a (f (* 2 (+ 'h 'k)) 'c)))))
      (is (= "f(x, y)" (s->infix (f 'x 'y))))
      (is (= "down(∂₀f(x, y), ∂₁f(x, y))" (s->infix ((D f) 'x 'y))))
      (is (= "sin(t) cos(t)" (s->infix ((* sin cos) 't)))))))

(deftest exponents
  (is (= '"x⁴ + 4 x³ + 6 x² + 4 x + 1"
         (s->infix (expt (+ 1 'x) 4))))
  (is (= "x¹²" (->infix (expt 'x 12))))
  (is (= "y¹⁵ + 3 x⁴ y¹⁰ + 3 x⁸ y⁵ + x¹²"
         (s->infix (expt (+ (expt 'x 4) (expt 'y 5)) 3))))
  (is (= "x² + x^-2" (->infix '(+ (expt x 2) (expt x -2)))))
  (is (= "sin²(x)" (->infix '((expt sin 2) x))))
  (is (= "(x y)²" (->infix '(expt (* x y) 2))))
  (is (= "sin²(x)" (->infix ((expt sin 2) 'x))))
  (is (= "(sin(x))^y" (->infix (expt (sin 'x) 'y))))
  (is (= "(a + b)²" (->infix '(expt (+ a b) 2))))
  (is (= "(a + b)^(x + y)" (->infix '(expt (+ a b) (+ x y)))))
  (is (= "(a + b)^x" (->infix '(expt (+ a b) x))))
  (is (= "a^(x + y)" (->infix '(expt a (+ x y)))))
  (is (= "x^y" (->infix (expt 'x 'y)))))

(deftest more-with-D
  (af/with-literal-functions [f g]
    (is (= "f(s)" (->infix (f 's))))
    (is (= "(f + g)(x, y)" (->infix '((+ f g) x y))))
    (is (= "f(x) g(x)" (->infix ((* f g) 'x))))
    (is (= "f(t)" (->infix (f 't))))
    (is (= "Df(s)" (->infix ((D f) 's))))
    (is (= "D²f(s)" (->infix (((expt D 2) f) 's))))))

(deftest structures
  (is (= "down(up(1, 2), up(3, 4))"
         (->infix (down (up 1 2) (up 3 4)))
         (->infix (down [1 2] [3 4])))
      "vector renders as an up, infix")

  (is (= (str "\\begin{pmatrix}\\displaystyle{1} \\cr \\cr "
              "\\displaystyle{2} \\cr \\cr "
              "\\displaystyle{3}\\end{pmatrix}")
         (->TeX [1 2 3]))
      "vector renders as an up, TeX")

  (is (= "down(up(1, 2), up(3, 4))"
         (->infix
          (down (up 1 2) (up 3 4)))))

  (testing "customizable down tuple rendering in TeX"
    (is (= (str "\\begin{bmatrix}"
                "\\displaystyle{1}&\\displaystyle{2}&\\displaystyle{3}"
                "\\end{bmatrix}")
           (->TeX (down 1 2 3)))
        "Downs render horizontally by default")

    (binding [r/*TeX-vertical-down-tuples* true]
      (is (= (str "\\begin{bmatrix}\\"
                  "displaystyle{1} \\cr \\cr "
                  "\\displaystyle{2} \\cr \\cr "
                  "\\displaystyle{3}"
                  "\\end{bmatrix}")
             (->TeX (down 1 2 3)))
          "bind the dynamic variable truthy to get vertical down tuples."))))

(deftest variable-sub-super-scripts
  (testing "infix"
    (is (= "x⁰ + y¹ + z²" (->infix (+ 'x↑0 'y↑1 'z↑2))))
    (is (= "x₀ + y₁ + z₂" (->infix (+ 'x_0 'y_1 'z_2)))))

  (testing "TeX"
    (is (= "x^0 + y^1 + z^2" (->TeX (+ 'x↑0 'y↑1 'z↑2))))
    (is (= "x_0 + y_1 + z_2" (->TeX (+ 'x_0 'y_1 'z_2))))))

(deftest accent-tests
  (testing "various accents and special exponents in TeX"
    (is (= (str "\\begin{pmatrix}"
                "\\displaystyle{\\dot q} \\cr \\cr "
                "\\displaystyle{\\dot {qd}} \\cr \\cr "
                "\\displaystyle{\\hat {vz}} \\cr \\cr "
                "\\displaystyle{\\bar {cake}} \\cr \\cr "
                "\\displaystyle{\\vec x} \\cr \\cr "
                "\\displaystyle{\\tilde {zcake}} \\cr \\cr "
                "\\displaystyle{q^\\prime} \\cr \\cr "
                "\\displaystyle{{xx}^{\\prime\\prime}}"
                "\\end{pmatrix}")
           (->TeX (up 'qdot 'qddot
                      'vzhat 'cakebar
                      'xvec
                      'zcaketilde
                      'qprime 'xxprimeprime))))))

(deftest ratio-tests
  (testing "one-arg / == inverse"
    (is (= "1/2" (->infix (/ 2))))
    (is (= "\\frac{1}{2}" (->TeX (/ 2)))))

  (testing "two-arg / with numbers renders as a ratio, no parens"
    (is (= "x + 3/2" (->infix (+ 'x (/ 3 2)))))
    (is (= "x + \\frac{3}{2}" (->TeX (+ 'x (/ 3 2))))))

  (testing "two-arg / with numbers renders as a ratio"
    (is (= "x + 3 / y" (->infix (+ 'x (/ 3 'y))))
        "spaces around / for non-integrals")

    (is (= "x + \\left(\\frac{3}{y}\\right)"
           (->TeX (+ 'x (/ 3 'y)))))))

(deftest non-expression-literal-tests
  (testing "literal inputs render to strings"
    (is (= "10" (->infix 10)))
    (is (= "x" (->infix 'x)))
    (is (= "10" (->TeX 10)))
    (is (= "x" (->TeX 'x)))))

(deftest TeX-easy
  (is (= "a + b" (->TeX (+ 'a 'b))))
  (is (= "\\lambda + \\mu" (->TeX (+ 'lambda 'mu))))
  (is (= "x_0 + y_s" (->TeX (+ 'x_0 'y_s))))
  (is (= "\\frac{1}{x}" (->TeX (/ 1 'x))))
  (is (= "\\frac{a + b}{c + d}" (->TeX (/ (+ 'a 'b) (+ 'c 'd)))))
  (is (= "\\frac{a}{b}" (->TeX (/ 'a 'b)))))

(deftest equation-tests
  (is (= "x = 4" (->infix '(= x 4))))
  (is (= "x = 4" (->TeX '(= x 4))))

  (is (= "x > 4" (->infix '(> x 4))))
  (is (= "x > 4" (->TeX '(> x 4))))

  (is (= "x < 4" (->infix '(< x 4))))
  (is (= "x < 4" (->TeX '(< x 4))))

  (is (= "x >= 4" (->infix '(>= x 4))))
  (is (= "x \\geq 4" (->TeX '(>= x 4))))

  (is (= "x <= 4" (->infix '(<= x 4))))
  (is (= "x \\leq 4" (->TeX '(<= x 4))))

  (is (= "e^(i π) + 1 = 0"
         (->infix '(= (+ (expt e (* i pi)) 1) 0))))
  (is (= "{e}^{\\left(i\\,\\pi\\right)} + 1 = 0"
         (->TeX '(= (+ (expt e (* i pi)) 1) 0))))

  (is (= "4 = 2 + 2 = 1 + 3"
         (->infix '(= 4 (+ 2 2) (+ 1 3)))))
  (is (= "4 = 2 + 2 = 1 + 3"
         (->TeX '(= 4 (+ 2 2) (+ 1 3)))))

  (is (= "4 \\leq 2 + 2 \\leq 1 + 3"
         (->TeX '(<= 4 (+ 2 2) (+ 1 3)))))
  (is (= "4 \\geq 2 + 2 \\geq 1 + 3"
         (->TeX '(>= 4 (+ 2 2) (+ 1 3)))))

  (testing "infix symbols"
    (is (= "sin(π) + sin(Π) + cos(ο) + atan(Α) + ..."
           (->infix
            '(+ (sin pi) (sin Pi)
                (cos omicron) (atan Alpha)
                ldots)))))

  (testing "unicode->tex"
    (is (= (str "\\begin{pmatrix}"
                "\\displaystyle{{\\Delta}_{\\rho}} \\cr \\cr"
                " \\displaystyle{{\\varepsilon}^{\\omega}} \\cr \\cr"
                " \\displaystyle{{\\varrho}_{\\varsigma}}"
                "\\end{pmatrix}")
           (->TeX '(up Δ_ρ ϵ↑ω ϱ_ς)))
        "->TeX can handle unicode greek characters in subscript and superscript
        position."))

  (testing "superscript support"
    (is (= "{{{p^2}_1}^{12}}^{12}"
           (->TeX 'p↑2_1↑12↑12))
        "nested sequences don't look great; they certainly don't render as
        tensor indexing notation.")

    (is (= "p²₁¹²¹²"
           (->infix 'p↑2_1↑12↑12))
        "nested sequences don't look great; succcessive superscripts and
        subscripts don't demarcate.")

    (testing "Both sides if a subscript or superscript are rendered."
      (is (= "φ_θ" (->infix "phi_theta")))
      (is (= "φ↑θ₁↑ζ_π" (->infix 'phi↑theta_1↑zeta_pi))
          "numbers are subscripted when they appear")))

  (testing "boolean operations"
    (let [or    (sym/symbolic-operator 'or)
          and   (sym/symbolic-operator 'and)
          not   (sym/symbolic-operator 'not)
          sym:= (sym/symbolic-operator '=)]
      (is (= "\\left(a \\lor b\\right) \\land \\left(\\lnot\\left(c \\lor d\\right)\\right)"
             (->TeX (and (or 'a 'b)
                         (not (or 'c 'd))))))

      (is (= "((a ∨ b) ∧ ¬(c ∨ d)) = (x ∨ z)"
             (->infix (sym:= (and (or 'a 'b)
                                  (not (or 'c 'd)))
                             (or 'x 'z))))))))

(deftest equation-wrapper-tests
  (is (= (str "\\begin{equation}\n"
              "x + y\n"
              "\\end{equation}")
         (->TeX (g/+ 'x 'y) :equation true)
         (->TeX (g/+ 'x 'y) :equation 5)
         (->  (g/+ 'x 'y)
              (->TeX :equation "")))
      "truthy (including empty-string) :equation option wraps the result in an
      equation environment.")

  (is (= (str "\\begin{equation}\n"
              "\\label{label}\n"
              "x + y\n"
              "\\end{equation}")
         (->TeX (g/+ 'x 'y)
                :equation "label"))
      "Supplying a non-empty string triggers a label."))

(deftest symbol-rendering-tests
  (is (= "x" (->TeX 'x)))
  (is (= "\\mathsf{PV}" (->TeX 'PV)))
  (binding [emmy.expression.render/*TeX-sans-serif-symbols* false]
    (is (= "{PV}" (->TeX 'PV)))))

(deftest series-tests
  (is (= "1 + 1 + 1/2 + 1/6 + ..."
         (->infix (series/exp-series 1))))
  (is (= "1 + 1 + \\frac{1}{2} + \\frac{1}{6} + \\ldots"
         (->TeX (series/exp-series 1))))
  (is (= "(k + 1) + 1 + 1/2 + 1/6 + ..."
         (->infix (+ 'k (series/exp-series 1)))))
  (is (= "\\left(k + 1\\right) + 1 + \\frac{1}{2} + \\frac{1}{6} + \\ldots"
         (->TeX (+ 'k (series/exp-series 1)))))
  (is (= "_⁰ + 2 _¹ + 3 _² + 4 _³ + ..."
         (->infix (series/power-series 1 2 3 4))))
  (is (= "{\\_}^{0} + 2\\,{\\_}^{1} + 3\\,{\\_}^{2} + 4\\,{\\_}^{3} + \\ldots"
         (->TeX (series/power-series 1 2 3 4))))
  (is (= "1 + 2 x + 3 x² + 4 x³ + ..."
         (->infix ((series/power-series 1 2 3 4) 'x))))
  (is (= "1 + 2\\,x + 3\\,{x}^{2} + 4\\,{x}^{3} + \\ldots"
         (->TeX ((series/power-series 1 2 3 4) 'x)))))

(defn ^:private make-symbol-generator
  [p]
  (let [i (atom 0)]
    (fn [] (symbol (format "%s%d" p (swap! i inc))))))

(deftest JS
  (is (= "function(a, b, theta) {\n  return a + b + Math.sin(theta);\n}"
         (->JavaScript (+ 'a 'b (sin 'theta)))))
  (is (= "function(j) {\n  return 1 / j;\n}"
         (->JavaScript (g/invert 'j))))
  (is (= "function(y) {\n  return Math.pow(2.71828, y);\n}"
         (->JavaScript (expt 2.71828 'y))))
  (is (= "function(a, b, x) {\n  return a * Math.exp(b * Math.log(x));\n}"
         (->JavaScript (* 'a (g/exp (* 'b (g/log 'x)))))))
  (is (= "function(x) {\n  var _0001 = Math.sin(x);\n  return Math.pow(_0001, 2) + _0001;\n}"
         (s->JS (+ (sin 'x) (expt (sin 'x) 2)))))
  (is (= (str "function(x, dx) {\n"
              "  var t1 = Math.sin(x);\n"
              "  return -1/2 * Math.pow(dx, 2) * t1 + dx * Math.cos(x) + t1;\n"
              "}")
         (s->JS (series/sum ((taylor-series sin 'x) 'dx) 2)
                :symbol-generator (make-symbol-generator "t")
                :parameter-order '[x dx])))
  (is (= "function(x, y) {\n  return [1, x + y, 2];\n}"
         (s->JS (up 1 (+ 'x 'y) 2))))

  (is (= "function(a, b) {\n  return [[1, a], [b, 2]];\n}"
         (->JavaScript (down (up 1 'a) (up 'b 2))))))

(deftest systematic
  (let [all-formats (juxt s->infix s->JS s->TeX)]
    (is (= ["a + b"
            "function(a, b) {\n  return a + b;\n}"
            "a + b"]
           (all-formats (+ 'a 'b))))
    (is (= ["a b"
            "function(a, b) {\n  return a * b;\n}"
            "a\\,b"]
           (all-formats (* 'a 'b))))
    (is (= ["a / b"
            "function(a, b) {\n  return a / b;\n}"
            "\\frac{a}{b}"]
           (all-formats (/ 'a 'b))))
    (is (= ["a - b"
            "function(a, b) {\n  return a - b;\n}"
            "a - b"]
           (all-formats (- 'a 'b))))
    (is (= ["sin(t)"
            "function(t) {\n  return Math.sin(t);\n}"
            "\\sin\\left(t\\right)"]
           (all-formats (sin 't))))
    (is (= ["sin²(t)"
            "function(t) {\n  return Math.pow(Math.sin(t), 2);\n}"
            "{\\sin}^{2}\\left(t\\right)"]
           (all-formats ((expt sin 2) 't))))
    (is (= ["cos²(tan(t))"
            "function(t) {\n  return Math.pow(Math.cos(Math.tan(t)), 2);\n}"
            "{\\cos}^{2}\\left(\\tan\\left(t\\right)\\right)"]
           (all-formats ((expt cos 2) (g/tan 't)))))
    (is (= ["sin²(q + t)"
            "function(q, t) {\n  return Math.pow(Math.sin(q + t), 2);\n}"
            "{\\sin}^{2}\\left(q + t\\right)"]
           (all-formats ((expt sin 2) (+ 't 'q)))))
    (is (= ["a b + c d"
            "function(a, b, c, d) {\n  return a * b + c * d;\n}"
            "a\\,b + c\\,d"]
           (all-formats (+ (* 'a 'b) (* 'c 'd)))))
    (is (= ["a c + a d + b c + b d"
            "function(a, b, c, d) {\n  return a * c + a * d + b * c + b * d;\n}"
            "a\\,c + a\\,d + b\\,c + b\\,d"]
           (all-formats (* (+ 'a 'b) (+ 'c 'd)))))
    (is (= ["(a + b) / c"
            "function(a, b, c) {\n  return (a + b) / c;\n}"
            "\\frac{a + b}{c}"]
           (all-formats (/ (+ 'a 'b) 'c))))
    (is (= ["a / (b + c)"
            "function(a, b, c) {\n  return a / (b + c);\n}"
            "\\frac{a}{b + c}"]
           (all-formats (/ 'a (+ 'b 'c)))))
    (is (= ["a / (b c)"
            "function(a, b, c) {\n  return a / (b * c);\n}"
            "\\frac{a}{b\\,c}"]
           (all-formats (/ 'a (* 'b 'c)))))
    (is (= ["(b c) / a"
            "function(a, b, c) {\n  return (b * c) / a;\n}"
            "\\frac{b\\,c}{a}"]
           (all-formats (/ (* 'b 'c) 'a))))
    (is (= ["(a b) / (c d)"
            "function(a, b, c, d) {\n  return (a * b) / (c * d);\n}"
            "\\frac{a\\,b}{c\\,d}"]
           (all-formats (/ (* 'a 'b) (* 'c 'd)))))
    (is (= ["- a - b - c"
            "function(a, b, c) {\n  return - a - b - c;\n}"
            "- a - b - c"]
           (all-formats (- (+ 'a 'b 'c)))))
    (is (= ["- a b c"
            "function(a, b, c) {\n  return - a * b * c;\n}"
            "- a\\,b\\,c"]
           (all-formats (- (* 'a 'b 'c)))))
    (let [f (af/literal-function 'f)]
      (is (= ["Df(x)"
              "function(D, f, x) {\n  return D(f)(x);\n}"
              "Df\\left(x\\right)"]
             (all-formats ((D f) 'x))))
      (is (= ["D²f(x)"
              "function(D, f, x) {\n  return Math.pow(D, 2)(f)(x);\n}"
              "{D}^{2}f\\left(x\\right)"]
             (all-formats ((D (D f)) 'x))))

      (let [expr (-> ((taylor-series
                       (af/literal-function 'f (up 0 0) 0)
                       (up 'x 'y))
                      (up 'dx 'dy))
                     (series/sum 2))]
        (is (= "1/2 dx² ∂₀²f(up(x, y)) + dx dy (∂₀ ∂₁)(f)(up(x, y)) + 1/2 dy² ∂₁²f(up(x, y)) + dx ∂₀f(up(x, y)) + dy ∂₁f(up(x, y)) + f(up(x, y))"
               (s->infix expr)))


        (is (= (str "function(dx, dy, f, partial, x, y) {\n"
                    "  var _0001 = 1/2;\n"
                    "  var _0002 = partial(0);\n"
                    "  var _0003 = partial(1);\n"
                    "  var _0004 = [x, y];\n"
                    "  return _0001 * Math.pow(dx, 2) * Math.pow(_0002, 2)(f)(_0004) + dx * dy * (_0002 * _0003)(f)(_0004) + _0001 * Math.pow(dy, 2) * Math.pow(_0003, 2)(f)(_0004) + dx * _0002(f)(_0004) + dy * _0003(f)(_0004) + f(_0004);\n}")
               (s->JS expr :deterministic? true)))

        (is (= "\\frac{1}{2}\\,{dx}^{2}\\,{\\partial_0}^{2}f\\left(\\begin{pmatrix}\\displaystyle{x} \\cr \\cr \\displaystyle{y}\\end{pmatrix}\\right) + dx\\,dy\\,\\left(\\partial_0\\,\\partial_1\\right)\\left(f\\right)\\left(\\begin{pmatrix}\\displaystyle{x} \\cr \\cr \\displaystyle{y}\\end{pmatrix}\\right) + \\frac{1}{2}\\,{dy}^{2}\\,{\\partial_1}^{2}f\\left(\\begin{pmatrix}\\displaystyle{x} \\cr \\cr \\displaystyle{y}\\end{pmatrix}\\right) + dx\\,\\partial_0f\\left(\\begin{pmatrix}\\displaystyle{x} \\cr \\cr \\displaystyle{y}\\end{pmatrix}\\right) + dy\\,\\partial_1f\\left(\\begin{pmatrix}\\displaystyle{x} \\cr \\cr \\displaystyle{y}\\end{pmatrix}\\right) + f\\left(\\begin{pmatrix}\\displaystyle{x} \\cr \\cr \\displaystyle{y}\\end{pmatrix}\\right)"
               (s->TeX expr)))))))
