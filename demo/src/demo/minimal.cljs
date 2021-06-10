(ns demo.minimal
  (:require
    ; sicmutils.env has aliases for common functions. If you are using a REPL, run
    ; `sicmutils.env/bootstrap-repl!` to pull in all the aliases into the current namespace.
    [sicmutils.env :as s :include-macros true]
    [sicmutils.expression.render :as sr]))

(defn output [expr]
  ; `simplify` collapses expressions while `->infix` prints them using Unicode operators. 
  (let [string (sr/->infix (s/simplify expr))]
    ; Depending on your build options, the default output might go to the browser console or the
    ; terminal's console.
    (println string)
    ; Also hackishly append to the <body> for browser builds.
    (if (resolve 'js/document) (.append js/document.body string "\n"))))

(output "Running demo.minimal...\n")

; It's important to use SICM's math operators so that the result remains a ratio.
(output (s/- (s/* 7 (s// 1 2)) 2))    ; 3/2
; The default math operators will turn this expression into a float.
(output (- (* 7 (/ 1 2)) 2))          ; 1.5

; Define a basic expression.
(def f (s/* 3 (s/expt s/sin 2)))
(output (f 'x))                       ; 3 sin²(x)
; And take the derivative.
(output ((s/D f) 'x))                 ; 6 sin(x) cos(x)

; More examples are available at /demo.clj