(ns demo.minimal
  (:require
   ;; sicmutils.env has aliases for common functions. If you are using a REPL, run
   ;; `sicmutils.env/bootstrap-repl!` to pull in all the aliases into the current namespace.
   [sicmutils.env :as e :include-macros true]))

(defn output [expr]
  ;; `simplify` collapses expressions while `->infix` prints them using Unicode operators.
  (let [string (e/->infix (e/simplify expr))]
    ;; Depending on your build options, the default output might go to the browser console or the
    ;; terminal's console.
    (println string)
    ;; Also hackishly append to the <body> for browser builds.
    (when (resolve 'js/document)
      (.append js/document.body string "\n"))))

(output "Running demo.minimal...\n")

;; It's important to use SICM's math operators so that the result remains a ratio.
(output (e/- (e/* 7 (e// 1 2)) 2))    ; 3/2
;; The default math operators will turn this expression into a float.
(output (- (* 7 (/ 1 2)) 2))          ; 1.5

;; Define a basic expression.
(def f (e/* 3 (e/expt e/sin 2)))
(output (f 'x))                       ; 3 sin²(x)

;; And take the derivative.
(output ((e/D f) 'x))                 ; 6 sin(x) cos(x)
