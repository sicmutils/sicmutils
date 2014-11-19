(ns math.function
  (:require [math.value :as v]
            [math.expression :as x]
            [math.generic :as g]))

(defrecord Fn [name]
  v/Value
  (zero? [x] false)
  (one? [x] false)
  (zero-like [x] false)
  (exact? [x] false)
  (sort-key [x] 35)
  clojure.lang.IFn
  (invoke [f x] (x/literal-number (list (.name f) x)))
  ;;(applyTo [this args] (clojure.lang.AFn/applyToHelper this args))
  )

(defn literal-function [f] (Fn. f))

;; what literal functions work out to in scmutils:

;; 1 ]=> (define f (literal-function 'x))
;; #| f |#

;; 1 ]=> (display f)
;; #[apply-hook 14]

;; 1 ]=> (display (f 't))
;; (*number* (expression (x t)) (literal-function #[apply-hook 14]) (type-expression Real))
