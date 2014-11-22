(ns math.function
  (:require [math.value :as v]
            [math.expression :as x]
            [math.generic :as g]))

(defrecord Fn [name arity]
  v/Value
  (zero? [x] false)
  (one? [x] false)
  (zero-like [x] false)
  (exact? [x] false)
  (sort-key [x] 35)
  clojure.lang.IFn
  (invoke [f x] (x/literal-number (list (.name f) x)))
  ;; XXX (applyTo [this args] (clojure.lang.AFn/applyToHelper this args))
  )

(defn literal-function [f] (Fn. f 1))

(defn arity
  [f]
  ;; XXX: we need a way to find the arity of something
  ;; besides a gn
  (cond (instance? Fn f) (.arity f)
        (instance? clojure.lang.IFn f) 1
        :else 0))

;; what literal functions work out to in scmutils:

;; 1 ]=> (define f (literal-function 'x))
;; #| f |#

;; 1 ]=> (display f)
;; #[apply-hook 14]

;; 1 ]=> (display (f 't))
;; (*number* (expression (x t)) (literal-function #[apply-hook 14]) (type-expression Real))
