(ns math.expression
  (:require [math.value :as v]
            [clojure.walk :refer :all]))

(defrecord Expression [type expression]
  v/Value
  (zero? [x] false)
  (one? [x] false)
  (zero-like [x] false)
  (exact? [x] false)
  (sort-key [x] 17)
  ;; clojure.lang.IFn
  ;; (invoke [f x] ...)
  )

(defn make-expression [x]
  (Expression. :number x))

(defn expression? [x]
  (instance? Expression x))

(defn make-numeric-literal [expression]
  (if (number? expression)
    expression
    (Expression. :number expression)))

(defn variables-in [expr]
  (->> (.expression expr) flatten (filter symbol?) (into #{})))

(defn walk-expression [environment expr]
  (postwalk (fn [a]
              (cond (number? a) a
                    (symbol? a) (if-let [binding (a environment)]
                                  binding
                                  (throw (IllegalArgumentException.
                                          (str "no binding for " a " " (type a)
                                               " " (namespace a) " in "
                                               environment))))
                    (sequential? a) (apply (first a) (rest a))
                    :else (throw (IllegalArgumentException.
                                  (str "unknown expression type " a)))))
            (.expression expr)))

;; this guy goes in here. metadata? or a Value?
;; expression of type T? predicate for experssionator?
;;
;; (define (make-real-literal expression)
;;   (let ((e (make-numerical-literal expression)))
;;     (add-property! e 'real #t)
;;     e))

;; (define (make-literal type-tag expression)
;;   (list type-tag (list 'expression expression)))

;; property management: plan: do this with metadata ?

;;
;; (define (add-property! abstract-quantity property-name property-value)
;;   (if (pair? abstract-quantity)
;;       (set-cdr! (last-pair abstract-quantity)
;; 		(list (list property-name property-value)))
;;       (error "Bad abstract quantity -- ADD-PROPERTY!")))

;; (define ((has-property? property-name) abstract-quantity)
;;   (cond ((pair? abstract-quantity)
;; 	 (assq property-name (cdr abstract-quantity)))
;; 	((symbol? abstract-quantity)
;; 	 (if (eq? property-name 'expression)
;; 	     (list 'expression abstract-quantity)
;; 	     (error "Symbols have only EXPRESSION properties")))
;; 	(else
;; 	 (error "Bad abstract quantity"))))

;; (define (get-property abstract-quantity property-name #!optional default)
;;   (cond ((pair? abstract-quantity)
;; 	 (let ((default (if (default-object? default) #f default))
;; 	       (v (assq property-name (cdr abstract-quantity))))
;; 	   (if v (cadr v) default)))
;; 	((symbol? abstract-quantity)
;; 	 (if (eq? property-name 'expression)
;; 	     abstract-quantity
;; 	     default))
;; 	(else
;; 	 (error "Bad abstract quantity"))))
