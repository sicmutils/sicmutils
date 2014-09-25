(ns math.expression
  (:require [math.generic :as g]))

(defn make-literal [type expression] (with-meta expression {:type :number}))

(defn make-numeric-literal [expression]
  (if (number? expression)
    expression
    (make-literal '*number* expression)))

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
	 
