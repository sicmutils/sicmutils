(ns math.numsymb
  (:require [math.generic :as g]
            [math.expression :as x]))

;; N.B.: (define numerical-expression-canonicalizer #f)
;;       (define heuristic-number-canonicalizer #f)
;;       (define incremental-simplifier #f)

(defn- numerical-expression [expr] expr)
(def symbolic-operator-table {})

(defn make-numsymb-expression [operator operands]
  (let [operand-exprs (map numerical-expression operands)
        v (operator symbolic-operator-table)]
    (if v
      (let [newexp (apply v operand-exprs)]
        (x/make-literal :number newexp)))))


;; (define (make-numsymb-expression operator-symbol operands)
;;   (let ((operand-expressions (map numerical-expression operands)))
;;     (let ((v (hash-table/get symbolic-operator-table operator-symbol #f)))
;;       (if v
;; 	  (let ((newexp (apply v operand-expressions)))
;; 	    (make-literal number-type-tag
;; 			  (if incremental-simplifier
;; 			      (incremental-simplifier newexp)
;; 			      newexp)))
;; 	  (make-combination number-type-tag operator-symbol operands)))))


;; (define (numerical-expression expr)
;;   (cond ((number? expr) ;; ok.
;; 	 (if (and (inexact? expr) heuristic-number-canonicalizer)
;; 	     (heuristic-number-canonicalizer expr)
;; 	     expr)) ;; but h-n-c is false, so expr.
;; 	((symbol? expr) expr) ;; ok, expr
;; 	((literal-number? expr) 
;; 	 (if numerical-expression-canonicalizer
;; 	     (numerical-expression-canonicalizer (expression-of expr))
;; 	     (expression-of expr)) 
;; 	 (expression-of expr)) ;; but n-e-c is false, so (expression-of expr)
;; 	((pair? expr) ;; works out to expr.
;; 	 (cond ((memq (car expr) type-tags) expr)
;; 	       (numerical-expression-canonicalizer
;; 		(numerical-expression-canonicalizer expr))
;; 	       (else expr)))
;; 	(else expr)))
;;
;; so this works out to expr, unless literal-number? expr, in which
;; case (expression-of expr).

;; (define (literal-number? x)
;;   (and (pair? x)
;;        (eq? (car x) number-type-tag)))

;; this comes from: express.scm

;; (define (expression-of abstract-quantity)
;;   (cond ((pair? abstract-quantity)
;; 	 (let ((v (assq 'expression (cdr abstract-quantity))))
;; 	   (if v
;; 	       (cadr v)
;; 	       (error "No expression for abstract quantity"
;; 		      abstract-quantity))))
;; 	((symbol? abstract-quantity)
;; 	 abstract-quantity)
;; 	(else
;; 	 (error "Bad abstract quantity"))))






