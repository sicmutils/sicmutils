(ns math.numsymb
  (:require [math.generic :as g]
            [math.expression :as x]))

;; N.B.: (define numerical-expression-canonicalizer #f)
;;       (define heuristic-number-canonicalizer #f)
;;       (define incremental-simplifier #f)
(declare symbolic-operator-table)
(defn- numerical-expression [expr] expr)

(defn make-numsymb-expression [operator operands]
  (let [operand-exprs (map numerical-expression operands)
        v (operator symbolic-operator-table)]
    (if v
      (let [newexp (apply v operand-exprs)]
        (x/make-literal :number newexp)))))

(defn- sum? [x]
  (and (seq? x) (= (first x) (symbol "math.generic" "add"))))

(defn- operands [x]
  (rest x))

;; this is without constructor simplifications!

(defn- add [a b]
  (cond (and (number? a) (number? b)) (+ a b)
        (number? a) (cond (zero? a) b
                          (sum? b) `(g/add ~a ~@(operands b))
                          :else `(g/add ~a ~b))
        (number? b) (cond (zero? b) a
                          (sum? a) `(g/add ,b ~@(operands a))
                          :else `(g/add ~b, ~a))
        (sum? a) (cond (sum? b) `(g/add ~@(operands a) ~@(operands b))
                       :else `(g/add ~@(operands a) ~b))
        (sum? b) `(g/add ~a ~@(operands b))
        :else `(g/add ~a ~b)))

(defn add-n [& args]
  (reduce add 0 args))

(def ^:private symbolic-operator-table {:+ add-n})

;; (define (sum? x)
;;   (and (pair? x) (eq? (car x) '+)))

;; (define (symb:addends expr) (cdr expr))

;; (define (symb:+ a1 a2)
;;   (cond ((and (number? a1) (number? a2)) (+ a1 a2))
;;         ((number? a1)
;; 	 (cond ((zero? a1) a2)
;; 	       ((sum? a2)
;; 		`(+ ,a1 ,@(operands a2)))
;; 	       (else `(+ ,a1 ,a2))))
;;         ((number? a2)
;; 	 (cond ((zero? a2) a1)
;; 	       ((sum? a1)
;; 		`(+ ,a2 ,@(operands a1)))
;; 	       (else `(+ ,a2 ,a1))))
;; 	((sum? a1)
;; 	 (cond ((sum? a2)
;; 		`(+ ,@(operands a1) ,@(operands a2)))
;; 	       (else `(+ ,@(operands a1) ,a2))))
;; 	((sum? a2)
;; 	 `(+ ,a1 ,@(operands a2)))
;;         (else `(+ ,a1 ,a2))))

;; (define (symb:add x y)
;;   (if enable-constructor-simplifications?
;;       (symb1:+ x y)
;;       (symb:+ x y)))

;; (define (symb:add:n args)
;;   (cond ((null? args) :zero)
;; 	((null? (cdr args)) (car args))
;; 	(else
;; 	 (let lp ((args (cddr args))
;; 		  (ans (symb:add (car args) (cadr args))))
;; 	   (if (null? args)
;; 	       ans
;; 	       (lp (cdr args)
;; 		   (symb:add ans (car args))))))))

;; (define (symb:sum . args)
;;   (symb:add:n args))
    
;; (addto-symbolic-operator-table '+ symb:sum)

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






