(ns math.numsymb
  (:require [math.generic :as g]
            [math.expression :as x]))

;; N.B.: (define numerical-expression-canonicalizer #f)
;;       (define heuristic-number-canonicalizer #f)
;;       (define incremental-simplifier #f)

(declare symbolic-operator-table)
(defn- numerical-expression [expr] expr)

;; XXX use if-let here
(defn make-numsymb-expression [operator operands]
  (let [operand-exprs (map numerical-expression operands)
        v (operator symbolic-operator-table)]
    (if v
      (let [newexp (apply v operand-exprs)]
        (x/make-numeric-literal newexp))
      (throw (IllegalArgumentException.
              (str "unknown numeric operator " operator))))))

(defn- sum? [x]
  (and (seq? x) (= (first x) (symbol "math.generic" "+"))))

(defn- product? [x]
  (and (seq? x) (= (first x) (symbol "math.generic" "*"))))

(defn- operands [x]
  (rest x))

;; BEGIN
;; these are without constructor simplifications!

(defn- add [a b]
  (cond (and (number? a) (number? b)) (+ a b)
        (number? a) (cond (zero? a) b
                          (sum? b) `(g/+ ~a ~@(operands b))
                          :else `(g/+ ~a ~b))
        (number? b) (cond (zero? b) a
                          (sum? a) `(g/+ ,b ~@(operands a))
                          :else `(g/+ ~b, ~a))
        (sum? a) (cond (sum? b) `(g/+ ~@(operands a) ~@(operands b))
                       :else `(g/+ ~@(operands a) ~b))
        (sum? b) `(g/+ ~a ~@(operands b))
        :else `(g/+ ~a ~b)))

(defn add-n [& args]
  (reduce add 0 args))

(defn- sub [a b]
  (cond (and (number? a) (number? b)) (- a b)
        (number? a) (if (zero? a) `(g/- ~b) `(g/- ~a ~b))
        (number? b) (if (zero? b) a `(g/- ~a ~b))
        :else `(- ~a ~b)))

(defn sub-n [& args]
  (cond (empty? args) 0
        (empty? (rest args)) (sub 0 (first args))
        :else (sub (first args) (add-n (rest args)))))

(defn- mul [a b]
  (cond (and (number? a) (number? b)) (* a b)
        (number? a) (cond (zero? a) a
                          (g/id*? a) b
                          (product? b) `(g/* ~a ~@(operands b))
                          :else `(g/* ~a ~b))
        (number? b) (cond (zero? b) b
                          (g/id*? b) a
                          (product? a) `(g/* ~b ~@(operands a))
                          :else `(g/* ~b ~a))
        (product? a) (cond (product? b) `(g/* ~@(operands a) ~@(operands b))
                           :else `(g/* ~@(operands a) ~b))
        (product? b) `(g/* ~a ~@(operands b))
        :else `(g/* ~a ~b)))

(defn- mul-n [& args]
  (reduce mul 1 args))

(defn- div [a b]
  (cond (and (number? a) (number? b)) (/ a b)
        (number? a) (if (zero? a) a `(g// ~a ~b))
        (number? b) (cond (zero? b) (throw (IllegalArgumentException.
                                            "division by zero"))
                          (g/id*? b) a
                          :else `(g// ~a ~b))
        :else `(g// ~a ~b)))

(defn div-n [& args]
  (cond (empty? args) 1
        (empty? (rest args)) (div 1 (first args))
        :else (div (first args) (apply mul-n (rest args)))))


;; END

(defn- addup-args-notfinished [pos neg]
  (defn make-answer [sum pos neg]
    (if (zero? sum)
      (if (empty? pos)
        (if (empty? neg)
          0
          (if (empty? (rest neg))
            `(g/sub ;; this isn't finished!!!
              )))))))
;;
;; TRIG
;;

(def ^:private machine-epsilon
  (loop [e 1.0]
    (if (not= 1.0 (+ 1.0 (/ e 2.0)))
      (recur (/ e 2.0))
      e)))

(def ^:private relative-integer-tolerance (* 100 machine-epsilon))
(def ^:private absolute-integer-tolerance 1e-20)

(defn almost-integer? [x] ;; XXX make this private
  (or (integer? x)
      (and (float? x)
           (let [z (Math/round x)]
             (if (zero? z)
               (< (Math/abs x) absolute-integer-tolerance)
               (< (Math/abs (/ (- x z) z)) relative-integer-tolerance))))))

;; (define (n:zero-mod-pi? x) (almost-integer? (/ x n:pi)))
;; (define (symb:zero-mod-pi? x) (memq x '(:-pi :pi :+pi :-2pi :2pi)))

;; (define (n:pi/2-mod-2pi? x) (almost-integer? (/ (- x n:pi/2) n:2pi)))
;; (define (symb:pi/2-mod-2pi? x) (memq x '(:pi/2 :+pi/2)))

;; (define (n:-pi/2-mod-2pi? x) (almost-integer? (/ (+ x n:pi/2) n:2pi)))
;; (define (symb:-pi/2-mod-2pi? x) (memq x '(:-pi/2)))

;; (define (n:pi/2-mod-pi? x) (almost-integer? (/ (- x n:pi/2) n:pi)))
;; (define (symb:pi/2-mod-pi? x) (memq x '(:-pi/2 :pi/2 :+pi/2)))

;; (define (n:zero-mod-2pi? x) (almost-integer? (/ x n:2pi)))
;; (define (symb:zero-mod-2pi? x) (memq x '(:-2pi :2pi :+2pi)))

;; (define (n:pi-mod-2pi? x) (almost-integer? (/ (- x n:pi) n:2pi)))
;; (define (symb:pi-mod-2pi? x) (memq x '(:-pi :pi :+pi)))

;; (define (n:pi/4-mod-pi? x) (almost-integer? (/ (- x n:pi/4) n:pi)))
;; (define (symb:pi/4-mod-pi? x) (memq x '(:pi/4 :+pi/4)))

;; (define (n:-pi/4-mod-pi? x) (almost-integer? (/ (+ x n:pi/4) :pi)))
;; (define (symb:-pi/4-mod-pi? x) (memq x '(:-pi/4)))


;; (define (symb:sin x)
;;   (cond ((number? x)
;;       	 (if (exact? x)
;; 	     (if (zero? x) 0 `(sin ,x))
;; 	     (cond ((n:zero-mod-pi? x) 0.)
;; 		   ((n:pi/2-mod-2pi? x) +1.)
;; 		   ((n:-pi/2-mod-2pi? x) -1.)
;; 		   (else (sin x)))))
;; 	((symbol? x)
;; 	 (cond ((symb:zero-mod-pi? x) 0)
;; 	       ((symb:pi/2-mod-2pi? x) +1)
;; 	       ((symb:-pi/2-mod-2pi? x) -1)
;; 	       (else `(sin ,x))))
;; 	(else `(sin ,x))))

(defn- exact? [x] (not (float? x)))
;; are there any other non-exact native types in clojure?

(defn- sine [x]
  (cond (number? x) (if (exact? x)
                      (if (zero? x) 0 `(g/sin ~x)) 
                      ;; this is where we would detect combinations of
                      ;; π.
                      (Math/sin x)
                      )
        (symbol? x) `(g/sin ~x) ;; also would look for π here. 
        :else `(g/sin ~x)))

(def ^:private symbolic-operator-table {:+ add-n
                                        :- sub-n
                                        :* mul-n
                                        :negate (fn [x] (sub 0 x))
                                        :/ div-n
                                        :sin sine})

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






