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
        (number? a) (cond (g/zero? a) a
                          (g/one? a) b
                          (product? b) `(g/* ~a ~@(operands b))
                          :else `(g/* ~a ~b))
        (number? b) (cond (g/zero? b) b
                          (g/one? b) a
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
                          (g/one? b) a
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

(def ^:private pi Math/PI)
(def ^:private pi-over-4 (/ pi 4))
(def ^:private two-pi (* 2 pi))
(def ^:private pi-over-2 (* 2 pi-over-4))
(def ^:private pi-over-3 (/ pi 3))
(def ^:private pi-over-6 (/ pi-over-2 3))

(defn- n:zero-mod-pi? [x]
  (almost-integer? (/ x pi)))
(defn- symb:zero-mod-pi? [s]
  (#{'-pi 'pi '+pi '-two-pi 'two-pi} s))
(defn- n:pi-over-2-mod-2pi? [x]
  (almost-integer? (/ (- x pi-over-2 two-pi))))
(defn- symb:pi-over-2-mod-2pi? [s]
  (#{'pi-over-2 '+pi-over-2} s))
(defn- n:-pi-over-2-mod-2pi? [x]
  (almost-integer? (/ (+ x pi-over-2) two-pi)))
(defn- symb:-pi-over-2-mod-2pi? [s]
  (#{'-pi-over-2} s))

;; (define (n:pi-over-2-mod-2pi? x) (almost-integer? (/ (- x n:pi-over-2) n:2pi)
;; (define (n:pi-over-2-mod-pi? x) (almost-integer? (/ (- x n:pi-over-2) n:pi)))
;; (define (symb:pi-over-2-mod-pi? x) (memq x '(:-pi-over-2 :pi-over-2 :+pi-over-2)))

;; (define (n:zero-mod-2pi? x) (almost-integer? (/ x n:2pi)))
;; (define (symb:zero-mod-2pi? x) (memq x '(:-2pi :2pi :+2pi)))

;; (define (n:pi-mod-2pi? x) (almost-integer? (/ (- x n:pi) n:2pi)))
;; (define (symb:pi-mod-2pi? x) (memq x '(:-pi :pi :+pi)))

;; (define (n:pi-over-4-mod-pi? x) (almost-integer? (/ (- x n:pi-over-4) n:pi)))
;; (define (symb:pi-over-4-mod-pi? x) (memq x '(:pi-over-4 :+pi-over-4)))

;; (define (n:-pi-over-4-mod-pi? x) (almost-integer? (/ (+ x n:pi-over-4) :pi)))
;; (define (symb:-pi-over-4-mod-pi? x) (memq x '(:-pi-over-4)))



(defn- exact? [x] (not (float? x)))
;; are there any other non-exact native types in clojure?

(defn- sine [x]
  (cond (number? x) (if (exact? x)
                      (if (zero? x) 0 `(g/sin ~x))
                      (cond (n:zero-mod-pi? x) 0.0
                            (n:pi-over-2-mod-2pi? x) 1.0
                            (n:-pi-over-2-mod-2pi? x) -1.0
                            :else (Math/sin x)))
        (symbol? x) (cond (symb:zero-mod-pi? x) 0
                          (symb:pi-over-2-mod-2pi? x) 1
                          (symb:-pi-over-2-mod-2pi? x) -1
                          :else `(g/sin ~x))
        :else `(g/sin ~x)))

(def ^:private symbolic-operator-table {:+ add-n
                                        :- sub-n
                                        :* mul-n
                                        :negate (fn [x] (sub 0 x))
                                        :/ div-n
                                        :sin sine})

;; (define (numerical-expression expr)
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






