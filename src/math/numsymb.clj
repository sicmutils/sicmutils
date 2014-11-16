(ns math.numsymb
  (:require [math.generic :as g]
            [math.expression :as x]
            [clojure.math.numeric-tower :as nt]))

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
        (x/make-numeric-literal newexp))
      (throw (IllegalArgumentException.
              (str "unknown numeric operator " operator))))))

(defmacro is-expression?
  "True if the expression is a form with head corresponding to
  the math.generic operator with the given name."
  [name]
  `(fn [x#] (and (seq? x#) (= (first x#) (symbol "math.generic" ~(str name))))))

(def ^:private sum? (is-expression? +))
(def ^:private product? (is-expression? *))
(def ^:private sqrt? (is-expression? sqrt))
(def ^:private expt? (is-expression? expt))

(defn- operands [x]
  (rest x))

(defn- canonically-ordered-operation
  [operator operands]
  (cons operator (g/canonical-order operands))
  )

;; BEGIN
;; these are without constructor simplifications!

(defn- add [a b]
  (let [sum (partial canonically-ordered-operation `g/+)]
    (cond (and (number? a) (number? b)) (+ a b)
         (number? a) (cond (g/zero? a) b
                           (sum? b) (sum (cons a (operands b)))
                           :else (sum (list a b)))
         (number? b) (cond (g/zero? b) a
                           (sum? a) (sum (cons b (operands a)))
                           :else (sum (list b a)))
         (sum? a) (cond (sum? b) (sum (concat (operands a) (operands b)))
                        :else (sum (cons b (operands a))))
         (sum? b) (sum (cons a (operands b)))
         :else (sum (list a b)))))

(defn- add-n [& args]
  (reduce add 0 args))

(defn- sub [a b]
  (cond (and (number? a) (number? b)) (- a b)
        (number? a) (if (g/zero? a) `(g/- ~b) `(g/- ~a ~b))
        (number? b) (if (g/zero? b) a `(g/- ~a ~b))
        :else `(g/- ~a ~b)))

(defn- sub-n [& args]
  (cond (nil? args) 0
        (nil? (next args)) (g/negate (first args))
        :else (sub (first args) (apply add-n (next args)))))

(defn- mul [a b]
  (let [product (partial canonically-ordered-operation `g/*)]
    (cond (and (number? a) (number? b)) (* a b)
         (number? a) (cond (g/zero? a) a
                           (g/one? a) b
                           (product? b) (product (cons a (operands b)))
                           :else (product (list a b));
                           )
         (number? b) (cond (g/zero? b) b
                           (g/one? b) a
                           (product? a) (product (cons b (operands a))) ;`(g/* ~b ~@(operands a))
                           :else (product (list b a))
                           )
         (product? a) (cond (product? b) (product (concat (operands a) (operands b)))
                            :else (product (cons b (operands a)))); `(g/* ~@(operands a) ~b))
         (product? b) (product (cons a (operands b)))
         :else (product (list a b))
         )))

(defn- mul-n [& args]
  (reduce mul 1 args))

(defn- div [a b]
  (cond (and (number? a) (number? b)) (/ a b)
        (number? a) (if (g/zero? a) a `(g// ~a ~b))
        (number? b) (cond (g/zero? b) (throw (ArithmeticException. "division by zero"))
                          (g/one? b) a
                          :else `(g// ~a ~b))
        :else `(g// ~a ~b)))

(defn- div-n [& args]
  (cond (nil? args) 1
        (nil? (next args)) (div 1 (first args))
        :else (div (first args) (apply mul-n (next args)))))

;; END

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

(defn- almost-integer? [x] ;; XXX make this private
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
;; (def ^:private pi-over-3 (/ pi 3))
;; (def ^:private pi-over-6 (/ pi-over-2 3))

(defn- n:zero-mod-pi? [x]
  (almost-integer? (/ x pi)))
(defn- symb:zero-mod-pi? [s]
  (#{'-pi 'pi '-two-pi 'two-pi} s))
(defn- n:pi-over-2-mod-2pi? [x]
  (almost-integer? (/ (- x pi-over-2 two-pi))))
(defn- symb:pi-over-2-mod-2pi? [s]
  (#{'pi-over-2} s))
(defn- n:-pi-over-2-mod-2pi? [x]
  (almost-integer? (/ (+ x pi-over-2) two-pi)))
(defn- symb:-pi-over-2-mod-2pi? [s]
  (#{'-pi-over-2} s))
(defn- n:pi-mod-2pi? [x]
  (almost-integer? (/ (- x pi) two-pi)))
(defn- symb:pi-mod-2pi? [s]
  (#{'-pi 'pi} s))
(defn- n:pi-over-2-mod-pi? [x]
  (almost-integer? (/ (- x pi-over-2) pi)))
(defn- symb:pi-over-2-mod-pi? [s]
  (#{'-pi-over-2 'pi-over-2} s))
(defn- n:zero-mod-2pi? [x]
  (almost-integer? (/ x two-pi)))
(defn- symb:zero-mod-2pi? [s]
  (#{'-two-pi 'two-pi} s))
(defn- n:-pi-over-4-mod-pi? [x]
  (almost-integer? (/ (+ x pi-over-4) pi)))
(defn- symb:-pi-over-4-mod-pi? [s]
  (#{'-pi-over-4} s))

;; (define (n:pi-over-4-mod-pi? x) (almost-integer? (/ (- x n:pi-over-4) n:pi)))
;; (define (symb:pi-over-4-mod-pi? x) (memq x '(:pi-over-4 :+pi-over-4)))

(defn- sine [x]
  (cond (number? x) (if (g/exact? x)
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

(defn- cosine [x]
  (cond (number? x) (if (g/exact? x)
                      (if (zero? x) 1 `(g/cos ~x))
                      (cond (n:pi-over-2-mod-pi? x) 0.0
                            (n:zero-mod-2pi? x) 1.0
                            (n:pi-mod-2pi? x) -1.0
                            :else (Math/cos x)))
        (symbol? x) (cond (symb:pi-over-2-mod-pi? x) 0
                          (symb:zero-mod-2pi? x) +1
                          (symb:pi-mod-2pi? x) -1
                          :else `(g/cos ~x))
        :else `(g/cos ~x)))

(defn- cube [x]
  (g/expt x 3))

(defn- square [x]
  (g/* x x))

(defn- abs [x]
  (cond (number? x) (Math/abs x)
        :else `(g/abs ~x)))

(defn- sqrt [s]
  (if (number? s)
    (if-not (g/exact? s)
      (nt/sqrt s)
      (cond (g/zero? s) s
            (g/one? s) :one
            :else (let [q (nt/sqrt s)]
                    (if (g/exact? q)
                      q
                      `(g/sqrt ~s)))))
    `(g/sqrt ~s)))

(defn- log [s]
  (if (number? s)
    (if-not (g/exact? s)
      (Math/log s)
      (if (g/one? s) 0 `(g/log ~s)))
    `(g/log ~s)))

(defn- exp [s]
  (if (number? s)
    (if-not (g/exact? s)
      (Math/exp s)
      (if (g/zero? s) 1 `(g/exp ~s)))
    `(g/exp ~s)))

(defn- expt [b e]
  (cond (and (number? b) (number? e)) (nt/expt b e)
        (number? b) (cond (g/one? b) 1
                          :else `(g/expt ~b ~e))
        (number? e) (cond (g/zero? e) 1
                          (g/one? e) b
                          (and (integer? e) (even? e) (sqrt? b))
                          (expt (first (operands b)) (quot e 2))
                          (and (expt? b)
                               (number? (second (operands b)))
                               (integer? (* (second (operands b)) e)))
                          (expt (first (operands b))
                                (* (second (operands b)) e))
                          (< e 0) (div-n 1 (expt b (- e)))
                          :else `(g/expt ~b ~e))
        :else `(g/expt ~b ~e)
        ))

(def ^:private symbolic-operator-table {:+ add-n
                                        :- sub-n
                                        :* mul-n
                                        :negate #(sub 0 %)
                                        :invert #(div 1 %)
                                        :/ div-n
                                        :sin sine
                                        :cos cosine
                                        :cube cube
                                        :square square
                                        :abs abs
                                        :sqrt sqrt
                                        :log log
                                        :exp exp
                                        :** expt})
