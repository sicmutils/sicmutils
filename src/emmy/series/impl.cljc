#_"SPDX-License-Identifier: GPL-3.0"

(ns ^:no-doc emmy.series.impl
  "Backing implementation for the types defined in [[emmy.series]], written
  against pure Clojure sequences."
  (:require [emmy.generic :as g]
            [emmy.numbers]
            [emmy.special.factorial :as sf]
            [emmy.util :as u]
            [emmy.value :as v]))

;; # Power Series
;;
;; This namespace contains an implementation of two data types:
;;
;; - `Series`, which represents a generic infinite series of arbitrary values, and
;; - `PowerSeries`, a series that represents a power series in a single
;;   variable; in other words, a series where the nth entry is interpreted as
;;   the coefficient of $x^n$:
;;
;; $$[a b c d ...] == $a + bx + cx^2 + dx^3 + ...$$
;;
;; We'll proceed by building up implementations of the arithmetic operations +,
;; -, *, / and a few others on bare Clojure lazy sequences, and then introduce
;; `deftype` wrappers so that we can install these types into the generic
;; dispatch system.
;;
;; The implementation follows Doug McIlroy's beautiful paper, ["Power Series,
;; Power
;; Serious"](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.333.3156&rep=rep1&type=pdf).
;; Doug also has a 10-line version in Haskell on [his
;; website](https://www.cs.dartmouth.edu/~doug/powser.html).
;;
;; Okay, let's proceed, in roughly the same order as the paper.
;;
;; ## Sequence Operations
;;
;; A 'series' is an infinite sequence of numbers, represented by Clojure's lazy
;; sequence. First, a function `->series` that takes some existing sequence,
;; finite or infinite, and coerces it to an infinite seq by concatenating it
;; with an infinite sequence of zeros. (We use `v/zero-like` so that everything
;; plays nicely with generic arithmetic.)

(defn ->series
  "Form the infinite sequence starting with the supplied values. The
  remainder of the series will be filled with the zero-value
  corresponding to the first of the given values."
  [xs]
  (lazy-cat xs (repeat (v/zero-like (first xs)))))

;; This works as expected:

#_
(= [1 2 3 4 0 0 0 0 0 0]
   (take 10 (->series [1 2 3 4])))

;; The core observation we'll use in the following definitions (courtesy of
;; McIlroy) is that a power series $F$ in a variable $x$:
;;
;; $$F(x)=f_{0}+x f_{1}+x^{2} f_{2}+\cdots$$
;;
;; Decomposes into a head element $f_0$ plus a tail series, multiplied by $x$:
;;
;; $$F(x) = F_0(x) = f_0 + x F_1(x)$$
;;
;; We'll use this observation to derive the more complicated sequence operations
;; below.

;; ### Negation

;; To negate a series, negate each element:

(defn negate [xs]
  (map g/negate xs))

#_
(let [xs [1 2 3 4]]
  (= [-1 -2 -3 -4 0 0 0]
     (take 7 (negate (->series xs)))))

;; ### Addition
;;
;; We can derive series addition by expanding the series $F$ and $G$ into head
;; and tail and rearranging terms:
;;
;; $$F+G=\left(f+x F_{1}\right)+\left(g+x G_{1}\right)=(f+g)+x\left(F_{1}+G_{1}\right)$$
;;
;; This is particularly straightforward in Clojure, where `map` already merges
;; sequences elementwise:
(defn seq:+ [f g]
  (map g/+ f g))

#_
(= [0 2 4 6 8]
   (take 5 (seq:+ (range) (range))))

;; A constant is a series with its first element populated, all zeros otherwise.
;; To add a constant to another series we only need add it to the first element.
;; Here are two versions, constant-on-left vs constant-on-right:

(defn c+seq [c f]
  (lazy-seq
   (cons (g/+ c (first f)) (rest f))))

(defn seq+c [f c]
  (lazy-seq
   (cons (g/+ (first f) c) (rest f))))

#_
(let [series (->series [1 2 3 4])]
  (= [11 2 3 4 0 0]
     (take 6 (seq+c series 10))
     (take 6 (c+seq 10 series))))

;; ### Subtraction
;;
;; Subtraction comes for free from the two definitions above:

(defn seq:- [f g]
  (map g/- f g))

#_
(= [0 0 0 0 0]
   (take 5 (seq:- (range) (range))))

;; We /should/ get equivalent results from mapping `g/-` over both sequences,
;; and in almost all cases we do... but until we understand and fix this bug
;; https://github.com/emmy/emmy/issues/151 that method would
;; return different results.

;; Subtract a constant from a sequence by subtracting it from the first element:

(defn seq-c [f c]
  (lazy-seq
   (cons (g/- (first f) c) (rest f))))

#_
(= [-10 1 2 3 4]
   (take 5 (seq-c (range) 10)))

;; To subtract a sequence from a constant, subtract the first element as before,
;; but negate the tail of the sequence:

(defn c-seq [c f]
  (lazy-seq
   (cons (g/- c (first f)) (negate (rest f)))))

#_
(= [10 -1 -2 -3 -4]
   (take 5 (c-seq 10 (range))))

;; ### Multiplication
;;
;; What does it mean to multiply two infinite sequences? As McIlroy notes,
;; multiplication is where the lazy-sequence-based approach really comes into
;; its own.
;;
;; First, the simple cases of multiplication by a scalar on either side of a
;; sequence:

(defn seq*c [f c] (map #(g/mul % c) f))
(defn c*seq [c f] (map #(g/mul c %) f))

;; To multiply sequences, first recall from above that we can decompose each
;; sequence $F$ and $G$ into a head and tail.
;;
;; Mutiply the expanded representations out and rearrange terms:
;;
;; $$F \times G=\left(f+x F_{1}\right) \times\left(g+x G_{1}\right)=f g+x\left(f G_{1}+F_{1} \times G\right)$$
;;
;; $G$ appears on the left and the right, so use an inner function that closes
;; over $g$ to simplify matters, and rewrite the above definition in Clojure:

(defn seq:* [f g]
  (letfn [(step [f]
            (lazy-seq
             (if (v/zero? (first f))
               (cons (first f) (step (rest f)))
               (let [f*g  (g/mul (first f) (first g))
                     f*G1 (c*seq (first f) (rest g))
                     F1*G (step (rest f))]
                 (cons f*g (seq:+ f*G1 F1*G))))))]
    (lazy-seq
     (if (v/zero? (first g))
       (cons (first g) (seq:* f (rest g)))
       (step f)))))

;; This works just fine on two infinite sequences:

#_
(= [0 4 11 20 30 40 50 60 70 80]
   (take 10 (seq:* (range) (->series [4 3 2 1]))))

;; NOTE This is also called the "Cauchy Product" of the two sequences:
;; https://en.wikipedia.org/wiki/Cauchy_product The description on the Wikipedia
;; page has complicated index tracking that simply doesn't come in to play with
;; the stream-based approach. Amazing!

;; ### Division
;;
;; The quotient $Q$ of $F$ and $G$ should satisfy:
;;
;; $$F = Q \times G$$
;;
;; From McIlroy, first expand out $F$, $Q$ and one instance of $G$:
;;
;; $$
;; \begin{aligned}
;; f+x F_{1} &=\left(q+x Q_{1}\right) \times G=q G+x Q_{1} \times G=q\left(g+x G_{1}\right)+x Q_{1} \times G \\
;; &=q g+x\left(q G_{1}+Q_{1} \times G\right)
;; \end{aligned}
;; $$
;;
;; Look at just the constant terms and note that $q = {f \over g}$.
;;
;; Consider the terms multiplied by $x$ and solve for $Q_1$:
;;
;; $$Q_1 = {(F_1 - qG_1) \over G}$$.
;;
;; There are two special cases to consider:
;;
;; - If $g=0$, $q = {f \over g}$ can only succeed if $f=0$; in this case, $Q =
;;   {F_1 \over G1}$, from the larger formula above.
;; - If $f=0$, $Q_1 = {(F_1 - 0 G_1) \over G} = {F_1 \over G}$
;;
;; Encoded in Clojure:

(defn div [f g]
  (lazy-seq
   (let [f0 (first f) fs (rest f)
         g0 (first g) gs (rest g)]
     (cond (and (v/zero? f0) (v/zero? g0))
           (div fs gs)

           (v/zero? f0)
           (cons f0 (div fs g))

           (v/zero? g0)
           (u/arithmetic-ex "ERROR: denominator has a zero constant term")

           :else (let [q (g/div f0 g0)]
                   (cons q (-> (seq:- fs (c*seq q gs))
                               (div g))))))))

;; A simple example shows success:

#_
(let [series (->series [0 0 0 4 3 2 1])]
  (= [1 0 0 0 0]
     (take 5 (div series series))))

;; ### Reciprocal
;;
;; We could generate the reciprocal of $F$ by dividing $(1, 0, 0, ...)$ by $F$.
;; Page 21 of an earlier [paper by
;; McIlroy](https://swtch.com/~rsc/thread/squint.pdf) gives us a more direct
;; formula.
;;
;; We want $R$ such that $FR = 1$. Expand $F$:
;;
;; $$(f + xF_1)R = 1$$
;;
;; Solve for R:
;;
;; $$R = {1 \over f} (1 - x(F_1 R))$$
;;
;; A recursive definition is no problem in the stream abstraction:

(defn invert [f]
  (lazy-seq
   (let [finv    (g/invert (first f))
         F1*Finv (seq:* (rest f) (invert f))
         tail    (c*seq finv (negate F1*Finv))]
     (cons finv tail))))

;; This definition of `invert` matches the more straightforward division
;; implementation:

#_
(let [series (iterate inc 3)]
  (= (take 5 (invert series))
     (take 5 (div (->series [1]) series))))

;; An example:

#_
(let [series (iterate inc 3)]
  (= [1 0 0 0 0]
     (take 5 (seq:* series (invert series)))
     (take 5 (div series series))))

;; Division of a constant by a series comes easily from our previous
;; multiplication definitions and `invert`:

(defn c-div-seq [c f]
  (c*seq c (invert f)))

;; It's not obvious that this works:

#_
(let [nats (iterate inc 1)]
  (= [4 -8 4 0 0 0]
     (take 6 (c-div-seq 4 nats))))

;; But we can recover the initial series:

#_
(let [nats       (iterate inc 1)
      divided    (c-div-seq 4 nats)
      seq-over-4 (invert divided)
      original   (seq*c seq-over-4 4)]
  (= (take 5 nats)
     (take 5 original)))

;; To divide a series by a constant, divide each element of the series:

(defn seq-div-c [f c]
  (map #(g// % c) f))

;; Division by a constant undoes multiplication by a constant:

#_
(let [nats (iterate inc 1)]
  (= [1 2 3 4 5]
     (take 5 (seq-div-c (seq*c nats 2) 2))))

;; ### Functional Composition
;;
;; To compose two series $F(x)$ and $G(x)$ means to create a new series
;; $F(G(x))$. Derive this by substituting $G$ for $x$ in the expansion of $F$:
;;
;; $$F(G)=f+G \times F_{1}(G)=f+\left(g+x G_{1}\right) \times F_{1}(G)=\left(f+g F_{1}(G)\right)+x G_{1} \times F_{1}(G)$$
;;
;; For the stream-based calculation to work, we need to be able to calculate the
;; head element and attach it to an infinite tail; unless $g=0$ above the head
;; element depends on $F_1$, an infinite sequence.
;;
;; If $g=0$ the calculation simplifies:
;;
;; $$F(G)=f + x G_{1} \times F_{1}(G)$$
;;
;; In Clojure, using an inner function that captures $G$:

(defn compose [f g]
  (letfn [(step [f]
            (lazy-seq
             ;; TODO I don't understand why we get a StackOverflow if I move
             ;; this assertion out of the `letfn`.
             (assert (v/zero? (first g)))
             (let [[f0 & fs] f
                   gs (rest g)
                   tail (seq:* gs (step fs))]
               (cons f0 tail))))]
    (step f)))

;; Composing $x^2 = (0, 0, 1, 0, 0, ...)$ should square all $x$s, and give us a
;; sequence of only even powers:
#_
(= [1 0 1 0 1 0 1 0 1 0]
   (take 10 (compose (repeat 1)
                     (->series [0 0 1]))))

;; ### Reversion
;;
;; The functional inverse of a power series $F$ is a series $R$ that satisfies
;; $F(R(x)) = x$.
;;
;; Following McIlroy, we expand $F$ (substituting $R$ for $x$) and one
;; occurrence of $R$:
;;
;; $$F(R(x))=f+R \times F_{1}(R)=f+\left(r+x R_{1}\right) \times F_{1}(R)=x$$
;;
;; Just like in the composition derivation, in the general case the head term
;; depends on an infinite sequence. Set $r=0$ to address this:
;;
;; $$f+x R_{1} \times F_{1}(R)=x$$
;;
;; For this to work, the constant $f$ must be 0 as well, hence
;;
;; $R_1 = {1 \over F_1(R)}$
;;
;; This works as an implementation because $r=0$. $R_1$ is allowed to reference
;; $R$ thanks to the stream-based approach:

(defn revert [f]
  {:pre [(v/zero? (first f))]}
  (letfn [(step [f]
            (lazy-seq
             (let [F1   (rest f)
                   R    (step f)]
               (cons 0 (invert
                        (compose F1 R))))))]
    (step f)))

;; An example, inverting a series starting with 0:

#_
(let [f (cons 0 (iterate inc 1))]
  (= [0 1 0 0 0]
     (take 5 (compose f (revert f)))))

;; ### Series Calculus
;;
;; Derivatives of power series are simple and mechanical:
;;
;; $$D(a x^n)$ = aD(x^n) = a n x^{n-1}$$
;;
;; Implies that all entries shift left by 1, and each new entry gets multiplied
;; by its former index (ie, its new index plus 1).

(defn deriv [f]
  (map g/* (rest f) (iterate inc 1)))

#_
(= [1 2 3 4 5 6] ;; 1 + 2x + 3x^2 + ...
   (take 6 (deriv (repeat 1))))

;; The definite integral $\int_0^{x}F(t)dt$ is similar. To take the
;; anti-derivative of each term, move it to the right by appending a constant
;; term onto the sequence and divide each element by its new position:

(defn integral
  ([s] (integral s 0))
  ([s constant-term]
   (cons constant-term
         (map g/div s (iterate inc 1)))))

;; With a custom constant term:

#_
(= [5 1 1 1 1 1]
   (take 6 (integral (iterate inc 1) 5)))

;; By default, the constant term is 0:

#_
(= [0 1 1 1 1 1]
   (take 6 (integral (iterate inc 1))))

;; ### Exponentiation
;;
;; Exponentiation of a power series by some integer is simply repeated
;; multiplication. The implementation here is more efficient the iterating
;; `seq:*`, and handles negative exponent terms by inverting the original
;; sequence.

(defn expt [s e]
  (letfn [(expt [base pow]
            (loop [n pow
                   y (->series [1])
                   z base]
              (let [t (even? n)
                    n (quot n 2)]
                (cond
                  t (recur n y (seq:* z z))
                  (zero? n) (seq:* z y)
                  :else (recur n (seq:* z y) (seq:* z z))))))]
    (cond (pos? e)  (expt s e)
          (zero? e) (->series [1])
          :else (invert (expt s (g/negate e))))))

;; We can use `expt` to verify that $(1+x)^3$ expands to $1 + 3x + 3x^2 + x^3$:

#_
(= [1 3 3 1 0]
   (take 5 (expt (->series [1 1]) 3)))

;; ### Square Root of a Series
;;
;; The square root of a series $F$ is a series $Q$ such that $Q^2 = F$. We can
;; find this using our calculus methods from above:
;;
;; $$D(F) = 2Q D(Q)$$
;;
;; or
;;
;; D(Q) = {D(F) \over {2Q}}
;;
;; When the head term of $F$ is nonzero, ie, $f != 0$, the head of $Q =
;; \sqrt(F)$ must be $\sqrt(f)$ for the multiplication to work out.
;;
;; Integrate both sides:
;;
;; Q = \sqrt(f) + \int_0^x {D(F) \over {2Q}}
;;
;; One optimization appears if the first two terms of $F$ vanish, ie,
;; $F=x^2F_2$. In this case $Q = 0 + x \sqrt(F_2)$.
;;
;; Here it is in Clojure:

(defn sqrt [[f1 & [f2 & fs] :as f]]
  (if (and (v/zero? f1)
           (v/zero? f2))
    (cons f1 (sqrt fs))
    (let [const (g/sqrt f1)
          step  (fn step [g]
                  (lazy-seq
                   (-> (div (deriv g)
                            (c*seq 2 (step g)))
                       (integral const))))]
      (step f))))

;; And a test that we can recover the naturals:

#_
(let [xs (iterate inc 1)]
  (= [1 2 3 4 5 6]
     (take 6 (seq:* (sqrt xs)
                    (sqrt xs)))))

;; We can maintain precision of the first element is the square of a rational
;; number:

#_
(let [xs (iterate inc 9)]
  (= [9 10 11 12 13 14]
     (take 6 (seq:* (sqrt xs)
                    (sqrt xs)))))

;; We get a correct result if the sequence starts with 0, 0:

#_
(let [xs (concat [0 0] (iterate inc 9))]
  (= [0 0 9 10 11 12]
     (take 6 (seq:* (sqrt xs)
                    (sqrt xs)))))

;; ## Examples

;; Power series computations can encode polynomial computations. Encoding
;; $(1-2x^2)^3$ as a power series returns the correct result:

#_
(= [1 0 -6 0 12 0 -8 0 0 0]
   (take 10 (expt (->series [1 0 -2]) 3)))

;; Encoding $1 \over (1-x)$ returns the power series $1 + x + x^2 + ...$ which
;; sums to that value in its region of convergence:

#_
(= (take 10 (repeat 1))
   (take 10 (div (->series [1])
                 (->series [1 -1]))))

;; $1 \over (1-x)^2$ is the derivative of the above series:

#_
(= (take 10 (iterate inc 1))
   (take 10 (div (->series [1])
                 (-> (->series [1 -1])
                     (expt 2)))))

;; ## Various Power Series
;;
;; With the above primitives we can define a number of series with somewhat
;; astonishing brevity.
;;
;; $e^x$ is its own derivative, so $e^x = 1 + e^x$:

(def expx
  (lazy-seq
   (integral expx 1)))

;; This bare definition is enough to generate the power series for $e^x$:

#_
(= '(1
     1
     (/ 1 2)
     (/ 1 6)
     (/ 1 24)
     (/ 1 120)
     (/ 1 720)
     (/ 1 5040)
     (/ 1 40320)
     (/ 1 362880))
   (v/freeze (take 10 expx)))

;; $sin$ and $cos$ afford recursive definitions. $D(sin) = cos$ and $D(cos) =
;; -sin$, so (with appropriate constant terms added) on:

(declare cosx)
(def sinx (lazy-seq (integral cosx)))
(def cosx (lazy-seq (c-seq 1 (integral sinx))))

#_
(= '(0
     1
     0
     (/ -1 6)
     0
     (/ 1 120)
     0
     (/ -1 5040)
     0
     (/ 1 362880))
   (v/freeze (take 10 sinx)))

#_
(= '(1
     0
     (/ -1 2)
     0
     (/ 1 24)
     0
     (/ -1 720)
     0
     (/ 1 40320)
     0)
   (v/freeze (take 10 cosx)))

;; tangent and secant come easily from these:

(def tanx (div sinx cosx))
(def secx (invert cosx))

;; Reversion lets us define arcsine from sine:

(def asinx (revert sinx))
(def atanx (integral (cycle [1 0 -1 0])))

;; These two are less elegant, perhaps:

(def acosx (c-seq (/ Math/PI 2) asinx))
(def acotx (c-seq (/ Math/PI 2) atanx))

;; The hyperbolic trig functions are defined in a similar way:

(declare sinhx)
(def coshx (lazy-seq (integral sinhx 1)))
(def sinhx (lazy-seq (integral coshx)))
(def tanhx (div sinhx coshx))
(def asinhx (revert sinhx))
(def atanhx (revert tanhx))

(def log1-x
  (integral (repeat -1)))

;; https://en.wikipedia.org/wiki/Mercator_series
(def log1+x
  (integral (cycle [1 -1])))

;; ## Generating Functions

;; ### Catalan numbers
;;
;; These are a few more examples from McIlroy's "Power Serious" paper, presented
;; here without context. (If you have the energy to write about these, please
;; feel free and send us a PR!)

(def catalan
  (lazy-cat [1] (seq:* catalan catalan)))

#_
(= [1 1 2 5 14 42 132 429 1430 4862]
   (take 10 catalan))

;; ordered trees...

(declare tree' forest' list')
(def tree' (lazy-cat [0] forest'))
(def list' (lazy-cat [1] list'))
(def forest' (compose list' tree'))

#_
(= [0 1 1 2 5 14 42 132 429 1430]
   (take 10 tree'))

;; The catalan numbers again!

(def fib (lazy-cat [0 1] (map + fib (rest fib))))

;; See here for the recurrence relation:
;; https://en.wikipedia.org/wiki/Binomial_coefficient#Multiplicative_formula

(defn binomial* [n]
  (letfn [(f [acc prev n k]
            (if (zero? n)
              acc
              (let [next (/ (* prev n) k)
                    acc' (conj! acc next)]
                (recur acc' next (dec n) (inc k)))))]
    (persistent!
     (f (transient [1]) 1 n 1))))

(defn binomial
  "The coefficients of (1+x)^n"
  [n]
  (->series (binomial* n)))

;;

(def ^{:doc "The sequence of [Harmonic
  numbers](https://en.wikipedia.org/wiki/Harmonic_number), starting from n=1."}
  harmonic
  (reductions
   g/+ (map g// (iterate inc 1))))

(def ^{:doc "The sequence of [Bell
  numbers](https://en.wikipedia.org/wiki/Bell_number), starting from n=1."} bell
  (map sf/bell (iterate inc 1)))
