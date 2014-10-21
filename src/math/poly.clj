(ns math.poly
  (:refer-clojure :rename {zero? core-zero?})
  (:require [clojure.math.numeric-tower :as nt]
            [clojure.set :as set]
            [math.expression :as x]
            [math.generic :as g]
            ))

;; Hmm. I sort of think this should become a deftype. Doing so might help
;; the arithmetic become genericized in the event that were ever useful.

(declare operator-table operators-known)

;; format in scheme:
;; (*dense* arity termlist)

;; bit of tension here: why does this library not participate in generic
;; arithmetic? Wouldn't it be fun to have polynomials with coefficients
;; from exotic rings? Or does this conflict with the goal to use this
;; canonical form for simplification?

;; XXX s.b. (not (g/zero? c)) ??

(defn- make-with-arity [a & oc-pairs]
  (let [ocs (into (sorted-map) (filter (fn [[o c]] (not= c 0)) oc-pairs))]
    (if (empty? ocs) 0
        (with-meta ocs {:generic-type :poly :arity a}))))

(defn- make-sparse
  "Create a polynomial specifying the terms in sparse form: supplying
  pairs of [order, coefficient]. For example, x^2 - 1 can be
  constructed by (make-sparse [2 1] [0 -1]). The order of the pairs
  doesn't matter."
  [& oc-pairs]
  (apply make-with-arity 1 oc-pairs))

(defn make
  "Create a polynomial specifying the terms in dense form, supplying
  the coefficients of the terms starting with the constant term and
  proceeding as far as needed. For example, x^2 - 1 can be constructed
  by (make -1 0 1). The order of the coefficients corresponds to the
  order of the terms, and zeros must be filled in to get to higher
  powers."
  [& coefs]
  (apply make-sparse (map vector (iterate inc 0) coefs)))

;; should we rely on the constructors and manipulators never to allow
;; a zero coefficient into the list, or should we change degree to
;; scan for nonzero coefficients? In the normal case, there would be
;; none, but in corner cases it would still be robust.

(defn make-identity
  "Produce the identity polynomial of the given arity."
  [arity]
  (make-with-arity arity [1 1]))

(def ^:private poly-identity
  "The univariate identity polynomial p(x) = x"
  (make-identity 1))

(defn- poly-extend
  "Interpolates a variable at position n in polynomial p."
  [n p]
  nil)

(defn- zero? [p]
  (and (number? p) (core-zero? p)))

(defn- one? [p]
  (and (number? p) (= 1 p)))

(defn degree [p]
  (cond (zero? p) -1
        (number? p) 0
        :else (first (first (rseq p)))))

;; ARITY

(defn- arity [p]
  (if (number? p)
    0
    (-> p meta :arity)))

(defn- check-same-arity [p q]
  (let [ap (arity p)
        aq (arity q)]
    (cond (number? p) aq
          (number? q) ap
          (= ap aq) ap
          :else (throw (IllegalArgumentException. "mismatched polynomial arity")))))

(defn- normalize-with-arity [a p]
  (if (number? p) p
      (let [fp (->> p (filter #(not (zero? (second %)))) (into (sorted-map)))]
        (if-let [[order coef] (first fp)]
         (if (and (= (count p) 1) (= order 0)) coef
             (with-meta fp {:generic-type :poly :arity a}))
         0))))

(defn- poly-map [f p]
  (normalize-with-arity (arity p) (into (empty p) (map #(vector (first %) (f (second %))) p))))

(defn- poly-merge [f p q]
  (loop [P p
         Q q
         R (sorted-map)]
    (cond
     (empty? P) (into R Q)
     (empty? Q) (into R P)
     :else (let [[op cp] (first P)
                 [oq cq] (first Q)]
             (cond
              (= op oq) (let [v (f cp cq)]
                          (recur (rest P) (rest Q)
                                 (if (not= v 0)
                                   (assoc R op v)
                                   R)))
              (< op oq) (recur (rest P) Q (assoc R op (f cp)))
              :else (recur P (rest Q) (assoc R oq (f cq))))))))

;; (define (poly/make-vars arity)
;;   (if (fix:= arity 0)
;;     '()
;;     (let lp1 ((n 1) (l (list poly/identity)))
;;          (if (fix:= n arity)
;;            l
;;            (lp1 (fix:+ n 1)
;;                 (cons (poly/make-identity (fix:+ n 1))
;;                       (map (lambda (c)
;;                                    (poly/extend 0 c))
;;                            l)))))))

(defn make-vars [arity]
  (if (= arity 0) ()
      (loop [n 1 l (list poly-identity)]
        (if (= n arity) l
            (recur (inc n)
                   (cons (make-identity (inc n))
                         (map (fn [c] (poly-extend 0 c)) l)))))))

(def ^:private negate (partial poly-map -))

(defn- add-constant [poly c]
  (if (number? poly) (+ poly c)
      (normalize-with-arity (arity poly)
                            (assoc poly 0 (+ (get poly 0 0) c)))))

(defn add [p q]
  (cond (and (number? p) (number? q)) (+ p q)
        (zero? p) q
        (zero? q) p
        (number? p) (add-constant q p)
        (number? q) (add-constant p q)
        :else (let [a (check-same-arity p q)
                    sum (poly-merge g/+ p q)]
                (normalize-with-arity a sum))))

(defn- add-denormal
  "Add-denormal adds the (order, coefficient) pair to the polynomial p,
  expecting that p is currently in sparse form (i.e., not a primitive number)
  and without normalizing the result (e.g., to see if the polynomial has
  become constant or a term has dropped out). Useful in intermediate steps
  of polynomial computations."
  [p [o c]]

  (assoc p o (+ (get p o 0) c)))

(defn sub [p q]
  (cond (and (number? p) (number? q)) (- p q)
        (zero? p) (negate q)
        (zero? q) p
        (number? p) (add-constant (negate q) p)
        (number? q) (add-constant p q)
        :else (let [a (check-same-arity p q)
                    diff (poly-merge g/- p q)]
                (normalize-with-arity a diff))))

(defn mul [p q]
  (cond (and (number? p) (number? q)) (* p q)
        (zero? p) 0
        (zero? q) 0
        (one? p) q
        (one? q) p
        (number? p) (poly-map #(* p %) q)
        (number? q) (poly-map #(* % q) p)
        :else (let [a (check-same-arity p q)]
                (normalize-with-arity a (reduce add-denormal (sorted-map)
                                                (for [[op cp] p [oq cq] q]
                                                  [(g/+ op oq) (g/* cp cq)]))))))

(defn- square [p]
  (mul p p))

(defn expt [p n]
  (cond (number? p) (nt/expt p n)
        (or
         (not (integer? n))
         (< n 0)) (throw (IllegalArgumentException. (str "can't raise poly to " n)))
        (one? p) p
        (zero? p) (if (zero? n)
                    (throw (IllegalArgumentException. "poly 0^0"))
                    p)
        (zero? n) 1
        :else (loop [x p c n a 1]
                (if (zero? c) a
                    (if (even? c)
                      (recur (square x) (quot c 2) a)
                      (recur x (dec c) (mul x a)))))))

(defn expression->
  [expr cont]
  (let [expression-vars (set/difference (x/variables-in expr) operators-known)
        new-bindings (into {} (map vector
                                   expression-vars
                                   (make-vars (count expression-vars))))
        environment (into operator-table new-bindings)]
    (cont (x/walk-expression environment expr) expression-vars)))

(def ^:private operator-table
  {'math.generic/+ add
   'math.generic/- sub
   'math.generic/* mul
   'math.generic/negate negate
   'math.generic/expt expt
   'math.generic/square square
   ;g/gcd gcd
   })

(def ^:private operators-known (into #{} (keys operator-table)))
