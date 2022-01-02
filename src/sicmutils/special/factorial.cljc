(ns sicmutils.special.factorial
  "Namespace holding implementations of variations on the factorial function."
  (:require [sicmutils.generic :as g]
            [sicmutils.numbers]
            [sicmutils.util :as u]
            [sicmutils.util.def :refer [defgeneric]
             #?@(:cljs [:include-macros true])]
            [sicmutils.value :as v]))

;; TODO factorial should be symbolic too, and render as !... make a ticket!

(defn factorial
  "Returns the factorial of `n`, ie, the product of 1 to `n` (inclusive).

  [[factorial]] will return a platform-specific [[sicmutils.util/bigint]] given
  some `n` that causes integer overflow."
  [n]
  {:pre [(v/native-integral? n)
         (>= n 0)]}
  (let [elems (range 1 (inc n))]
    #?(:clj
       (apply *' elems)
       :cljs
       (if (<= n 20)
         (apply * elems)
         (transduce (map u/bigint) g/* elems)))))

;; ## Falling and Rising Factorials
;;
;; TODO fill these in https://en.wikipedia.org/wiki/Falling_and_rising_factorials

;; NOTE from wiki: The rising and falling factorials are well defined in any
;; unital ring, and therefore x can be taken to be, for example, a complex
;; number, including negative integers, or a polynomial with complex
;; coefficients, or any complex-valued function.

(declare rising-factorial)

#_(comment
    ;; here is another impl for negative that we can test against.
    (g/invert
     (transduce (comp
                 (map #(g/add x (inc %)))
                 #?(:cljs (map u/bigint)))
                g/*
                (range (- n)))))

;; TODO coefficients of expansions are stirling numbers of the first kind, see
;; https://en.wikipedia.org/wiki/Falling_and_rising_factorials#cite_note-10

;; Cool, this works!
;;
;; TODO see https://proofwiki.org/wiki/Properties_of_Falling_Factorial tests for
;; falling

(defgeneric falling-factorial 2
  "Falling factorial docstring.")

(def ^{:doc "Alias for [[falling-factorial]]."}
  factorial-power
  falling-factorial)

(defmethod falling-factorial :default [x n]
  {:pre [(v/native-integral? n)]}
  (cond (zero? n) 1
        (neg? n)
        (let [denom (rising-factorial (g/add x 1) (- n))]
          (if (v/zero? denom)
            ##Inf
            (g/invert denom)))

        :else
        (transduce (comp
                    (map #(g/add x (- %)))
                    #?(:cljs (map u/bigint)))
                   g/*
                   (range n))))

(defmethod falling-factorial [::v/native-integral ::v/native-integral] [x n]
  (cond (zero? n) 1
        (neg? n)
        (let [denom (rising-factorial (inc x) (- n))]
          (if (zero? denom)
            ##Inf
            (/ 1 denom)))

        :else
        (let [elems (range x (- x n) -1)]
          #?(:clj
             (apply *' elems)
             :cljs
             (transduce (map u/bigint) * elems)))))

(defgeneric rising-factorial 2
  "Rising factorial docstring.")

(def ^{:doc "Alias for [[falling-factorial]]."}
  pochhammer
  rising-factorial)

(defmethod rising-factorial :default [x n]
  {:pre [(v/native-integral? n)]}
  (cond (zero? n) 1
        (neg? n)
        (let [denom (falling-factorial (g/sub x 1) (- n))]
          (if (v/zero? denom)
            ##Inf
            (g/invert denom)))

        :else
        (transduce (comp
                    (map #(g/add x %))
                    #?(:cljs (map u/bigint)))
                   g/*
                   (range n))))

(defmethod rising-factorial [::v/native-integral ::v/native-integral] [x n]
  (cond (zero? n) 1
        (neg? n)
        (let [denom (falling-factorial (dec x) (- n))]
          (if (zero? denom)
            ##Inf
            (/ 1 denom)))

        :else
        (let [elems (range x (+ x n))]
          #?(:clj
             (apply *' elems)
             :cljs
             (transduce (map u/bigint) * elems)))))

;; https://www.johndcook.com/blog/2010/09/21/variations-on-factorial/

;; https://www.johndcook.com/blog/2021/10/14/multifactorial/

;; https://en.wikipedia.org/wiki/Double_factorial#Generalizations

(defn multi-factorial
  [n k]
  ;; TODO double checkw with wolfram alpha, what are the conditions?
  ;;
  ;; TODO thanks for the implementation here!
  ;; https://www.johndcook.com/blog/2021/10/14/multifactorial/
  {:pre [(v/native-integral? n)
         (v/native-integral? k)
         (>= n 0), (> k 0)]}
  (let [elems (range n 0 (- k))]
    #?(:clj
       (reduce *' elems)
       :cljs
       (transduce (map u/bigint) g/* elems))))

;; double factorial notes:
;; https://www.johndcook.com/blog/2010/09/21/variations-on-factorial/

(defn double-factorial
  "Why did we define this separately? Note that this worls for negative arguments!
  https://en.wikipedia.org/wiki/Double_factorial#Negative_arguments"
  [n]
  {:pre [(v/native-integral? n)]}
  (cond (zero? n) 1
        (pos? n)  (multi-factorial n 2)
        (even? n) ##Inf
        :else (g/div
               (double-factorial (+ n 2))
               (+ n 2))))

(defn subfactorial
  "https://mathworld.wolfram.com/Subfactorial.html

   https://www.johndcook.com/blog/2010/09/21/variations-on-factorial/

  get the number of derangements of size n
  https://en.wikipedia.org/wiki/Derangement

  More details: https://www.johndcook.com/blog/2010/04/06/subfactorial/"
  [n]
  (if (zero? n)
    1
    (let [nf-div-e (g/div (factorial n) Math/E)]
      (g/floor
       (g/add 0.5 nf-div-e)))))

;; TODO close that ticket after we get this in.

(defn binomial-coefficient
  [n m]
  {:pre [(<= 0 n m)]}
  ;; TODO move the good code over to here, and call it from
  ;; number-of-combinations!
  #_(number-of-combinations n m))
