(ns math.function
  (:require [math.value :as v]
            [math.expression :as x]
            [math.structure :as s]
            [math.numsymb :as ns]
            [math.calculus.derivative :as d]
            [math.generic :as g])
  (:import [math.structure Struct]))

(declare literal-apply)

(defrecord Fn [expr arity domain range]
  v/Value
  (zero? [x] false)
  (one? [x] false)
  (zero-like [x] false)
  (exact? [x] false)
  (sort-key [x] 35)
  (arity [x] (:arity x))
  clojure.lang.IFn
  (invoke [f x] (do (prn "literal-applying" (.expr f) "to" x)) (literal-apply f [x]))
  (applyTo [f xs] (do (prn "literal-applying*" (.expr f) "to" xs) (literal-apply f xs)))
  )

(defn literal-function [f] (Fn. f 1 [:real] :real))
(def ^:private derivative-symbol `g/D)


(defn symbolic-derivative?
  [expr]
  #_(prn "SYMB-DER" expr (first expr) (= (first expr) derivative-symbol) (list? expr) (type expr))
  (and (sequential? expr)
       ;; XXX GJS uses 'derivative here; should we? doesn't he just
       ;; have to change it back to D when printing?
       (= (first expr) derivative-symbol)))

(defn iterated-symbolic-derivative?
  [expr]
  (and (sequential? expr)
       (sequential? (first expr))
       (ns/expt? (first expr))
       (= (second (first expr)) derivative-symbol)))

(defn symbolic-increase-derivative [expr]
  (cond (symbolic-derivative? expr)
        (list (ns/expt derivative-symbol 2) (first (next expr)))
        (iterated-symbolic-derivative? expr)
        (list (ns/expt derivative-symbol
                       (+ (first (next (next (first expr))))
                          1))
              (first (next expr)))
        :else
        (list derivative-symbol expr)))

(defn- make-partials
  [f v]
  ;; GJS calls this function (the loop below) "fd"; we have no idea
  ;; what that stands for or what
  ;; is being attempted here
  (prn "make-partials" "f" f "v")
  (letfn [(fd [indices vv]
            (prn "fd" "indices" indices "vv" vv)
            (cond (s/structure? vv)
                  (Struct. (.orientation vv)
                           (map-indexed (fn [i element]
                                          (fd (conj indices i) element)) vv))
                  (or (g/numerical-quantity? vv)
                      (g/abstract-quantity? vv))
                  (let [fexp (if (= (:arity f) 1)  ; univariate
                               (if (= (first indices) 0)
                                 (if (= (count indices) 1)
                                   (symbolic-increase-derivative (.expr f))
                                   `((partial-derivative ~@(next indices))))
                                 (throw (IllegalArgumentException. "wrong indices")))
                               `((partial-derivative ~@indices) ~(.expr f)))]
                    (Fn. fexp (:arity f) (.domain f) (.range f)))
                  :else
                  (throw (IllegalArgumentException. (str "make-partials WTF " vv)))))]
    (fd [] v)
    ))

(defn- literal-derivative
  [f xs]
  (let [v (s/seq-> xs)
        maxtag (->> v flatten d/max-order-tag)
        ve (->> v (s/mapr #(d/without-tag maxtag %)) seq)
        dv (->> v (s/mapr #(d/with-tag maxtag %)))]
    (prn "LIT-DERIV" "v" v "maxtag" maxtag "ve" ve "dv" dv)
    (prn "make-partials" "f" f "v" v "->" (make-partials f v) "flattened" (flatten (make-partials f v)))
    (d/dx+dy (apply f ve)
             (reduce d/dx+dy (map (fn [partialx dx]
                                    (d/dx*dy (apply partialx ve) dx))
                                  (flatten (make-partials f v))
                                  (flatten dv))))))

(defn- literal-apply
  [f xs]
  (if (some d/differential? xs)
    (literal-derivative f xs)
    (x/literal-number (list* (.expr f) xs)))) ;; XXX cons

(println "function initialized")
;; what literal functions work out to in scmutils:

;; 1 ]=> (define f (literal-function 'x))
;; #| f |#

;; 1 ]=> (display f)
;; #[apply-hook 14]

;; 1 ]=> (display (f 't))
;; (*number* (expression (x t)) (literal-function #[apply-hook 14]) (type-expression Real))
