(ns math.function
  (:require [math.value :as v]
            [math.expression :as x]
            [math.structure :as s]
            [math.numsymb :as ns]
            [math.calculus.derivative :as d]
            [math.generic :as g])
  (:import [math.structure Struct]
           [math.operator Operator]
           (clojure.lang IFn)))

(declare literal-apply)

(defrecord Fn [expr arity domain range]
  v/Value
  (nullity? [_] false)
  (compound? [_] false)
  (unity? [_] false)
  (numerical? [_] false)
  (sort-key [_] 35)
  (freeze [f] (-> f :expr x/freeze-expression))
  IFn
  (invoke [f x] (literal-apply f [x]))
  (applyTo [f xs] (literal-apply f xs))
  )

(defn literal-function [f] (Fn. f 1 [:real] :real))
(def ^:private derivative-symbol `g/D)

;; --------------------
;; Algebra of functions
;;

(defn- function?
  [x]
  (and (ifn? x)
       (not (instance? Struct x))
       (not (symbol? x))
       (not (vector? x))))

(defn- cofunction?
  "True if f may be combined with a function."
  [f]
  (not (instance? Operator f)))

(defn- unary-operation
  "For a unary operator (like sqrt), returns a function of one function
  which when called will apply the operation to the result of the
  original function (so that ((unary-operation sqrt) f) x) will return
  (sqrt (f x))."
  [operator]
  (with-meta (partial comp operator) {:arity 1}))

(defn- binary-operation
  "For a given binary operator (like +), returns a function of two
  functions which will produce the pointwise operation of the results
  of applying the two functions to the input. That
  is, (binary-operation +) applied to f and g will produce a function
  which computes (+ (f x) (g x)) given x as input."
  [operator]
  (with-meta (fn [f g]
               (let [f1 (if (g/numerical-quantity? f) (constantly f) f)
                     g1 (if (g/numerical-quantity? g) (constantly g) g)]
                 (with-meta #(operator (f1 %) (g1 %)) {:arity 1})))
    {:arity 2}))

(g/defhandler :negate [function?] (unary-operation g/negate))
(g/defhandler :invert [function?] (unary-operation g/invert))
(g/defhandler :sqrt   [function?] (unary-operation g/sqrt))
(g/defhandler :square [function?] (unary-operation g/square))
(g/defhandler :abs    [function?] (unary-operation g/abs))
(g/defhandler :exp    [function?] (unary-operation g/exp))
(g/defhandler :log    [function?] (unary-operation g/log))
(g/defhandler :sin    [function?] (unary-operation g/sin))
(g/defhandler :cos    [function?] (unary-operation g/cos))
;; TODO asin acos sinh cosh ...

(g/defhandler :+      [function? cofunction?] (binary-operation g/+))
(g/defhandler :+      [cofunction? function?] (binary-operation g/+))
(g/defhandler :-      [function? cofunction?] (binary-operation g/-))
(g/defhandler :-      [cofunction? function?] (binary-operation g/-))
(g/defhandler :*      [function? cofunction?] (binary-operation g/*))
(g/defhandler :*      [cofunction? function?] (binary-operation g/*))
(g/defhandler :div    [function? cofunction?] (binary-operation g/divide))
(g/defhandler :div    [cofunction? function?] (binary-operation g/divide))
(g/defhandler :expt   [function? cofunction?] (binary-operation g/expt))
(g/defhandler :expt   [cofunction? function?] (binary-operation g/expt))

;; ------------------------------------
;; Differentiation of literal functions
;;

(defn symbolic-derivative?
  [expr]
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
  (letfn [(fd [indices vv]
            (cond (s/structure? vv)
                  (let [^Struct s vv]
                    (Struct. (.orientation s)
                             (map-indexed (fn [i element]
                                            (fd (conj indices i) element)) s)))
                  (or (g/numerical-quantity? vv)
                      (g/abstract-quantity? vv))
                  (let [fexp (if (= (:arity f) 1)  ; univariate
                               (if (= (first indices) 0)
                                 (if (= (count indices) 1)
                                   (symbolic-increase-derivative (:expr f))
                                   `((g/partial-derivative ~@(next indices))))
                                 (throw (IllegalArgumentException. "wrong indices")))
                               `((g/partial-derivative ~@indices) ~(:expr f)))]
                    (Fn. fexp (:arity f) (:domain f) (:range f)))
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
    (d/dx+dy (apply f ve)
             (reduce d/dx+dy (map (fn [partialx dx]
                                    (d/dx*dy (apply partialx ve) dx))
                                  (flatten (make-partials f v))
                                  (flatten dv))))))

(defn- literal-apply
  [f xs]
  (if (some d/differential? xs)
    (literal-derivative f xs)
    (x/literal-number (list* (:expr f) xs)))) ;; XXX cons

(g/defhandler :simplify [#(instance? Fn %)] #(-> % :expr g/simplify))

(println "function initialized")
;; what literal functions work out to in scmutils:

;; 1 ]=> (define f (literal-function 'x))
;; #| f |#

;; 1 ]=> (display f)
;; #[apply-hook 14]

;; 1 ]=> (display (f 't))
;; (*number* (expression (x t)) (literal-function #[apply-hook 14]) (type-expression Real))
