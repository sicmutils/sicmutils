(ns math.generic
  (:refer-clojure :rename {+ core-+
                           - core--
                           / core-div
                           * core-*
                           zero? core-zero?})
  (:require [clojure.math.numeric-tower :as nt]
            [math.expression :as x]
            [math.operator :as o]
            [math.value :refer :all])
  (:import [math.expression Expression]))

(extend-protocol Value
  clojure.lang.Symbol
  (zero? [x] false)
  (one? [x] false)
  (zero-like [x] 0)
  (one-like [x] 1)
  (exact? [x] false) ;; XXX maybe true?
  (compound? [x] false)
  (sort-key [x] 90)
  (numerical? [x] false)
  (freeze [x]
    ;; to freeze a symbol we drop its namespace component if it has one.
    (symbol (name x)))
  clojure.lang.IFn
  (arity [f]
    (let [m (first (.getDeclaredMethods (class f)))
          p (.getParameterTypes m)]
      (alength p)))
  )

(def empty-dtree {:steps {} :stop nil})
(def ^:private the-operator-table (atom {}))

;; or how about something like
;; (assoc (assoc-in dtree (mapcat (fn [x] [:step x]) p))

(defn dtree-insert [{:keys [steps stop] :as dtree} op [predicate & predicates]]
  (if predicate
    ;; if there is another predicate, the next dtree is either the one
    ;; that governs this predicate at this stage, or a new empty one.
    (let [next-dtree (or (steps predicate) empty-dtree)]
      ;; augment the binding at this level
      (assoc dtree :steps
             (assoc steps predicate (dtree-insert next-dtree op predicates))))
    ;; no more predicates? store the current stop function.
    (do
      (if stop (prn "overwriting a binding!!" stop op dtree))
      (assoc dtree :stop op))))

(defn dtree-lookup [{:keys [steps stop]} [argument & arguments]]
  (if argument
    ;; take a step: that means finding a predicate that matches at
    ;; this step and seeing if the subordinate dtree also matches. The
    ;; first step that matches this pair of conditions is chosen.
    (some identity
          (map (fn [[step dtree]]
                 (and (step argument)
                      (dtree-lookup dtree arguments))) steps))
    ;; otherwise we stop here.
    stop))

(defn defhandler [operator predicates f]
  (swap! the-operator-table
         (fn [operator-table]
           (let [dtree (get operator-table operator empty-dtree)]
             (assoc operator-table operator
                    (dtree-insert dtree f predicates))))))

(defn findhandler [operator arguments]
  (if-let [dtree (@the-operator-table operator)]
    (dtree-lookup dtree arguments)))

(defn make-operation [operator]
  (fn [& args]
    (if-let [h (findhandler operator args)]
      (apply h args)
      (throw (IllegalArgumentException.
              (str "no variant of " operator
                   " will work for " args))))))

(def ^:private mul (make-operation :*))
(def ^:private add (make-operation :+))
(def ^:private sub (make-operation :-))
(def ^:private div (make-operation :/))

(def expt (make-operation :**))
(def negate (make-operation :negate))
(def invert (make-operation :invert))
(def sin (make-operation :sin))
(def cos (make-operation :cos))
(def square (make-operation :square))
(def cube (make-operation :cube))
(def abs (make-operation :abs))
(def sqrt (make-operation :sqrt))
(def exp (make-operation :exp))
(def log (make-operation :log))
(def partial-derivative (make-operation #_(2) :∂))

(defn canonical-order [args]
  ;; NB: we are relying on the fact that this sort is stable, although
  ;; the Clojure documentation does not explicity guarantee this
  (sort-by #(if (satisfies? Value %) (sort-key %) 99) args))

(defn- bin+ [a b]
  (cond (and (number? a) (number? b)) (core-+ a b)
        (zero? a) b
        (zero? b) a
        :else (add a b))
  )

(defn + [& args]
  (reduce bin+ 0 (canonical-order args)))

(defn- bin- [a b]
  (cond (and (number? a) (number? b)) (core-- a b)
        (zero? b) a
        (zero? a) (negate b)
        :else (sub a b)))

(defn - [& args]
  (cond (empty? args) 0
        (= (count args) 1) (negate (first args))
        :else (bin- (first args) (apply + (next args)))))

(defn- bin* [a b]
  (cond (and (number? a) (number? b)) (core-* a b)
        (and (number? a) (zero? a)) (zero-like b)
        (and (number? b) (zero? b)) (zero-like a)
        (one? a) b
        (one? b) a
        :else (mul a b)))

;;; In bin* we test for exact (numerical) zero
;;; because it is possible to produce a wrong-type
;;; zero here, as follows:
;;;
;;;               |0|             |0|
;;;       |a b c| |0|   |0|       |0|
;;;       |d e f| |0| = |0|, not  |0|
;;;
;;; We are less worried about the zero? below,
;;; because any invertible matrix is square.

(defn * [& args]
  (reduce bin* 1 (canonical-order args)))

(defn- bin-div [a b]
  (cond (and (number? a) (number? b)) (core-div a b)
        (one? b) a
        :else (div a b)))

(defn / [& args]
  (cond (nil? args) 1
        (nil? (next args)) (invert (first args))
        :else (bin-div (first args) (apply * (next args)))))

;; XXX move these to expression?


(defn literal-number?
  [x]
  (and (instance? Expression x)
       (numerical? x)))

(defn abstract-number?
  [x]
  (or (symbol? x)
      (literal-number? x)))

(defn abstract-quantity?
  [x]
  (and (instance? Expression x)
       ;; TODO: these type symbols might not correspond to what
       ;; we do in the future, so these should be checked.
       ((.type x) #{:number :up :down :matrix})))

(defn numerical-quantity?
  [x]
  (or (number? x)
      (abstract-number? x)
      (numerical? x)))

(def D
  (o/make-operator
   (fn [f]
     (partial-derivative f []))
   :derivative))

(defn pd
  [& selectors]
  (o/make-operator
   (fn [f]
     (prn "in PD function" f selectors)
     (partial-derivative f selectors))
   :partial-derivative))

(println "generic initialized")
