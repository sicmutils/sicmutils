(ns math.structure
  (:import (clojure.lang Sequential Seqable IFn))
  (:require [math.value :as v]
            [math.expression :as x]
            [math.generic :as g]))

(deftype Struct [orientation v]
  v/Value
  (nullity? [s] (every? g/zero? v))
  (unity? [_] false)
  (zero-like [s] (Struct. orientation (-> v count (repeat 0) vec)))
  (exact? [s] (every? g/exact? v))
  (numerical? [_] false)
  (compound? [_] true)
  (sort-key [_] 18)
  (freeze [s]
    `(~(orientation {:up 'up :down 'down}) ~@(map x/print-expression v)))
  Object
  (equals [a b]
    (and (instance? Struct b)
         (let [^Struct bs b]
           (= orientation (.orientation bs))
          (= v (.v bs)))))
  (toString [s] (str (cons orientation v)))
  Sequential
  Seqable
                                        ;(seq [x] (-> x .v seq))
  (seq [x] (seq v))
  IFn
  (invoke [s x]
    (Struct. orientation (vec (map #(% x) v))))
  )

(defn up [& xs]
  (Struct. :up (apply vector xs)))

(defn down [& xs]
  (Struct. :down (apply vector xs)))

(defn structure? [s]
  (or (instance? Struct s)
      (vector? s)
      (list? s)))

(defn- down? [^Struct s]
  (and (instance? Struct s) (= (.orientation s) :down)))

(defn- up? [^Struct s]
  (or (vector? s)
      (list? s)
      (and (instance? Struct s) (= (.orientation s) :up))))

(defn- elements [^Struct s]
  (if (instance? Struct s) (.v s)
      s))

(defn- size [s]
  (count (elements s)))

(defn- orientation [^Struct s]
  (if (instance? Struct s) (.orientation s) :up))

(defn- elementwise [op s t]
  (if (= (size s) (size t))
    (Struct. (orientation s) (vec (map op (elements s) (elements t))))
    (throw (ArithmeticException.
            (str op " provided arguments of differing length")))))

(defn mapr
  "Return a structure with the same shape as s but with f applied to
  each primitive (that is, not structural) component."
  [f ^Struct s]
  (cond (instance? Struct s) (Struct. (.orientation s) (map #(mapr f %) (.v s)))
        (sequential? s) (map f s)  ;; XXX what happens if we don't do this?
        :else (f s))
  )

(defn structure-assoc-in
  "Like assoc-in, but works for structures. At this writing we're not
  sure if we want to overwrite the stock definition of assoc-in to
  something that would fall through for standard clojure data types"
  [^Struct s keys value]
  (if (empty? keys) value
      (let [w (.v s)
            k1 (first keys)]
        (assoc w k1 (structure-assoc-in (w k1) (next keys) value)))))

(defn structure-get-in
  "Like get-in, but for structures. See structure-assoc-in"
  [^Struct s keys]
  (if (empty? keys) s
      (recur (-> s .v (get (first keys))) (next keys))))

(defn- compatible-for-contraction? [s t]
  (and (= (size s) (size t))
       (not= (orientation s) (orientation t))))

(defn- inner-product [s t]
  (apply g/+ (map g/* (elements s) (elements t))))

(defn- outer-product [a s]
  (Struct. (orientation s) (vec (map #(g/* a %) (elements s)))))

(defn- mul [s t]
  (if (compatible-for-contraction? s t)
    (inner-product s t)
    (outer-product s t)))

;; hmmm. why not do the repeated-squaring trick here?
;; perhaps structures are not typically raised to high
;; exponents.

(defn- expt [s n]
  (cond (= n 1) s
        (> n 1) (g/* s (g/expt s (- n 1)))
        :else (throw (ArithmeticException. (str "Cannot: " `(expt ~s ~n))))))

(defn- matrix->structure "TODO: implement" [m] m)

(defn seq->
  "Convert a sequence (typically, of function arguments) to an up-structure.
  GJS: Any matrix in the argument list wants to be converted to a row of
  columns (TODO: this is not implemented yet)"
  [s]
  (apply up (map matrix->structure s)))

(g/defhandler :+   [down? down?]           (partial elementwise g/+))
(g/defhandler :+   [up? up?]               (partial elementwise g/+))
(g/defhandler :-   [down? down?]           (partial elementwise g/-))
(g/defhandler :-   [up? up?]               (partial elementwise g/-))
(g/defhandler :*   [g/scalar? structure?]    outer-product) ;; this was kind of a late thing to add: maybe we
;; were depending on canonical order in the past to sort all scalars to the left of structures. That might
;; be a useful idea to reconsider, or maybe not.
(g/defhandler :*   [structure? g/scalar?]    #(outer-product %2 %1))
(g/defhandler :div [structure? g/scalar?]    #(outer-product (/ %2) %1))
(g/defhandler :*   [structure? structure?] mul)
(g/defhandler :**  [structure? integer?]   expt)
(g/defhandler :∂   [structure? (constantly true)] (fn [_ _] (throw (IllegalArgumentException. "OUCH"))))

(g/defhandler :square [structure?]
  (fn [s] (inner-product s s)))
(g/defhandler :cube [structure?]
  (fn [s] (g/* s s s)))
(g/defhandler :negate [structure?]
  (fn [s] (Struct. (orientation s) (vec (map g/negate (elements s))))))

(println "struct initialized")
