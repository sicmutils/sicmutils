(ns math.structure
  (:require [math.value :as v]
            [math.expression :as x]
            [math.generic :as g]))

(deftype Struct [orientation v]
  v/Value
  (zero? [s] (every? v/zero? (.v s)))
  (one? [s] false)
  (zero-like [s] (Struct. (.orientation s) (-> s .v count (repeat 0) vec)))
  (exact? [s] (every? v/exact? (.v s)))
  (compound? [s] true)
  (sort-key [s] 18)
  (freeze [s]
    `(~((.orientation s) {:up 'up :down 'down}) ~@(map x/freeze-expression (.v s))))
  Object
  (equals [a b]
    (and (= (.orientation a) (.orientation b))
         (= (.v a) (.v b))))
  (toString [a] (str (cons (.orientation a) (.v a))))
  clojure.lang.Sequential
  clojure.lang.Seqable
  (seq [x] (-> x .v seq))
  clojure.lang.IFn
  (invoke [s x]
    (Struct. (.orientation s) (vec (map #(% x) (.v s)))))
  )

(defn up [& xs]
  (Struct. :up (apply vector xs)))

(defn down [& xs]
  (Struct. :down (apply vector xs)))

(extend-protocol v/Value
  clojure.lang.PersistentVector
  (zero? [x] (every? v/zero? x))
  (one? [x] false)
  (zero-like [x] (-> x count (repeat 0) vec))
  (exact? [x] (every? v/exact? x))
  (compund? [x] true)
  (sort-key [x] 20)
  (numerical? [v] false)
  )

(defn structure? [s]
  (or (instance? Struct s)
      (vector? s)
      (list? s)))

(defn- down? [s]
  (and (instance? Struct s) (= (.orientation s) :down)))

(defn- up? [s]
  (or (vector? s)
      (list? s)
      (and (instance? Struct s) (= (.orientation s) :up))))

(defn- elements [s]
  (if (instance? Struct s) (.v s)
      s))

(defn- size [s]
  (count (elements s)))

(defn- orientation [s]
  (if (instance? Struct s) (.orientation s) :up))

(defn- elementwise [op s t]
  (if (= (size s) (size t))
    (Struct. (orientation s) (vec (map op (elements s) (elements t))))
    (throw (ArithmeticException.
            (str op " provided arguments of differing length")))))

(defn mapr
  "Return a structure with the same shape as s but with f applied to
  each primitive (that is, not structural) component."
  [f s]
  (cond (instance? Struct s) (Struct. (.orientation s) (map #(mapr f %) (.v s)))
        (sequential? s) (map f s)  ;; XXX what happens if we don't do this?
        :else (f s))
  )

(defn structure-assoc-in
  "Like assoc-in, but works for structures. At this writing we're not
  sure if we to overwrite the stock definition of assoc-in to
  something that would fall through for standard clojure data types"
  [s keys value]
  (if (empty? keys) value
      (let [w (.v s)
            k1 (first keys)]
        (assoc w k1 (structure-assoc-in (w k1) (next keys) value)))))

(defn structure-get-in
  "Like get-in, but for structures. See structure-assoc-in"
  [s keys]
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

(defn- scalar? [s]
  (or (number? s) (g/abstract-number? s)))

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

(g/defhandler :+  [down? down?]           (partial elementwise g/+))
(g/defhandler :+  [up? up?]               (partial elementwise g/+))
(g/defhandler :-  [down? down?]           (partial elementwise g/-))
(g/defhandler :-  [up? up?]               (partial elementwise g/-))
(g/defhandler :*  [number? structure?]    outer-product)
(g/defhandler :*  [structure? scalar?]    #(outer-product %2 %1))
(g/defhandler :/  [structure? scalar?]    #(outer-product (/ %2) %1))
(g/defhandler :*  [structure? structure?] mul)
(g/defhandler :** [structure? integer?]   expt)
(g/defhandler :∂  [structure? (constantly true)] (fn [a b] (throw (IllegalArgumentException. "OUCH"))))

(g/defhandler :square [structure?]
  (fn [s] (inner-product s s)))
(g/defhandler :cube [structure?]
  (fn [s] (g/* s s s)))
(g/defhandler :negate [structure?]
  (fn [s] (Struct. (orientation s) (vec (map g/negate (elements s))))))

(println "struct initialized")
