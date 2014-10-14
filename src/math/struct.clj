(ns math.struct
  (:require [math.generic :as g]))

(defn up [& xs] (with-meta (apply vector xs) {:orientation :up}))
(defn down [& xs] (with-meta (apply vector xs) {:orientation :down}))

(defn- orientation [s]
  (or (:orientation (meta s)) :up))

(defn- with-orientation-of [s t]
  (with-meta t {:orientation (orientation s)}))

(extend-protocol g/Value
  clojure.lang.PersistentVector
  (zero? [x] (every? g/zero? x))
  (one? [x] false)
  (zero-like [x] (with-orientation-of x (-> x count (repeat 0) vec)))
  (exact? [x] (every? g/exact? x))
  (sort-key [x] 20))

(def ^:private structure? vector?)

(defn- down? [s]
  (and (structure? s) (= (orientation s) :down)))

(defn- up? [s]
  (and (structure? s) (= (orientation s) :up)))

(defn- elementwise [op s t]
  (if (= (count s) (count t))
    (with-orientation-of s (vec (map op s t)))
    (throw (IllegalArgumentException.
            (str op " provided arguments of differing length")))))

(defn- compatible-for-contraction? [s t]
  (and (= (count s) (count t))
       (not= (orientation s) (orientation t))))

(defn- inner-product [s t]
  (apply g/+ (map g/* s t)))

(defn- scalar-multiply [a s]
  (with-orientation-of s (vec (map #(g/* a %) s))))

(defn- outer-product [s t]
  (with-orientation-of t (vec (map #(g/* s %) t))))

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
        :else (throw (IllegalArgumentException. (str "Cannot: " `(expt ~s ~n))))))

(g/defhandler :+  [down? down?]           (partial elementwise g/+))
(g/defhandler :+  [up? up?]               (partial elementwise g/+))
(g/defhandler :-  [down? down?]           (partial elementwise g/-))
(g/defhandler :-  [up? up?]               (partial elementwise g/-))
(g/defhandler :*  [number? structure?]    scalar-multiply)
(g/defhandler :*  [structure? scalar?]    #(scalar-multiply %2 %1))
(g/defhandler :/  [structure? scalar?]    #(scalar-multiply (/ %2) %1))
(g/defhandler :*  [structure? structure?] mul)
(g/defhandler :** [structure? integer?]   expt)

(g/defhandler :square [structure?]
  (fn [s] (inner-product s s)))
(g/defhandler :cube [structure?]
  (fn [s] (g/* s s s)))
(g/defhandler :negate [structure?]
  (fn [s] (with-orientation-of s (vec (map g/negate s)))))

(println "struct initialized")
