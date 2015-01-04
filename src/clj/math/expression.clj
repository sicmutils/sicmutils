(ns math.expression
  (:require [math.value :as v]
            [clojure.walk :refer :all]))

(declare freeze-expression)

(defrecord Expression [type expression]
  v/Value
  (nullity? [_] false)                                      ;; XXX what if it's a wrapped zero? one?
  (unity? [_] false)
  (zero-like [_] 0)
  (numerical? [x] (= (:type x) ::number))
  (exact? [_] false)
  (sort-key [_] 17)
  (compound? [_] false)
  (freeze [x] (-> x :expression freeze-expression)))

(defn make [x]
  (Expression. ::number x))

(defn literal-number
  [expression]
  (if (number? expression)
    expression
    (Expression. ::number expression)))

(defn abstract? [^Expression x]
  ;; TODO: GJS also allows for up, down, matrix here. We do not yet have
  ;; abstract structures.
  (= (:type x) ::number))

(defn expression-of
  [expr]
  (cond (instance? Expression expr) (:expression expr)
        (symbol? expr) expr
        :else (throw (IllegalArgumentException. (str "unknown expression type:" expr)))))


(defn variables-in
  "Return the 'variabls' (e.g. symbols) found in the expression x,
  which is an unwrapped expression."
  [x]
  (if (symbol? x) #{x}
                  (->> x flatten (filter symbol?) (into #{}))))


(defn walk-expression
  "Walk the unwrapped expression x in postorder, replacing symbols found there
  with their values in the map environment, if present; an unbound
  symbol is an error. Function applications are applied."
  [environment]
  (fn walk [x]
    ;(prn "WALK" x environment)
    (cond (number? x) x
          (symbol? x) (if-let [binding (x environment)]
                        binding
                        (throw (IllegalArgumentException.
                                 (str "no binding for " x " found."))))
          (instance? Expression x) (walk (expression-of x))
          (sequential? x) (apply (walk (first x)) (map walk (rest x)))

          :else (throw (IllegalArgumentException. (str "unknown expression type " x))))))

(defn print-expression
  [x]
  (postwalk
    (fn [x]
      (cond (symbol? x) (let [sym-ns (namespace x)
                              sym-name (name x)]
                          ; kind of a hack, but we don't want a circular dependency
                          ; here.
                          (if (and (= sym-ns "math.generic")
                                   (= sym-name "divide"))
                            '/
                            (symbol sym-name)))
            :else x))
    (freeze-expression x)))

(defn freeze-expression
  "Freezing an expression means removing wrappers and other metadata
  from subexpressions, so that the result is basically a pure
  S-expression with the same structure as the input. Doing this will
  rob an expression of useful information fur further computation; so
  this is intended to be done just before simplification and printing, to
  simplify those processes."
  [x]
  (cond (keyword? x) x
        (satisfies? v/Value x) (v/freeze x)
        (sequential? x) (map freeze-expression x)
        :else x))

(println "expression initialized")
