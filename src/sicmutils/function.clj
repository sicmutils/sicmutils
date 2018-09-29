;
; Copyright © 2017 Colin Smith.
; This work is based on the Scmutils system of MIT/GNU Scheme:
; Copyright © 2002 Massachusetts Institute of Technology
;
; This is free software;  you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3 of the License, or (at
; your option) any later version.
;
; This software is distributed in the hope that it will be useful, but
; WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
; General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this code; if not, see <http://www.gnu.org/licenses/>.
;

(ns sicmutils.function
  (:require [sicmutils
             [polynomial]
             [value :as v]
             [expression :as x]
             [structure :as s]
             [matrix :as m]
             [numsymb :as ns]
             [generic :as g]]
            [sicmutils.calculus.derivative :as d])
  (:import [sicmutils.polynomial Polynomial]
           [sicmutils.structure Structure]
           (clojure.lang IFn RestFn)
           (java.lang.reflect Method)))

(declare literal-apply)

(defn ^:private sicm-set->exemplar
  "Convert a SICM-style set (e.g., Real or (UP Real Real)) to
  an exemplar (an instance of the relevant type)."
  [s]
  (cond
    (= s 'Real) 0

    (sequential? s)
    (let [[constructor & args] s]
      (case constructor
        X     (mapv sicm-set->exemplar args)
        UP    (apply s/up (map sicm-set->exemplar args))
        DOWN  (apply s/down (map sicm-set->exemplar args))
        UP*   (apply s/up (repeat (second args) (sicm-set->exemplar (first args))))
        DOWN* (apply s/down (repeat (second args) (sicm-set->exemplar (first args))))
        X*    (into [] (repeat (second args) (sicm-set->exemplar (first args))))))))

(defn sicm-signature->domain-range
  "Convert a SICM-style literal function signature (e.g.,
  '(-> Real (X Real Real)) ) to our 'exemplar' format."
  [[arrow domain range]]
  (when-not (and (= '-> arrow) domain range)
    (throw (IllegalArgumentException.
            (str "A SICM signature is of the form '(-> domain range), got: " arrow domain range))))
  [(let [d (sicm-set->exemplar domain)]
     (if (vector? d) d [d]))
   (sicm-set->exemplar range)])

(defrecord Function [name arity domain range]
  Object
  (toString [_] (str name) )
  v/Value
  (kind [_] Function)
  IFn
  (invoke [f x] (literal-apply f [x]))
  (invoke [f x y] (literal-apply f [x y]))
  (invoke [f x y z] (literal-apply f [x y z]))
  (invoke [f w x y z] (literal-apply f [w x y z]))
  (applyTo [f xs] (literal-apply f xs)))

(def ^:private orientation->symbol {::s/up "↑" ::s/down "_"})

(defn literal-function
  ([f] (Function. f [:exactly 1] [0] 0))
  ([f signature]
   (let [[domain range] (sicm-signature->domain-range signature)]
     (literal-function f domain range)))
  ([f domain range]
   (cond (number? range)
         (Function. f [:exactly (if (vector? domain) (count domain) 1)]
                    (if (vector? domain) domain [domain])
                    range)

         (s/structure? range)
         (s/same range (map-indexed (fn [index component]
                                      (literal-function
                                       (symbol (str f
                                                    (orientation->symbol (s/orientation range))
                                                    index))
                                       domain
                                       component))
                                    range))

         :else
         (throw (IllegalArgumentException. (str "WTF range" domain))))))

(def ^:private derivative-symbol 'D)

;; --------------------
;; Algebra of functions
;;

(defn ^:private unary-operation
  "For a unary operator (like sqrt), returns a function of one function
  which when called will apply the operation to the result of the
  original function (so that ((unary-operation sqrt) f) x) will return
  (sqrt (f x))."
  [operator]
  (with-meta (partial comp operator) {:arity [:exactly 1]}))

(defn ^:private binary-operation
  "For a given binary operator (like +), returns a function of two
  functions which will produce the pointwise operation of the results
  of applying the two functions to the input. That
  is, (binary-operation +) applied to f and g will produce a function
  which computes (+ (f x) (g x)) given x as input."
  [operator]
  (let [h (fn [f g]
            (let [arity (g/joint-arity [f g])
                  h (condp = arity
                      [:exactly 0]
                      #(operator (f) (g))
                      [:exactly 1]
                      #(operator (f %) (g %))
                      [:exactly 2]
                      #(operator (f %1 %2) (g %1 %2))
                      [:exactly 3]
                      #(operator (f %1 %2 %3) (g %1 %2 %3))
                      [:exactly 4]
                      #(operator (f %1 %2 %3 %4) (g %1 %2 %3 %4))
                      [:exactly 5]
                      #(operator (f %1 %2 %3 %4 %5) (g %1 %2 %3 %4 %5))
                      [:exactly 6]
                      #(operator (f %1 %2 %3 %4 %5 %6) (g %1 %2 %3 %4 %5 %6))
                      [:exactly 7]
                      #(operator (f %1 %2 %3 %4 %5 %6 %7) (g %1 %2 %3 %4 %5 %6 %7))
                      [:exactly 8]
                      #(operator (f %1 %2 %3 %4 %5 %6 %7 %8) (g %1 %2 %3 %4 %5 %6 %7 %8))
                      [:exactly 9]
                      #(operator (f %1 %2 %3 %4 %5 %6 %7 %8 %9) (g %1 %2 %3 %4 %5 %6 %7 %8 %9))
                      [:exactly 10]
                      #(operator (f %1 %2 %3 %4 %5 %6 %7 %8 %9 %10) (g %1 %2 %3 %4 %5 %6 %7 %8 %9 %10))
                      [:at-least 0]
                      #(operator (apply f %&) (apply g %&))
                      (throw (IllegalArgumentException.
                              (str  "unsupported arity for function arithmetic " arity))))]
              (with-meta h {:arity arity :from :function-binop})))]
    (with-meta h {:arity [:exactly 2]})))

(defn lift-to-function
  [f arity]
  (with-meta
    (constantly f)
    {:arity arity
     :from :binop}))

(defmacro ^:private make-binary-operation
  "Given a generic and binary function operation,
  define the multimethods necessary to introduce this operation
  to function arguments."
  [generic-op binary-op]
  `(let [binop# (binary-operation ~binary-op)]
     (doseq [signature# [[::function ::function]
                         [::function ::cofunction]
                         [::cofunction ::function]]]
       (defmethod ~generic-op signature# [a# b#] (binop# a# b#)))
     (defmethod ~generic-op
       [::function ::lifts-to-function]
       [f# n#]
       (binop# f# (lift-to-function n# (g/arity f#))))
     (defmethod ~generic-op
       [::lifts-to-function ::function]
       [n# f#]
       (binop# (lift-to-function n# (g/arity f#)) f#))))

(defmacro ^:private make-unary-operation
  [generic-op]
  `(defmethod ~generic-op [::function] [a#] ((unary-operation ~generic-op) a#)))

(make-binary-operation g/add g/+)
(make-binary-operation g/sub g/-)
(make-binary-operation g/mul g/*)
(make-binary-operation g/div g/divide)
(make-binary-operation g/expt g/expt)

(make-unary-operation g/negate)
(make-unary-operation g/invert)
(make-unary-operation g/sqrt)
(make-unary-operation g/sin)
(make-unary-operation g/asin)
(make-unary-operation g/cos)
(make-unary-operation g/acos)
(make-unary-operation g/tan)
(make-unary-operation g/atan)
(make-unary-operation g/square)
(make-unary-operation g/cube)
(make-unary-operation g/exp)
(make-unary-operation g/log)
(make-unary-operation g/transpose)

;; TODO sinh cosh ...

(defmethod g/simplify [Function] [a] (-> a :name g/simplify))
(defmethod g/zero-like [::function] [a] (fn [& _] (g/zero-like (:range a))))
(defmethod g/one-like [::function] [a] (fn [& _] (g/one-like (:range a))))

(derive clojure.lang.Symbol ::lifts-to-function)
(derive ::x/numerical-expression ::lifts-to-function)
(derive ::ns/numeric-type ::lifts-to-function)
(derive ::s/structure ::cofunction)
(derive ::m/matrix ::cofunction)
(derive ::function :sicmutils.series/coseries)
(derive ::v/function ::function)  ;; TODO: WTF

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
        (list (ns/expt derivative-symbol 2) (fnext expr))
        (iterated-symbolic-derivative? expr)
        (list (ns/expt derivative-symbol
                       (+ (first (nnext (first expr)))
                          1))
              (fnext expr))
        :else
        (list derivative-symbol expr)))

(defn ^:private make-partials
  [f v]
  ;; GJS calls this function (the loop below) "fd"; we have no idea
  ;; what that stands for or what
  ;; is being attempted here
  (letfn [(fd [indices vv]
            (cond (s/structure? vv)
                  (let [^Structure s vv]
                    (s/same s (map-indexed (fn [i element]
                                             (fd (conj indices i) element))
                                           s)))
                  (or (g/numerical? vv)
                      (g/abstract-quantity? vv))
                  (let [fexp (if (= (:arity f) [:exactly 1])  ; univariate
                               (if (= (first indices) 0)
                                 (if (= (count indices) 1)
                                   (symbolic-increase-derivative (:name f))
                                   `((~'∂ ~@(next indices)) ~(:name f)))
                                 (throw (IllegalArgumentException. "wrong indices")))
                               `((~'∂ ~@indices) ~(:name f)))]
                    (Function. fexp (:arity f) (:domain f) (:range f)))
                  :else
                  (throw (IllegalArgumentException. (str "make-partials WTF " vv)))))]
    (fd [] v)))


(defn ^:private literal-derivative
  [f xs]
  (let [v (m/seq-> xs)
        maxtag (->> v flatten d/max-order-tag)
        ve (->> v (s/mapr #(d/without-tag maxtag %)) seq)
        dv (->> v (s/mapr #(d/with-tag maxtag %)))]
    (d/canonicalize-differential
     (d/dx+dy (apply f ve)
              (reduce d/dx+dy (map (fn [partialx dx]
                                     (d/dx*dy (apply partialx ve) dx))
                                   (flatten (make-partials f v))
                                   (flatten dv)))))))

(defn ^:private check-argument-type
  "Check that the argument provided at index i has the same type as
  the exemplar expected."
  [f provided expected indexes]
  (cond (number? expected)
        (when-not (g/numerical? provided)
          (throw (IllegalArgumentException.
                  (str "expected numerical quantity in argument " indexes
                       " of function call " f
                       " but got " provided))))
        (s/structure? expected)
        (do (when-not (and (or (s/structure? provided) (sequential? provided))
                           (= (s/orientation provided) (s/orientation expected))
                           (= (count provided) (count expected)))
              (throw (IllegalArgumentException.
                      (str "expected structure matching " expected
                           " but got " provided ))))
            (doseq [[provided expected sub-index] (map list provided expected (range))]
              (check-argument-type f provided expected (conj indexes sub-index))))
        (keyword? expected) ;; a keyword has to match the argument's kind
        (when-not (= (v/kind provided) expected)
          (throw (IllegalArgumentException.
                  (str "expected argument of type " expected " but got " (v/kind provided)
                       " in call to function " f))))

        :else (throw (IllegalArgumentException.
                      (str "unexpected argument example. got " provided " want " expected)))))

(defn ^:private literal-apply
  [f xs]
  (check-argument-type f xs (:domain f) [0])
  (if (some d/differential? xs)
    (literal-derivative f xs)
    (x/literal-number `(~(:name f) ~@(map g/freeze xs)))))

;;; Utilities

(defn compose
  "Compose is like Clojure's standard comp, but for this system we
  like to know the arity of our functions, so that we can calculate
  their derivatives with structure, etc. The arity of a composition is
  simply the arity of its rightmost (that is, first to be applied)
  function term."
  [& fns]
  (let [a (g/arity (last fns))]
    (with-meta (apply comp fns) {:arity a})))

(defmacro with-literal-functions
  [litfns & body]
  `(let ~(vec (interleave
               (map (fn [s]
                      (if (symbol? s) s (first s)))
                    litfns)
               (map (fn [s]
                      (cond (symbol? s)
                            `(literal-function (quote ~s))
                            (and (sequential? s)
                                 (= (count s) 3))
                            `(literal-function (quote ~(first s))
                                               ~(second s)
                                               ~(nth s 2))
                            :else (throw
                                   (IllegalArgumentException.
                                    (str "unknown literal function type" s)))))
                    litfns)))
     ~@body))

(defmethod g/freeze
  [::function]
  [a]
  (or (and (instance? clojure.lang.MultiFn a)
           (if-let [m (get-method a :name)]
             (m :name)))
      (:name a)
      ((get-method g/freeze :default) a)))

(def ^:private reflect-on-arity
  "Returns the arity of the native function f.
  Computing arities of clojure functions is a bit complicated.
  It involves reflection, so the results are definitely worth
  memoizing."
  (memoize
   (fn [f]
     (let [^"[java.lang.reflect.Method" methods (.getDeclaredMethods (class f))
                     ;; tally up arities of invoke, doInvoke, and
                     ;; getRequiredArity methods. Filter out invokeStatic.
                     ^RestFn rest-fn f
                     facts (group-by first
                                     (for [^Method m methods
                                           :let [name (.getName m)]
                                           :when (not= name "invokeStatic")]
                                       (condp = name
                                         "invoke" [:invoke (alength (.getParameterTypes m))]
                                         "doInvoke" [:doInvoke true]
                                         "getRequiredArity" [:getRequiredArity (.getRequiredArity rest-fn)])))]
                 (cond
                   ;; Rule one: if all we have is one single case of invoke, then the
                   ;; arity is the arity of that method. This is the common case.
                   (and (= 1 (count facts))
                        (= 1 (count (:invoke facts))))
                   [:exactly (second (first (:invoke facts)))]
                   ;; Rule two: if we have exactly one doInvoke and getRequiredArity,
                   ;; and possibly an invokeStatic, then the arity at
                   ;; least the result of .getRequiredArity.
                   (and (= 2 (count facts))
                        (= 1 (count (:doInvoke facts)))
                        (= 1 (count (:getRequiredArity facts))))
                   [:at-least (second (first (:getRequiredArity facts)))]
                   ;; Rule three: if we have invokes for the arities 0..3, getRequiredArity
                   ;; says 3, and we have doInvoke, then we consider that this function
                   ;; was probably produced by Clojure's core "comp" function, and
                   ;; we somewhat lamely consider the arity of the composed function 1.
                   (and (= #{0 1 2 3} (into #{} (map second (:invoke facts))))
                        (= 3 (second (first (:getRequiredArity facts))))
                        (:doInvoke facts))
                   [:exactly 1]
                   :else (throw (IllegalArgumentException. (str "arity? " f " " facts))))))))

(defmethod g/arity
  [clojure.lang.MultiFn]
  [a]
  (if-let [m (get-method a :arity)]
    (m :arity)
    (throw (UnsupportedOperationException. "unknown multifn arity"))))

(defmethod g/arity
  [Function]
  [^Function a]
  (.arity a))

(defmethod g/arity
  [::function]
  [a]
  (if (fn? a)
    (or (:arity (meta a))
        (reflect-on-arity a))
    (throw (UnsupportedOperationException. (str "arity of unusual object: " a " of type " (type a))))))

(derive clojure.lang.MultiFn ::function)
(derive Function ::function)
