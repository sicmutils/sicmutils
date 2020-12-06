
;; Copyright © 2017 Colin Smith.
;; This work is based on the Scmutils system of MIT/GNU Scheme:
;; Copyright © 2002 Massachusetts Institute of Technology

;; This is free software;  you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or (at
;; your option) any later version.

;; This software is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this code; if not, see <http://www.gnu.org/licenses/>.


(ns sicmutils.expression
  (:refer-clojure :rename {compare core-compare}
                  #?@(:cljs [:exclude [compare]]))
  (:require [clojure.walk :as w]
            [sicmutils.util :as u]
            [sicmutils.value :as v])
  #?(:clj
     (:import (clojure.lang IObj))))

(def ^{:doc "These keywords reference 'abstract' types that stand in for some
  concrete data type in the system."}
  abstract-types
  #{::numeric
    ::vector
    ::abstract-down
    ::abstract-matrix})

;; A Literal is a container type for literal expressions, abstract structures
;; that stand in for some other type. The canonical example is a symbolic
;; expression built out of Lisp data structures.
;;
;; Currently we only support these, but this type will be able to handle the
;; other abstract structures referenced in [[abstract-types]].

(deftype Literal [type expression m]
  v/Value
  (zero? [_]
    (and (v/number? expression)
         (v/zero? expression)))

  (one? [_]
    (and (v/number? expression)
         (v/one? expression)))

  (identity? [_]
    (and (v/number? expression)
         (v/one? expression)))

  (zero-like [_] 0)
  (one-like [_] 1)
  (identity-like [_] 1)
  (numerical? [_] (= type ::numeric))
  (exact? [_]
    (and (v/number? expression)
         (v/exact? expression)))
  (freeze [_] (v/freeze expression))
  (kind [_] type)

  Object
  (toString [_] (str expression))
  #?(:clj
     (equals [a b]
             (if (instance? Literal b)
               (let [b ^Literal b]
                 (and (= type (.-type b))
                      (= expression (.-expression b))
                      (= m (.-m b))))
               (v/eq a b))))

  #?@(:clj
      [IObj
       (meta [_] m)
       (withMeta [_ meta] (Literal. type expression meta))]

      :cljs
      [IMeta
       (-meta [_] m)

       IWithMeta
       (-with-meta [_ meta] (Literal. type expression meta))

       IEquiv
       (-equiv [a b]
               (if (instance? Literal b)
                 (let [b ^Literal b]
                   (and (= type (.-type b))
                        (= expression (.-expression b))
                        (= m (.-m b))))
                 (v/eq a b)))

       IPrintWithWriter
       (-pr-writer
        [_ writer opts]
        (-write writer (str expression)))]))

#?(:clj
   (defmethod print-method Literal [^Literal s ^java.io.Writer w]
     (.write w (.toString s))))

(defn make-literal
  "Constructs a [[Literal]] instance with the supplied type and an empty metadata
  map out of the literal form `expr`."
  [type expr]
  (->Literal type expr nil))

(defn literal-apply
  "Similar to [[make-literal]], but accepts:

  - some operation
  - the arguments to which it applies

  Similar to [[clojure.core/apply]].

  For example:

  (literal-apply ::numeric 'cos [1 2 3])
  ;;=> (cos 1 2 3)"
  [type op args]
  (make-literal type (cons op (seq args))))

(defn literal?
  "Returns true if `x` is a [[Literal]] instance, false otherwise."
  [x]
  (instance? Literal x))

(defn abstract?
  "Returns true if `x` is both a [[Literal]] and has a type specified
  in [[abstract-types]], false otherwise."
  [x]
  (and (literal? x)
       (contains? abstract-types
                  (.-type ^Literal x))))

(defn literal-type
  "If `x` is a [[Literal]] instance, returns its type. Else, returns nil."
  [x]
  (when (literal? x)
    (.-type ^Literal x)))

(defn fmap
  "Applies f to the expression part of e and creates from that a Literal
  otherwise like e."
  [f ^Literal e]
  (->Literal (.-type e)
             (f (.-expression e))
             (.-m e)))

;; ## Metadata
;;
;; TODO these need more docs and investigation.

(defn has-property? [literal k]
  (contains? (meta literal) k))

(defn get-property
  ([literal k]
   (get (meta literal) k))
  ([literal k default]
   (get (meta literal) k default)))

(defn with-property [x k v]
  (with-meta x (assoc (meta x) k v)))

(defn expression-of
  "If the supplied argument is a [[Literal]] (or a symbol, interpreted elsewhere
  as a numerical literal expression), returns the wrapped expression (or the
  symbol).

  Throws otherwise."
  [expr]
  (cond (literal? expr) (.-expression ^Literal expr)
        (symbol? expr)  expr
        :else (u/illegal (str "unknown expression type: " expr))))

;; ## Expression Walking

(defn variables-in
  "Return the set of 'variables' (e.g. symbols) found in `expr`. `expr` is either
  a symbol, a [[Literal]] instance or some sequence representing a symbolic
  expression."
  [expr]
  (cond (symbol? expr) #{expr}
        (literal? expr) (recur (expression-of expr))
        :else (into #{} (filter symbol?) (flatten expr))))

(defn evaluate
  "Walk the unwrapped expression x in postorder, replacing symbols found there
  with their values in the map environment, if present; the functions
  association is used for elements in function application position (first of a
  sequence)."
  [expr sym->var sym->f]
  (letfn [(walk [node]
            (cond (symbol? node) (sym->var node node)
                  (sequential? node)
                  (let [[f-sym & args] node]
                    (if-let [f (sym->f f-sym)]
                      (apply f (map walk args))
                      (u/illegal (str "Missing fn for symbol - " f-sym))))
                  :else node))]
    (walk expr)))

(defn substitute
  "Performs substitutions from the map."
  ([expr old new]
   (substitute expr {old new}))
  ([expr s-map]
   (w/postwalk-replace s-map expr)))

(defn compare
  "Compare expressions. The rule is that types have the following ordering:

  - empty sequence is < anything (except another empty seq)
  - real < symbol < string < sequence
  - sequences compare element-by-element

  Any types NOT in this list compare with the other type using hashes."
  [l r]
  (let [lseq?    (sequential? l)
        rseq?    (sequential? r)
        rsym?    (symbol? r)
        rstr?    (string? r)
        l-empty? (and lseq? (empty? l))
        r-empty? (and rseq? (empty? r))
        raw-comp (delay (core-compare (hash l) (hash r)))]
    (cond (and l-empty? r-empty?) 0
          l-empty?                -1
          r-empty?                1
          (v/real? l) (cond (v/real? r) (core-compare l r)
                            (or rsym? rstr? rseq?)
                            -1
                            :else @raw-comp)
          (v/real? r) 1

          (symbol? l) (cond rsym? (core-compare l r)
                            (or rstr? rseq?) -1
                            :else @raw-comp)
          rsym? 1

          (string? l) (cond rstr? (core-compare l r)
                            rseq? -1
                            :else @raw-comp)
          rstr? 1

          lseq? (if rseq?
                  (let [n1 (count l)
                        n2 (count r)]
                    (cond (< n1 n2) -1
                          (< n2 n1) 1
                          :else (let [head-compare (compare
                                                    (first l) (first r))]
                                  (if (zero? head-compare)
                                    (recur (rest l) (rest r))
                                    head-compare))))
                  @raw-comp)
          rseq? 1

          :else @raw-comp)))
