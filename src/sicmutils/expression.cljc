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

(ns sicmutils.expression
  (:refer-clojure :rename {compare core-compare}
                  #?@(:cljs [:exclude [compare]]))
  (:require [clojure.walk :as w]
            [sicmutils.util :as u]
            [sicmutils.value :as v]))

;; TODO talk about what "expression" means here, and how it's related to "freeze". Freeze is the thing that returns, finally, an expression.
;;
(def abstract-types
  #{::numeric
    ::vector
    ::abstract-down
    ::abstract-matrix})

(deftype Literal [type expression meta]
  v/Value
  (nullity? [_]
    (and (v/number? expression)
         (v/nullity? expression)))

  (unity? [_]
    (and (v/number? expression)
         (v/unity? expression)))

  (zero-like [_] 0)
  (one-like [_] 1)
  (numerical? [_] (= type ::numeric))
  (exact? [_] (and (v/number? expression)
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
                      (= meta (.-meta b))))
               (v/eq a b))))

  #?@(:cljs
      [IEquiv
       (-equiv [a b]
               (if (instance? Literal b)
                 (let [b ^Literal b]
                   (and (= type (.-type b))
                        (= expression (.-expression b))
                        (= meta (.-meta b))))
                 (v/eq a b)))

       IPrintWithWriter
       (-pr-writer
        [_ writer opts]
        (-write writer (str expression)))]))

#?(:clj
   (defmethod print-method Literal [^Literal s ^java.io.Writer w]
     (.write w (.toString s))))

(defn make-literal [type expr]
  (->Literal type expr {}))

(defn literal-apply [type op args]
  (make-literal type (cons op (seq args))))

(defn literal?
  "Returns true if the argument is a literal, false otherwise."
  [x]
  (instance? Literal x))

(defn abstract? [x]
  (and (literal? x)
       (contains? abstract-types
                  (.-type ^Literal x))))

(defn literal-type [x]
  (when (literal? x)
    (.-type ^Literal x)))

(defn fmap
  "Applies f to the expression part of e and creates from that a Literal
  otherwise like e."
  [f ^Literal e]
  (->Literal (.-type e)
             (f (.-expression e))
             (.-meta e)))

;; ## Metadata

(defn properties [x]
  (when (literal? x)
    (.-meta ^Literal x)))

(defn has-property? [literal k]
  (contains? (properties literal) k))

(defn get-property
  ([literal k]
   (get (properties literal) k))
  ([literal k default]
   (get (properties literal) k default)))

(defn with-property
  "TODO we probably want a merge version..."
  [x k v]
  {:pre [(literal? x)]}
  (let [x ^Literal x]
    (->Literal (.-type x)
               (.-expression x)
               (assoc (.-meta x) k v))))

(defn expression-of [expr]
  (cond (literal? expr) (.-expression ^Literal expr)
        (symbol? expr)  expr
        :else (u/illegal (str "unknown expression type: " expr))))

;; ## Expression Walking

(defn variables-in
  "Return the 'variables' (e.g. symbols) found in the expression x,
  which is a wrapped or unwrapped expression, as a set"
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

(defn- compare
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
