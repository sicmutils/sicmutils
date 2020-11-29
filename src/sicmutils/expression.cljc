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
  (:require [sicmutils.util :as u]
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
                 (v/eq a b)))]))

#?(:clj
   (defmethod print-method Literal [^Literal s ^java.io.Writer w]
     (.write w (.toString s))))

(defn literal?
  "Returns true if the argument is a literal, false otherwise."
  [x]
  (instance? Literal x))

(defn literal-type [x]
  (when (literal? x)
    (.-type ^Literal x)))

(defn make-literal [type expr]
  (->Literal type expr {}))

(defn make-combination [type op args]
  (make-literal type (cons op (seq args))))

(defn fmap
  "Applies f to the expression part of e and creates from that a Literal
  otherwise like e."
  [f ^Literal e]
  (->Literal (.-type e)
             (f (.-expression e))
             (.-meta e)))

(defn abstract? [x]
  (and (literal? x)
       (contains? abstract-types
                  (.-type ^Literal x))))

(defn expression-of [expr]
  (cond (literal? expr) (.-expression ^Literal expr)
        (symbol? expr)  expr
        :else (u/illegal (str "unknown expression type: " expr))))

(defn variables-in
  "Return the 'variables' (e.g. symbols) found in the expression x,
  which is an unwrapped expression, as a set"
  [x]
  (if (symbol? x)
    #{x}
    (into #{} (filter symbol?) (flatten x))))

(defn walk-expression
  "Walk the unwrapped expression x in postorder, replacing symbols found there
  with their values in the map environment, if present; the functions association
  is used for elements in function application position (first of a sequence)."
  [x variables functions]
  (let [walk (fn walk [x]
               (cond (symbol? x) (or (variables x) x)
                     (sequential? x) (apply (functions (first x)) (map walk (rest x)))
                     :else x))]
    (walk x)))

(comment
  (define (substitute new old expression)
    (define (sloop exp)
      (cond ((equal? old exp) new)
            ((pair? exp)
             (cons (sloop (car exp))
                   (sloop (cdr exp))))
            ((vector? exp)
             ((vector-elementwise sloop) exp))
            (else exp)))
    (if (equal? new old) expression (sloop expression))))

(comment
  ;; Returns a checker that checks if we have a particular proerty...
  (define ((has-property? property-name) abstract-quantity)
    (cond ((pair? abstract-quantity)
           (assq property-name (cdr abstract-quantity)))
          ((symbol? abstract-quantity)
           (if (eq? property-name 'expression)
             (list 'expression abstract-quantity)
             (error "Symbols have only EXPRESSION properties")))
          (else
           (error "Bad abstract quantity"))))

  (define (get-property abstract-quantity property-name)
    (cond ((pair? abstract-quantity)
           (let ((default (if (default-object? default) false default))
                 (v (assq property-name (cdr abstract-quantity))))
             (if v (cadr v) default)))
          ((symbol? abstract-quantity)
           (if (eq? property-name 'expression)
             abstract-quantity
             default))
          (else
           (error "Bad abstract quantity"))))

  ;; TODO call this with-property
  (define (add-property! abstract-quantity property-name property-value)
    (if (pair? abstract-quantity)
      (set-cdr! (last-pair abstract-quantity)
                (list (list property-name property-value)))
      (error "Bad abstract quantity -- ADD-PROPERTY!"))))
