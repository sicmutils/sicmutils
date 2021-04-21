;;
;; Copyright © 2021 Sam Ritchie.
;; This work is based on the Scmutils system of MIT/GNU Scheme:
;; Copyright © 2002 Massachusetts Institute of Technology
;;
;; This is free software;  you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or (at
;; your option) any later version.
;;
;; This software is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this code; if not, see <http://www.gnu.org/licenses/>.
;;

(ns pattern.consequence
  (:require [pattern.syntax :as ps]
            [sicmutils.util :as u]))

;; ### Consequence Functions
;;
;; The contract for a "consequence" function is that it can return `false` or
;; `nil` to signal failure. But what if the function wants to succeed with those
;; values?
;;
;; Wrapping a return value with [[succeed]] will allow a successful return of
;; those values. This only matters for skeleton compilation if the skeleton is
;; identical to `nil` or `false`. In those cases, the returned function will
;; produced `(succeed nil)` or `(succeed false)`.

(defn succeed
  "Wraps the argument `x` in a form that will always successfully return from a
  consequence function, whatever its value.

  Use [[succeed]] to return `nil` or `false` from a consequence function. For
  all other return values, returning `(succeed x)` is identical to returning
  `x`"
  [x]
  {::succeed x})

(defn unwrap
  "Given a form returned by a consequence function, unwraps the top level
  `succeed` wrapper if present to return the final value."
  [x]
  (if (map? x)
    (::succeed x x)
    x))

;; ### Skeleton
;;
;; A Skeleton is a template form that we can transform into a function of a
;; matcher's binding map, called a "consequence". The function should take the
;; binding map and return a copy of the skeleton with:
;;
;; - all variable binding forms replaced by their entries in the binding map
;; - same with any segment binding form, with the added note that these should
;;   be spliced in
;; - any `unquote` or `unquote-splicing` forms respected
;;
;; Any non-binding symbol will be quoted.

(defn- apply-form
  "Given symbols `f` representing a function and `m` representing its argument,
  returns a form that represents function application.

  Symbols are quoted, [[unquote?]] forms are included without quote and all
  other forms are left untouched."
  [f x]
  (let [f (cond (simple-symbol? f) `(quote ~f)
                (ps/unquote? f) (ps/unquoted-form f)
                :else f)]
    (list f x)))

(defn compile-skeleton
  "Takes a skeleton expression `skel` and returns a form that will evaluate to a
  function from a pattern matcher's binding map to a data structure of identical shape to `skel`, with

  - all variable binding forms replaced by their entries in the binding map
  - same with any segment binding form, with the added note that these should
    be spliced in
  - any `unquote` or `unquote-splicing` forms respected

  NOTE: reverse-segment variables are NOT evaluated here; these currently only
  apply when matching an already-bound segment variable."
  [skel]
  (let [frame-sym (gensym)]
    (letfn [(compile-sequential [xs]
              (let [acc (ps/splice-reduce (some-fn ps/segment? ps/unquote-splice?)
                                          compile xs)]
                (cond (empty? acc) ()
                      (= 1 (count acc)) (first acc)
                      :else `(concat ~@acc))))

            (compile [form]
              (cond (or (ps/binding? form)
                        (ps/segment? form))
                    (let [v (ps/variable-name form)]
                      (apply-form v frame-sym))

                    (symbol? form) (list 'quote form)

                    (ps/unquote? form)
                    (ps/unquoted-form form)

                    (ps/unquote-splice? form)
                    (into [] (ps/unquoted-form form))

                    (map? form)
                    (u/map-vals compile form)

                    (vector? form)
                    `(vec ~(compile-sequential form))

                    (sequential? form)
                    (if (empty? form)
                      form
                      `(seq ~(compile-sequential form)))

                    :else form))]
      (if skel
        `(fn [~frame-sym]
           ~(compile skel))
        `(fn [_#]
           (succeed ~skel))))))

(comment
  (= [] ((consequence []) {}))
  (= () ((consequence ()) {})))
