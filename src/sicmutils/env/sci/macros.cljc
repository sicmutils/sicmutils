;;
;; Copyright © 2020 Colin Smith.
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

(ns sicmutils.env.sci.macros
  "This namespace contains reimplementations of various macros from sicmutils,
  defined in the form required by SCI."
  (:require [pattern.consequence :as pc]
            [pattern.rule :as r]
            [pattern.syntax :as ps]
            [sicmutils.abstract.function :as af]
            [sicmutils.calculus.coordinate :as cc]
            [sicmutils.calculus.manifold :as m]
            [sicmutils.calculus.vector-field :as vf]
            [sicmutils.calculus.form-field :as ff]
            [sicmutils.util :as u]))

;; ## Pattern Matching Macros

(defn pattern
  "Originally defined in `pattern.rule`."
  ([_ _ form]
   `(r/pattern*
     ~(ps/compile-pattern form)))
  ([_ _ form pred]
   `(r/pattern*
     ~(ps/compile-pattern form)
     ~@(when pred [pred]))))

(defn consequence
  "Originally defined in `pattern.rule`."
  [_ _ form]
  (let [sym (gensym)]
    `(fn [~sym]
       ~(pc/compile-skeleton sym form))))

(defn template
  "Originally defined in `pattern.rule`."
  ([_ _ form]
   (pc/compile-skeleton (gensym) form))
  ([_ _ m form]
   (let [sym (gensym)]
     `(let [~sym ~m]
        ~(pc/compile-skeleton sym form)))))

(defn rule
  ([_ _ pattern consequent-fn]
   (r/compile-rule pattern consequent-fn))
  ([_ _ pattern pred skeleton]
   (r/compile-rule pattern pred skeleton)))

(defn ruleset
  "Originally defined in `pattern.rule`."
  [_ _ & patterns-and-consequences]
  {:pre (zero? (mod (count patterns-and-consequences) 3))}
  (let [inputs (partition 3 patterns-and-consequences)
        rules  (map #(apply r/compile-rule %) inputs)]
    `(r/ruleset* ~@rules)))

;; ## SICMUtils Macros

(defn literal-function
  "Originally defined in `sicmutils.env`."
  ([_ _ f] `(af/literal-function ~f))
  ([_ _ f sicm-signature]
   (if (and (list? sicm-signature)
            (= '-> (first sicm-signature)))
     `(af/literal-function ~f '~sicm-signature)
     `(af/literal-function ~f ~sicm-signature)))
  ([_ _ f domain range]
   `(af/literal-function ~f ~domain ~range)))

(defn with-literal-functions
  "Originally defined in `sicmutils.abstract.function`."
  [_ _ litfns & body]
  (let [pairs    (af/binding-pairs litfns)
        bindings (into [] cat pairs)]
    `(let ~bindings ~@body)))

(defn let-coordinates
  "Originally defined in `sicmutils.calculus.coordinate`."
  [_ _ bindings & body]
  (when-not (even? (count bindings))
    (u/illegal "let-coordinates requires an even number of bindings"))
  (let [pairs                         (partition 2 bindings)
        prototypes                    (map first pairs)
        c-systems                     (map second pairs)
        system-names                  (map (comp symbol name) c-systems)
        coordinate-names              (mapcat cc/symbols-from-prototype prototypes)
        coordinate-vector-field-names (map vf/coordinate-name->vf-name coordinate-names)
        coordinate-form-field-names   (map ff/coordinate-name->ff-name coordinate-names)]
    `(let [[~@c-systems :as c-systems#]
           (mapv m/with-coordinate-prototype
                 ~(into [] c-systems)
                 ~(mapv #(cc/quotify-coordinate-prototype identity %) prototypes))

           ~(into [] coordinate-names)
           (flatten
            (map cc/coordinate-functions c-systems#))

           ~(into [] coordinate-vector-field-names)
           (flatten
            (map vf/coordinate-system->vector-basis c-systems#))

           ~(into [] coordinate-form-field-names)
           (flatten
            (map ff/coordinate-system->oneform-basis c-systems#))]
       ~@body)))

(defn using-coordinates
  "Originally defined in `sicmutils.calculus.coordinate`."
  [env form coordinate-prototype coordinate-system & body]
  (apply let-coordinates
         env form
         [coordinate-prototype coordinate-system]
         body))

(defn- tag-as-macro [f]
  (vary-meta f assoc :sci/macro true))

(def all
  {'literal-function       (tag-as-macro literal-function)
   'with-literal-functions (tag-as-macro with-literal-functions)
   'let-coordinates        (tag-as-macro let-coordinates)
   'using-coordinates      (tag-as-macro using-coordinates)})

(def pattern-macros
  {'pattern     (tag-as-macro pattern)
   'consequence (tag-as-macro consequence)
   'template    (tag-as-macro template)
   'rule        (tag-as-macro rule)
   'ruleset     (tag-as-macro ruleset)})

(def ns-bindings
  {'pattern.rule pattern-macros

   'sicmutils.env all

   'sicmutils.abstract.function
   (select-keys all ['with-literal-functions])

   'sicmutils.calculus.coordinate
   (select-keys all ['let-coordinates 'using-coordinates])})
