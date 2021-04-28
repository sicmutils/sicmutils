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

(ns sicmutils.util.def
  (:refer-clojure :rename {defmethod core-defmethod}))

(defmacro ^:no-doc fork
  "I borrowed this lovely, mysterious macro from `macrovich`:
  https://github.com/cgrand/macrovich. This allows us to fork behavior inside of
  a macro at macroexpansion time, not at read time."
  [& {:keys [cljs clj]}]
  (if (contains? &env '&env)
    `(if (:ns ~'&env) ~cljs ~clj)
    (if #?(:clj (:ns &env) :cljs true)
      cljs
      clj)))

(def ^:no-doc lowercase-symbols
  (map (comp symbol str char)
       (range 97 123)))

(defn ^:no-doc arglists
  "returns a list of `:arglists` entries appropriate for a generic function with
  arities between `a` and `b` inclusive."
  [a b]
  (let [arities (if b
                  (range a (inc b))
                  [a])]
    (map #(into [] (take %) lowercase-symbols)
         arities)))

(defn method-dispatched [multimethod & args]
  (get (clojure.set/map-invert (.getMethodTable multimethod))
       (.getMethod multimethod
                   (apply (.dispatchFn multimethod) args))))

(defn method-dispatch [multimethod & args]
  (apply (.dispatchFn multimethod) args))

(defmacro defgeneric
  "Defines a multifn using the provided symbol. Arranges for the multifn
  to answer the :arity message, reporting either `[:exactly a]` or
  `[:between a b]` according to the arguments given.

  - `arities` can be either a single or a vector of 2 numbers.

  The `options` allowed differs slightly from `defmulti`:

  - the first optional argument is a docstring.

  - the second optional argument is a dict of metadata. When you query the
  defined multimethod with a keyword, it will pass that keyword along as a query
  to this metadata map. (`:arity` is always overridden if supplied, and `:name`
  defaults to the symbol `f`.)

  Any remaining options are passed along to `defmulti`."
  {:arglists '([name arities docstring? attr-map? & options])}
  [f arities & options]
  (let [[a b]     (if (vector? arities) arities [arities])
        arity     (if b [:between a b] [:exactly a])
        docstring (if (string? (first options))
                    (str "generic " f ".\n\n" (first options))
                    (str "generic " f))
        options   (if (string? (first options))
                    (next options)
                    options)
        [attr options] (if (map? (first options))
                         [(first options) (next options)]
                         [{} options])
        kwd-klass (fork :clj clojure.lang.Keyword :cljs 'cljs.core/Keyword)
        attr (assoc attr
                    :arity arity
                    :name (:name attr `'~f))]
    `(do
       (defmulti ~f
         ~docstring
         {:arglists '~(arglists a b)}
         v/argument-kind ~@options)
       (defn ~(symbol (str (name f) "-selected")) [& args#]
         (apply method-dispatched ~f args#))
       (core-defmethod ~f [~kwd-klass] [k#]
                       (if (= :specifics k#)
                         (keys (.getMethodTable ~f))
                         (~attr k#))))))

(def ^:dynamic *debug-generics* (= (System/getProperty "sicmutils.debug_generics") "true"))
(def generic-call-id (atom 0))
(def ^:dynamic *generic-call-chain* [])
(def ^:dynamic *generic-call-tap?* (constantly false))
(def ^:dynamic *on-generic-call* (constantly nil))

(defmacro defmethod [multifn dispatch-val & fn-tail]
  (if *debug-generics*
    `(let [call-info# {:dispatch ~dispatch-val :name (if (method-dispatched ~multifn :name)
                                                       (~multifn :name)
                                                       '~multifn)}]
       (core-defmethod ~multifn ~dispatch-val ~@(butlast fn-tail)
                       (let [call-id# (swap! generic-call-id inc)
                             call-info# (assoc call-info# :id call-id# :parent-id (:id (last *generic-call-chain*)))
                             tap?# (*generic-call-tap?* call-info#)]
                         (when tap?#
                           (*on-generic-call*
                            (assoc (try (assoc call-info#
                                               :args ~(first fn-tail)
                                               :dispatch-with (method-dispatch ~multifn ~@(first fn-tail)))
                                        ;; If the function destructures I'm not going to try to figure that out.
                                        (catch Exception e# call-info#))
                                   :chain *generic-call-chain*)))
                         (let [result#
                               (binding [*generic-call-chain* (conj *generic-call-chain* call-info#)]
                                 ~(last fn-tail))]
                           (when tap?#
                             (*on-generic-call* (assoc call-info# :result result#)))
                           result#))))
    `(core-defmethod ~multifn ~dispatch-val ~@fn-tail)))

(defmacro import-def
  "Given a regular def'd var from another namespace, defined a new var with the
   same name in the current namespace.

  This macro is modeled after `potemkin.namespaces/import-def` but meant to be
  usable from Clojurescript. In Clojurescript, it's not possible to:

  - alter the metadata of a var after definition
  - call `resolve` at macro-time

  And therefore not possible to mirror the metadata from one var to another.
  This simplified version therefore suffices in the cljs case."
  ([sym]
   `(import-def ~sym nil))
  ([sym var-name]
   (let [n (or var-name (symbol (name sym)))]
     `(def ~n ~sym))))

(defmacro import-vars
  "import multiple defs from multiple namespaces. works for vars and fns. not
  macros.

  [[import-vars]] has the same syntax as `potemkin.namespaces/import-vars`:

   ```clojure
  (import-vars
     [m.n.ns1 a b]
     [x.y.ns2 d e f]) =>
   (def a m.n.ns1/a)
   (def b m.n.ns1/b)
    ...
   (def d m.n.ns2/d)
    ... etc
  ```"
  [& imports]
  (let [expanded-imports (for [[from-ns & defs] imports
                               d defs
                               :let [sym (symbol (str from-ns)
                                                 (str d))]]
                           `(def ~d ~sym))]
    `(do ~@expanded-imports)))
