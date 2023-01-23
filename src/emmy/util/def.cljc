#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.util.def
  #?(:clj
     (:import (clojure.lang Keyword RT)))
  #?(:cljs
     (:require-macros [emmy.util.def])))

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
                    (str "generic " f ))
        options   (if (string? (first options))
                    (next options)
                    options)
        [attr options] (if (map? (first options))
                         [(first options) (next options)]
                         [{} options])
        kwd-klass (fork :clj Keyword :cljs 'cljs.core/Keyword)
        attr (assoc attr
                    :arity arity
                    :name (:name attr `'~f))]
    `(do
       (defmulti ~f
         ~docstring
         {:arglists '~(arglists a b)}
         v/argument-kind ~@options)
       (defmethod ~f [~kwd-klass] [k#]
         (~attr k#)))))

#?(:clj
   (defn link-vars
     "Makes sure that all changes to `src` are reflected in `dst`.

  NOTE that [[link-vars]] comes
  from [`potemkin.namespaces`](https://github.com/clj-commons/potemkin/blob/master/src/potemkin/namespaces.clj);
  we import it here to avoid importing the full library."
     [src dst]
     (add-watch
      src dst
      (fn [_ src _old _new]
        (alter-var-root dst (constantly @src))
        (alter-meta! dst merge (dissoc (meta src) :name))))))

#?(:clj
   (defmacro import-macro
     "Given a macro in another namespace, defines a macro with the same name in
   the current namespace. Argument lists, doc-strings, and original line-numbers
   are preserved.

  NOTE that [[import-macro]] comes
  from [`potemkin.namespaces`](https://github.com/clj-commons/potemkin/blob/master/src/potemkin/namespaces.clj);
  we import it here to avoid importing the full library."
     ([sym]
      `(import-macro ~sym nil))
     ([sym name]
      (let [vr (resolve sym)
            m (meta vr)
            n (or name (with-meta (:name m) {}))]
        (when-not vr
          (throw (IllegalArgumentException. (str "Don't recognize " sym))))
        (when-not (:macro m)
          (throw (IllegalArgumentException.
                  (str "Calling import-macro on a non-macro: " sym))))
        `(do
           (def ~n ~(resolve sym))
           (alter-meta! (var ~n) merge (dissoc (meta ~vr) :name))
           (.setMacro (var ~n))
           (link-vars ~vr (var ~n))
           ~vr)))))

(defmacro import-def
  "Given a regular def'd var from another namespace, defined a new var with the
   same name in the current namespace.

  NOTE that this macro is taken
  from [`potemkin.namespaces/import-def`](https://github.com/clj-commons/potemkin/blob/master/src/potemkin/namespaces.clj)
  with an additional internal branch for ClojureScript support. but meant to be
  usable from ClojureScript. In ClojureScript, it's not possible to:

  - alter the metadata of a var after definition
  - call `resolve` at macro-time

  And therefore not possible to mirror the metadata from one var to another.
  This simplified version therefore suffices in the cljs case."
  ([sym]
   `(import-def ~sym nil))
  ([sym var-name]
   (fork
    :cljs
    (let [n (or var-name (symbol (name sym)))]
      `(def ~n ~sym))

    :clj
    (let [vr (resolve sym)
          m  (meta vr)
          n  (or var-name (:name m))
          n  (with-meta n (if (:dynamic m) {:dynamic true} {}))]
      (when-not vr
        (throw (IllegalArgumentException. (str "Don't recognize " sym))))
      (when (:macro m)
        (throw (IllegalArgumentException.
                (str "Calling import-def on a macro: " sym))))
      `(do
         (def ~n @~vr)
         (alter-meta! (var ~n) merge (dissoc (meta ~vr) :name))
         (link-vars ~vr (var ~n))
         ~vr)))))

(defmacro import-vars
  "import multiple defs from multiple namespaces. works for vars and fns, macros
  only work in Clojure.

  NOTE that [[import-vars]] is a copy
  of [`potemkin.namespaces/import-vars`](https://github.com/clj-commons/potemkin/blob/master/src/potemkin/namespaces.clj),
  with an additional fork for ClojureScript support. The syntax is the same as
  Potemkin's macro:

   ```clj
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
  (fork
   :cljs
   `(do
      ~@(for [[from-ns & defs] imports
              d defs
              :let [sym (symbol (str from-ns)
                                (str d))]]
          `(def ~d ~sym)))

   :clj
   (letfn [(unravel [x]
             (if (sequential? x)
               (->> x
                    rest
                    (mapcat unravel)
                    (map
                     #(symbol
                       (str (first x)
                            (when-let [n (namespace %)]
                              (str "." n)))
                       (name %))))
               [x]))]
     (let [imports (mapcat unravel imports)]
       `(do
          ~@(map
             (fn [sym]
               (let [vr (resolve sym)
                     m  (meta vr)]
                 (cond
                   (nil? vr)     `(throw (ex-info (format "`%s` does not exist" '~sym) {}))
                   (:macro m)    `(import-macro ~sym)
                   :else         `(import-def ~sym))))
             imports))))))

#_{:clj-kondo/ignore [:redundant-fn-wrapper]}
(defn careful-def
  "Given some namespace `ns`, returns a function of some binding symbol and a form
  to bind. The function returns either

  - A form like `(def ~sym ~form)`, if `sym` is not currently bound into `ns`

  - If `sym` is bound already, returns a form that emits a warning and then
    uses `ns-unmap` and `intern` to reassign the binding.

  In Clojure, this behavior matches redefinitions of symbols bound in
  `clojure.core`. Symbols bound with `def` that are already imported from other
  namespaces cause an exception, hence this more careful workaround.

  (In ClojureScript, only forms like `(def ~sym ~form)` are emitted, since the
  compiler does not currently error in case 2 and already handles emitting the
  warning for us.)"
  [#?(:clj ns :cljs _)]
  #?(:cljs
     (fn [sym form]
       `(def ~sym ~form))

     :clj
     (let [ns-sym (ns-name ns)
           nsm (ns-map ns)
           remote? (fn [sym]
                     (when-let [v (nsm sym)]
                       (not= ns (:ns (meta v)))))
           warn (fn [sym]
                  `(.println
                    (RT/errPrintWriter)
                    (str "WARNING: "
                         '~sym
                         " already refers to: "
                         ~(nsm sym)
                         " in namespace: "
                         '~ns-sym
                         ", being replaced by: "
                         ~(str "#'" ns-sym "/" sym))))]
       (fn [sym form]
         (if (remote? sym)
           `(do
              ~(warn sym)
              (ns-unmap '~ns-sym '~sym)
              (intern '~ns-sym '~sym ~form))
           `(def ~sym ~form))))))
