(ns sicmutils.env.sci
  (:require [sicmutils.env :as env]
            [sci.core :as sci]))

(defn copy-var
  ([the-var ns-obj ns-name]
   (let [val (deref the-var)
         m (-> the-var meta)
         name (:name m)
         name (symbol (str ns-name) (str name))
         new-m {:doc (:doc m)
                :name name
                :arglists (:arglists m)
                :ns ns-obj}]
     (cond (:dynamic m)
           (sci/new-dynamic-var name val new-m)
           (:macro m)
           (sci/new-macro-var name val new-m)
           :else (sci/new-var name val new-m)))))

(def publics (into {} (filter (complement (comp :macro meta second))) (ns-publics 'sicmutils.env)))
(def publics-macros (dissoc (into {} (filter (comp :macro meta second)) (ns-publics 'sicmutils.env)) 'bootstrap-repl!))
(def sci-ns (sci/create-ns 'sicmutils.env nil))
(def sci-namespace (zipmap (keys publics) (map (fn [the-var]
                                                 (copy-var the-var sci-ns 'sicmutils.env))
                                               (vals publics))))
(sci/eval-string "(simplify (+ (square (sin 'x))
                               (square (cos 'x))))" {:bindings {'clojure.set2 set-namespace}})
