(ns sicmutils.env.sci
  (:refer-clojure :exclude [eval])
  (:require [sicmutils.env :as env]
            [sicmutils.abstract.function :as af #?@(:cljs [:include-macros true])]
            [sicmutils.calculus.coordinate :as cc #?@(:cljs [:include-macros true])]
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

(def public-fns (into {} (filter (complement (comp :macro meta second))) (ns-publics 'sicmutils.env)))
(def whitelisted-macros '#{literal-function with-literal-functions let-coordinates using-coordinates})
(def macros (into {} (comp (filter (comp :macro meta second))
                           (filter (comp whitelisted-macros first))) (ns-publics 'sicmutils.env)))

(defn make-sci-namespace [ns-name publics]
  (let [ns (sci/create-ns ns-name nil)]
    (into {} (map (fn [[var-name the-var]]
                    [var-name (copy-var the-var ns var-name)])) publics)))

(def namespaces
  ;; TODO: get 'let-coordinates and 'using-coordinates working
  {'sicmutils.env (make-sci-namespace 'sicmutils.env (dissoc (merge public-fns macros) 'let-coordinates 'using-coordinates))
   'sicmutils.abstract.function (make-sci-namespace 'sicmutils.abstract.function (select-keys (ns-publics 'sicmutils.abstract.function) whitelisted-macros))
   'sicmutils.calculus.coordinate (make-sci-namespace 'sicmutils.calculus.coordinate (select-keys (ns-publics 'sicmutils.calculus.coordinate) whitelisted-macros))})

(def opts {:bindings (get namespaces 'sicmutils.env)
           :namespaces (dissoc namespaces 'sicmutils.env)})

(def ctx (sci/init opts))

(comment
  (defn eval [form]
    (sci/eval-string* ctx (pr-str form)))

  (eval '(simplify (+ (square (sin 'x))
                      (square (cos 'x)))))

  (eval '(literal-function 'U))
  (eval '(do (defn L-central-polar [m U]
               (fn [[_ [r] [rdot φdot]]]
                 (- (* 1/2 m
                       (+ (square rdot)
                          (square (* r φdot))))
                    (U r))))
             (let [potential-fn (literal-function 'U)
                   L     (L-central-polar 'm potential-fn)
                   state (up (literal-function 'r)
                             (literal-function 'φ))]
               (->infix
                (simplify
                 (((Lagrange-equations L) state) 't)))))))
