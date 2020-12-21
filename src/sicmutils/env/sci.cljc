(ns sicmutils.env.sci
  (:refer-clojure :exclude [eval])
  (:require [clojure.set :as set]
            [sci.core :as sci]
            [sicmutils.env :as env]
            [sicmutils.abstract.function :as af #?@(:cljs [:include-macros true])]
            [sicmutils.calculus.coordinate :as cc #?@(:cljs [:include-macros true])]))

(defn ->sci-var [[var-name the-var]]
  [var-name (cond-> @the-var
              (-> the-var meta :macro)
              (with-meta {:sci/macro true}))])

(defn make-sci-namespace
  ([ns-name]
   (make-sci-namespace ns-name (ns-publics ns-name)))
  ([_ns-name publics]
   (into {} (map ->sci-var) publics)))

(def namespaces
  {'sicmutils.env (merge (make-sci-namespace 'sicmutils.env (-> (ns-publics 'sicmutils.env)
                                                                (dissoc 'bootstrap-repl! 'let-coordinates 'using-coordinates)))
                         (make-sci-namespace 'sicmutils.calculus.coordinate
                                             (select-keys (ns-publics 'sicmutils.calculus.coordinate)
                                                          ['let-coordinates 'using-coordinates])))
   'sicmutils.abstract.function (make-sci-namespace 'sicmutils.abstract.function)
   'sicmutils.calculus.coordinate (make-sci-namespace 'sicmutils.calculus.coordinate)})

(def opts {:namespaces (set/rename-keys namespaces {'sicmutils.env 'user})})

(def ctx (sci/init opts))

(comment
  (defn eval [form]
    (sci/eval-string* ctx (pr-str form)))

  (eval '(simplify (+ (square (sin 'x))
                      (square (cos 'x)))))

  (eval '(->TeX (simplify (+ (square (sin (square 'x)))
                             (square (cos 'x))))))

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
               (->TeX
                (simplify
                 (((Lagrange-equations L) state) 't)))))))
