(ns sicmutils.env.sci
  (:refer-clojure :exclude [eval])
  (:require [clojure.set :as set]
            [sci.core :as sci]
            [sicmutils.env.sci.macros :as macros]
            [sicmutils.env :as env]
            [sicmutils.abstract.function :as af]
            [sicmutils.calculus.coordinate :as cc]))

(defn ->sci-var [[var-name the-var]]
  [var-name (cond-> (deref the-var)
              (-> the-var meta :macro)
              (with-meta {:sci/macro true}))])

(defn ->sci-ns [publics]
  (into {} (map ->sci-var) publics))

(def namespaces
  {'sicmutils.env (-> 'sicmutils.env
                      ns-publics
                      (dissoc 'literal-function
                              'with-literal-functions
                              'bootstrap-repl!
                              'let-coordinates
                              'using-coordinates)
                      ->sci-ns

                      (merge (select-keys macros/all ['literal-function
                                                      'with-literal-functions
                                                      'let-coordinates
                                                      'using-coordinates])))
   'sicmutils.abstract.function (-> 'sicmutils.abstract.function ns-publics ->sci-ns)
   'sicmutils.calculus.coordinate (-> 'sicmutils.calculus.coordinate
                                      ns-publics
                                      ->sci-ns
                                      (merge (select-keys macros/all ['let-coordinates
                                                                      'using-coordinates])))})

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
