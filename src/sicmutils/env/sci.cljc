(ns sicmutils.env.sci
  (:refer-clojure :exclude [eval])
  (:require [clojure.set :as set]
            [sci.core :as sci]
            [sicmutils.env :as env]
            [sicmutils.abstract.function :as af #?@(:cljs [:include-macros true])]
            [sicmutils.calculus.coordinate :as cc #?@(:cljs [:include-macros true])]
            [sicmutils.calculus.vector-field :as vf]
            [sicmutils.calculus.form-field :as ff]
            [sicmutils.util :as u]))

(defn ->sci-var [[var-name the-var]]
  [var-name (cond-> (deref the-var)
              (-> the-var meta :macro)
              (with-meta {:sci/macro true}))])

(defn ->sci-ns [publics]
  (into {} (map ->sci-var) publics))

(defn literal-function
  ([_ _ f] `(af/literal-function ~f))
  ([_ _ f sicm-signature]
   (if (and (list? sicm-signature)
            (= '-> (first sicm-signature)))
     `(af/literal-function ~f '~sicm-signature)
     `(af/literal-function ~f ~sicm-signature)))
  ([_ _ f domain range] `(af/literal-function ~f ~domain ~range)))

(defn with-literal-functions
  [_ _ & args]
  `(af/with-literal-functions ~@args))

(defn let-coordinates
  [_ _ bindings & body]
  (when-not (even? (count bindings))
    (u/illegal "let-coordinates requires an even number of bindings"))
  (let [pairs (partition 2 bindings)
        prototypes (map first pairs)
        c-systems (mapv second pairs)
        coordinate-names (mapcat #'cc/symbols-from-prototype prototypes)
        coordinate-vector-field-names (map vf/coordinate-name->vf-name coordinate-names)
        coordinate-form-field-names (map ff/coordinate-name->ff-name coordinate-names)]
    `(let [[~@c-systems :as c-systems#]
           (mapv m/with-coordinate-prototype
                 ~c-systems
                 ~(mapv #(sicmutils.calculus.coordinate/quotify-coordinate-prototype identity %) prototypes))
           c-fns# (map coordinate-functions c-systems#)
           c-vfs# (map vf/coordinate-basis-vector-fields c-systems#)
           c-ffs# (map ff/coordinate-basis-oneform-fields c-systems#)
           ~(vec coordinate-names) (flatten c-fns#)
           ~(vec coordinate-vector-field-names) (flatten c-vfs#)
           ~(vec coordinate-form-field-names) (flatten c-ffs#)]
       ~@body)))

(defn using-coordinates
  [_ _ coordinate-prototype coordinate-system & body]
  `(let-coordinates [~coordinate-prototype ~coordinate-system] ~@body))

(defn macrofy [f]
  (with-meta f {:sci/macro true}))

(def namespaces
  {'sicmutils.env (-> 'sicmutils.env
                      ns-publics
                      (dissoc 'literal-function
                              'with-literal-functions
                              'bootstrap-repl!
                              'let-coordinates
                              'using-coordinates)
                      ->sci-ns
                      (merge {'literal-function (macrofy literal-function)
                              'with-literal-functions (macrofy with-literal-functions)
                              'let-coordinates (macrofy let-coordinates)
                              'using-coordinates (macrofy using-coordinates)}))
   'sicmutils.abstract.function (-> 'sicmutils.abstract.function ns-publics ->sci-ns)
   'sicmutils.calculus.coordinate (-> 'sicmutils.calculus.coordinate
                                      ns-publics
                                      ->sci-ns
                                      (merge {'let-coordinates (macrofy let-coordinates)
                                              'using-coordinates (macrofy using-coordinates)}))})

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
