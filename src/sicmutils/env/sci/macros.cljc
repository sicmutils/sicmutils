(ns sicmutils.env.sci.macros
  (:require [sicmutils.abstract.function :as af #?@(:cljs [:include-macros true])]
            [sicmutils.calculus.coordinate :as cc #?@(:cljs [:include-macros true])]
            [sicmutils.calculus.vector-field :as vf]
            [sicmutils.calculus.form-field :as ff]
            [sicmutils.util :as u]))

(defn literal-function
  ([_ _ f] `(af/literal-function ~f))
  ([_ _ f sicm-signature]
   (if (and (list? sicm-signature)
            (= '-> (first sicm-signature)))
     `(af/literal-function ~f '~sicm-signature)
     `(af/literal-function ~f ~sicm-signature)))
  ([_ _ f domain range]
   `(af/literal-function ~f ~domain ~range)))

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
                 ~(mapv #(#'sicmutils.calculus.coordinate/quotify-coordinate-prototype identity %) prototypes))
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

(def all
  {'literal-function (macrofy literal-function)
   'with-literal-functions (macrofy with-literal-functions)
   'let-coordinates (macrofy let-coordinates)
   'using-coordinates (macrofy using-coordinates)})
