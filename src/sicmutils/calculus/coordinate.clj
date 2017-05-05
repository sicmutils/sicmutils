(ns sicmutils.calculus.coordinate
  (:require [sicmutils
             [structure :as s]]
            [sicmutils.calculus
             [manifold :refer :all]
             [vector-field :refer :all]
             [form-field :refer :all]]))

(defn coordinate-functions
  [coordinate-system coordinate-prototype]
  (s/mapr (fn [access-chain]
            (comp (apply s/component access-chain)
                  #(point->coords coordinate-system %)))
          (s/structure->access-chains coordinate-prototype)))

(defn ^:private quotify-coordinate-prototype
  "Scmutils wants to allow forms like this:
     (using-coordinates (up x y) R2-rect ...)
   Note that x, y are unquoted. This function converts such an unquoted for
   into a quoted one that could be evaluated to return an up-tuple of the symbols:
     (up 'x 'y)
   Such an object is useful for s/mapr. The function xf is applied before quoting."
  [xf p]
  (let [q (fn q [p]
            (cond (and (sequential? p)
                       ('#{up down} (first p))) `(~(first p) ~@(map q (rest p)))
                  (vector? p) (mapv q p)
                  (symbol? p) `'~(xf p)
                  :else (throw (IllegalArgumentException. "Invalid coordinate prototype"))))]
    (q p)))

(defn ^:private symbols-from-prototype
  [p]
  (cond (and (sequential? p)
             ('#{up down} (first p))) (mapcat symbols-from-prototype (rest p))
        (vector? p) (mapcat symbols-from-prototype p)
        (symbol? p) `(~p)
        :else (throw (IllegalArgumentException. (str "Invalid coordinate prototype: " p)))))

(defmacro let-coordinates
  "Example:
    (let-coordinates [[x y] R2-rect
                      [r theta] R2-polar]
      body...)"
  [bindings & body]
  (when-not (even? (count bindings))
    (throw (IllegalArgumentException. "let-coordinates requires an even number of bindings")))
  (let [pairs (partition 2 bindings)
        prototypes (map first pairs)
        c-systems (mapv second pairs)
        coordinate-names (mapcat symbols-from-prototype prototypes)
        coordinate-vector-field-names (map coordinate-name->vf-name coordinate-names)
        coordinate-form-field-names (map coordinate-name->ff-name coordinate-names)
        ]
    `(let [prototypes# ~(mapv #(quotify-coordinate-prototype identity %) prototypes)
           vf-prototypes# ~(mapv #(quotify-coordinate-prototype coordinate-name->vf-name %) prototypes)
           ff-prototypes# ~(mapv #(quotify-coordinate-prototype coordinate-name->ff-name %) prototypes)
           c-systems# ~c-systems
           c-fns# (map coordinate-functions c-systems# prototypes#)
           c-vfs# (map coordinate-basis-vector-fields c-systems# vf-prototypes#)
           c-ffs# (map coordinate-basis-oneform-fields c-systems# ff-prototypes#)
           f# (fn ~(into [] (concat coordinate-names
                                    coordinate-vector-field-names
                                    coordinate-form-field-names))
                ~@body)]
       (apply f# (mapcat flatten (concat c-fns# c-vfs# c-ffs#))))))

(defmacro using-coordinates
  "Example:
    (using-coordinates (up x y) R2-rect
      body...)

  Note: this is just a macro wrapping let-coordinates, the use of which is
  preferred."
  [coordinate-prototype coordinate-system & body]
  `(let-coordinates [~coordinate-prototype ~coordinate-system] ~@body))
