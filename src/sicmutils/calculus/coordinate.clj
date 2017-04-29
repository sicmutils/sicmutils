(ns sicmutils.calculus.coordinate
  (:require [sicmutils
             [structure :as s]]
            [sicmutils.calculus
             [manifold :refer :all]
             [vector-field :refer :all]]))

(defn coordinate-functions
  [coordinate-system coordinate-prototype]
  (s/mapr (fn [coordinate-name access-chain]
            (comp (apply s/component access-chain)
                  #(point->coords coordinate-system %)))
          coordinate-prototype
          (s/structure->access-chains coordinate-prototype)))

(defn ^:private quotify-coordinate-prototype
  [p]
  (cond (and (sequential? p)
             ('#{up down} (first p))) `(~(first p) ~@(map quotify-coordinate-prototype (rest p)))
        (vector? p) (mapv quotify-coordinate-prototype p)
        (symbol? p) `'~p
        :else (throw (IllegalArgumentException. "Invalid coordinate prototype"))))

(defn ^:private symbols-from-prototype
  [p]
  (cond (and (sequential? p)
             ('#{up down} (first p))) (mapcat symbols-from-prototype (rest p))
        (vector? p) (mapcat symbols-from-prototype p)
        (symbol? p) `(~p)
        :else (throw (IllegalArgumentException. (str "Invalid coordinate prototype: " p)))))

(defmacro using-coordinates
  "Example:
    (using-coordinates (up x y) R2-rect
      body...)"
  [coordinate-prototype coordinate-system & body]
  (let [qcp (quotify-coordinate-prototype coordinate-prototype)
        coordinates (symbols-from-prototype coordinate-prototype)]
    `(let [prototype# ~qcp
           c-fns# (coordinate-functions ~coordinate-system prototype#)
           f# (fn ~(vec coordinates) ~@body)]
       (apply f# (flatten c-fns#)))))

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
        coordinates (mapcat symbols-from-prototype prototypes)
        coordinate-vector-field-names (map #(symbol (str "d:d" %)) coordinates)
        ]
    `(let [prototypes# ~(mapv quotify-coordinate-prototype prototypes)
           c-systems# ~c-systems
           c-fns# (map coordinate-functions c-systems# prototypes#)
           c-vfs# (map coordinate-basis-vector-fields c-systems# prototypes#)
           f# (fn ~(into [] (concat coordinates
                                    coordinate-vector-field-names))
                ~@body)]
       (apply f# (mapcat flatten (concat c-fns# c-vfs#))))))
