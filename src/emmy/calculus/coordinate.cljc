#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.calculus.coordinate
  (:require [emmy.calculus.form-field :as ff]
            [emmy.calculus.manifold :as m]
            [emmy.calculus.vector-field :as vf]
            [emmy.structure :as s]
            [emmy.util :as u]
            [emmy.util.def :as ud])
  #?(:cljs
     (:require-macros [emmy.calculus.coordinate])))

(defn coordinate-functions
  "Returns a structure similar to the [[manifold/coordinate-prototype]] of
  `coordinate-system`, where every entry is a function from manifold point =>
  the associated component of the point in the coordinate representation
  described by `coordinate-system`."
  [coordinate-system]
  (let [prototype (m/coordinate-prototype coordinate-system)]
    (s/map-chain (fn [coord chain _]
                   (-> (fn [point]
                         (-> (m/point->coords coordinate-system point)
                             (get-in chain)))
                       (with-meta {:name coord})))
                 prototype)))

(defn quotify-coordinate-prototype
  "Scmutils wants to allow forms like this:

  ```clojure
  (using-coordinates (up x y) R2-rect ...)
  ```

   Note that `x`, `y` are unquoted. This function converts such an unquoted form
  into a quoted one that could be evaluated to return an up-tuple of the
  symbols:

  ```clojure
  (up 'x 'y)
  ```

  Such an object is useful for [[structure/mapr]]."
  [p]
  (letfn [(q [p]
            (cond (and (sequential? p) ('#{up down} (first p)))
                  (let [s (first p)
                        prefix (if (= s 'up)
                                 `s/up
                                 `s/down)]
                    `(~prefix ~@(map q (rest p))))

                  (vector? p) (mapv q p)
                  (symbol? p) (list 'quote p)
                  :else (u/illegal "Invalid coordinate prototype")))]
    (q p)))

(defn ^:no-doc symbols-from-prototype
  "Generates a list of symbols from the supplied argument prototype. The
  prototype is allowed to be a vector, a list like `(up x y)` or a bare symbol.
  Anything else causes an exception.

  Nested structures are fine! The return value is a flat sequence."
  [p]
  (cond (and (sequential? p)
             ('#{up down} (first p))) (mapcat symbols-from-prototype (rest p))
        (vector? p) (mapcat symbols-from-prototype p)
        (symbol? p) [p]
        :else (u/illegal (str "Invalid coordinate prototype: " p))))

(defmacro let-coordinates
  "similar to a `let` binding that holds pairs of

  <coordinate-structure-prototype>, <coordinate-system>

  And internally binds, for each pair: (take `[x y]` and `m/R2-rect` as
  examples):

  - The coordinate system symbol `R2-rect` to a new version of the coordinate
    system with its `coordinate-prototype` replaced by the one you supplied.
    That's `(up x y)` in this example.

  - the entries `x` and `y` to coordinate functions, ie, functions from manifold
    point to this particular coordinate

  - `d:dx` and `d:dy` vector field procedures (I'm fuzzy here!)

  - `dx` and `dy` 1-forms for each coordinate (fuzzy here too!)

  Example:

  ```clojure
  (let-coordinates [[x y]    R2-rect
                   [r theta] R2-polar]
    ;; bindings:
    ;; R2-rect, x, y, d:dx, d:dy, dx, dy
    ;; R2-polar, r, theta, d:dr, d:dtheta, dr, dtheta
    body...)
  ```"
  [bindings & body]
  (when-not (even? (count bindings))
    (u/illegal "let-coordinates requires an even number of bindings"))
  (let [pairs                         (partition 2 bindings)
        prototypes                    (map first pairs)
        c-systems                     (map second pairs)
        system-names                  (map (comp symbol name) c-systems)
        coordinate-names              (mapcat symbols-from-prototype prototypes)
        coordinate-vector-field-names (map vf/coordinate-name->vf-name coordinate-names)
        coordinate-form-field-names   (map ff/coordinate-name->ff-name coordinate-names)]
    `(let [[~@system-names :as c-systems#]
           (mapv m/with-coordinate-prototype
                 ~(into [] c-systems)
                 ~(mapv quotify-coordinate-prototype prototypes))

           ~(into [] coordinate-names)
           (flatten
            (map coordinate-functions c-systems#))

           ~(into [] coordinate-vector-field-names)
           (flatten
            (map vf/coordinate-system->vector-basis c-systems#))

           ~(into [] coordinate-form-field-names)
           (flatten
            (map ff/coordinate-system->oneform-basis c-systems#))]
       ~@body)))

(defmacro using-coordinates
  "[[using-coordinates]] wraps [[let-coordinates]] and allows you to supply a
  single coordinate prototype and a single coordinate system.
  See [[let-coordinates]] for details about what symbols are bound inside the
  body.

  Example:

  ```clojure
  (using-coordinates (up x y) R2-rect
                     body...)
  ```"
  [coordinate-prototype coordinate-system & body]
  `(let-coordinates [~coordinate-prototype ~coordinate-system]
     ~@body))

(defmacro define-coordinates
  "Give some `coordinate-system` like `R2-rect` and a `coordinate-prototype` like
  `[x y]` or `(up x y), `binds the following definitions into the namespace
  where [[define-coordinates]] is invoked:

  - `R2-rect` binds to a new version of the coordinate system with its
    `coordinate-prototype` replaced by the supplied prototype

  - `x` and `y` bind to coordinate functions, ie, functions from manifold point
  to that particular coordinate

  - `d:dx` and `d:dy` bind to the corresponding vector field procedures

  - `dx` and `dy` bind to 1-forms for each coordinate."
  [coordinate-prototype coordinate-system]
  (let [sys-name           (symbol (name coordinate-system))
        coord-names        (symbols-from-prototype coordinate-prototype)
        vector-field-names (map vf/coordinate-name->vf-name coord-names)
        form-field-names   (map ff/coordinate-name->ff-name coord-names)
        sys-sym            (gensym)
        value-sym          (gensym)
        bind               (ud/careful-def *ns*)]
    `(let [~sys-sym (m/with-coordinate-prototype
                      ~coordinate-system
                      ~(quotify-coordinate-prototype coordinate-prototype))]
       ~(bind sys-name sys-sym)
       (let [~value-sym
             (into [] (flatten
                       [(coordinate-functions ~sys-sym)
                        (vf/coordinate-system->vector-basis ~sys-sym)
                        (ff/coordinate-system->oneform-basis ~sys-sym)]))]
         ~@(map-indexed
            (fn [i sym]
              (bind sym `(nth ~value-sym ~i)))
            (concat coord-names vector-field-names form-field-names))))))
