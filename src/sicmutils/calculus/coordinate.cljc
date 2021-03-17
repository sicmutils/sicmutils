;;
;; Copyright © 2017 Colin Smith.
;; This work is based on the Scmutils system of MIT/GNU Scheme:
;; Copyright © 2002 Massachusetts Institute of Technology
;;
;; This is free software;  you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or (at
;; your option) any later version.
;;
;; This software is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this code; if not, see <http://www.gnu.org/licenses/>.
;;

(ns sicmutils.calculus.coordinate
  (:require [sicmutils.calculus.manifold :as m]
            [sicmutils.calculus.vector-field :as vf]
            [sicmutils.calculus.form-field :as ff]
            [sicmutils.structure :as s]
            [sicmutils.util :as u]))

(defn coordinate-functions
  "Returns a structure similar to the [[manifold/coordinate-prototype]] of
  `coordinate-system`, where every entry is a function from manifold point =>
  the associated component of the point in the coordinate representation
  described by `coordinate-system`."
  [coordinate-system]
  (let [prototype (m/coordinate-prototype coordinate-system)]
    (s/map-chain (fn [_ chain _]
                   (fn [point]
                     (-> (m/point->coords coordinate-system point)
                         (get-in chain))))
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

  Such an object is useful for [[structure/mapr]]. The function `xf` is applied
  before quoting."
  [xf p]
  (letfn [(q [p]
            (cond (and (sequential? p)
                       ('#{up down} (first p))) `(~(first p) ~@(map q (rest p)))
                  (vector? p) (mapv q p)
                  (symbol? p) `'~(xf p)
                  :else (u/illegal "Invalid coordinate prototype")))]
    (q p)))

(defn ^:no-doc symbols-from-prototype
  "TODO note that this allows the prototype to be `up` etc..."
  [p]
  (cond (and (sequential? p)
             ('#{up down} (first p))) (mapcat symbols-from-prototype (rest p))
        (vector? p) (mapcat symbols-from-prototype p)
        (symbol? p) `(~p)
        :else (u/illegal (str "Invalid coordinate prototype: " p))))

(defmacro let-coordinates
  "TODO note WHAT this is binding! For everything, we get vector field and form
  field names, PLUS coordinate bindings. And, remember, bindings for the
  `m/R2-rect` vs `R2-rect`. These get bound with the coordinate prototype bound
  to the symbols that you've referenced.

  Example:

  ```clojure
  (let-coordinates [[x y]    R2-rect
                   [r theta] R2-polar]
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
                 ~(mapv #(quotify-coordinate-prototype identity %) prototypes))

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
  "Example:

  ```clojure
  (using-coordinates (up x y) R2-rect
                     body...)
  ```

  NOTE: [[using-coordinates]] is just a macro wrapping [[let-coordinates]].
  Prefer [[let-coordinates]] when possible."
  [coordinate-prototype coordinate-system & body]
  `(let-coordinates [~coordinate-prototype ~coordinate-system]
     ~@body))

(defn generate
  "NOTE: from GJS, this is a kludge, because a single coordinate is NOT a
  structure, it's just the number."
  [n orientation f]
  (if (= n 1)
    (f 0)
    (s/generate n orientation f)))
