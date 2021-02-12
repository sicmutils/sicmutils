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
  (:require [sicmutils.calculus.basis :as b]
            [sicmutils.calculus.manifold :as m]
            [sicmutils.calculus.vector-field :as vf]
            [sicmutils.calculus.form-field :as ff]
            [sicmutils.matrix :as matrix]
            [sicmutils.structure :as s]
            [sicmutils.util :as u]))

(defn coordinate-functions
  [coordinate-system]
  (let [prototype (m/coordinate-prototype coordinate-system)]
    (s/mapr (fn [access-chain]
              (comp (apply s/component access-chain)
                    #(m/point->coords coordinate-system %)))
            (s/structure->access-chains prototype))))

(defn quotify-coordinate-prototype
  "Scmutils wants to allow forms like this:

  ```clojure
  (using-coordinates (up x y) R2-rect ...)
  ```

   Note that `x`, `y` are unquoted. This function converts such an unquoted for
  into a quoted one that could be evaluated to return an up-tuple of the
  symbols:

  ```clojure
  (up 'x 'y)
  ```

  Such an object is useful for [[s/mapr]]. The function `xf` is applied before
  quoting."
  [xf p]
  (let [q (fn q [p]
            (cond (and (sequential? p)
                       ('#{up down} (first p))) `(~(first p) ~@(map q (rest p)))
                  (vector? p) (mapv q p)
                  (symbol? p) `'~(xf p)
                  :else (u/illegal "Invalid coordinate prototype")))]
    (q p)))

(defn- symbols-from-prototype [p]
  (cond (and (sequential? p)
             ('#{up down} (first p))) (mapcat symbols-from-prototype (rest p))
        (vector? p) (mapcat symbols-from-prototype p)
        (symbol? p) `(~p)
        :else (u/illegal (str "Invalid coordinate prototype: " p))))

(defmacro let-coordinates
  "Example:

  ```clojure
  (let-coordinates [[x y] R2-rect
                  [r theta] R2-polar]
    body...)
  ```"
  [bindings & body]
  (when-not (even? (count bindings))
    (u/illegal "let-coordinates requires an even number of bindings"))
  (let [pairs (partition 2 bindings)
        prototypes (map first pairs)
        c-systems (mapv second pairs)
        coordinate-names (mapcat symbols-from-prototype prototypes)
        coordinate-vector-field-names (map vf/coordinate-name->vf-name coordinate-names)
        coordinate-form-field-names (map ff/coordinate-name->ff-name coordinate-names)]
    `(let [[~@c-systems :as c-systems#]
           (mapv m/with-coordinate-prototype
                 ~c-systems
                 ~(mapv #(quotify-coordinate-prototype identity %) prototypes))
           c-fns# (map coordinate-functions c-systems#)
           c-vfs# (map vf/coordinate-basis-vector-fields c-systems#)
           c-ffs# (map ff/coordinate-basis-oneform-fields c-systems#)
           ~(vec coordinate-names) (flatten c-fns#)
           ~(vec coordinate-vector-field-names) (flatten c-vfs#)
           ~(vec coordinate-form-field-names) (flatten c-ffs#)]
       ~@body)))

(defmacro using-coordinates
  "Example:

  ```clojure
  (using-coordinates (up x y) R2-rect
                     body...)
  ```

  Note: [[using-coordinates]] is just a macro wrapping [[let-coordinates]].
  Prefer [[let-coordinates]] when possible."
  [coordinate-prototype coordinate-system & body]
  `(let-coordinates [~coordinate-prototype ~coordinate-system] ~@body))

(defn coordinate-system->vector-basis
  [coordinate-system]
  (vf/coordinate-basis-vector-fields coordinate-system))

(defn coordinate-system->oneform-basis
  [coordinate-system]
  (ff/coordinate-basis-oneform-fields coordinate-system))

(defn- c:generate
  [n orientation f]
  (if (= n 1)
    (f 0)
    (s/generate n orientation f)))

(defn vector-basis->dual
  [vector-basis coordinate-system]
  (let [prototype (m/coordinate-prototype coordinate-system)
        vector-basis-coefficient-functions (s/mapr #(vf/vector-field->components % coordinate-system) vector-basis)
        guts (fn [coords]
               (matrix/s:transpose (s/compatible-shape prototype)
                                   (matrix/s:inverse
                                    (s/compatible-shape prototype)
                                    (s/mapr #(% coords) vector-basis-coefficient-functions)
                                    prototype)
                                   prototype))
        oneform-basis-coefficient-functions (c:generate (:dimension (m/manifold coordinate-system))
                                                        ::s/up
                                                        #(comp (s/component %) guts))
        oneform-basis (s/mapr #(ff/components->oneform-field % coordinate-system) oneform-basis-coefficient-functions)]
    oneform-basis))

(defn coordinate-system->basis
  "Returns the standard basis object for `coordinate-system`."
  [coordinate-system]
  (b/make-basis
   (vf/coordinate-basis-vector-fields coordinate-system)
   (ff/coordinate-basis-oneform-fields coordinate-system)))

(defn Jacobian
  "Returns the Jacobian of transition from `from-basis` to `to-basis`."
  [to-basis from-basis]
  (s/mapr (b/basis->oneform-basis to-basis)
          (b/basis->vector-basis from-basis)))
