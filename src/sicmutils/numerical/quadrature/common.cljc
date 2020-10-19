;;
;; Copyright © 2020 Sam Ritchie.
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

(ns sicmutils.numerical.quadrature.common
  "Implements utilities shared by all integrators, for example:

  - code to wrap a sequence of progressively better estimates in a common `integrator` interface
  - data structures implementing various integration intervals."
  (:require [sicmutils.util.stream :as us]
            [taoensso.timbre :as log]))


;; This dynamic variable holds the default "roundoff cutoff" used by all
;; integrators to decide when to return an estimate based on a single slice, vs
;; attempting to converge a sequence of progressively finer estimates. When this
;; condition is satisfied:
;;
;; |b - a| / |a| + |b| <= `cutoff`
;;
;; An integrator will estimate a single slice directly. Else, it will attempt to
;; converge a sequence.
;;
;; NOTE - we don't have an interface yet to bind this dynamic variable. bind it
;; manually to modify the cutoff for a specific call to some integrator:
;;
;; (binding [*roundoff-cutoff* 1e-6]
;;   (integrate f a b))

(def ^:dynamic *roundoff-cutoff* 1e-14)

;; ## Intervals
;;
;; Implementations of the various intervals used by the adaptive integral
;; interface. By default, integration endpoints are considered /open/.

(def open        [::open ::open])
(def closed      [::closed ::closed])
(def open-closed [::open ::closed])
(def closed-open [::closed ::open])
(def infinities #{##Inf ##-Inf})
(def infinite? (comp boolean infinities))

(defn closed?
  "Returns true if the argument represents an explicit `closed` interval, false
  otherwise."
  [x] (= x closed))
(def open? (complement closed?))

;; These functions modify an interval by opening or closing either of its
;; endpoints.

(defn close-l [[_ r]] [::closed r])
(defn close-r [[l _]] [l ::closed])
(defn open-l [[_ r]] [::open r])
(defn open-r [[l _]] [l ::open])
(defn flip [[l r]] [r l])

(defn interval
  "Extracts the interval (or `open` as a default) from the supplied integration
  options dict."
  [opts]
  (get opts :interval open))

(defn with-interval
  "Sets the specified interval to a key inside the suppled `opts` map of arbitrary
  integration options."
  [opts interval]
  (assoc opts :interval interval))

(defn update-interval
  "Accepts:

  - a dictionary of arbitrary options
  - one of the 4 interval modification functions

  and returns a dict of options with `f` applied to the contained interval (or
  `open` if no interval is set).
  "
  [opts f]
  (let [k :interval]
    (assoc opts k (f (interval opts)))))

;; ## Common Integration Interface
;;
;; The following two functions define a shared interface that integration
;; namespaces can use to create an "integrator" from:
;;
;; - a fn that can estimate the area of a single integration slice, and
;; - a fn that can return a sequence of progressively finer estimates.
;;
;; The first function is called in the case that the integration range $(a,
;; b)$ (open or closed) is too fine for subdivision. The second function takes
;; over in all other (most!) cases.

(defn- narrow-slice?
  "Returns true if the range $[a, b]$ is strip narrow enough to pass the following
  test:

  |b - a| / |a| + |b| <= `cutoff`

  False otherwise. This inequality measures how close the two floating point
  values are, scaled by the sum of their magnitudes."
  [a b cutoff]
  (let [diff   (Math/abs (- b a))
        sum    (+ (Math/abs a)
                  (Math/abs b))]
    (<= diff (* cutoff sum))))

(defn make-integrator-fn
  "Generates an `integrator` function from two functions with the following
  signatures and descriptions:

  - `(area-fn f a b)` estimates the integral of `f` over the interval `(a, b)`
  with no subdivision, nothing clever at all.

  - `(seq-fn f a b opts)` returns a sequence of successively refined estimates
  of the integral of `f` over `(a, b)`. `opts` can contain kv pairs that
  configure the behavior of the sequence function (a sequence of the number of
  integration slices to use, for example.)

  The returned function has the signature:

  `(f a b opts)`

  All `opts` are passed on to `seq-fn`, /and/ to `us/seq-limit` internally,
  where the options configure the checks on sequence convergence."
  [area-fn seq-fn]
  (fn call
    ([f a b] (call f a b {}))
    ([f a b {:keys [roundoff-cutoff]
             :or {roundoff-cutoff *roundoff-cutoff*}
             :as opts}]
     (if (narrow-slice? a b roundoff-cutoff)
       (do (log/info "Integrating narrow slice: " a b)
           {:converged? true
            :terms-checked 1
            :result (area-fn f a b)})
       (-> (seq-fn f a b opts)
           (us/seq-limit opts))))))

(defn- name-with-attributes
  "Taken from `clojure.tools.macro/name-with-attributes`.

  Handles optional docstrings and attribute maps for a name to be defined in a
  list of macro arguments. If the first macro argument is a string, it is added
  as a docstring to name and removed from the macro argument list. If afterwards
  the first macro argument is a map, its entries are added to the name's
  metadata map and the map is removed from the macro argument list. The return
  value is a vector containing the name with its extended metadata map and the
  list of unprocessed macro arguments."
  ([name body] (name-with-attributes name body {}))
  ([name body meta]
   (let [[docstring body] (if (string? (first body))
                            [(first body) (next body)]
                            [nil body])
         [attr body]      (if (map? (first body))
                            [(first body) (next body)]
                            [{} body])
         attr             (merge meta attr)
         attr             (if docstring
                            (assoc attr :doc docstring)
                            attr)
         attr             (if (meta name)
                            (conj (meta name) attr)
                            attr)]
     [(with-meta name attr) body])))

(defmacro defintegrator
  "Helper macro for defining integrators."
  [sym & body]
  (let [meta       {:arglists (list 'quote '([f a b] [f a b opts]))}
        [sym body] (name-with-attributes sym body meta)
        {:keys [area-fn seq-fn]} (apply hash-map body)]
    (assert seq-fn (str "defintegrator " sym ": seq-fn cannot be nil"))
    (assert area-fn (str "defintegrator " sym ": area-fn cannot be nil"))
    `(def ~sym
       (make-integrator-fn ~area-fn ~seq-fn))))
