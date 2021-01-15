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

(ns sicmutils.calculus.derivative
  "This namespace implements a number of differential operators like [[D]], and
  the machinery to apply [[D]] to various structures."
  (:refer-clojure :rename {partial core-partial}
                  #?@(:cljs [:exclude [partial]]))
  (:require [sicmutils.differential :as d]
            [sicmutils.function :as f]
            [sicmutils.generic :as g]
            [sicmutils.matrix :as matrix]
            [sicmutils.operator :as o]
            [sicmutils.structure :as s]
            [sicmutils.util :as u]
            [sicmutils.util.vector-set :as uv]
            [sicmutils.value :as v])
  #?(:clj
     (:import (clojure.lang Fn MultiFn))))

;; ## IPerturbed Implementation for Functions
;;
;; The following section, along with [[sicmutils.collection]]
;; and [[sicmutils.differential]], rounds out the implementations
;; of [[d/IPerturbed]] for native Clojure(script) data types. The function
;; implementation is quite subtle, as described by [Manzyuk et al.
;; 2019](https://www.cambridge.org/core/journals/journal-of-functional-programming/article/perturbation-confusion-in-forward-automatic-differentiation-of-higherorder-functions/A808189A3875A2EDAC6E0D62CF2AD262).
;; ([[sicmutils.derivative.calculus-test]], in the "Amazing Bug" sections,
;; describes the pitfalls at length.)
;;
;; [[sicmutils.differential]] describes how each in-progress perturbed variable
;; in a derivative is assigned a "tag" that accumulates the variable's partial
;; derivative.
;;
;; What subtleties do we have to track when calling a a function's
;; derivative `(D f)` produces a _function_?
;;
;; It's all about Closures! The returned function has probably captured an
;; internal reference to the original [[d/Differential]] input; Our goal is to
;; place a barrier in its way, so that the original tangent component gets
;; extracted whenever someone calls this returned $f$ down the road.
;;
;; This is true for any Functor-shaped return value, like a structure or Map.
;; The difference with functions is that they take _inputs_. If you contrive a
;; situation where you can feed the original captured [[d/Differential]] into
;; the returned function, this can trigger "perturbation confusion", where two
;; different layers try to extract the tangent corresponding to the SAME tag,
;; and one is left with nothing.
;;
;; See [[sicmutils.calculus.derivative-test/amazing-bug]] and its surrounding
;; tests for lengthy examples of this problem. You can manufacture your own bugs
;; by taking the derivative of a function that returns a function that _itself_
;; takes a function. Feed that function to itself and you've got a nice arena
;; for perturbation confusion.
;;
;; ### Tag Replacement
;;
;; The key to the solution lives in [[extract-tangent-fn]], called when
;; something like `((D f) x)` produces a function. We have to armor the returned
;; function so that:
;;
;; - it extracts the originally-asked-for tag when someone eventually calls the
;;   function
;;
;; - if some caller passes a new [[d/Differential]] instance into the function,
;;   any tags in that [[d/Differential]] will survive on their way back out...
;;   even if they happen to contain the originally-requested tag.
;;
;; We do this by:
;;
;; - replacing any instance of the original `tag` in the returned fn's arguments
;;   with a temporary tag, (let's call it `fresh`)
;; - calling the function and extracting `tag`, as requested (note now that the
;;   only instances of `tag` that can appear in the result come from variables
;;   captured in the function's close)
;; - remapping `fresh` back to `tag`.
;;
;; This last step ensures that any input tagged with `tag` can make it back out
;; without tangling with closure-captured `tag` instances that some higher level
;; might want.
;;
;; NOTE (@sritchie): I think there is still a lurking bug here! If some
;; functional input PRODUCES a captured perturbation with `tag`, that ALSO needs
;; to get remapped on the way out of any internal call. And then mapped BACK
;; again if it escapes this function, after extraction. See `sams-amazing-bug`
;; and [Discussion #237](https://github.com/sicmutils/sicmutils/discussions/237)
;; for more detail and color about this problem.

(defn- extract-tangent-fn
  "Returns a new function that composes a 'tag extraction' step with `f`. The
  returned fn will

  - call the underlying `f`, producing `result`
  - return `(extract-tangent result tag)`

  The returned function will also remap any instance of `tag` that appears in
  any differential argument passed to it to a private `fresh` tag, to prevent
  internal perturbation confusion. Any `fresh` instances that appear in the
  output will be remapped in the final result back to `tag`."
  [f tag]
  (-> (fn [& args]
        (let [fresh (d/fresh-tag)]
          (-> (apply f (map #(d/replace-tag % tag fresh) args))
              (d/extract-tangent tag)
              (d/replace-tag fresh tag))))
      (f/with-arity (f/arity f))))

;; Note that the tag-remapping that the docstring for `extract-tag-fn` describes
;; might _also_ have to apply to a functional argument.
;;
;; `replace-tag` on a function is meant to be a `replace-tag` call applied to
;; the function's _output_. To prevent perturbation confusion inside the
;; function, we perform a similar remapping of any occurrence of `tag` in the
;; function's arguments.

(defn- replace-tag-fn
  "Returns a new function that composes a 'tag replacement' step with `f`. The
  returned fn will:

  - call the underlying `f`, producing `result`
  - return `(replace-tag result old new)`


  ;; - make a fresh tag, and replace all `old` tags with `fresh` in the inputs
  ;; - call `f`, producing `result`
  ;; - return `(replace-tag result old new)`
  ;; - remap any leaked `fresh` in the result back to `old`

  The returned function will also remap any instance of `old` that appears in
  any differential argument passed to it to a private `fresh` tag, to prevent
  internal perturbation confusion. Any `fresh` instances that appear in the
  output will be remapped in the final result back to `old`.

  NOTE: `new` instances can also leak out! See [Discussion
  #237](https://github.com/sicmutils/sicmutils/discussions/237)
  and [[sicmutils.calculus.derivative-test/sams-amazing-bug]] for a particularly
  subtle example. This seems like a form of perturbation confusion that we need
  to address."
  [f old new]
  (-> (fn [& args]
        (let [fresh (d/fresh-tag)
              args  (map #(d/replace-tag % old fresh) args)]
          (-> (apply f args)
              (d/replace-tag old new)
              (d/replace-tag fresh old))))
      (f/with-arity (f/arity f))))

;; ## Protocol Implementation
;;
;; The implementation for functions handles functions, multimethods, and, in
;; CLJS, MetaFns. Metadata in the original function is preserved by tag
;; replacement and extraction.

(extend-protocol d/IPerturbed
  #?(:clj Fn :cljs function)
  (perturbed? [f]
    #?(:clj (:perturbed? (meta f) false)
       :cljs false))
  (replace-tag [f old new] (replace-tag-fn f old new))
  (extract-tangent [f tag] (extract-tangent-fn f tag))

  #?@(:cljs
      [MetaFn
       (perturbed? [f] (:perturbed? (.-meta f) false))
       (replace-tag [f old new]
                    (replace-tag-fn (.-afn f) old new))
       (extract-tangent [f tag]
                        (extract-tangent-fn (.-afn f) tag))])

  MultiFn
  (perturbed? [f] false)
  (replace-tag [f old new] (replace-tag-fn f old new))
  (extract-tangent [f tag] (extract-tangent-fn f tag)))



;; ## Single and Multivariable Calculus

(defn derivative [f]
  (let [tag (d/fresh-tag)]
    (fn [x]
      (-> (f (d/bundle x 1 tag))
          (d/extract-tangent tag)))))

(defn- euclidean-structure [selectors f]
  (letfn [(structural-derivative [g v]
            (cond (s/structure? v)
                  (s/opposite v
                              (map-indexed
                               (fn [i v_i]
                                 (structural-derivative
                                  (fn [w]
                                    (g (assoc v i w)))
                                  v_i))
                               v))
                  (v/numerical? v) ((derivative g) v)

                  :else
                  (u/illegal (str "bad structure " g ", " v))))
          (a-euclidean-derivative [v]
            (cond (s/structure? v)
                  (structural-derivative
                   (fn [w]
                     (f (if (empty? selectors)
                          w
                          (assoc-in v selectors w))))
                   (get-in v selectors))

                  (empty? selectors)
                  ((derivative f) v)

                  :else
                  (u/illegal (str "Bad selectors " f selectors v))))]
    a-euclidean-derivative))

(defn- multivariate-derivative [f selectors]
  (let [a (f/arity f)
        d (core-partial euclidean-structure selectors)
        make-df #(with-meta % {:arity a :from :multivariate-derivative})]
    (condp = a
      [:exactly 0] (make-df (constantly 0))
      [:exactly 1] (make-df (d f))
      [:exactly 2] (make-df (fn [x y]
                              ((d (fn [[x y]] (f x y)))
                               (matrix/seq-> [x y]))))
      [:exactly 3] (make-df (fn [x y z]
                              ((d (fn [[x y z]] (f x y z)))
                               (matrix/seq-> [x y z]))))
      [:exactly 4] (make-df (fn [w x y z]
                              ((d (fn [[w x y z]] (f w x y z)))
                               (matrix/seq-> [w x y z]))))
      [:between 1 2] (make-df (fn
                                ([x] ((d f) x))
                                ([x y]
                                 ((d (fn [[x y]] (f x y)))
                                  (matrix/seq-> [x y])))))
      (make-df
       (fn [& xs]
         (when (empty? xs) (u/illegal "No args passed to derivative?"))
         (if (= (count xs) 1)
           ((d f) (first xs))
           ((d #(apply f %)) (matrix/seq-> xs))))))))

(doseq [t [::v/function ::s/structure]]
  (defmethod g/partial-derivative [t v/seqtype] [f selectors]
    (multivariate-derivative f selectors))

  (defmethod g/partial-derivative [t nil] [f _]
    (multivariate-derivative f [])))

(def derivative-symbol 'D)

(def ^{:doc "Derivative operator. Produces a function whose value at some point
  can multiply an increment in the arguments, to produce the best linear
  estimate of the increment in the function value."}
  D
  (o/make-operator #(g/partial-derivative % [])
                   derivative-symbol))

(defn partial
  "Partial differentiation of a function at the (zero-based) slot index
  provided."
  [& selectors]
  (o/make-operator #(g/partial-derivative % selectors)
                   `(~'partial ~@selectors)))

(defn taylor-series
  "Returns a `Series` of the coefficients of the taylor series of the function `f`
  evaluated at `x`, with incremental quantity `dx`.

  NOTE: The `(constantly dx)` term is what allows this to work with arbitrary
  structures of `x` and `dx`. Without this wrapper, `((g/* dx D) f)` with `dx`
  == `(up 'dx 'dy)` would expand to this:

  `(fn [x] (* (s/up ('dx x) ('dy x))
              ((D f) x)))`

  `constantly` delays the interpretation of `dx` one step:

  `(fn [x] (* (s/up 'dx 'dy)
              ((D f) x)))`
  "
  [f x dx]
  (((g/exp (g/* (constantly dx) D)) f) x))
