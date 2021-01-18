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
;; implementation is subtle, as described by [Manzyuk et al.
;; 2019](https://www.cambridge.org/core/journals/journal-of-functional-programming/article/perturbation-confusion-in-forward-automatic-differentiation-of-higherorder-functions/A808189A3875A2EDAC6E0D62CF2AD262).
;; ([[sicmutils.derivative.calculus-test]], in the "Amazing Bug" sections,
;; describes the pitfalls at length.)
;;
;; [[sicmutils.differential]] describes how each in-progress perturbed variable
;; in a derivative is assigned a "tag" that accumulates the variable's partial
;; derivative.
;;
;; What subtleties do we have to track when `((D f) x)` produces a _function_?
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
;; The key to the solution lives in [[extract-tangent-fn]], called on the result
;; of `((D f) x)` when `((D f) x)` produces a function. We have to armor the
;; returned function so that:
;;
;; - it extracts the originally-injected tag when someone eventually calls the
;;   function
;;
;; - if some caller passes a new [[d/Differential]] instance into the function,
;;   any tags in that [[d/Differential]] will survive on their way back out...
;;   even if they happen to contain the originally-injected tag.
;;
;; We do this by:
;;
;; - replacing any instance of the original `tag` in the returned function's
;;   arguments with a temporary tag (let's call it `fresh`)
;; - calling the function and extracting the tangent component associated with
;;   `tag`, as requested (note now that the only instances of `tag` that can
;;   appear in the result come from variables captured in the function's
;;   closure)
;; - remapping `fresh` back to `tag` inside the remaining [[d/Differential]]
;;   instance.
;;
;; This last step ensures that any tangent tagged with `tag` in the input can
;; make it back out without tangling with closure-captured `tag` instances that
;; some higher level might want.

(defn- extract-tangent-fn
  "Returns a new function that composes a 'tag extraction' step with `f`. The
  returned fn will

  - call the underlying `f`, producing `result`
  - return `(extract-tangent result tag)`

  The returned function will also remap any instance of `tag` that appears in
  any differential argument passed to it to a private `fresh` tag, to prevent
  internal perturbation confusion. Any tangent components in the final result
  tagged with `fresh` will be remapped in the final result back to `tag`."
  [f tag]
  (-> (fn [& args]
        (let [fresh (d/fresh-tag)]
          (-> (apply f (map #(d/replace-tag % tag fresh) args))
              (d/extract-tangent tag)
              (d/replace-tag fresh tag))))
      (f/with-arity (f/arity f))))

;; NOTE: that the tag-remapping that the docstring for `extract-tag-fn`
;; describes might _also_ have to apply to a functional argument!
;;
;; `replace-tag` on a function is meant to be a `replace-tag` call applied to
;; the function's _output_. To prevent perturbation confusion inside the
;; function, we perform a similar remapping of any occurrence of `tag` in the
;; function's arguments.

(defn- replace-tag-fn
  "Returns a new function that composes a 'tag replacement' step with `f`. The
  returned function will:

  - make a fresh tag, and replace all `old` tags with `fresh` in the inputs
  - call `f`, producing `result`
  - return `(replace-tag result old new)`
  - remap any tangent component in the result tagged with `fresh` back to `old`."
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
;; Clojurescript, [[MetaFn]] instances. Metadata in the original function is
;; preserved through tag replacement and extraction.

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
;;
;; These functions put together the pieces laid out
;; in [[sicmutils.differential]] and declare an interface for taking
;; derivatives.

(defn derivative
  "Returns a single-argument function of that, when called with an argument `x`,
  returns the derivative of `f` at `x` using forward-mode automatic
  differentiation.

  For numerical differentiation,
  see [[sicmutils.numerical.derivative/D-numeric]].

  `f` must be built out of generic operations that know how to
  handle [[d/Differential]] inputs in addition to any types that a normal `(f
  x)` call would present. This restriction does _not_ apply to operations like
  putting `x` into a container or destructuring; just primitive function calls."
  [f]
  (let [tag (d/fresh-tag)]
    (fn [x]
      (-> (f (d/bundle x 1 tag))
          (d/extract-tangent tag)))))

;; The result of applying the derivative `(D f)` of a multivariable function `f`
;; to a sequence of `args` is a structure of the same shape as `args` with all
;; orientations flipped. (For a partial derivative like `((partial 0 1) f)` the
;; result has the same-but-flipped shape as `(get-in args [0 1])`.)
;;
;; `args` is coerced into an `up` structure. The only special case where this
;; does not happen is if `(= 1 (count args))`.
;;
;; To generate the result:
;;
;; - For a single non-structural argument, return `(derivative f)`
;; - else, bundle up all arguments into a single [[s/Structure]] instance `xs`
;; - Generate `xs'` by replacing each entry in `xs` with `((derivative f')
;;   entry)`, where `f'` is a function of ONLY that entry that
;;   calls `(f (assoc-in xs path entry))`. In other words, replace each entry
;;   with the result of the partial derivative of `f` at only that entry.
;; - Return `(s/transpose xs')` (the same structure with all orientations
;;   flipped.)
;;
;; A multivariable derivative is a multiple-arity function that performs the
;; above.

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

;; ## Generic [[g/partial-derivative]] Installation
;;
;; [[g/partial-derivative]] is meant to produce either a full Jacobian or some
;; entry specified by a `selectors` vector.
;;
;; When called on a function `f`, [[g/partial-derivative]] returns a function
;; wrapped in the machinery provided by [[multivariate]]; this allows the same
;; operator to serve functions of:
;;
;; - a single numerical input
;; - a single structural input
;; - multiple numerical OR structural inputs
;;
;; NOTE: The reason that this implementation is also installed
;; for [[s/Structure]] is that structures act as functions that apply their args
;; to every (functional) entry. Calling `(multivariate structure selectors)`
;; allows all of the machinery that handles structure-walking and argument
;; conversion to run a SINGLE time before getting passed to the structure of
;; functions, instead of separately for every entry in the structure.
;;
;; TODO: I think this is going to cause problems for, say, a Structure of
;; PowerSeries, where there is actually a cheap `g/partial-derivative`
;; implementation for the components. I vote to back out this `::s/structure`
;; installation.

(doseq [t [::v/function ::s/structure]]
  (defmethod g/partial-derivative [t v/seqtype] [f selectors]
    (multivariate-derivative f selectors))

  (defmethod g/partial-derivative [t nil] [f _]
    (multivariate-derivative f [])))

;; ## Operators
;;
;; This section exposes various differential operators as [[o/Operator]]
;; instances.

(def derivative-symbol 'D)

(def ^{:doc "Derivative operator. Takes some function `f` and returns a function
  whose value at some point can multiply an increment in the arguments, to
  produce the best linear estimate of the increment in the function value.

  For univariate functions, [[D]] computes a derivative. For vector-valued
  functions, [[D]] computes
  the [Jacobian](https://en.wikipedia.org/wiki/Jacobian_matrix_and_determinant)
  of `f`.

  The related [[Grad]] returns a function that produces a structure of the
  opposite orientation as [[D]]. Both of these functions use forward-mode
  automatic differentiation."} D
  (o/make-operator #(g/partial-derivative % [])
                   derivative-symbol))

(defn partial
  "Returns an operator that, when applied to a function `f`, produces a function
  that computes the partial derivative of `f` at the (zero-based) slot index
  provided via `selectors`."
  [& selectors]
  (o/make-operator #(g/partial-derivative % selectors)
                   `(~'partial ~@selectors)))

(def ^{:doc "Operator that takes a function `f` and returns a new function that
  calculates the [Gradient](https://en.wikipedia.org/wiki/Gradient) of `f`.

  The related [[D]] operator returns a function that produces a structure of the
  opposite orientation as [[Grad]]. Both of these functions use forward-mode
  automatic differentiation."}
  Grad
  (-> (fn [f]
        (f/compose s/opposite
                   (g/partial-derivative f [])))
      (o/make-operator 'Grad)))

(def ^{:doc "Operator that takes a function `f` and returns a function that
  calculates the [Divergence](https://en.wikipedia.org/wiki/Divergence) of
  `f` at its input point.

 The divergence is a one-level contraction of the gradient."}
  Div
  (-> (f/compose g/trace Grad)
      (o/make-operator 'Div)))

(def ^{:doc "Operator that takes a function `f` and returns a function that
  calculates the [Curl](https://en.wikipedia.org/wiki/Curl_(mathematics)) of `f`
  at its input point.

  `f` must be a function from $\\mathbb{R}^3 \\to \\mathbb{R}^3$."}
  Curl
  (-> (fn [f-triple]
        (let [[Dx Dy Dz] (map partial [0 1 2])
              fx (f/get f-triple 0)
              fy (f/get f-triple 1)
              fz (f/get f-triple 2)]
          (s/up (g/- (Dy fz) (Dz fy))
                (g/- (Dz fx) (Dx fz))
                (g/- (Dx fy) (Dy fx)))))
      (o/make-operator 'Curl)))

(def ^{:doc "Operator that takes a function `f` and returns a function that
  calculates the [Vector
  Laplacian](https://en.wikipedia.org/wiki/Laplace_operator#Vector_Laplacian) of
  `f` at its input point."}
  Lap
  (-> (f/compose g/trace (g/* Grad Grad))
      (o/make-operator 'Lap)))

;; ## Derivative Utilities
;;
;; Functions that make use of the differential operators defined above in
;; standard ways.

(defn taylor-series
  "Returns a [[s/Series]] of the coefficients of the taylor series of the function
  `f` evaluated at `x`, with incremental quantity `dx`.

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
