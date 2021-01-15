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

;; Multivariable derivatives are performed by bundling up all arguments into a
;; single [[s/Structure]] instance, and then calling `((derivative f) x)` for
;; every entry `x` in the input structure.
;;
;; [[jacobian]] handles this main logic. [[jacobian]] can only take a structural
;; input. [[euclidean]] and [[multivariate]] below widen handle, respectively,
;; optionally-structural and multivariable arguments.

(defn- jacobian
  "Takes:

  - some function `f` of a single [[s/structure?]] argument
  - the unperturbed structural `input`
  - a `selectors` vector that can be empty or contain a valid path into the
    `input` structure

  and returns either:

  - The
  full [Jacobian](https://en.wikipedia.org/wiki/Jacobian_matrix_and_determinant)
  of `f` at `input`, if `selectors` is empty
  - the entry of the Jacobian at `selectors`.

  The Jacobian has the same shape as `input` (or the entry at `selectors`) with
  all orientations flipped. Multiplying this by an increment in the shape of
  `input` will give you a proper increment in the output of `f`."
  ([f input] (jacobian f input []))
  ([f input selectors]
   (letfn [(prefixed [path]
             (if (empty? selectors)
               path
               (into selectors path)))

           (substitute [path entry]
             (assoc-in input (prefixed path) entry))]
     (if-let [piece (get-in input selectors)]
       (let [frame         (s/transpose piece)
             perturb-entry (fn [entry path]
                             (letfn [(f-entry [x]
                                       (f (substitute path x)))]

                               ;; Each entry takes the derivative of a function
                               ;; of THAT entry; internally, `f-entry`
                               ;; substitutes the perturbed entry into the
                               ;; appropriate place in the full `input` before
                               ;; calling `f`.
                               ((derivative f-entry) entry)))]

         ;; Visit each entry in `frame`, a copy of either the full input or the
         ;; sub-piece living at `selectors` (with all orientations flipped), and
         ;; replace the entry with the result of the partial derivative of `f`
         ;; with that entry perturbed.
         (s/map-chain (fn [entry path _]
                        (if (v/numerical? entry)
                          (perturb-entry entry path)
                          (u/illegal
                           (str "non-numerical entry " entry " in input structure " input))))
                      frame))

       ;; The call to `get-in` will return nil if the `selectors` don't index
       ;; correctly into the supplied `input`, triggering this exception.
       (u/illegal (str "Bad selectors " selectors " for structure " input))))))

(defn- euclidean
  "Slightly more general version of [[jacobian]] that can handle a single
  non-structural input; dispatches to either [[jacobian]] or [[derivative]]
  depending on the input type.

  If you pass non-empty `selectors`, the returned function will throw if it
  receives a non-structural, non-numerical argument."
  ([f] (euclidean f []))
  ([f selectors]
   (let [selectors (vec selectors)]
     (fn [input]
       (cond (s/structure? input)
             (jacobian f input selectors)

             ;; non-empty selectors are only allowed for functions that receive
             ;; a structural argument. This case passes that single,
             ;; non-structural argument on to `(derivative f)`.
             (empty? selectors)
             ((derivative f) input)

             ;; Any attempt to index (via non-empty selectors) into a
             ;; non-structural argument will throw.
             ;;
             ;; NOTE: What about matrices, maps or sequences? The current
             ;; implementation (as of 0.14.0) pushes the derivative operator
             ;; into the entries, or values, of those types, so they won't reach
             ;; this clause. There is a case I (@sritchie) can make for actually
             ;; allowing the first clause here to work for ANY associative
             ;; structure; then you're on your own if you want to call this fn
             ;; directly.
             :else
             (u/illegal
              (str "Selectors " selectors
                   " not allowed for non-structural input " input)))))))

(defn- multivariate
  "Slightly wider version of [[euclidean]]. Accepts:

  - some function `f` of potentially many arguments
  - optionally, a sequence of selectors meant to index into the structural
    argument, or argument vector, of `f`

  And returns a new function that computes either the
  full [Jacobian](https://en.wikipedia.org/wiki/Jacobian_matrix_and_determinant)
  or the entry at `selectors`.

  Any multivariable function will have its argument vector coerced into an `up`
  structure. Any [[matrix/Matrix]] in a multiple-arg function call will be
  converted into a `down` of `up`s (a row of columns).

  Single-argument functions don't transform their arguments."
  ([f] (multivariate f []))
  ([f selectors]
   (let [a  (f/arity f)
         d  #(euclidean % selectors)
         df (condp = a
              [:exactly 0]   (constantly 0)
              [:exactly 1]   (d f)
              [:exactly 2]   (fn [x y]
                               ((d (fn [[x y]] (f x y)))
                                (matrix/seq-> [x y])))
              [:exactly 3]   (fn [x y z]
                               ((d (fn [[x y z]] (f x y z)))
                                (matrix/seq-> [x y z])))
              [:exactly 4]   (fn [w x y z]
                               ((d (fn [[w x y z]] (f w x y z)))
                                (matrix/seq-> [w x y z])))
              [:between 1 2] (fn
                               ([x] ((d f) x))
                               ([x y]
                                ((d (fn [[x y]] (f x y)))
                                 (matrix/seq-> [x y]))))
              (fn [& xs]
                (when (empty? xs) (u/illegal "No args passed to derivative?"))
                (if (= (count xs) 1)
                  ((d f) (first xs))
                  ((d #(apply f %)) (matrix/seq-> xs)))))]
     (f/with-arity df a {:from ::multivariate}))))

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
;; for [[s/Structure]] and [[matrix/Matrix]] is that structures act as functions
;; that apply their args to every (functional) entry. Calling `(multivariate
;; structure selectors)` allows all of the machinery that handles
;; structure-walking and argument conversion to run a SINGLE time before getting
;; passed to the structure of functions, instead of separately for every entry
;; in the structure.
;;
;; TODO: install this for other IFn entries like `Series`?

(doseq [t [::v/function ::s/structure ::matrix/matrix]]
  (defmethod g/partial-derivative [t v/seqtype] [f selectors]
    (multivariate f selectors))

  (defmethod g/partial-derivative [t nil] [f _]
    (multivariate f [])))

;; ## Operators
;;
;; This section exposes various differential operators as [[o/Operator]]
;; instances.

(def derivative-symbol 'D)

(def ^{:doc "Derivative operator. Produces a function whose value at some point
  can multiply an increment in the arguments, to produce the best linear
  estimate of the increment in the function value."}
  D
  (o/make-operator #(g/partial-derivative % [])
                   derivative-symbol))

(defn partial
  "Returns an operator that, when applied to a function `f`, produces a function
  that computes the partial derivative of `f` at the (zero-based) slot index
  provided via `selectors`."
  [& selectors]
  (o/make-operator #(g/partial-derivative % selectors)
                   `(~'partial ~@selectors)))

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
