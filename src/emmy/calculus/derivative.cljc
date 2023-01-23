#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.calculus.derivative
  "This namespace implements a number of differential operators like [[D]], and
  the machinery to apply [[D]] to various structures."
  (:refer-clojure :exclude [partial])
  (:require [emmy.differential :as d]
            [emmy.expression :as x]
            [emmy.function :as f]
            [emmy.generic :as g]
            [emmy.matrix :as matrix]
            [emmy.operator :as o]
            [emmy.series :as series]
            [emmy.structure :as s]
            [emmy.util :as u]
            [emmy.value :as v])
  #?(:clj
     (:import (clojure.lang Fn MultiFn))))

;; ## IPerturbed Implementation for Functions
;;
;; The following section, along with [[emmy.collection]]
;; and [[emmy.differential]], rounds out the implementations
;; of [[d/IPerturbed]] for native Clojure(script) data types. The function
;; implementation is subtle, as described by [Manzyuk et al.
;; 2019](https://arxiv.org/pdf/1211.4892.pdf).
;; ([[emmy.derivative.calculus-test]], in the "Amazing Bug" sections,
;; describes the pitfalls at length.)
;;
;; [[emmy.differential]] describes how each in-progress perturbed variable
;; in a derivative is assigned a "tag" that accumulates the variable's partial
;; derivative.
;;
;; How do we interpret the case where `((D f) x)` produces a _function_?
;;
;; [Manzyuk et al. 2019](https://arxiv.org/pdf/1211.4892.pdf) extends `D` to
;; functions `f` of type $\mathbb{R}^n \rightarrow \alpha$, where
;;
;; $$\alpha::=\mathbb{R}^m \mid \alpha_{1} \rightarrow \alpha_{2}$$
;;
;; By viewing
;;
;; - `f` as a (maybe curried) multivariable function that _eventually_ must
;;   produce an $\mathbb{R}^m$
;; - The derivative `(D f)` as the partial derivative with respect to the first
;;   argument of `f`
;;
;; A 3-level nest of functions will respond to `D` just like the flattened,
;; non-higher-order version would respond to `(partial 0)`. In other words,
;; these two forms should evaluate to equivalent results:

(comment
  (let [f (fn [x]
            (fn [y]
              (fn [z]
                (g/* x y z))))]
    ((((D f) 'x) 'y) 'z))
  ;;=> (* y z)
  )

(comment
  (((partial 0) g/*) 'x 'y 'z)
  ;;=> (* y z)
  )

;; To `extract-tangent` from a function, we need to compose the
;; `extract-tangent` operation with the returned function.
;;
;; The returned function needs to capture an internal reference to the
;; original [[d/Differential]] input. This is true for any Functor-shaped return
;; value, like a structure or Map. However! There is a subtlety present with
;; functions that's not present with vectors or other containers.
;;
;; The difference with functions is that they take _inputs_. If you contrive a
;; situation where you can feed the original captured [[d/Differential]] into
;; the returned function, this can trigger "perturbation confusion", where two
;; different layers try to extract the tangent corresponding to the SAME tag,
;; and one is left with nothing.
;;
;; If you engineer an
;; example (see [[emmy.calculus.derivative-test/amazing-bug]]) where:
;;
;; - this function takes another function, which then receives the closed-over
;;   `x` as an argument
;; - you pass this function to itself, so the closed-over `x` instances can both
;;   be multiplied
;;
;; Then your program isn't going to make any distinction between the instances
;; of `x`. They're both references to the same value.
;;
;; HOWEVER! `((D f) x)` returns a function which, when you eventually provide
;; all arguments, will return the sensitivity of `f` to the first argument `x`.
;;
;; If you perform the trick above, pass `((D f) x)` into itself, and the `x`
;; instances meet (multiply, say) - should final return value treat them as the
;; /same/ instance?
;;
;; Manzyuk et al. says _NO!_. If `((D f) x)` returns a function, that function
;; closes over:
;;
;; - the value of `x`
;; - an _intention_ to start the derivative-taking process on that isolated copy
;;   of `x` once the final argument is supplied.
;;
;; How does the implementation keep the values separate?
;;
;; ### Tag Replacement
;;
;; The key to the solution lives in [[extract-tangent-fn]], called on the result
;; of `((D f) x)` when `((D f) x)` produces a function. We have to armor the
;; returned function so that:
;;
;; - it extracts the originally-injected tag when someone eventually calls the
;;   function
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

  If called within the scope of a function waiting for the same `tag`, the
  returned function will remap any instance of `tag` that appears in any
  differential argument passed to it to a private `fresh` tag, to prevent
  internal perturbation confusion. Any tangent components in the final result
  tagged with `fresh` will be remapped in the final result back to `tag`.

  If called _outside_ of a function waiting for `tag` no tag remapping will
  occur."
  [f tag]
  (-> (fn [& args]
        (if (d/tag-active? tag)
          (let [fresh (d/fresh-tag)]
            (-> (d/with-active-tag tag f (map #(d/replace-tag % tag fresh) args))
                (d/extract-tangent tag)
                (d/replace-tag fresh tag)))
          (-> (d/with-active-tag tag f args)
              (d/extract-tangent tag))))
      (f/with-arity (f/arity f))))

;; NOTE: that the tag-remapping that the docstring for `extract-tag-fn`
;; describes might _also_ have to apply to a functional argument!
;;
;; `replace-tag` on a function is meant to be a `replace-tag` call applied to
;; the function's _output_. To prevent perturbation confusion inside the
;; function, we perform a similar remapping of any occurrence of `tag` in the
;; function's arguments.

(defn- replace-tag-fn
  "Returns a new function that composes a 'tag replacement' step with `f`.

  If called within the scope of a function waiting for the same `tag`, the
  returned function will:

  - make a fresh tag, and replace all `old` tags with `fresh` in the inputs
  - call `f`, producing `result`
  - return `(replace-tag result old new)`
  - remap any tangent component in the result tagged with `fresh` back to `old`.

  If called _outside_ of a function waiting for `tag`, the returned function
  will apply `f` to its arguments and call `(replace-tag result old new)` with
  no tag-rerouting."
  [f old new]
  (-> (fn [& args]
        (if (d/tag-active? old)
          (let [fresh (d/fresh-tag)
                args  (map #(d/replace-tag % old fresh) args)]
            (-> (apply f args)
                (d/replace-tag old new)
                (d/replace-tag fresh old)))
          (-> (apply f args)
              (d/replace-tag old new))))
      (f/with-arity (f/arity f))))

;; ## Protocol Implementation
;;
;; The implementation for functions handles functions, multimethods, and, in
;; ClojureScript, [[MetaFn]] instances. Metadata in the original function is
;; preserved through tag replacement and extraction.

(extend-protocol d/IPerturbed
  #?(:clj Fn :cljs function)
  (perturbed? [#?(:cljs _ :clj f)]
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
  (perturbed? [_] false)
  (replace-tag [f old new] (replace-tag-fn f old new))
  (extract-tangent [f tag] (extract-tangent-fn f tag)))

;; ## Single and Multivariable Calculus
;;
;; These functions put together the pieces laid out
;; in [[emmy.differential]] and declare an interface for taking
;; derivatives.

(defn derivative
  "Returns a single-argument function of that, when called with an argument `x`,
  returns the derivative of `f` at `x` using forward-mode automatic
  differentiation.

  For numerical differentiation,
  see [[emmy.numerical.derivative/D-numeric]].

  `f` must be built out of generic operations that know how to
  handle [[d/Differential]] inputs in addition to any types that a normal `(f
  x)` call would present. This restriction does _not_ apply to operations like
  putting `x` into a container or destructuring; just primitive function calls."
  [f]
  (fn [x]
    (let [tag    (d/fresh-tag)
          lifted (d/bundle-element x 1 tag)]
      (-> (d/with-active-tag tag f [lifted])
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
;;
;; [[jacobian]] handles this main logic. [[jacobian]] can only take a structural
;; input. [[euclidean]] and [[multivariate]] below widen handle, respectively,
;; optionally-structural and multivariable arguments.

(defn- deep-partial
  "Returns the partial derivative of `f` with respect to the entry in `structure`
  at the location `path`.

  `entry` defaults to `(get-in structure path)`."
  ([f structure path]
   (let [entry (get-in structure path)]
     (deep-partial f structure path entry)))
  ([f structure path entry]
   (if (v/numerical? entry)
     (letfn [(f-entry [x]
               (f (assoc-in structure path x)))]
       ((derivative f-entry) entry))
     (u/illegal
      (str "non-numerical entry " entry
           " at path " path
           " in input structure " structure)))))

(defn- jacobian
  "Takes:

  - some function `f` of a single [[emmy.structure/structure?]] argument
  - the unperturbed structural `input`
  - a `selectors` vector that can be empty or contain a valid path into the
    `input` structure

  and returns either:

  - The full [Jacobian](https://en.wikipedia.org/wiki/Jacobian_matrix_and_determinant)
    of `f` at `input`, if `selectors` is empty
  - the entry of the Jacobian at `selectors`

  The Jacobian has the same shape as `input` (or the entry at `selectors`) with
  all orientations flipped. Multiply this by an increment in the shape of
  `input` to produce an increment in the output of `f`."
  ([f input] (jacobian f input []))
  ([f input selectors]
   (letfn [(prefixed [path]
             (if (empty? selectors)
               path
               (into selectors path)))]
     (if-let [piece (get-in input selectors)]
       (let [frame (s/transpose piece)]
         ;; Visit each entry in `frame`, a copy of either the full input or the
         ;; sub-piece living at `selectors` (with all orientations flipped), and
         ;; replace the entry with the result of the partial derivative of `f`
         ;; with that entry perturbed.
         (s/map-chain
          (fn [entry path _]
            (deep-partial f input (prefixed path) entry))
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
             ;; implementation (as of 0.15.0) pushes the derivative operator
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
   (let [d #(euclidean % selectors)
         df (d f)
         df* (d (fn [args] (apply f args)))]
     (-> (fn
           ([] (constantly 0))
           ([x] (df x))
           ([x & more]
            (df* (matrix/seq-> (cons x more)))))
         (f/with-arity (f/arity f) {:from ::multivariate})))))

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
;; for [[emmy.structure/Structure]] is that structures act as functions
;; that apply their args to every (functional) entry. Calling `(multivariate
;; structure selectors)` allows all of the machinery that handles
;; structure-walking and argument conversion to run a SINGLE time before getting
;; passed to the structure of functions, instead of separately for every entry
;; in the structure.
;;
;; TODO: I think this is going to cause problems for, say, a Structure of
;; PowerSeries, where there is actually a cheap `g/partial-derivative`
;; implementation for the components. I vote to back out this `::s/structure`
;; installation.

(doseq [t [::v/function ::s/structure]]
  (defmethod g/partial-derivative [t v/seqtype] [f selectors]
    (multivariate f selectors))

  (defmethod g/partial-derivative [t nil] [f _]
    (multivariate f [])))

;; ## Operators
;;
;; This section exposes various differential operators as [[o/Operator]]
;; instances.

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
                   g/derivative-symbol))

(defn D-as-matrix [F]
  (fn [s]
    (matrix/s->m
     (s/compatible-shape (F s))
     ((D F) s)
     s)))

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
  "Given a differentiable function `f` and any number of arguments `xs`, returns
  a [[emmy.series/PowerSeries]] representing the [Taylor
  series](https://en.wikipedia.org/wiki/Taylor_series) of the function `f`
  expanded at `xs`.

  Calling [[taylor-series]] with no arguments will return the [Maclaurin
  series](https://en.wikipedia.org/wiki/Taylor_series#List_of_Maclaurin_series_of_some_common_functions)
  of `f`, ie, the Taylor series expansion at `(= x 0)`.

  Calling the returned power series with incremental argument `dx` will produce
  a [[emmy.series/Series]] representing the terms of the Taylor series of
  `f` expanded at `x` and evaluated at `x+dx`.

  NOTE: Just like the [[D]] operator, functions `f` of multiple-arguments are
  treated as a function of a single structural argument. If you pass multiple
  arguments `xs`, you'll have to manually wrap your multiple-argument `dx` in
  a [[emmy.structure/up]] or a vector before passing it to the returned
  power series.

  NOTE: The typical definition of a Taylor series of `f` expanded around some
  point `x` is

  $$T(p) = f(x) + \\frac{f'(x)}{1!}(p-x) + \\frac{f''(x)}{2!} (p-x)^2 + \\ldots,$$

  where `p` is the evaluation point. When `(= p x)`, all derivatives of the
  Taylor series expansion of `f` will exactly match the derivatives of `f`
  itself.

  The Taylor series returned here (call it $T'$) is actually a function of `dx`,
  where

  $$T'(dx) = T(x+dx) = f(x) + \\frac{f'(x)}{1!}(dx) + \\frac{f''(x)}{2!} (dx)^2 + \\ldots.$$"
  ([f] (taylor-series f 0))
  ([f & xs]
   (series/->function
    (apply ((g/exp D) f) xs))))

(defn symbolic-taylor-series
  "Similar to [[taylor-series]], except `f` is evaluated with symbolic arguments,
  and these arguments are only replaced with the values `xs` after Taylor series
  expansion.

  Please see the docs for [[taylor-series]]!"
  ([f] (symbolic-taylor-series f 0))
  ([f & xs]
   (let [syms      (map s/typical-object xs)
         replace-m (zipmap (flatten syms)
                           (flatten xs))
         series    (apply taylor-series f syms)]
     (letfn [(process-term [term]
               (g/simplify
                (s/mapr (fn rec [x]
                          (if (d/differential? x)
                            (d/map-coefficients rec x)
                            (-> (g/simplify x)
                                (x/substitute replace-m))))
                        term)))]
       (series/fmap process-term series)))))
