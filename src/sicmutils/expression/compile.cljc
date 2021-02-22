;; Copyright © 2021 Sam Ritchie.
;; This work is based on the Scmutils system of MIT/GNU Scheme:
;; Copyright © 2002 Massachusetts Institute of Technology

;; This is free software;  you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or (at
;; your option) any later version.

;; This software is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this code; if not, see <http://www.gnu.org/licenses/>.
;;

(ns sicmutils.expression.compile
  "This namespace contains tools for compiling functions implemented with the
  numeric operations defined in [[sicmutils.generic]] down to fast, native
  functions."
  (:require #?(:cljs [goog.string :refer [format]])
            [clojure.set :as set]
            [clojure.walk :as w]
            [sci.core :as sci]
            [sicmutils.expression :as x]
            [sicmutils.expression.analyze :as a]
            [sicmutils.function :as f]
            [sicmutils.generic :as g]
            [sicmutils.structure :as struct]
            [sicmutils.util :as u]
            [sicmutils.util.stopwatch :as us]
            [taoensso.timbre :as log]))

;; # Function Compilation
;;
;; Functions built out of generic operations can be compiled down into fast,
;; efficient functions that operate on doubles. The process is:
;;
;; 1. Pass symbols like `'x` in for all arguments. This will cause the function
;;    to return a "numerical expression", a syntax tree representing the
;;    function's body:
#_
(let [f (fn [x] (g/sqrt
                (g/+ (g/square (g/sin x))
                     (g/square (g/cos x)))))]
(= '(sqrt (+ (expt (sin x) 2) (expt (cos x) 2)))
   (x/expression-of (f 'x))))

;; 2. `g/simplify` the new function body. Sometimes this results in large
;;    simplifications:

#_
(let [f (fn [x] (g/sqrt
                (g/+ (g/square (g/sin x))
                     (g/square (g/cos x)))))]
  (= 1 (g/simplify (f 'x))))

;; 3. Apply "common subexpression elimination". Any subexpression inside the
;;    new, simplified body that appears more than once gets extracted out into a
;;    let-bound variable and calculated just once, then subbed in to the
;;    computation using the generated variable.
;;
;; 4. Wrap the new, efficient body up in `(fn [x] ,,,)` with the proper number
;;    of arguments and pass it to `eval`.
;;
;; Amazing!
;;
;; ## Implementation
;;
;; Function compilation is potentially expensive, so allocate a cache of
;; function `f` to compiled function for use below:

(def ^:private fn-cache (atom {}))

(def ^{:private true
       :doc "Dict of symbol -> function body. The keys constitute the set of
       operations allowed to appear within the body of the compiled function.

       If you're compiling a function for use with the numerical routines, the
       library assumes that your function operates only on doubles (even though
       you wrote it with generic routines)."}
  compiled-fn-whitelist
  {'up struct/up
   'down struct/down
   '+ +
   '- -
   '* *
   '/ /
   'expt #(Math/pow %1 %2)
   'sqrt #(Math/sqrt %)
   'abs (fn [^double x] (Math/abs x))
   'log #(Math/log %)
   'exp #(Math/exp %)
   'cos #(Math/cos %)
   'sin #(Math/sin %)
   'tan #(Math/tan %)
   'acos #(Math/acos %)
   'asin #(Math/asin %)
   'atan #(Math/atan %)
   'cosh #(Math/cosh %)
   'sinh #(Math/sinh %)
   'tanh #(Math/tanh %)
   #?@(:cljs
       ;; JS-only entries.
       ['acosh #(Math/acosh %)
        'asinh #(Math/asinh %)
        'atanh #(Math/atanh %)])})

;; ## Subexpression Elimination
;;
;; This section implements subexpression extraction and "elimination", the
;; process we use to avoid redundant computation inside of a simplified function
;; body.
;;
;; The goal of this process is to split some symbolic expression into:
;;
;; - a map of symbol -> redundant subexpression
;; - a new expression with each redundant subexpr replaced with its
;;   corresponding symbol.
;;
;; The invariant we want to achieve is that the new expression, rehydrated using
;; the symbol->subexpression map, should be equivalent to the old expression.
;;
;; First, we write a function to determine how often each subexpression appears.
;; Subexpressions that appear just once aren't worth extracting, but two
;; appearances is enough.

(defn- expr-frequencies
  "Returns a map from distinct subexpressions in the supplied `expr` to the
  number of times each appears.

  If `skip?` returns true given a subexpression it won't be included as a key in
  the returned map."
  [expr skip?]
  (let [children (partial filter seq?)]
    (->> (rest
          (tree-seq seq? children expr))
         (remove skip?)
         (frequencies))))

;; The next function assumes that we have the two data structures we referenced
;; earlier. We want to be careful that we only generate and bind subexpressions
;; that are actually used in the final computation.
;;
;; `discard-unferenced-syms` ensures this by removing any entry from our
;; replacement map that doesn't appear in the expression it's passed, or any
;; subexpression referenced by a symbol in the expression, etc etc.
;;
;; The algorithm is:
;;
;; - consider the intersection of all variables in the replacement map and the
;;   supplied expression, and store these into a map of "referenced" symbols.
;;
;; - Look up the corresponding subexpression for each of these symbols, and add
;;   all potential replacements from each subexpression into the "referenced"
;;   list, continuing this process recursively until all candidates are
;;   exhausted.
;;
;; - Return subset of the original `sym->expr` by removing any key not found in
;; - the accumulated "referenced" set.

(defn- discard-unreferenced-syms
  "Trims down a proposed map of `sym->expr` replacements (suitable for a let-binding,
  say) to include only entries relevant to the supplied `expr`.

  Accepts:

  - `sym->expr`, a map of symbol -> symbolic expression
  - `expr`, an expression that potentially contains symbols referenced in the
    keyset of `sym->expr`

  And returns a subset of `sym->expr` containing only entries where the
  symbol-key is found in:

  - the original `expr`, or
  - in an expression referenced by a symbol in the original `expr`"
  [sym->expr expr]
  (let [syms        (u/keyset sym->expr)
        lookup-syms (mapcat (comp x/variables-in sym->expr))]
    (loop [referenced #{}
           sym-batch  (-> (x/variables-in expr)
                          (set/intersection syms))]
      (if (empty? sym-batch)
        (select-keys sym->expr referenced)
        (let [referenced'   (set/union referenced sym-batch)
              syms-in-exprs (-> (into #{} lookup-syms sym-batch)
                                (set/intersection syms)
                                (set/difference referenced'))]
          (recur referenced' syms-in-exprs))))))

;; This final function implements common subexpression extraction in
;; continuation-passing-style. Pass it a callback and it will invoke the
;; callback with the two arguments described above, and detailed below in its
;; docstring.
;;
;; The algorithm is:
;;
;; - For every subexpression that appears more than once in the supplied
;;   expression, generate a new, unique symbol.
;;
;; - Generate a new expression by replacing every subexpression in the supplied
;;   expression with a symbol using the new mapping of symbol -> subexpression.
;;
;; - Recursively keep going until there are no more common subexpressions to
;;   replace. At this point, discard all extra bindings (see
;;   `discard-unreferenced-syms` above) and call the continuation function with
;;   the /new/ slimmed expression, and a sorted-by-symbol list of binding pairs.
;;
;; These two return values satisfy the invariant we described above: the new
;; expression, rehydrated using the symbol->subexpression map, should give us
;; back the old expression.
;;
;; NOTE that the algorithm as implemented below uses a postwalk tree traversal!
;; This is important, as it forces us to consider smaller subexpressions first.
;; Consider some expression like:

#_
(+ (* (sin x) (cos x))
   (* (sin x) (cos x))
   (* (sin x) (cos x)))

;; At first pass, we have three repeated subexpressions:
;;
;; - `(sin x)`
;; - `(cos x)`
;; - `(* (sin x) (cos x))`
;;
;; Postwalk traversal guarantees that we replace the `sin` and `cos` terms
;; before the larger term that contains them both. And in fact the returned pair
;; looks like:

#_
[(+ g3 g3 g3) ([g1 (sin x)] [g2 (cos x)] [g3 (* g1 g2)])]

;; NOTE also that:
;;
;; - this is why the `:symbol-generator` below must generate symbols that sort
;;   in the order they're generated. Else, the final binding vector might put
;;   the `g3` term in the example above /before/ the smaller subexpressions it
;;   uses.
;;
;; - This algorithm justifies `discard-unreferenced-syms` above. Each pass will
;;   larger subexpressions like `'(* (sin x) (cos x))` that should never make it
;;   out, since they never appear in this form (since they contain smaller
;;   subexpressions).

(def sortable-gensym
  (a/monotonic-symbol-generator "G"))

(defn extract-common-subexpressions
  "Considers an S-expression from the point of view of optimizing its evaluation
  by isolating common subexpressions into auxiliary variables.

  Accepts:

  - A symbolic expression `expr`
  - a continuation fn `continue` of two arguments:
    - a new equivalent expression with possibly some subexpressions replaced by
      new variables (delivered by the supplied generator, see below)
    - a seq of pairs of `[aux variable, subexpression]` used to reconstitute the
      value.

  Calls the continuation at completion and returns the continuation's value.

  ### Optional Arguments

  `:symbol-generator`: side-effecting function that returns a new, unique
  variable name on each invocation. `sortable-gensym` by default.

  NOTE that the symbols should appear in sorted order! Otherwise we can't
  guarantee that the binding sequence passed to `continue` won't contain entries
  that reference previous entries.

  `:deterministic?`: if true, the function will assign aux variables by sorting
  the string representations of each term before assignment. Otherwise, the
  nondeterministic order of hash maps inside this function won't guarantee a
  consistent variable naming convention in the returned function. For tests, set
  `:deterministic? true`."
  ([expr continue] (extract-common-subexpressions expr continue {}))
  ([expr continue {:keys [symbol-generator deterministic?]
                   :or {symbol-generator sortable-gensym}}]
   (let [sort (if deterministic?
                (partial sort-by (comp str vec first))
                identity)]
     (loop [x         expr
            expr->sym {}]
       (let [expr->count (expr-frequencies x expr->sym)
             new-syms    (into {} (for [[k v] (sort expr->count)
                                        :when (> v 1)]
                                    [k (symbol-generator)]))]
         (if (empty? new-syms)
           (let [sym->expr (-> (set/map-invert expr->sym)
                               (discard-unreferenced-syms x))]
             (continue x (sort-by key sym->expr)))
           (let [expr->sym' (merge expr->sym new-syms)]
             (recur (w/postwalk-replace expr->sym' x)
                    expr->sym'))))))))

;; This final wrapper function invokes `extract-common-subexpressions` to turn a
;; symbolic expression a new, valid Clojure(script) form that uses a `let`
;; binding to bind any common subexpressions exposed during the above search.
;;
;; If there are no common subexpressions, `cse-form` will round-trip its input.

(defn cse-form
  "Given a symbolic expression `expr`, returns a new expression potentially
  wrapped in a `let` binding with one binding per extracted common
  subexpression.

  ## Optional Arguments

  `:symbol-generator`: side-effecting function that returns a new, unique symbol
  on each invocation. These generated symbols are used to create unique binding
  names for extracted subexpressions. `sortable-gensym` by default.

  NOTE that the symbols should appear in sorted order! Otherwise we can't
  guarantee that the binding sequence won't contain entries that reference
  previous entries, resulting in \"Unable to resolve symbol\" errors.

  `:deterministic?`: if true, the function will order the let-binding contents
  by sorting the string representations of each term before assignment. If false
  the function won't guarantee a consistent variable naming convention in the
  returned function. For tests, we recommend `:deterministic? true`."
  ([expr] (cse-form expr {}))
  ([expr opts]
   (letfn [(callback [new-expression bindings]
             (let [n-bindings (count bindings)]
               (if (pos? n-bindings)
                 (let [binding-vec (into [] cat bindings)]
                   (log/info
                    (format "common subexpression elimination: %d expressions" n-bindings))
                   `(let ~binding-vec
                      ~new-expression))
                 new-expression)))]
     (extract-common-subexpressions expr callback opts))))

;; ### SCI vs Native Compilation
;;
;; Armed with the above compiler optimization we can move on to the actual
;; compilation step.
;;
;; This library provides two compilation modes:
;;
;; - Native compilation via `eval`
;; - interpreted compilation via [SCI](https://github.com/borkdude/sci), the
;;   Small Clojure Interpreter.
;;
;; We default to SCI mode in CLJS, but :native in Clojure for performance.

(def ^{:dynamic true
       :no-doc true}
  *mode*
  #?(:clj :native
     :cljs :sci))

(defn- native?
  "Returns true if native compilation mode is enabled, false otherwise."
  []
  (= *mode* :native))

;; Native compilation works on the JVM, and on Clojurescript if you're running
;; in a self-hosted CLJS environment. Enable this mode by wrapping your call in
;;
;; `(binding [*mode* :native] ,,,)`
;;
;; NOTE that we may remove native compilation support if it doesn't prove to be
;; a performance problem; sci with its sandboxing is a safer thing to offer in a
;; library that might get hosted for others to interact with via remote REPLs.

;; ## State Functions
;;
;; `compile.cljc` currently supports compilation of:
;;
;; - univariate functions (for use with ODEs, `definite-integral` etc)
;; - "state" functions.
;;
;; A state function is a function that takes any number of arguments and returns
;; a new function of a "structure", usually an up-or-down tuple or a Clojure
;; sequence.
;;
;; The compiled version of a state function like

#_
(fn [mass g]
  (fn [q] ,,,))

;; Has a signature like

#_
(fn [q [mass g]] ,,,)

;; IE, first the structure, then a vector of the original function's arguments.

(defn- state-argv
  "Returns the argument vector for a compiled state function, given:

  - `params`: a seq of symbols equal in count to the original state function's
    args
  - `state-model`: a sequence of variables representing the structure of the
    nested function returned by the state function"
  [params state-model]
  [(into [] (flatten state-model))
   (into [] params)])

;; The following two functions compile state functions in either native or SCI
;; mode. The primary difference is that native compilation requires us to
;; explicitly replace all instances of symbols from `compiled-fn-whitelist`
;; above with actual functions.
;;
;; SCI handles this behind its interface, and simply takes a reusable context
;; that wraps the fn replacement mapping.

(def ^{:private true
       :doc "Reuseable context for SCI compilation. Fork with `sci/fork` to
  ensure that no call to `sci/eval-*` can inject state that another call can
  see."}
  sci-context
  (sci/init
   {:bindings compiled-fn-whitelist}))

(defn- compile-state-native
  "Returns a natively-evaluated Clojure function that implements `body`, given:

  - `params`: a seq of symbols equal in count to the original state function's
    args
  - `state-model`: a sequence of variables representing the structure of the
    nested function returned by the state function
  - `body`: a function body making use of any symbol in the args above"
  [params state-model body]
  (let [body (w/postwalk-replace compiled-fn-whitelist body)]
    (eval
     `(fn ~(state-argv params state-model) ~body))))

(defn- compile-state-sci
  "Returns a Clojure function evaluated using SCI. The returned fn implements
  `body`, given:

  - `params`: a seq of symbols equal in count to the original state function's
    args
  - `state-model`: a sequence of variables representing the structure of the
    nested function returned by the state function
  - `body`: a function body making use of any symbol in the args above"
  ([params state-model body]
   (let [f `(fn ~(state-argv params state-model) ~body)]
     (sci/eval-form (sci/fork sci-context) f))))

;; ### State Fn Interface
;;
;; Now we come to the final interface for state function compilation. Two
;; versions of the following function exist, one that uses the global cache
;; defined above and one that doesn't.

(defn compile-state-fn*
  "Returns a compiled, simplified function, given:

  - a state function that can accept a symbolic arguments

  - `params`; really any sequence of count equal to the number of arguments
    taken by `f`. The values are ignored.

  - `initial-state`: Some structure of the same shape as the argument expected
    by the fn returned by the state function `f`. Only the shape matters; the
    values are ignored.

  The returned, compiled function expects `Double` (or `js/Number`) arguments.
  The function body is simplified and all common subexpressions identified
  during compilation are extracted and computed only once.

  NOTE this function uses no cache. To take advantage of the global compilation
  cache, see `compile-state-fn`."
  [f params initial-state]
  (let [sw             (us/stopwatch)
        generic-params (for [_ params] (gensym 'p))
        generic-state  (struct/mapr (fn [_] (gensym 'y)) initial-state)
        g              (apply f generic-params)
        body           (cse-form
                        (g/simplify (g generic-state)))
        compiler       (if (native?)
                         compile-state-native
                         compile-state-sci)
        compiled-fn    (compiler generic-params generic-state body)]
    (log/info "compiled state function in" (us/repr sw) "with mode" *mode*)
    compiled-fn))

(defn compile-state-fn
  "Returns a compiled, simplified function, given:

  - a state function that can accept a symbolic arguments

  - `params`; really any sequence of count equal to the number of arguments
    taken by `f`. The values are ignored.

  - `initial-state`: Some structure of the same shape as the argument expected
    by the fn returned by the state function `f`. Only the shape matters; the
    values are ignored.

  The returned, compiled function expects `Double` (or `js/Number`) arguments.
  The function body is simplified and all common subexpressions identified
  during compilation are extracted and computed only once.

  NOTE that this function makes use of a global compilation cache, keyed by the
  value of `f`. Passing in the same `f` twice, even with different arguments for
  `param` and `initial-state`, will return the cached value. See
  `compile-state-fn*` to avoid the cache."
  [f params initial-state]
  (if-let [cached (@fn-cache f)]
    (do
      (log/info "compiled state function cache hit")
      cached)
    (let [compiled (compile-state-fn* f params initial-state)]
      (swap! fn-cache assoc f compiled)
      compiled)))

;; ## Non-State-Functions
;;
;; Compiled functions are excellent input for `definite-integral`, ODE solvers,
;; single variable function minimization, root finding and more.
;;
;; The implementation and compilation steps are simpler than the state function
;; versions above; the function you pass in has to take `n` symbolic arguments,
;; that's it.

(defn- compile-native
  "Returns a natively-evaluated Clojure function that implements `body`, given
  some sequence `args` of argument symbols.

  `body` should of course make use of the symbols in `args`."
  [args body]
  (let [body (w/postwalk-replace compiled-fn-whitelist body)]
    (eval `(fn ~(vec args) ~body))))

(defn- compile-sci
  "Returns a Clojure function evaluated
  using [SCI](https://github.com/borkdude/sci) The returned fn implements
  `body`, given some sequence `args` of argument symbols.

  `body` should of course make use of the symbols in `args`."
  [args body]
  (let [f `(fn ~(vec args) ~body)]
    (sci/eval-form (sci/fork sci-context) f)))

(defn- retrieve-arity [f]
  (let [[kwd n :as arity] (f/arity f)]
    (if (= kwd :exactly)
      n
      (u/illegal
       (str "`compile-fn` can only infer arity for functions with just one
           arity, not " arity ". Please pass an explicit `n`.")))))

(defn compile-fn*
  "Returns a compiled, simplified version of `f`, given a function `f` of arity
  `n` (ie, able to accept `n` symbolic arguments).

  `n` defaults to `([[f/arity]] f)`.

  The returned, compiled function expects `n` `Double` (or `js/Number`)
  arguments. The function body is simplified and all common subexpressions
  identified during compilation are extracted and computed only once.

  NOTE: this function uses no cache. To take advantage of the global compilation
  cache, see `compile-fn`."
  ([f] (compile-fn* f (retrieve-arity f)))
  ([f n]
   (let [sw       (us/stopwatch)
         args     (repeatedly n #(gensym 'x))
         body     (cse-form
                   (g/simplify (apply f args)))
         compiled (if (native?)
                    (compile-native args body)
                    (compile-sci args body))]
     (log/info "compiled function of arity" n "in" (us/repr sw) "with mode" *mode*)
     compiled)))

(defn compile-fn
  "Returns a compiled, simplified version of `f`, given a function `f` of arity
  `n` (ie, able to accept `n` symbolic arguments).

  `n` defaults to `([[f/arity]] f)`.

  The returned, compiled function expects `n` `Double` (or `js/Number`)
  arguments. The function body is simplified and all common subexpressions
  identified during compilation are extracted and computed only once.

  NOTE: that this function makes use of a global compilation cache, keyed by the
  vector `[f n]`. See `compile-fn*` to avoid the cache."
  ([f] (let [[kwd n :as arity] (f/arity f)]
         (when-not (= kwd :exactly)
           (u/illegal
            (str "`compile-fn` can only infer arity for functions with just one
           arity, not " arity ". Please pass an explicit `n`.")))
         (compile-fn f n)))
  ([f n]
   (if-let [cached (@fn-cache [f n])]
     (do
       (log/info "compiled function cache hit - arity " n)
       cached)
     (let [compiled (compile-fn* f n)]
       (swap! fn-cache assoc [f n] compiled)
       compiled))))
