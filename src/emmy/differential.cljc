#_"SPDX-License-Identifier: GPL-3.0"

^{:nextjournal.clerk/visibility #{:hide-ns}}
(ns emmy.differential
  "This namespace contains an implementation of [[Differential]], a generalized
  dual number type that forms the basis for the forward-mode automatic
  differentiation implementation in emmy.

  See [[emmy.calculus.derivative]] for a fleshed-out derivative
  implementation using [[Differential]]."
  (:refer-clojure :exclude [compare])
  (:require [clojure.core :as core]
            [clojure.string :refer [join]]
            [emmy.generic :as g]
            [emmy.util :as u]
            [emmy.util.aggregate :as ua]
            [emmy.util.stream :as us]
            [emmy.util.vector-set :as uv]
            [emmy.value :as v]))

;; ## Differentials, Dual Numbers and Automatic Differentiation
;;
;; This namespace develops an implementation of a type called [[Differential]].
;; A [[Differential]] is a generalization of a type called a ["dual
;; number"](https://en.wikipedia.org/wiki/Dual_number).
;;
;; As we'll discuss, passing these numbers as arguments to some function $f$
;; built out of the [[emmy.generic]] operators allows us to build up the
;; _derivative_ of $f$ in parallel to our evaluation of $f$. Complex programs
;; are built out of simple pieces that we know how to evaluate; we can build up
;; derivatives of entire programs in a similar way by building them out of the
;; derivatives of the smaller pieces of those programs.
;;
;; ### Forward-Mode Automatic Differentiation
;;
;; For many scientific computing applications, it's valuable be able to generate
;; a "derivative" of a function; given some tiny increment in the inputs, what
;; tiny increment will the function produce in the output values?
;;
;; we know how to take derivatives of many of the generic functions exposed by
;; Emmy, like [[+]], [[*]], [[g/sin]] and friends. It turns out that we can
;; take the derivatives of large, complicated functions by combining the
;; derivatives of these smaller functions using the [chain
;; rule]((https://en.wikipedia.org/wiki/Automatic_differentiation#The_chain_rule,_forward_and_reverse_accumulation))
;; as a clever bookkeeping device.
;;
;; The technique of evaluating a function and its derivative in parallel is
;; called "forward-mode [Automatic
;; Differentiation](https://en.wikipedia.org/wiki/Automatic_differentiation)".
;; The [Emmy
;; wiki](https://github.com/emmy/emmy/wiki/Automatic-Differentiation)
;; has more information on the history of this technique, and links to the many
;; other implementations you'll find in different languages. See the [cljdocs
;; Automatic Differentiation
;; page](https://cljdoc.org/d/emmy/emmy/CURRENT/doc/calculus/automatic-differentiation)
;; for "how do I use this?"-style questions.
;;
;; NOTE: The other flavor of automatic differentiation (AD) is "reverse-mode
;; AD". See [[emmy.tape]] for an implementation of this style, coming soon!
;;
;; ### Dual Numbers and AD
;;
;; Our goal is to build up derivatives of complex functions out of the
;; derivatives of small pieces. A [dual
;; number](https://en.wikipedia.org/wiki/Dual_number) is a relatively simple
;; piece of machinery that will help us accomplish this goal.

;; A [dual number](https://en.wikipedia.org/wiki/Dual_number) is a pair of
;; numbers of the form
;;
;; $$a + b \varepsilon$$
;;
;; where $a$ and $b$ are real numbers, and $\varepsilon$ is an abstract thing,
;; with the property that $\varepsilon^2 = 0$.

;; NOTE: This might remind you of the definition of a complex number of the form
;; $a + bi$, where $i$ is also a new thing with the property that $i^2 = -1$.
;; You are very wise! The bigger idea lurking here is the ["generalized complex
;; number"](https://people.rit.edu/harkin/research/articles/generalized_complex_numbers.pdf).
;;
;; Why are dual numbers useful (in Emmy)? If you pass $a+b\varepsilon$ in
;; to a function $f$, the result is a dual number $f(a) + Df(a) b \varepsilon$;
;; the result contains both the function evaluation and the derivative
;; evaluation at $a$!

;; To see why, look at what happens when you pass a dual number into the [Taylor
;; series expansion](https://en.wikipedia.org/wiki/Taylor_series) of some
;; arbitrary function $f$. As a reminder, the Taylor series expansion of $f$
;; around some point $a$ is:
;;
;; $$f(x) = f(a)+\frac{Df(a)}{1!}(x-a)+\frac{D^2f(a)}{2!}(x-a)^{2}+\frac{D^3f(a)}{3!}(x-a)^{3}+\cdots$$
;;
;; NOTE: See this nice overview of [Taylor series
;; expansion](https://medium.com/@andrew.chamberlain/an-easy-way-to-remember-the-taylor-series-expansion-a7c3f9101063)
;; by Andrew Chamberlain if you want to understand this idea and why we can
;; approximate (smooth) functions this way.
;;
;; If you evaluate the expansion of $f(x)$ around $a$ with a dual number
;; argument whose first component is $a$ -- take $x=a+b\varepsilon$, for example
;; -- watch how the expansion simplifies:
;;
;; $$f(a+b\varepsilon) = f(a)+\frac{Df(a)}{1!}(b\varepsilon)+\frac{D^2f(a)}{2!}(b\varepsilon)^2+\cdots$$
;;
;; Since $\varepsilon^2=0$ we can ignore all terms beyond the first two:
;;
;; $$f(a+b\varepsilon) = f(a)+ (Df(a)b)\varepsilon$$
;;
;; NOTE: See [[lift-1]] for an implementation of this idea.
;;
;; This justifies our claim above: applying a function to some dual number
;; $a+\varepsilon$ returns a new dual number, where
;;
;; - the first component is $f(a)$, the normal function evaluation
;; - the second component is $Df(a)$, the derivative.
;;
;; If we do this twice, the second component of the returned dual number
;; beautifully recreates the [Chain
;; Rule](https://en.wikipedia.org/wiki/Chain_rule):
;;
;; $$
;; \begin{aligned}
;; g(f(a+\varepsilon)) &= g(f(a) + Df(a)\varepsilon) \\
;; &= g(f(a)) + (Dg(f(a)))(Df(a))\varepsilon
;; \end{aligned}
;; $$
;;
;; ### Terminology Change
;;
;; A "dual number" is a very general idea. Because we're interested in dual
;; numbers as a bookkeeping device for derivatives, we're going to specialize
;; our terminology. From now on, we'll rename $a$ and $b$ to $x$ and $x'$. Given
;; a dual number of the form $x+x'\varepsilon$: we'll refer to:
;;
;; - $x$ as the "primal" part of the dual number
;; - $x'$ as the "tangent" part
;; - $\varepsilon$ as the "tag"
;;
;; NOTE: "primal" means $x$ is tracking the "primal", or "primary", part of the
;; computation. "tangent" is a synonym for "derivative". "tag" is going to make
;; more sense shortly, when we start talking about mixing together multiple
;; $\varepsilon_1$, $\varepsilon_2$ from different computations.
;;
;; ### Binary Functions
;;
;; What about functions of more than one variable? We can use the same approach
;; by leaning on the [multivariable Taylor series
;; expansion](https://en.wikipedia.org/wiki/Taylor_series#Taylor_series_in_several_variables).
;; Take $f(x, y)$ as a binary example. If we pass dual numbers in to the taylor
;; series expansion of $f$, the $\varepsilon$ multiplication rule will erase all
;; higher-order terms, leaving us with:
;;
;; $$f(x+x'\varepsilon, y+y'\varepsilon) = f(x,y) + \left[\partial_1 f(x,y)x' + \partial_2 f(x,y)y'\right]\varepsilon$$
;;
;; NOTE: See [[lift-2]] for an implementation of this idea.
;;
;; This expansion generalizes for n-ary functions; every new argument $x_n +
;; x'_n\varepsilon$ contributes $\partial_n f(...)x'_n$ to the result.
;;
;; We can check this with the simple cases of addition, subtraction and
;; multiplication.
;;
;; The real parts of a dual number add commutatively, so we can rearrange the
;; components of a sum to get a new dual number:
;;
;; $$(x+x'\varepsilon)+(y+y'\varepsilon) == (x+y)+(x'+y')\varepsilon$$
;;
;; This matches the [sum
;; rule](https://en.wikipedia.org/wiki/Differentiation_rules#Differentiation_is_linear)
;; of differentiation, since the partials of $x + y$ with respect to either $x$
;; or $y$ both equal 1.
;;
;; Subtraction is almost identical and agrees with the [subtraction
;; rule](https://en.wikipedia.org/wiki/Differentiation_rules#Differentiation_is_linear):
;;
;; $$(x+x'\varepsilon)-(y+y'\varepsilon) == (x-y)+(x'-y')\varepsilon$$
;;
;; Multiplying out the components of two dual numbers again gives us a new dual
;; number, whose tangent component agrees with the [product
;; rule](https://en.wikipedia.org/wiki/Product_rule):
;;
;; $$
;; \begin{aligned}
;; (x+ x'\varepsilon)*(y+y'\epsilon) &= xy+(xy')\varepsilon+(x'y)\varepsilon+(x'y')\epsilon^2 \\
;; &= xy+(xy'+x'y)\varepsilon
;; \end{aligned}
;; $$
;;
;; Stare at these smaller derivations and convince yourself that they agree with
;; the Taylor series expansion method for binary functions.
;;
;; The upshot is that, armed with these techniques, we can implement a
;; higher-order `derivative` function (almost!) as simply as this:

(comment
  (defn derivative [f]
    (fn [x]
      (extract-tangent
       (f (make-dual x 1))))))

;; As long as `f` is built out of functions that know how to apply themselves to
;; dual numbers, this will all Just Work.
;;
;; ### Multiple Variables, Nesting
;;
;; All of the examples above are about first-order derivatives. Taking
;; higher-order derivatives is, in theory, straightforward:

(comment
  (derivative
   (derivative f)))

;; But this guess hits one of many subtle problems with the implementation of
;; forward-mode AD. The double-call to `derivative` will expand out to this:

(comment
  (fn [x]
    (letfn [(inner-d [x]
              (extract-tangent
               (f (make-dual x 1))))]
      (extract-tangent
       (inner-d
        (make-dual x 1))))))

;; the `x` received by `inner-d` will ALREADY be a dual number $x+\varepsilon$!
;; This will cause two immediate problems:
;;
;; - `(make-dual x 1)` will return $(x+\varepsilon)+\varepsilon = x+2\varepsilon$,
;;    which is not what we we want

;; - The `extract-tangent` call inside `inner-d` will return the `Df(x)`
;;   component of the dual number... which, remember, is no longer a dual
;;   number! So the SECOND call to `extract-tangent` have nothing to extract,
;;   and can only sensibly return 0.
;;
;; The problem here is called "perturbation confusion", and is covered in great
;; detail in
;; ["Confusion of Tagged Perturbations in Forward Automatic Differentiation of
;; Higher-Order Functions"](https://arxiv.org/abs/1211.4892), by Manzyuk et
;; al. (2019).
;;
;; The solution is to introduce a new $\varepsilon$ for every level, and allow
;; different $\varepsilon$ instances to multiply without annihilating. Each
;; $\varepsilon$ is called a "tag". [[Differential]] (implemented below) is a
;; generalized dual number that can track many tags at once, allowing nested
;; derivatives like the one described above to work.
;;
;; This implies that `extract-tangent` needs to take a tag, to determine _which_
;; tangent to extract:

(comment
  (defn derivative [f]
    (let [tag (fresh-tag)]
      (fn [x]
        (-> (f (make-dual x 1 tag))
            (extract-tangent tag))))))

;; This is close to the final form you'll find
;; at [[emmy.calculus.derivative/derivative]].
;;
;; ### What Return Values are Allowed?
;;
;; Before we discuss the implementation of dual
;; numbers (called [[Differential]]), [[lift-1]], [[lift-2]] and the rest of the
;; machinery that makes this all possible; what sorts of objects is `f` allowed
;; to return?
;;
;; The dual number approach is beautiful because we can bring to bear all sorts
;; of operations in Clojure that never even _see_ dual numbers. For example,
;; `square-and-cube` called with a dual number returns a PAIR of dual numbers:

(comment
  (defn square-and-cube [x]
    (let [x2 (g/square x)
          x3 (g/cube x)]
      [x2 x3])))

;; Vectors don't care what they contain! We want the derivative of
;; `square-and-cube` to also return a vector, whose entries represent the
;; derivative of _that entry_ with respect to the function's input.
;;
;; But this implies that [[extract-tangent]] from the example above needs to
;; know how to handle vectors and other collections; in the case of a vector `v`
;; by returning `(mapv extract-tangent v)`.
;;
;; What about higher-order functions?

(comment
  (defn offset-fn
    "Returns a function that takes a single-argument function `g`, and returns a new
  function like `g` that offsets its input by `offset`."
    [offset]
    (fn [g]
      (fn [x]
        (g (+ x offset))))))

;; `(derivative offset-fn)` here returns a function! Manzyuk et al. 2019 makes
;; the reasonable claim that, if `(f x)` returns a function, then `(derivative
;; f)` should treat `f` as a multi-argument function with its first argument
;; curried.
;;
;; Let's say `f` takes a number `x` and returns a function `g` that maps number
;; => number. `(((derivative f) x) y)` should act just like the partial
;; derivative of the equivalent multi-argument function, with respect to the
;; first argument:
;;
;;```clj
;;(((partial 0) f-flattened) x y)
;;```
;;
;; In other words, `(derivative offset-fn)` should act just like:

(comment
  (derivative
   (fn [offset] (g (+ x offset)))))

;; for some known `g` and `x`, but with the ability to store `(derivative
;; offset-fn)` and call it later with many different `g`.
;;
;; NOTE: We might accomplish this by composing `extract-tangent` with the
;; returned function, so that the extraction happens later, when the function's
;; called... but that will fail. The real implementation is more subtle! See
;; the [[emmy.calculus.derivative]] namespace for the actual implementation
;; of [[IPerturbed]] for functions and multimethods.
;;
;; All of this suggests that we need to make [[extract-tangent]] an open
;; function that other folks can extend for other container-like
;; types ([functors](https://en.wikipedia.org/wiki/Functor), specifically).
;;
;; The [[IPerturbed]] protocol accomplishes this, along with two other functions
;; that we'll use later:

(defprotocol IPerturbed
  (perturbed? [this]
    "Returns true if the supplied object has some known non-zero tangent to be
    extracted via [[extract-tangent]], false otherwise. (Return `false` by
    default if you can't detect a perturbation.)")

  (replace-tag [this old-tag new-tag]
    "If `this` is perturbed, Returns a similar object with the perturbation
    modified by replacing any appearance of `old-tag` with `new-tag`. Else,
    return `this`.")

  (extract-tangent [this tag]
    "If `this` is perturbed, return the tangent component paired with the
    supplied tag. Else, returns `([[emmy.value/zero-like]] this)`."))

;; `replace-tag` exists to handle subtle bugs that can arise in the case of
;; functional return values. See the "Amazing Bug" sections
;; in [[emmy.calculus.derivative-test]] for detailed examples on how this
;; might bite you.
;;
;; The default implementations are straightforward, and match the docstrings:

(extend-protocol IPerturbed
  #?(:clj Object :cljs default)
  (perturbed? [_] false)
  (replace-tag [this _ _] this)
  (extract-tangent [this _] (v/zero-like this)))

;; ## Differential Implementation
;;
;; We now have a template for how to implement `derivative`. What's left? We
;; need a dual number type that we can build and split back out into primal and
;; tangent components, given some tag. We'll call this type a [[Differential]].
;;
;; A [[Differential]] is a generalized dual number with a single primal
;; component, and potentially many tagged terms. Identical tags cancel to 0 when
;; multiplied, but are allowed to multiply by each other:
;;
;; $$a + b\varepsilon_1 + c\varepsilon_2 + d\varepsilon_1 \varepsilon_2 + \cdots$$
;;
;; Alternatively, you can view a [[Differential]] as a dual number with a
;; specific tag, that's able to hold dual numbers with some _other_ tag in its
;; primal and tangent slots. You can turn a [[Differential]] into a dual number
;; by specifying one of its tags. Here are the primal and tangent components for
;; $\varepsilon_2$:
;;
;; $$(a + b\varepsilon_1) + (c + d\varepsilon_1)\varepsilon_2$$
;;
;; And for $\varepsilon_1$:
;;
;; $$(a + c\varepsilon_2) + (b + d \varepsilon_2) \varepsilon_1$$
;;
;; A differential term is implemented as a pair whose first element is a set of
;; tags and whose second is the coefficient.

(defn- tags [term]
  (nth term 0))

(defn- coefficient [term]
  (nth term 1))

;; The set of tags is implemented as a "vector set",
;; from [[emmy.util.vector-set]]. This is a sorted set data structure,
;; backed by a Clojure vector. vector sets provide cheap "max" and "min"
;; operations, and acceptable union, intersection and difference performance.

(defn make-term
  "Returns a [[Differential]] term with the supplied vector-set of `tags` paired
  with coefficient `coef`.

  `tags` defaults to [[uv/empty-set]]"
  ([coef] [uv/empty-set coef])
  ([tags coef] [tags coef]))

;; Since the only use of a tag is to distinguish each unnamed $\varepsilon_n$,
;; we'll assign a new, unique positive integer for each new tag:

(let [next-tag (atom -1)]
  (defn fresh-tag
    "Returns a new, unique tag for use inside of a [[Differential]] term list."
    []
    (swap! next-tag inc)))

(defn- tag-in-term?
  "Return true if `t` is in the tag-set of the supplied `term`, false otherwise."
  [term t]
  (uv/contains? (tags term) t))

;; ## Term List Algebra
;;
;; The discussion above about Taylor series expansions revealed that for single
;; variable functions, we can pass a dual number into any function whose
;; derivative we already know:
;;
;; $$f(a+b\varepsilon) = f(a) + (Df(a)b)\varepsilon$$
;;
;; Because we can split a [[Differential]] into a primal and tangent component
;; with respect to some tag, we can reuse this result. We'll default to
;; splitting [[Differential]] instances by the highest-index tag:
;;
;; $$
;; \begin{aligned}
;; f(a &+ b\varepsilon_1 + c\varepsilon_2 + d\varepsilon_1 \varepsilon_2) \\
;; &= f((a + b\varepsilon_1)+(c + d\varepsilon_1)\varepsilon_2) \\
;; &= f(a + b\varepsilon_1)+Df(a + b\varepsilon_1)(c + d\varepsilon_1)\varepsilon_2 \\
;; \end{aligned}
;; $$
;;
;; Note that $f$ and $Df$ both received a dual number! One more expansion, this
;; time in $\varepsilon_1$, completes the evaluation (and makes abundantly clear
;; why we want the computer doing this, not pencil-and-paper):
;;
;; $$
;; \begin{aligned}
;; f(a &+ b\varepsilon_1)+Df(a+b\varepsilon_1)(c+d\varepsilon_1)\varepsilon_2 \\
;; &= (f(a)+Df(a)b\varepsilon_1)+(Df(a)+D^2f(a)b\varepsilon_1)(c + d\varepsilon_1)\varepsilon_2 \\
;; &= f(a)+(Df(a)b+D^2f(a)bc)\varepsilon_1+Df(a)c\varepsilon_2+Df(a)d\varepsilon_1\varepsilon_2
;; \end{aligned}
;; $$
;;
;; The only operations we need to implement between lists of terms are addition
;; and multiplication.
;;
;; ### Add and Multiply
;;
;; To efficiently add two [[Differential]] instances (represented as lists of
;; terms), we keep all terms in sorted order, sorted first by the length of each
;; tag list (the "order" of the differential term), and secondarily by the
;; values of the tags inside the tag list.
;;
;; NOTE: Clojure vectors already implement this ordering properly, so we can
;; use [[clojure.core/compare]] to determine an ordering on a tag list.

(def ^{:private true
       :doc "Returns the sum of the two supplied sequences of differential terms; any terms
  in the result with a zero coefficient will be removed.

  Each input must be sequence of `[tag-set, coefficient]` pairs, sorted by
  `tag-set`."}
  terms:+
  (ua/merge-fn core/compare g/add v/zero? make-term))

;; Because we've decided to store terms as a vector, we can multiply two vectors
;; of terms by:
;;
;; - taking the cartesian product of both term lists
;; - discarding all pairs of terms that share any tag (since $\varepsilon^2=0$)
;; - multiplying the coefficients of all remaining pairs and union-ing their tag
;;   lists
;; - grouping and adding any new terms with the SAME list of tags, and filtering
;;   out zeros
;;
;; This final step is required by a number of different operations later, so we
;; break it out into its own [[collect-terms]] function:

(defn- collect-terms
  "Build a term list up of pairs of tags => coefficients by grouping together and
  summing coefficients paired with the same term list.

  The result will be sorted by term list, and contain no duplicate term lists."
  [tags->coefs]
  (let [terms (for [[tags tags-coefs] (group-by tags tags->coefs)
                    :let [c (transduce (map coefficient) g/+ tags-coefs)]
                    :when (not (v/zero? c))]
                [tags c])]
    (into [] (sort-by tags terms))))

(defn- terms:map-coefficients
  "Given some function `f` and a sequence of `terms`, returns a vector of terms
  with all each `c` mapped to `(f c)`. Any term where `(v/zero? (f c))` is true
  will be filtered out."
  [f terms]
  (let [xform (mapcat
               (fn [term]
                 (let [c' (f (coefficient term))]
                   (if (v/zero? c')
                     []
                     [(make-term (tags term) c')]))))]
    (into [] xform terms)))

;; [[terms:*]] implements the first three steps, and calls [[collect-terms]] on
;; the resulting sequence:

(defn- t*ts
  "Multiplies a single term on the left by a vector of `terms` on the right.
  Returns a new vector of terms."
  [[tags coeff] terms]
  (loop [acc []
         i 0]
    (let [t (nth terms i nil)]
      (if (nil? t)
        acc
	      (let [[tags1 coeff1] t]
	        (if (empty? (uv/intersection tags tags1))
		        (recur (conj acc (make-term
		                          (uv/union tags tags1)
		                          (g/* coeff coeff1)))
		               (inc i))
		        (recur acc (inc i))))))))

(defn terms:*
  "Returns a vector of non-zero [[Differential]] terms that represent the product
  of the differential term lists `xs` and `ys`.

  NOTE that this function doesn't need to call [[collect-terms]] internally
  because grouping is accomplished by the internal [[terms:+]] calls."
  [xlist ylist]
  (letfn [(call [i]
            (let [x (nth xlist i nil)]
              (if (nil? x)
                []
                (terms:+ (t*ts x ylist)
	                       (call (inc i))))))]
    (call 0)))

;; ## Differential Type Implementation
;;
;; Armed with our term list arithmetic operations, we can finally implement
;; our [[Differential]] type and implement a number of important Clojure and
;; Emmy protocols.
;;
;; A [[Differential]] will respond to [[v/kind]] with `::differential`. Because
;; we want [[Differential]] instances to work in any place that real numbers or
;; symbolic argument work, let's make `::differential` derive from `::v/scalar`:

(derive ::differential ::v/scalar)

;; Now the actual type. The `terms` field is a term-list vector that will
;; remain (contractually!) sorted by its list of tags.

(declare compare equiv finite-term from-terms one?)

(deftype Differential [terms]
  ;; A [[Differential]] as implemented can act as a chain-rule accounting device
  ;; for all sorts of types, not just numbers. A [[Differential]] is
  ;; only [[v/numerical?]] if its coefficients are numerical (or if `terms` is
  ;; empty, interpreted as a [[Differential]] equal to `0`.)
  v/Numerical
  (numerical? [_]
    (or (empty? terms)
        (v/numerical?
         (coefficient (nth terms 0)))))

  IPerturbed
  (perturbed? [_] true)

  ;; There are 3 cases to consider when replacing some tag in a term, annotated
  ;; below:
  (replace-tag [_ oldtag newtag]
    (letfn [(process [term]
              (let [tagv (tags term)]
                (if-not (uv/contains? tagv oldtag)
                  ;; if the term doesn't contain the old tag, ignore it.
                  [term]
                  (if (uv/contains? tagv newtag)
                    ;; if the term _already contains_ the new tag
                    ;; $\varepsilon_{new}$, then replacing $\varepsilon_1$
                    ;; with a new instance of $\varepsilon_2$ would cause a
                    ;; clash. Since $\varepsilon_2^2=0$, the term should be
                    ;; removed.
                    []
                    ;; else, perform the replacement.
                    [(make-term (-> tagv
                                    (uv/disj oldtag)
                                    (uv/conj newtag))
                                (coefficient term))]))))]
      (from-terms
       (mapcat process terms))))

  ;; To extract the tangent (with respect to `tag`) from a differential, return
  ;; all terms that contain the tag (with the tag removed!) This can create
  ;; duplicate terms, so use [[from-terms]] to massage the result into
  ;; well-formedness again.
  (extract-tangent [_ tag]
    (from-terms
     (mapcat (fn [term]
               (let [tagv (tags term)]
                 (if (uv/contains? tagv tag)
                   [(make-term (uv/disj tagv tag)
                               (coefficient term))]
                   [])))
             terms)))

  v/Value
  (zero? [_]
    (every? (comp v/zero? coefficient) terms))
  (one? [this] (one? this))
  (identity? [this] (one? this))
  (zero-like [_] 0)
  (one-like [_] 1)
  (identity-like [_] 1)
  (freeze [_]
    (letfn [(freeze-term [term]
              (make-term (tags term)
                         (v/freeze (coefficient term))))]
      `[~'Differential
        ~@(mapv freeze-term terms)]))
  (exact? [_] false)
  (kind [_] ::differential)

  Object
  ;; Comparing [[Differential]] objects using `equals` defaults to [[equiv]],
  ;; which compares instances only using their non-tagged ('finite') components.
  ;; If you want to compare two instances using their full term lists,
  ;; See [[eq]].
  #?(:clj (equals [a b] (equiv a b)))
  #?(:cljs (valueOf [this] (.valueOf (finite-term this))))
  (toString [_]
    (let [term-strs (map (fn [term]
                           (str (tags term)
                                " → "
                                (pr-str (coefficient term))))
                         terms)]
      (str "D[" (join " " term-strs) "]")))

  #?@(:clj
      ;; This one is slightly subtle. To participate in control flow operations,
      ;; like comparison with both [[Differential]] and non-[[Differential]]
      ;; numbers, [[Differential]] instances should compare using ONLY their
      ;; non-tagged ("finite") terms. This means that comparison will totally
      ;; ignore any difference in tags.
      [Comparable
       (compareTo [a b] (compare a b))]

      :cljs
      [IEquiv
       (-equiv [a b] (equiv a b))

       IComparable
       (-compare [a b]  (compare a b))

       IPrintWithWriter
       (-pr-writer [x writer _]
                   (write-all writer (.toString x)))]))

#?(:clj
   (defmethod print-method Differential
     [^Differential s ^java.io.Writer w]
     (.write w (.toString s))))

;; ## Accessor Methods

(defn differential?
  "Returns true if the supplied object is an instance of [[Differential]], false
  otherwise."
  [dx]
  (instance? Differential dx))

(defn- bare-terms
  "Returns the `-terms` field of the supplied [[Differential]] object. Errors if
  any other type is supplied."
  [dx]
  {:pre [(differential? dx)]}
  (.-terms ^Differential dx))

;; ## Constructors
;;
;; Because a [[Differential]] is really a wrapper around the idea of a
;; generalized dual number represented as a term-list, we need to be able to get
;; to and from the term list format from other types, not just [[Differential]]
;; instances.

(defn- ->terms
  "Returns a vector of terms that represent the supplied [[Differential]]; any
  term with a [[v/zero?]] coefficient will be filtered out before return.

  If you pass a non-[[Differential]], [[->terms]] will return a singleton term
  list (or `[]` if the argument was zero)."
  [dx]
  (cond (differential? dx) (bare-terms dx)
        (vector? dx)       dx
        (v/zero? dx)       []
        :else              [(make-term dx)]))

(defn- terms->differential
  "Returns a differential instance generated from a vector of terms. This method
  will do some mild cleanup, or canonicalization:

  - any empty term list will return 0
  - a singleton term list with no tags will return its coefficient

  NOTE this method assumes that the input is properly sorted, and contains no
  zero coefficients."
  [terms]
  {:pre [(vector? terms)]}
  (cond (empty? terms) 0

        (and (= (count terms) 1)
             (empty? (tags (nth terms 0))))
        (coefficient (nth terms 0))

        :else (->Differential terms)))

(defn from-terms
  "Accepts a sequence of terms (ie, pairs of `[tag-list, coefficient]`), and
  returns:

  - a well-formed [[Differential]] instance, if the terms resolve to a
    differential with non-zero infinitesimal terms
  - the original input otherwise

  Duplicate (by tag list) terms are allowed; their coefficients will be summed
  together and removed if they sum to zero."
  [tags->coefs]
  (terms->differential
   (collect-terms tags->coefs)))

(defn map-coefficients
  "Given a function `f` and [[Differential]] instance `d`, returns a
  new [[Differential]] generated by transforming all coefficients `c` of `d`
  to `(f c)`.

  Any term in the returned instance with a `v/zero?` coefficient
  will be filtered out."
  [f d]
  (let [terms (bare-terms d)]
    (->Differential
     (terms:map-coefficients f terms))))

;; ## Differential API
;;
;; These first two functions create a way to globally declare, via a dynamic
;; binding, the stack of tags that are currently in play. If three nested
;; derivatives are being taken, [[*active-tags*]] will contain three entries
;; from a perspective inside the function at the deepest level.
;;
;; The [[IPerturbed]] implementation for functions uses this information to
;; determine whether or not to use [[replace-tag]] to protect its tag from
;; perturbation confusion. If some higher level is not trying to extract the
;; same tag, there's no need.

(def ^:dynamic *active-tags* [])

(defn with-active-tag
  "Like `apply`, but conj-es `tag` onto the dynamic variable [[*active-tags*]]
  inside the scope of `f`.

  Returns the result of applying `f` to `args`."
  [tag f args]
  (binding [*active-tags* (conj *active-tags* tag)]
    (apply f args)))

(defn tag-active?
  "Returns true if `tag` is an element of [[*active-tags*]] (and therefore pending
  for extraction by some nested derivative), false otherwise."
  [tag]
  (boolean
   (some #{tag} (rseq *active-tags*))))

;; ### Differential Arithmetic
;;
;; This next section lifts slightly-augmented versions of [[terms:+]]
;; and [[terms:*]] up to operate on [[Differential]] instances. These work just
;; as before, but handle wrapping and unwrapping the term list.

(defn d:+
  "Returns an object representing the sum of the two objects `dx` and `dy`. This
  works by summing the coefficients of all terms with the same list of tags.

    Works with non-[[Differential]] instances on either or both sides, and returns
  a [[Differential]] only if it contains any non-zero tangent components."
  ([] 0)
  ([dx] dx)
  ([dx dy]
   (terms->differential
    (terms:+ (->terms dx)
             (->terms dy))))
  ([dx dy & more]
   (terms->differential
    (transduce (map ->terms)
               terms:+
               (cons dx (cons dy more))))))

(defn d:*
  "Returns an object representing the product of the two objects `dx` and `dy`.

  This works by multiplying out all terms:

  $$(dx1 + dx2 + dx3 + ...)(dy1 + dy2 + dy3...)$$

  and then collecting any duplicate terms by summing their coefficients.

  Works with non-[[Differential]] instances on either or both sides, and returns
  a [[Differential]] only if it contains any non-zero tangent components."
  ([] 1)
  ([dx] dx)
  ([dx dy]
   (terms->differential
    (terms:* (->terms dx)
             (->terms dy)))))

(defn d:+*
  "Identical to `(d:+ a) (d:* b c)`, but _slightly_ more efficient as the function
  is able to skip creating a [[Differential]] instance during [[d:*]] and then
  immediately tearing it down for [[d:+]]."
  [a b c]
  (terms->differential
   (terms:+ (->terms a)
            (terms:* (->terms b)
                     (->terms c)))))

;; Finally, the function we've been waiting for! [[bundle-element]] allows you
;; to augment some non-[[Differential]] thing with a tag and push it through the
;; generic arithmetic system, where it will accumulate the derivative of your
;; original input (tagged with `tag`.)

(defn bundle-element
  "Generate a new [[Differential]] object with the supplied `primal` and `tangent`
  components, and the supplied internal `tag` that this [[Differential]] will
  carry around to prevent perturbation confusion.

  If the `tangent` component is `0`, acts as identity on `primal`. `tangent`
  defaults to 1.

  `tag` defaults to a side-effecting call to [[fresh-tag]]; you can retrieve
  this unknown tag by calling [[max-order-tag]]."
  ([primal]
   {:pre [(v/numerical? primal)]}
   (bundle-element primal 1 (fresh-tag)))
  ([primal tag]
   {:pre [(v/numerical? primal)]}
   (bundle-element primal 1 tag))
  ([primal tangent tag]
   (let [term (make-term (uv/make [tag]) 1)]
     (d:+* primal tangent [term]))))

;; ## Differential Parts API
;;
;; These functions give higher-level access to the components of
;; a [[Differential]] you're typically interested in.

(defn max-order-tag
  "Given one or more well-formed [[Differential]] objects, returns the
  maximum ('highest order') tag found in the highest-order term of any of
  the [[Differential]] instances.

  If there is NO maximal tag (ie, if you provide [[Differential]] instances with
  no non-zero tangent parts, or all non-[[Differential]]s), returns nil."
  ([dx]
   (when (differential? dx)
     (let [last-term   (peek (bare-terms dx))
           highest-tag (peek (tags last-term))]
       highest-tag)))
  ([dx & dxs]
   (letfn [(max-termv [dx]
             (if-let [max-order (max-order-tag dx)]
               [max-order]
               []))]
     (when-let [orders (seq (mapcat max-termv (cons dx dxs)))]
       (apply max orders)))))

;; A reminder: the [[primal-part]] of a [[Differential]] is all terms except for
;; terms containing [[max-order-tag]], and [[tangent-part]] is
;; a [[Differential]] built out of the remaining terms, all of which contain
;; that tag.

(defn primal-part
  "Returns a [[Differential]] containing only the terms of `dx` that do NOT
  contain the supplied `tag`, ie, the primal component of `dx` with respect to
  `tag`.

  If no tag is supplied, defaults to `([[max-order-tag]] dx)`.

  NOTE: every [[Differential]] can be factored into a dual number of the form

      primal + (tangent * tag)

  For each tag in any of its terms. [[primal-part]] returns this first piece,
  potentially simplified into a non-[[Differential]] if the primal part contains
  no other tags."
  ([dx] (primal-part dx (max-order-tag dx)))
  ([dx tag]
   (if (differential? dx)
     (let [sans-tag? #(not (tag-in-term? % tag))]
       (->> (bare-terms dx)
            (filterv sans-tag?)
            (terms->differential)))
     dx)))

(defn tangent-part
  "Returns a [[Differential]] containing only the terms of `dx` that contain the
  supplied `tag`, ie, the tangent component of `dx` with respect to `tag`.

  If no tag is supplied, defaults to `([[max-order-tag]] dx)`.

  NOTE: Every [[Differential]] can be factored into a dual number of the form

      primal + (tangent * tag)

  For each tag in any of its terms. [[tangent-part]] returns a [[Differential]]
  representing `(tangent * tag)`, or 0 if `dx` contains no terms with the
  supplied `tag`.

  NOTE: the 2-arity case is similar to `([[extract-tangent]] dx tag)`; the only
  difference is that [[extract-tangent]] drops the `dx` tag from all terms in
  the returned value. Call [[extract-tangent]] if you want to drop `tag`."
  ([dx] (tangent-part dx (max-order-tag dx)))
  ([dx tag]
   (if (differential? dx)
     (->> (bare-terms dx)
          (filterv #(tag-in-term? % tag))
          (terms->differential))
     0)))

(defn primal-tangent-pair
  "Returns a pair of the primal and tangent components of the supplied `dx`, with
  respect to the supplied `tag`. See the docs for [[primal-part]]
  and [[tangent-part]] for more details.

  [[primal-tangent-pair]] is equivalent to

  `[([[primal-part]] dx tag) ([[tangent-part]] dx tag)]`

  but slightly more efficient if you need both."
  ([dx] (primal-tangent-pair dx (max-order-tag dx)))
  ([dx tag]
   (if-not (differential? dx)
     [dx 0]
     (let [[tangent-terms primal-terms]
           (us/separatev #(tag-in-term? % tag)
                         (bare-terms dx))]
       [(terms->differential primal-terms)
        (terms->differential tangent-terms)]))))

(defn finite-term
  "Returns the term of the supplied [[Differential]] `dx` that has no tags
  attached to it, `0` otherwise.

  [[Differential]] instances with many can be decomposed many times
  into [[primal-part]] and [[tangent-part]]. Repeated calls
  to [[primal-part]] (with different tags!) will eventually yield a
  non-[[Differential]] value. If you know you want this, [[finite-term]] will
  get you there in one shot.

  NOTE that this will only work with a well-formed [[Differential]], ie, an
  instance with all terms sorted by their list of tags."
  [dx]
  (if (differential? dx)
    (let [[head] (bare-terms dx)
          ts     (tags head)]
      (if (empty? ts)
        (coefficient head)
        0))
    dx))

;; ## Comparison, Control Flow
;;
;; Functions like `=`, `<` and friends don't have derivatives; instead, they're
;; used for control flow inside of Clojure functions. To play nicely with these
;; functions, the [[Differential]] API exposes a number of methods for comparing
;; numbers on ONLY their finite parts.
;;
;; Why? If `x` is a [[Differential]] instance, `(< x 10)` needs to return true
;; whenever a non-[[Differential]] `x` would return true. To make this work,
;; these operations look only at the [[finite-part]].
;;
;; HOWEVER! [[v/one?]] and [[v/zero?]] are examples of Emmy functions that
;; are used to skip operations that we _want_ to happen, like multiplication.
;;
;; `(g/* x y)` will return `y` if `(v/one? x)` is true... but to propagate the
;; derivative through we need this multiplication to occur. The compromise is:
;;
;; - [[v/one?]] and [[v/zero?]] return true only when ALL [[tangent-part]]s are
;;   zero and the [[finite-part]] is either [[v/one?]] or [[v/zero?]]
;;   respectively
;; - [[eq]] and [[compare-full]] similarly looks at every component in
;;   the [[Differential]] supplied to both sides
;;
;; while:
;;
;; - [[equiv]] and [[compare]] only examine the [[finite-part]] of either side.

(defn one?
  "Returns true if the supplied instance has a [[finite-part]] that responds true
  to [[emmy.value/one?]], and zero coefficients on any of its tangent
  components; false otherwise.

  NOTE: This means that [[one?]] will not do what you expect as a conditional
  inside some function. If you want to branch inside some function you're taking
  the derivative of, prefer `(= 1 dx)`. This will only look at
  the [[finite-part]] and ignore the values of the tangent parts."
  [dx]
  (let [[p t] (primal-tangent-pair dx)]
    (and (v/one? p)
         (v/zero? t))))

(defn eq
  "For non-differentials, this is identical to [[clojure.core/=]].
  For [[Differential]] instances, equality acts on tangent components too.

  If you want to ignore the tangent components, use [[equiv]]."
  ([_] true)
  ([a b]
   (v/= (->terms a)
        (->terms b)))
  ([a b & more]
   (if (eq a b)
     (if (next more)
       (recur b (first more) (next more))
       (eq b (first more)))
     false)))

(defn compare-full
  "Comparator that compares [[Differential]] instances with each other or
  non-differentials using all tangent terms each instance. Matches the response
  of [[eq]].

  Acts as [[clojure.core/compare]] for non-differentials."
  [a b]
  (v/compare
   (->terms a)
   (->terms b)))

(defn equiv
  "Returns true if all of the supplied objects have equal [[finite-part]]s, false
  otherwise.

  Use [[equiv]] if you want to compare non-differentials with
  [[Differential]]s and ignore all tangent components. If you _do_ want to take
  the tangent components into account, prefer [[eq]]."
  ([_] true)
  ([a b]
   (v/= (finite-term a)
        (finite-term b)))
  ([a b & more]
   (if (equiv a b)
     (if (next more)
       (recur b (first more) (next more))
       (equiv b (first more)))
     false)))

(defn compare
  "Comparator that compares [[Differential]] instances with each other or
  non-differentials using only the [[finite-part]] of each instance. Matches the
  response of [[equiv]].

  Acts as [[clojure.core/compare]] for non-differentials."
  [a b]
  (v/compare
   (finite-term a)
   (finite-term b)))

;; ## Chain Rule and Lifted Functions
;;
;; Finally, we come to the heart of it! [[lift-1]] and [[lift-2]] "lift", or
;; augment, unary or binary functions with the ability to
;; handle [[Differential]] instances in addition to whatever other types they
;; previously supported.
;;
;; These functions are implementations of the single and multivariable Taylor
;; series expansion methods discussed at the beginning of the namespace.
;;
;; There is yet another subtlety here, noted in the docstrings below. [[lift-1]]
;; and [[lift-2]] really are able to lift functions like [[clojure.core/+]] that
;; can't accept [[Differential]]s. But the first-order derivatives that you have
;; to supply _do_ have to be able to take [[Differential]] instances.
;;
;; This is because the [[tangent-part]] of [[Differential]] might still be
;; a [[Differential]], and for `Df` to handle this we need to be able to take
;; the second-order derivative.
;;
;; Magically this will all Just Work if you pass an already-lifted function, or
;; a function built out of already-lifted components, as `df:dx` or `df:dy`.

(defn lift-1
  "Given:

  - some unary function `f`
  - a function `df:dx` that computes the derivative of `f` with respect to its
    single argument

  Returns a new unary function that operates on both the original type of `f`
  and [[Differential]] instances.

  If called without `df:dx`, `df:dx` defaults to `(f :dfdx)`; this will return
  the derivative registered to a generic function defined with [[g/defgeneric]].

  NOTE: `df:dx` has to ALREADY be able to handle [[Differential]] instances. The
  best way to accomplish this is by building `df:dx` out of already-lifted
  functions, and declaring them by forward reference if you need to."
  ([f]
   (if-let [df:dx (f :dfdx)]
     (lift-1 f df:dx)
     (u/illegal "No df:dx supplied for `f` or registered generically.")))
  ([f df:dx]
   (fn call [x]
     (if-not (differential? x)
       (f x)
       (let [[px tx] (primal-tangent-pair x)
             fx      (call px)]
         (if (v/numeric-zero? tx)
           fx
           (d:+* fx (df:dx px) tx)))))))

(defn lift-2
  "Given:

  - some binary function `f`
  - a function `df:dx` that computes the derivative of `f` with respect to its
    single argument
  - a function `df:dy`, similar to `df:dx` for the second arg

  Returns a new binary function that operates on both the original type of `f`
  and [[Differential]] instances.

  NOTE: `df:dx` and `df:dy` have to ALREADY be able to handle [[Differential]]
  instances. The best way to accomplish this is by building `df:dx` and `df:dy`
  out of already-lifted functions, and declaring them by forward reference if
  you need to."
  ([f]
   (let [df:dx (f :dfdx)
         df:dy (f :dfdy)]
     (if (and df:dx df:dy)
       (lift-2 f df:dx df:dy)
       (u/illegal "No df:dx, df:dy supplied for `f` or registered generically."))))
  ([f df:dx df:dy]
   (fn call [x y]
     (if-not (or (differential? x)
                 (differential? y))
       (f x y)
       (let [tag     (max-order-tag x y)
             [xe dx] (primal-tangent-pair x tag)
             [ye dy] (primal-tangent-pair y tag)
             a (call xe ye)
             b (if (v/numeric-zero? dx)
                 a
                 (d:+* a (df:dx xe ye) dx))]
         (if (v/numeric-zero? dy)
           b
           (d:+* b (df:dy xe ye) dy)))))))

(defn lift-n
  "Given:

  - some function `f` that can handle 0, 1 or 2 arguments
  - `df:dx`, a fn that returns the derivative wrt the single arg in the unary case
  - `df:dx1` and `df:dx2`, fns that return the derivative with respect to the
    first and second args in the binary case

  Returns a new any-arity function that operates on both the original type of
  `f` and [[Differential]] instances.

  NOTE: The n-ary case of `f` is populated by nested calls to the binary case.
  That means that this is NOT an appropriate lifting method for an n-ary
  function that isn't built out of associative binary calls. If you need this
  ability, please file an issue at the [emmy issue
  tracker](https://github.com/emmy/emmy/issues)."
  [f df:dx df:dx1 df:dx2]
  (let [f1 (lift-1 f df:dx)
        f2 (lift-2 f df:dx1 df:dx2)]
    (fn call
      ([] (f))
      ([x] (f1 x))
      ([x y] (f2 x y))
      ([x y & more]
       (reduce call (call x y) more)))))

;; ## Generic Method Installation
;;
;; Armed with [[lift-1]] and [[lift-2]], we can install [[Differential]] into
;; the Emmy generic arithmetic system.
;;
;; Any function built out of these components will work with
;; the [[emmy.calculus.derivative/D]] operator.

(defmethod g/simplify [::differential] [d]
  (map-coefficients g/simplify d))

(defn- defunary
  "Given:

  - a generic unary multimethod `generic-op`
  - a corresponding single-arity lifted function `differential-op`

  installs an appropriate unary implementation of `generic-op` for
  `::differential` instances."
  [generic-op differential-op]
  (defmethod generic-op [::differential] [a] (differential-op a)))

(defn- defbinary
  "Given:

  - a generic binary multimethod `generic-op`
  - a corresponding 2-arity lifted function `differential-op`

  installs an appropriate binary implementation of `generic-op` between
  `:differential` and `::v/scalar` instances."
  [generic-op differential-op]
  (doseq [signature [[::differential ::differential]
                     [::v/scalar ::differential]
                     [::differential ::v/scalar]]]
    (defmethod generic-op signature [a b] (differential-op a b))))

;; And now we're off to the races. The rest of the namespace
;; provides [[defunary]] and [[defbinary]] calls for all of the generic
;; operations for which we know how to declare partial derivatives.

;; First, install `equiv` as to perform proper equality between `Differential`
;; instances and scalars. `equiv` compares on only the finite part, not the
;; differential parts.

(defbinary v/= equiv)

(defbinary g/add d:+)
(defunary g/negate (lift-1 g/negate))
(defbinary g/sub (lift-2 g/sub))

(let [mul  (lift-2 g/mul)]
  (defbinary g/mul mul)
  (defbinary g/dot-product mul))
(defbinary g/expt (lift-2 g/expt))

(defunary g/square (lift-1 g/square))
(defunary g/cube (lift-1 g/cube))

(defunary g/invert (lift-1 g/invert))
(defbinary g/div (lift-2 g/div))

(defunary g/negative?
  (comp g/negative? finite-term))

(defunary g/infinite?
  (comp g/infinite? finite-term))

(defunary g/abs
  (fn [x]
    (let [f (finite-term x)
          func (cond (< f 0) (lift-1 g/negate)
                     (> f 0) (lift-1 identity (fn [_] 1))
                     (= f 0) (u/illegal "Derivative of g/abs undefined at zero")
                     :else (u/illegal (str "error! derivative of g/abs at" x)))]
      (func x))))

(defn- discont-at-integers [f dfdx]
  (let [f (lift-1 f (fn [_] dfdx))
        f-name (v/freeze f)]
    (fn [x]
      (if (v/integral? (finite-term x))
        (u/illegal
         (str "Derivative of g/" f-name " undefined at integral points."))
        (f x)))))

(defunary g/floor
  (discont-at-integers g/floor 0))

(defunary g/ceiling
  (discont-at-integers g/ceiling 0))

(defunary g/integer-part
  (discont-at-integers g/integer-part 0))

(defunary g/fractional-part
  (discont-at-integers g/fractional-part 1))

(let [div (lift-2 g/div)]
  (defbinary g/solve-linear (fn [l r] (div r l)))
  (defbinary g/solve-linear-right div))

(defunary g/sqrt (lift-1 g/sqrt))
(defunary g/log (lift-1 g/log))
(defunary g/exp (lift-1 g/exp))

(defunary g/cos (lift-1 g/cos))
(defunary g/sin (lift-1 g/sin))
(defunary g/tan (lift-1 g/tan))
(defunary g/cot (lift-1 g/cot))
(defunary g/sec (lift-1 g/sec))
(defunary g/csc (lift-1 g/csc))

(defunary g/atan (lift-1 g/atan))
(defbinary g/atan (lift-2 g/atan))
(defunary g/asin (lift-1 g/asin))
(defunary g/acos (lift-1 g/acos))
(defunary g/acot (lift-1 g/acot))
(defunary g/asec (lift-1 g/asec))
(defunary g/acsc (lift-1 g/acsc))

(defunary g/cosh (lift-1 g/cosh))
(defunary g/sinh (lift-1 g/sinh))
(defunary g/tanh (lift-1 g/tanh))
(defunary g/sech (lift-1 g/sech))
(defunary g/coth (lift-1 g/coth))
(defunary g/csch (lift-1 g/csch))

(defunary g/acosh (lift-1 g/acosh))
(defunary g/asinh (lift-1 g/asinh))
(defunary g/atanh (lift-1 g/atanh))
(defunary g/acoth (lift-1 g/acoth))
(defunary g/asech (lift-1 g/asech))
(defunary g/acsch (lift-1 g/acsch))

(defunary g/sinc (lift-1 g/sinc))
(defunary g/sinhc (lift-1 g/sinhc))
(defunary g/tanc (lift-1 g/tanc))
(defunary g/tanhc (lift-1 g/tanhc))
