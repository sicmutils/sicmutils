# Changelog

## [Unreleased]

- #223 fixes a problem where `(operator * structure)` would return a structure
  of operators instead of an operator that closed over the multiplication.
  `::s/structure` is now properly a `::o/co-operator`, matching its status as a
  `::f/cofunction`.

- The operator returned by `sicmutils.calculus.derivative/partial` now has a
  proper name field like `(partial 0)`, instead of `:partial-derivative` (#223).

- #223 converts the implementation of `sicmutils.calculus.derivative/D` to use
  the new `Differential` type; this fixes "Alexey's Amazing Bug" and allows `D`
  to operate on higher order functions. For some function `f` that returns
  another function, `((D f) x)` will return a function that keeps `x` "alive"
  for the purposes of differentiation inside its body. See
  `sicmutils.calculus.derivative-test/amazing-bug` for an extended example.

- #222 adds `v/Value` implementations for Clojure sequences and maps. Maps and
  vectors implement `f/Arity` and return `[:between 1 2]. `zero?` and
  `zero-like` work on sequence entries and map values. Maps can specify their
  `v/kind` return value with a `:type` key, and some of the calculus
  implementations do already make use of this feature. `g/partial-derivative` on
  a Clojure Map passes through to its values.

- New, literate `Differential` implementation lives at at
  `sicmutils.differential` (#221) (see [this
  page](https://samritchie.io/dual-numbers-and-automatic-differentiation/) for a
  readable version.) Notable changes to the original impl at
  `sicmutils.calculus.derivative` include:

  - We've changed our terminology from GJS's `finite-part`,
    `infinitesimal-part`, `make-x+dx` to the more modern `primal-part`,
    `tangent-part`, `bundle` that the Automatic Differentiation community has
    now adopted.

  - A new `sicmutils.differential.IPerturbed` protocol makes it possible to
    extend the Automatic Differentiation (AD) system to be able to handle
    different Functor-shaped return values, like Java or JS lists and objects.
    See the [cljdoc page on Automatic
    Differentiation](https://cljdoc.org/d/sicmutils/sicmutils/CURRENT/doc/calculus/automatic-differentiation)
    for more detail.

    - #222 implements `d/IPerturbed` for Clojure maps, vectors and sequences;
      all are now valid return types for functions you pass to `D`.

    - #222 also implements `d/IPerturbed` for SICMUtils `Matrix`, `Structure`,
      `Series`, `PowerSeries` and `Operator`.

    - #223 implements `d/IPerturbed` for Clojure functions and multimethods,
      handling the attendant subtlety that fixes "Alexey's Amazing Bug".

  - `sicmutils.differential/{lift-1,lift-2,lift-n}` allow you to make custom
    operations differentiable, provided you can supply a derivative.

  - `Differential` implements `sicmutils.function/arity`, `IFn`, and can be
    applied to arguments if its coefficients are function values. `Differential`
    instances also `v/freeze` and `g/simplify` properly (by pushing these
    actions into their coefficients).

  - New `compare` and `equiv` implementations allow `Differential` instances to
    compare themselves with other objects using only their primal parts; this
    makes it possible to use functions like `<=`, `>`, `=` to do control flow
    during automatic differentiation. (Use `compare-full` and `eq` if you want
    to do full equality comparisons on primal and tangent components.)

  - related, `g/abs` is now implemented for `Differential` instances, making
    this function available in functions passed to `D`.

  - proper `numerical?`, `one?` and `identity?` implementations. The latter two
    only respond `true` if there are NO tangent components; This means that
    `one?` and `(= % 1)` will not agree.

  - The new implementation fixes a subtle bug with nested, higher order
    automatic differentiation - it's too subtle for the CHANGELOG, so please the
    "amazing" bug sections in `sicmutils.calculus.derivative-test` for proper
    exposition.

- `sicmutils.generic/partial-derivative` gains a `Keyword` extension, so it can
  respond properly to `:name` and `:arity` calls (#221).

- Added missing `identity?`, `identity-like` for complex and rational numbers
  (#236)

- beefed up the Javascript numeric tower to allow objects like
  `sicmutils.differential/Differential`, `sicmutils.expression/Expression` and
  friends that WRAP numbers to compare properly using cljs-native `<`, `<=`,
  `=`, `>=` and `>` (#236)

- new `sicmutils.value/compare` function exposed in `sicmutils.env` returns a
  valid comparison bit between native numbers and numbers wrapped in
  `Differential` or `Expression` in both JVM Clojure and Clojurescript (#236).
  The behavior matches `clojure.core/compare` for all reals on the JVM; it
  doesn't in Clojurescript because native `compare` can't handle
  `goog.math.{Long,Integer}` or `js/BigInt`.

- New single-arity case for `sicmutils.structure/opposite` returns an identical
  structure with flipped orientation (#220). acts as `identity` for
  non-structures.

- #219 introduces a number of changes to `Operator`'s behavior:

  - `Operator` is now a `deftype` (not a `defrecord`); the keyword lookup for
    its `:name`, `:arity`, `:context` and `:o` fields have been replaced by,
    respectively, `o/name`, `sicmutils.function/arity`, `o/context` and
    `o/procedure` functions. This change happened to allow `Operator` to
    implement protocols like `ILookup`.

  - Native `get` and `get-in` now act on `Operator`. Given an operator function
    `f`, `get` and `get-in` compose `#(get % k)`, or similar with `f`. This
    deferred action matches the effect of all sicmutils generics on functions.

  - Combining an operator and a non-operator via `+` and `-`, the non-operator
    was previously lifted into an operator that multiplied itself by the new
    operator's argument. As of #219, this "multiplication" uses the operator
    definition of multiplication - meaning, the new operator composes the
    non-operator with its argument. Where does this matter?

    Previously adding the non-operator `sicmutils.function/I` to the identity
    operator `I` would act like this:

    ```clojure
    (((g/+ o/identity f/I) f/I) 10)
    ;; => 110 == (+ 10 (* 10 10))
    ```

    Because `f/I` multiplied itself by its argument... resulting in `(* f/I f/I)
    == g/square`.

    After the change, you see this:

    ```clojure
    (((g/+ o/identity f/I) f/I) 10)
    ;; => 20
    ```

    because `f/I` composes with its argument.

  - `sicmutils.operator/identity-operator` has been renamed to
    `sicmutils.operator/identity`

  - `o/make-operator` now takes an explicit `context` map, instead of a
    multi-arity implementation with key-value pairs.

  - `Operator` now implements `g/negate`.

  - `g/cross-product` is no longer implemented for `Operator`. operators were
    introduced by GJS to act like "differential operators", can only add, negate
    and multiply (defined as composition). We will probably relax this in the
    future, and add more functions like `g/cross-product` that compose with the
    operator's output; but for now we're cleaning house, since this function
    isn't used anywhere.

  - In this same spirit, `Operator` instances can now only be divided by scalars
    (not functions anymore), reflecting the ring structure of a differential
    operator.

- `sicmutils.env/ref` now accepts function and operators (#219). `(ref f 0 1)`,
  as an example, returns a new function `g` that acts like `f` but calls `(ref
  result 0 1)` on the result.

- The slightly more general `sicmutils.env/component` replaces
  `sicmutils.structure/component` in the `sicmutils.env` namespace (#219).
  `((component 0 1) x) == (ref x 0 1)`.

- `D` (or `sicmutils.generic/partial-derivative`) applied to a matrix of
  functions now takes the elementwise partials of every function in the matrix.
  (#218)

- `sicmutils.function/arity` is now a protocol method, under the
  `sicmutils.function/IArity` protocol (#218). In addition to functions, `arity`
  now correctly responds to:

    - `sicmutils.matrix/Matrix`: calling `arity` on a matrix assumes that the
      matrix has function elements; the returned arity is the most general arity
      that all functions will respond to.
    - `sicmutils.operator/Operator`: returns the arity of the operator's wrapped
      function.
    - `sicmutils.series/Series`: `arity` on a `Series` assumes that the series
      contains functions as entries, and returns, conservatively, the arity of
      the first element of the series.
   - `sicmutils.series/PowerSeries`: `arity` returns `[:exactly 1]`, since
     `PowerSeries` are currently single variable.
   - vectors, and `sicmutils.structure/Structure`: `arity` on these collections
     assumes that the collection contains functions as entries, and returns the
     most general arity that is compatible with all of the function elements.

- New functions `sicmutils.function/{get,get-in}` added that act like the
  `clojure.core` versions; but given a function `f`, they compose `#(get % k)`,
  or similar with `f`. This deferred action matches the effect of all sicmutils
  generics on functions. (#218)

- `sicmutils.function/I` aliases `clojure.core/identity` (#218). #219 exposes
  `I` in `sicmutils.env`.

- `up` and `down` tuples from `sicmutils.structure` gain a proper `print-method`
  implementation (#229); these now render as `(up 1 2 3)` and `(down 1 2 3)`,
  instead of the former more verbose representation (when using `pr`.)

- `sicmutils.env.sci` contains an SCI context and namespace mapping sufficient
  to evaluate all of sicmutils, macros and all, inside of an
  [SCI](https://github.com/borkdude/sci) environment (#216). Huge thanks to
  @borkdude for support and @mk for implementing this!

- `sicmutils.numerical.elliptic` gains a full complement of elliptic integral
  utilities (#211):

  - Carlson symmetric forms of the elliptic integrals: `carlson-rd`,
    `carlson-rc`, `carlson-rj` (`carlson-rf` was already present)
  - Legendre elliptic integrals of the second and third forms, as the two-arity
    forms of `elliptic-e` and `elliptic-pi` (`elliptic-f` already existed)
  - the complete elliptic integrals via `elliptic-k` (first kind) and the
    single-arity forms of `elliptic-e` and `elliptic-pi`
  - `k-and-deriv` returns a pair of the complete elliptical integral of the first form,
    `elliptic-k`, and its derivative with respect to `k`.
  - `jacobi-elliptic-functions` ported from `scmutils` and Press's Numerical
    Recipes

- fixed bug with `g/dimension` for row and column matrices (#214). previously
  they returned `1` in both cases; now they return the total number of entries.

## 0.14.0

- After the work below, `v/nullity?` renamed to `v/zero?`, and `v/unity?`
  renamed to `v/one?`
  ([#180](https://github.com/sicmutils/sicmutils/pull/180)). This
  affects the names listed in the CHANGELOG entries below.

### Miscellaneous

- expose `bootstrap-repl!` to Clojurescript, so that this is available in
  self-hosted CLJS (https://github.com/sicmutils/sicmutils/pull/157)

- modified `infix.cljc` to wrap forms in `displaystyle` and add proper carriage
  returns inside structures
  (https://github.com/sicmutils/sicmutils/pull/157)

- add `multidimensional-minimize` to the `sicmutils.env` namespace
  (https://github.com/sicmutils/sicmutils/pull/157)

- add more `sqrt` simplification rules to allow square roots to cancel out
  across a division boundary, with or without products in the numerator and
  denominator (https://github.com/sicmutils/sicmutils/pull/160)

- fix NPE bug that appears in nelder-mead, when callback isn't supplied
  (https://github.com/sicmutils/sicmutils/pull/162)

- Add `sqrt-expand` and `sqrt-contract`, to allow simplifications to push inside
  of square roots (https://github.com/sicmutils/sicmutils/pull/163)

- speed up power series multiplication by skipping work when either head term is
  zero (https://github.com/sicmutils/sicmutils/pull/166)

- File moves:
  - `sicmutils.polynomial-gcd`    => `sicmutils.polynomial.gcd`
  - `sicmutils.polynomial-factor` => `sicmutils.polynomial.factor`
  - `sicmutils.rules`             => `sicmutils.simplify.rules`
  - `sicmutils.analyze`           => `sicmutils.expression.analyze`
  - `sicmutils.infix`             => `sicmutils.expression.render`
  - `sicmutils.numerical.compile` => `sicmutils.expression.compile`

- `sicmutils.env/one?` now exposes/aliases `sicmutils.value/unity?`
  [#154](https://github.com/sicmutils/sicmutils/pull/154)

- Fixed [#93](https://github.com/sicmutils/sicmutils/issues/93) by
  adding an explicit `g/invert` implementation for polynomials in the rational
  fn namespace. The fix lives in
  [#169](https://github.com/sicmutils/sicmutils/pull/169).

- added `sicmutils.value/sqrt-machine-epsilon`
  ([#170](https://github.com/sicmutils/sicmutils/pull/170))

- fixed issues in `function.cljc` and `operator.cljc` where the Clojurescript
  `IFn` `-invoke` arguments shadowed either the `this` operator, or some
  parameter name in the deftype
  ([#169](https://github.com/sicmutils/sicmutils/pull/169))

- `g/sqrt` now maintains precision with Clojurescript's rational numbers.
  `(g/sqrt #sicm/ratio 9/4)` for example returns `#sicm/ratio 3/2`.
  ([#168](https://github.com/sicmutils/sicmutils/pull/168))

- `g/determinant` and `g/transpose` now act as identity for everything in the
  numeric tower, plus symbolic expressions
  ([#168](https://github.com/sicmutils/sicmutils/pull/168))

- `sicmutils.expression.Expression` is now `sicmutils.expression.Literal`; it
  has a new `meta` field, and is a `deftype` instead of a `defrecord`.
  ([#168](https://github.com/sicmutils/sicmutils/pull/168))
  - To get the internal expression, use `x/expression-of` instead of
    `:expression`.
  - to access the `type` field, use `x/literal-type` instead of `:type`

- 2-arity `g/atan`, `g/cross-product` and `g/gcd` now work for functions
  ([#168](https://github.com/sicmutils/sicmutils/pull/168))

- `Literal` now responds appropriately to `v/unity?` and `v/nullity?` if it
  wraps a numerical "0" or "1". `v/exact?` now returns true if the literal wraps
  an exact number ([#168](https://github.com/sicmutils/sicmutils/pull/168))

- `x/variables-in` now works with wrapped expressions; no more need to
  explicitly unwrap
  ([#168](https://github.com/sicmutils/sicmutils/pull/168))

- `x/walk-expression` renamed `x/evaluate`
  ([#168](https://github.com/sicmutils/sicmutils/pull/168))

- The new `x/substitute` performs substitutions on an _unwrapped_ expression
  ([#168](https://github.com/sicmutils/sicmutils/pull/168))

-  `x/compare` returns a comparator that works with unwrapped symbolic
   expression trees
   ([#168](https://github.com/sicmutils/sicmutils/pull/168)). The rules
   are that that types have the following ordering:
  - empty sequence is < anything (except another empty seq)
  - real < symbol < string < sequence
  - sequences compare element-by-element
  - Any types NOT in this list compare using hashes

- `g/transpose` now works properly for functions that act as linear maps. The
  defining relation is:

```clojure
(= (((transpose f) g) 'x)
   (g (f x)))
```

- added `g/determinant` implementation to functions
  ([#171](https://github.com/sicmutils/sicmutils/pull/171))

- Moved all `literal-function` machinery and definitions to
  `sicmutils.abstract.function`
  ([#171](https://github.com/sicmutils/sicmutils/pull/171)).
  `sicmutils.function` now contains only the generic method implementations for
  clojure functions and multimethods.

- Switched inheritance order for functions;
  `:sicmutils.abstract.function/function` (used to be
  `:sicmutils.function/function`) now inherits from `::v/function` instead of
  the other way around.
  ([#171](https://github.com/sicmutils/sicmutils/pull/171))

- Enhanced the `g/simplify` behavior for core functions that overlap with
  generic functions (`+`, `-`, `*`, `/`, `mod`, `quot`, `rem`, `neg?`). These
  now freeze to the same symbols as their generic counterparts.
  ([#173](https://github.com/sicmutils/sicmutils/pull/173))

- Add support for the hyperbolic trig functions `sinh`, `cosh`, `tanh`, `atanh`,
  `asinh` and `acosh` to `sicmutils.expression.render/->Javascript`.
  ([#174](https://github.com/sicmutils/sicmutils/pull/174))

- Add support for the hyperbolic trig functions `atanh`, `asinh` and `acosh` to
  `sicmutils.expression.compile`.
  ([#175](https://github.com/sicmutils/sicmutils/pull/175))

- `matrix.cljc` gains `m/nth-col` and `m/diagonal`
  ([#178](https://github.com/sicmutils/sicmutils/pull/178) introduces:)

- As of [#178](https://github.com/sicmutils/sicmutils/pull/178)
  introduces:, we have three new kinds for matrices. Square matrices return
  `::m/square-matrix`, and columns and rows return `::m/column-matrix` and
  `::row-matrix` respectively. These all derive from `::m/matrix`. This makes it
  easier to register methods or test specifically for these cases. We've also
  added `m/column?` and `m/row?` predicates to check for these cases.

- [#185](https://github.com/sicmutils/sicmutils/pull/185) specializes
  all matrix operations that return power series (trig operations and `g/exp` to
  `::square-matrix`).

- [#184](https://github.com/sicmutils/sicmutils/pull/184) modifies
  `v/exact?` on functions; `((v/exact? f) x) == (v/exact? (f x))` now, instead
  of false as before. `literal-function` forms now have a correct `v/one-like`
  implementation.

- clojure Vars now respond to function algebra
  ([#184](https://github.com/sicmutils/sicmutils/pull/184)). All
  functions implement `g/negative?`, `g/abs`, `g/quotient`, `g/remainder`,
  `g/modulo`, `g/dimension` and `g/exact-divide`, responding to the appropriate
  arities.

- `sicmutils.complex/complex` can now take any real type in its constructor, vs
  only numbers
  ([#184](https://github.com/sicmutils/sicmutils/pull/184)).

- `modint` instances now implement `v/freeze?`: `(sicmutils.modint/make 1 2)`
  freezes to that `(modint 1 2)`.
  ([#185](https://github.com/sicmutils/sicmutils/pull/185)).

- `v/eq` renamed to `v/=`.
  ([#186](https://github.com/sicmutils/sicmutils/pull/186)).

- `v/zero-like` on matrices now fills entries with appropriate `v/zero-like`
  versions of their existing types
  ([#188](https://github.com/sicmutils/sicmutils/pull/188))

- `v/Value` gains `identity-like` and `identity`
  ([#188](https://github.com/sicmutils/sicmutils/pull/188)). These are
  aliased into `sicmutils.env`. Implementations are installed on:

  - all numeric types, symbolic expressions, `Differential` (they return 1 of the appropriate type)
  - native and abstract functions, vars (they return an identity function)
  - operators (return an identity operator, same as `one-like`)
  - matrices (identity matrix, only works with `::m/square-matrix`)
  - `Polynomial` (only works on monomials for now, returns an identity polynomial)
  - `RationalFunction` (returns the identity poly divided by unit poly, so only
    works on monomials by extension)
  - `ModInt` (returns the same as `one-like`)
  - `Series` and `PowerSeries` (returns `[0 1 0 0 0 0...]`). This is slightly
    suspect in the case of `Series`, since `Series`, unlike `PowerSeries`, are
    general infinite sequences and not necessarily interpreted as polynomials.
    This decision follows `scmutils` convention.

- `sicmutils.complex/I` aliases `i`
  ([#189](https://github.com/sicmutils/sicmutils/pull/189))

- `matrix.cljc` has a new `by-cols` (analogous to `m/by-rows`), and `row` to
  generate a row matrix (analagous to `column`).
  [#197](https://github.com/sicmutils/sicmutils/pull/197) Also in
  `matrix.cljc`:

  - `num-rows`, `num-cols` access the row or column number without inspecting
    the deftype variables directly
  - `fmap-indexed`, like `fmap` but receives `i` and `j` indices as second and
    third arguments.
  -
  - `with-substituted-row`, for swapping out a single row in a matrix
  - `submatrix` generates a submatrix from low and high row and cols
  - `matrix-some` renamed to `some`: make sure to use a namespace prefix to
    avoid clashing with `clojure.core/some`.
  - new-matrix constructor `by-cols` (analogous to `by-rows`, takes a sequence
    of columns)
  - `row` constructor takes a sequence of values and returns a row matrix.
  - `by-rows*`, `by-cols*`, `row*` and `column*` are non-variadic versions of
    those functions. If you already have a sequence of rows, columns or
    elements, prefer these.
  - `up->row-matrix` => `down->row-matrix` and `row-matrix->up` =>
    `row-matrix->down`. A row is analogous to a `down`, so we make a change to
    reflect this.
  - `g/cross-product` between two `down` structures now returns a `down`.
  - `make-zero` generates a zero-valued matrix of the supplied dimensions.
  - `make-diagonal` generates a diagonal matrix containing the values of the
    supplied sequence.
  - `m/identity-like` returns an identity matrix (given a square matrix) with
    entries of identical type, but set appropriately to zero or one. This is
    installed as `v/one-like` and `v/identity-like`.
  - `v/identity?` now returns true for identity matrices, false otherwise.
    `v/one?` returns `false` for identity matrices! If it didn't, `(* 2 (I 10))`
    would return `2`, since `one?` signals multiplicative identity.

- `sicmutils.structure/up` and `sicmutils.structure/down` now have analogous
  `s/up*` and `s/down*` functions. These behave identically, but are
  non-variadic. If you already have a sequence you'd like to transform, prefer
  these ([#197](https://github.com/sicmutils/sicmutils/pull/197)).

- `sicmutils.value/kind-predicate` takes some item and returns a predicate that
  returns true if its argument has the same type (or inherits from it)
  ([#197](https://github.com/sicmutils/sicmutils/pull/197)).

- `sicmutils.function/arg-shift` and `sicmutils.function/arg-scale` take
  functions and return new functions that shift and scale their arguments
  (respectively) by the originally supplied shifts
  ([#197](https://github.com/sicmutils/sicmutils/pull/197)).

- `sicmutils.generic/factorial` computes the factorial of the supplied integer
  `n`.
  ([#197](https://github.com/sicmutils/sicmutils/pull/197)).

- Many new functions and constants exposed in `sicmutils.env` via
  [#197](https://github.com/sicmutils/sicmutils/pull/197):

  - `-pi` joins `pi` as a constant
  - `s:generate`, `m:generate`, `vector:generate` to generate matrices,
    structures and vectors
  - `constant-series`, from `series/constant`
  - `seq:print` and `seq:pprint`
  - `matrix-by-cols`, `row-matrix`, `v:make-basis-unit`
  - aliases for `sicmutils.function`'s `arity`, `arg-shift`, `arg-scale`
  - `dimension`, `factorial` aliased from `sicmutils.generic`
  - `derivative` aliased from `sicmutils.calculus.derivative`
  - `submatrix`, `up->column-matrix`, `down->row-matrix`,
    `row-matrix->{down,vector}`, `column-matrix->{up,vector}` aliased from
    `sicmutils.matrix`
  - `D-numeric` from `sicmutils.numerical.derivative`
  - `brent-min`, `brent-max`, `golden-section-min`, `golden-section-max`
  - `nelder-mead`
  - `sum` from `sicmutils.util.aggregate
  - `kind-predicate` from `sicmutils.value`

- Structures and matrices both gain the ability to do native `get-in`,
  `assoc-in` and `empty`. These work as expected, like a potentially nested
  vector. ([#193](https://github.com/sicmutils/sicmutils/pull/193))

- `matrix.cljc` gains `up->row-matrix`, `up->column-matrix`, `row-matrix->up`,
  `column-matrix->up`
  ([#193](https://github.com/sicmutils/sicmutils/pull/193))

- `structure.cljc` gains many features in
  ([#193](https://github.com/sicmutils/sicmutils/pull/193)):

  - `kronecker` and `basis-unit` for generating potentially infinite basis
    sequences
  - the ability to conj new items onto a structure: `(conj (up 1 2) 3) => (up 1
    2 3)`
  - The structure-preserving `map-chain` takes a 2-arg function and presents it
    with each element of a deeply nested structure, along with a vector of its
    "chain", the path into its location. The fn's return becomes the new item at
    that location.
  - `structure->prototype` generates a same-shape structure as its argument,
    with symbolic entries that display their location (preserving orientation).
  - `typical-object` returns a structure of the same shape and orientation as
    `s`, generated by substituting gensymmed symbols in for each entry.
  - `compatible-zero` returns a structure compatible for multiplication with `s`
    down to 0.
  - `transpose-outer` returns a new structure with the same orientation as the
    first element of `s`, filled with elements of the same orientation as `s`.
    Each element is generating by taking the first element of each entry in `s`,
    the the second, etc... In that sense this is similar to a traditional matrix
    transpose.
  - `dot-product` takes the dot product of two structures. They must be the same
    top-level orientation and dimension; beyond that, their entries are
    pairwise-multiplied and summed.
  - `inner-product` is the same, but the left structure is conjugated first.
  - `outer-product` now works multiple levels deep.
  - `vector-outer-product` and `vector-inner-product` are similar, but only
    enforce the top-level length; all internal structures are NOT flattened and
    must be compatible for `g/*`.
  - `compatible-for-contraction?` now searches recursively down into a
    structure; previously it only checked the top level.
  - The new `*allow-incompatible-multiplication*` dynamic variable is set to
    `true` by default. Set it false to force a setting where, when you multiply
    structures, they must be:
    - opposite orientation
    - every element of the right entry must be compatible for contraction with
      the left
  - structure multiplication with scalars, etc now respects ordering, just in
    case any multiplication is not commutative.
  - `sicmutils.generators` now holds generators for `up`, `down`, and
    `structure` generators; these produce potentially deeply nested structures.
    `up1`, `down1` and `structure1` generate only one level deep. Mix and match!
    See `structure_test.cljc` for many examples of how to use these.

### Literals

- `literal-matrix` fn generates a symbolic matrix
  (https://github.com/sicmutils/sicmutils/pull/169)
- `literal`, `literal-up` and `literal-down` generate symbolic structures
  (https://github.com/sicmutils/sicmutils/pull/169)

### Numeric Tower Adjustments

This release (courtesy of
[#168](https://github.com/sicmutils/sicmutils/pull/168)) brings
the numeric tower in line with the scmutils tower. Prior to this release, all
numbers, including complex, descended from `::x/numerical-expression`. Symbolic
expressions _also_ derived from this type! The problem this causes is that all
of generic implementations for the number types default to the symbolic
functions.

If I don't specify a `g/sec` method for numbers, for example, then `(g/sec 12)`
returns a symbolic `(/ 1 (cos 12))`, instead of actually evaluating the
expression.

The fix comes from these changes:

- `::v/number` now means, "the numeric tower ascending from integer -> rational
  -> real -> complex numbers. All of these types now respond `true` to
  `v/number?` (prior to this release, Complex numbers did NOT!)

- `::v/real` now means, "anything in the numeric tower except Complex". These
  all respond true to `v/real?`

- `::x/numeric-expression` has changed to `::x/numeric`, and now means "anything
  that responds to `::"v/number`, plus symbolic expressions, which now clearly
  _represent_ any number in the numeric tower. Query for these with `v/scalar?`

I can now make some comments that clear up my former misunderstandings:

- The `sicmutils.abstract.number` (I'll call this `an` here) namespace is
  responsible for installing generic implementations of all numeric methods for
  symbolic expressions and "literal numbers".

- the `an/literal-number` constructor promotes a number, symbol or symbolic
  expression up to `:xx/numeric`, which means that any operation you perform on
  it will pass it through the symbolic expressions defined in
  `sicmutils.numsymb`. A few notes on these expressions:

  - They will try to preserve exactness, but if they can't - ie, if you do
    something like `(cos (an/literal-number 2.2))` - the system will return
    `-.588`. If you call `(cos (an/literal-number 2))`, you'll get the
    expression `(cos 2)`, preserving exactness.

  - Symbols are automatically interpreted as "literal numbers".

  - The only ways to make a proper symbolic expression that works with the
    generics are:

    - Use the explicit `an/literal-number` constructor
    - pass a symbol to any generic arithmetic function
    - perform any unary or binary arithmetic operation on an existing symbolic
      expression.

  - `an/abstract-number?` returns true for symbolic expressions, anything
    wrapped in `literal-number` or symbols.

  - `literal-number?` only returns true for explicitly wrapped things and
    symbolic expressions, not symbols.

  - use `v/real?`, `v/number?` and `v/scalar?` to query the numeric tower.


- If you want to compare literal numbers and an expression like
  `(an/literal-number 12)`, use `v/=`. In Clojurescript, this will work with
  the built in `=` as well, since equality is implemented with a protocol that
  we can extend. For example:

```clojure
(v/= 12 (literal-number 12))
;;=> true

(= 12 (literal-number 12))
;; true in cljs, false in clj
```

If you keep the literal on the left side of `=`, this will work in both systems,
since we've overridden the `=` implementation for literals:

```clojure
(= (literal-number 12) 12)
;;=> true in both languages
```

This paves the way for the other abstract types that exist in `scmutils`, like
matrices, up and down tuples.

### New Generic Functions

This release brings us closer to the interface provided by `scmutils`.

PR [#193](https://github.com/sicmutils/sicmutils/pull/193) brings:

- `g/dot-product`, for scalars, differentials, structures, functions and
  row/column matrices
- `g/inner-product` for scalars, structures, functions and row/column matrices
- `g/outer-product` for functions, structures of length 3 and matrices, between
  a row and a column only
- `g/cross-product` now works for row/column matrices in addition to structures
  (and functions that accept these)

PR https://github.com/sicmutils/sicmutils/pull/169 brings:

- `g/exp2`, `g/exp10` for exponents with base 2 and 10
- `g/log2`, for base 2 logarithms
- `g/log10` for base 10 logs
- `g/gcd` and `g/lcm` are now exposed in `sicmutils.env`

[#178](https://github.com/sicmutils/sicmutils/pull/178) introduces:

- `g/dimension` for scalars (always 1), structures and matrices (square, column
  and row)
- `g/trace` returns the trace for square matrices and square structures

We now expose the following additional trigonometric functions in
`sicmutils.generic` (courtesy of
https://github.com/sicmutils/sicmutils/pull/154):

- `cosh`: hyperbolic cosine
- `sinh`: hyperbolic sine
- `tanh`: hyperbolic tangent, ie sinh/cosh
- `sech`: hyperbolic secant, ie 1/cosh
- `csch`: hyperbolic secant, ie 1/sinh
- `acosh`: inverse hyperbolic cosine, ie, `(= x (cosh (acosh x)))`
- `asinh`: inverse hyperbolic sine, ie, `(= x (sinh (asinh x)))`
- `atanh`: inverse hyperbolic tangent, ie, `(= x (tanh (atanh x)))`

These three methods existed in `sicmutils.env`, but not as extensible generics.
Now they're fully extensible:

- `cot`: cotangent, ie 1/tan
- `sec`: secant, ie 1/cos
- `csc`: cosecant, ie 1/sin

These all work with:

- real and complex numbers
- power series (missing a few implementations, operators and matrices are
  missing the same ones for this reason)
- matrices (square matrices return their power series expansions)
- operators (power series expansion of the operator)
- functions (where they create composition)
- symbolic expressions
- Derivatives and dual numbers! The new functions all work with `D`, the
  forward-mode automatic differentiation operator.

Additionally, four methods that lived in `sicmutils.generic` are now exposed as
generics:

- `real-part`
- `imag-part`
- `angle`
- `conjugate`

These now work on:

- _all_ numeric types, including symbolic expressions.
- functions
- structures (only `magnitude` and `conjugate`)
  - `magnitude` formerly didn't handle structures containing complex numbers by
    taking a proper inner product. This is fixed as of
    [#168](https://github.com/sicmutils/sicmutils/pull/168)

- PR [#189](https://github.com/sicmutils/sicmutils/pull/189) introduces:

  - `g/make-rectangular`, (build a complex number from real and imaginary parts)
  - `g/make-polar` (build a complex number from radius and angle)
  - note that these work with real numbers, symbolic numbers, functions and any
    combination of these.

These work with functions, real numbers and symbolic expressions (and any mix of
the three).

## 0.13.0

The main announcement for this release is _Clojurescript Support!_. Implementing
this resulted in a few upgrades along the way:

- more powerful numerics, specifically `definite-integral` and native
  minimization routines
- a generic numeric tower for Clojurescript
- Many more tests! The test coverage was great before, and it's stayed high as
  we've added new implementations.
- added explicit code coverage metrics via Codecov: [![Codecov branch](https://img.shields.io/codecov/c/github/littleredcomputer/sicmutils/master.svg?maxAge=3600)](https://codecov.io/github/littleredcomputer/sicmutils)

Here are more explicit details on the release.

### Misc

`generic.cljc` now includes a default implementation of:

- `expt` given a valid `mul`
- default `sub`, given a valid `add` and `negate`
- default `div`, given a valid `mul` and `invert`
- `Expression` and `Operator` both have better `print-method` implementations,
  so the repl experience feels more like `scmutils`
- `Operator` now has an `expn` method, which acts like `g/exp` on an operator
  but expands each term in order `n`.
- many, many more tests!

### Clojurescript Support

Full conversion of SICMUtils to Clojurescript. All functionality from v0.12.1
now works in both Clojure and Clojurescript!

Most of the conversion was straightforward. The major missing piece was a
numeric tower implementation for Clojurescript (complex numbers, ratios) that
bring it up to parity with Clojure:

- Add the `bigfraction` implementation from
  [fraction.js](https://www.npmjs.com/package/fraction.js) sicmutils.ratio for
  cross-platform ratio support (#99)
- Adds CLJS complex number support through [complex.js](https://github.com/infusion/Complex.js) (#41)
- `js/BigInt`, `goog.math.Long` and `goog.math.Integer` implementations round
  out the numeric tower (#45)

### Numerical Routines

The numerical routines in SICMUtils depend heavily on Apache Commons, which of
course only exists in Java. We had to implement much of the numerics code in
native Clojure. It's fast, efficient and functional. Give it a read if you're
curious about how these algorithms work.

- New, native minimization routines have replaced the Apache Commons implementations:

  - **Univariate Minimizers**
    - Port scipy's auto-bracketing + scmutils version (#104)
    - Port golden section search from scipy (#105)
    - Implement Brent's method for fn minimization in native clj (#106)

  - **Multivariate**
    - pure Clojure implementation of Nelder-Mead (#102)

- Native `definite-integral` numerics implementation, written as a series of
  computational essays:

  - **Basics**:
    - [Riemann Sums](https://github.com/littleredcomputer/sicmutils/blob/master/src/sicmutils/numerical/quadrature/riemann.cljc), all the way up through efficient, incremental, "accelerated" versions of these easy-to-understand methods:
    - [Midpoint method](https://github.com/littleredcomputer/sicmutils/blob/master/src/sicmutils/numerical/quadrature/midpoint.cljc), same development but shorter since it reuses functional abstractions. Also incremental, efficient, accelerated
    - [Trapezoid Method](https://github.com/littleredcomputer/sicmutils/blob/master/src/sicmutils/numerical/quadrature/trapezoid.cljc), same idea but for closed intervals.

  - **Sequence Acceleration / Extrapolation Methods**
    - [Polynomial interpolation](https://github.com/littleredcomputer/sicmutils/blob/master/src/sicmutils/numerical/interpolate/polynomial.cljc): the general thing that "richardson extrapolation" is doing below. Historically cool and used to accelerate arbitrary integration sequences
    - [Rational Function extrapolation](https://github.com/littleredcomputer/sicmutils/blob/master/src/sicmutils/numerical/interpolate/rational.cljc): used in bulirsch-stoer integration and ODE solving.
    - "[Richardson extrapolation](https://github.com/littleredcomputer/sicmutils/blob/master/src/sicmutils/numerical/interpolate/richardson.cljc)" is a special case, where we get more efficient by assuming that the x values for the polynomial interpolation go 1, 1/2, 1/4... and that we're extrapolating to 0.

  - **Higher-order Calculus:**
    - [Numerical derivatives](https://github.com/littleredcomputer/sicmutils/blob/master/src/sicmutils/numerical/derivative.cljc): derivatives using three kinds of central difference formulas... accelerated using Richardson extrapolation, with a nice technique for guarding against underflow.
    - [Simpson's Method](https://github.com/littleredcomputer/sicmutils/blob/master/src/sicmutils/numerical/quadrature/simpson.cljc)... fit a parabola to every slice. OR, "accelerate" the trapezoid method with one step of Richarsdson extrapolation!
    - [Simpson's 3/8 Method](https://github.com/littleredcomputer/sicmutils/blob/master/src/sicmutils/numerical/quadrature/simpson38.cljc): Same idea, but accelerate a sequence that triples its slices every iteration.
    - [Boole's Rule](https://github.com/littleredcomputer/sicmutils/blob/master/src/sicmutils/numerical/quadrature/boole.cljc): trapezoid method plus two steps of Richardson extrapolation. (Are you starting to see the pattern??)
    - [Romberg Integration](https://github.com/littleredcomputer/sicmutils/blob/master/src/sicmutils/numerical/quadrature/romberg.cljc): midpoint OR trapezoid, with as many steps of Richardson extrapolation as we can take!
    - [Milne's Rule](https://github.com/littleredcomputer/sicmutils/blob/master/src/sicmutils/numerical/quadrature/milne.cljc), MIDPOINT method, one step of extrapolation!
    - [Bulirsch-Stoer integration](https://github.com/littleredcomputer/sicmutils/blob/master/src/sicmutils/numerical/quadrature/bulirsch_stoer.cljc)... midpoint or trapezoid, with rational function extrapolation, as many steps as we can handle AND some custom step sizes.

  - **Combinators**:
    - [Variable Substitutions](https://github.com/littleredcomputer/sicmutils/blob/master/src/sicmutils/numerical/quadrature/substitute.cljc): implemented as functional wrappers that take an integrator and return a modified integrator.
    - [Improper Integrals](https://github.com/littleredcomputer/sicmutils/blob/master/src/sicmutils/numerical/quadrature/infinite.cljc): a template for a combinator that enables infinite endpoints on any integrator, using variable substitution on an appropriate, tunable range.
    - [Adaptive Integration](https://github.com/littleredcomputer/sicmutils/blob/master/src/sicmutils/numerical/quadrature/adaptive.cljc): a combinator that turns any of the integrators above into an "adaptive" integrator that's able to focus in on difficult regions.
  - And finally, "[Numerical Quadrature](https://github.com/littleredcomputer/sicmutils/blob/master/src/sicmutils/numerical/quadrature.cljc)", the namespace/essay that ties it all together.

- `sicmutils.numerical.compile` uses [SCI](https://github.com/borkdude/sci), the
  Small Clojure Interpreter, to generate compiled numerical code (#133)

- Implemented ODE solving using @littleredcomputer's
  [odex-js](https://github.com/littleredcomputer/odex-js) library (#135)

### Reader Literals

[data_readers.cljc](https://github.com/littleredcomputer/sicmutils/blob/master/src/data_readers.cljc)
provides 3 new data reader literals:

- `#sicm/ratio`

Use this with a ratio literal, like `#sicm/ratio 1/2`, or with a string like
`#sicm/ratio "1/4"`. If the denominator is `1` this literal will return a
`js/BigInt` in Clojurescript, or a Long in Clojure.

- `#sicm/bigint`

Use with a number literal, like, `#sicm/bigint 10`, or a string like
`#sicm/bigint "10000012"` to generate a `js/BigInt` in Clojurescript, or a
`clojure.lang.BigInt` in Clojure.

- `#sicm/complex`

Currently this only works with a string like `#sicm/complex "1 + 2i"`. In the
future it might work with a pair of `(real, complex)`, like:

    #sicm/complex [1 2]

### Power Serious, Power Serious

The Power Series implementation in `series.cljc` received an overhaul. The
implementation now follows Doug McIlroy's beautiful paper, ["Power Series, Power
Serious"](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.333.3156&rep=rep1&type=pdf).
Doug also has a 10-line version in Haskell on [his
website](https://www.cs.dartmouth.edu/~doug/powser.html).

The API now offers two types:

 - `Series`, which represents a generic infinite series of arbitrary values, and
 - `PowerSeries`, a series that represents a power series in a single
   variable; in other words, a series where the nth entry is interpreted as
   the coefficient of $x^n$:

    $$[a b c d ...] == $a + bx + cx^2 + dx^3 + ...$$

`series/series?` responds true to both. `series/power-series?` only responds
true to a `PowerSeries`.

To turn a `PowerSeries` into a `Series`, call it as a function with a single
argument, or pass the series and one argument to `series/value` to evaluate the
series using the above equation.

To turn a `Series` into a `PowerSeries`, call `series/->function`. None of the
functions discussed below can mix series types; you'll have to do the conversion
explicitly.

Each type supports the following generic operations:

- `*`, `+`, `-`, `/` between series and non-series
- `g/negate`, `g/invert`, `g/sqrt`, `g/expt` work as expected.
- `g/add` between series and non-series

`PowerSeries` additionally supports:

- `g/exp`, `g/cos`, `g/sin`, `g/asin`, `g/tan`
- `g/partial-derivative`, so `PowerSeries` works well with `D`

Each of these acts as function composition for the single variable function that
the `PowerSeries` represents. If `s` is a `PowerSeries` that applies as `(s x)`,
`(g/exp s)` returns a series that represents `(g/exp (s x))`.

There are many more new methods (see the namespace for full documentation):

- `starting-with` renamed to `series`
- `power-series`, analogous `series` but generates a `PowerSeries`
- `series*` and `power-series*` let you pass an explicit sequence
- `series/take` removed in favor of `clojure.core/take`, since both series
  objects act as sequences
- `generate` takes an additional optional argument to distinguish between series
  and power series
- `Series` now implements more of `v/Value`
- new `zero`, `one`, `identity` constants
- `constant` returns a constant power series
- `xpow` generates a series representing a bare power of `x`
- `->function` turns a `Series` into a `PowerSeries`
- `value`, `fmap` now handles both `Series` and `PowerSeries`
- `(inflate s n)` expands each term $x^i$ of `s` to $x^{in}$
- `compose` returns the functional composition of two `PowerSeries`
- `revert` returns the functional inverse of two `PowerSeries`
- `integral` returns a series representing the definite integral of the supplied
  `PowerSeries`, 0 => infinity (optionally takes an integration constant)

The namespace also provides many built-in `PowerSeries` instances:

- `exp-series`
- `sin-series`
- `cos-series`
- `tan-series`
- `sec-series`
- `asin-series`
- `acos-series`
- `atan-series`
- `acot-series`
- `sinh-series`
- `cosh-series`
- `tanh-series`
- `asinh-series`
- `atanh-series`
- `log1+x-series`
- `log1-x-series`
- `binomial-series`

And two `Series` (non-power):

- `fib-series`, the fibonacci sequence
- `catalan-series`, the [Catalan
  numbers](https://en.wikipedia.org/wiki/Catalan_number)

### Matrix Generic Operations

`::matrix` gained implementations for `exp`, `cos`, `sin`, `asin`, `tan`,
`acos`, `asin`; these now return taylor series expansions of the operator, where
multiplication is composition as before.

### Operator Generics

`Operator` gained implementations for `cos`, `sin`, `asin`, `tan`, `acos`,
`asin`; these now return taylor series expansions of the operator, where
multiplication is composition as before.

## [v0.21.1]

- Getting Github releases up to parity with the most recent release to Clojars.

## [v0.10.0]

- Did some refactoring and one breaking rename (Struct became Structure, since
  we don't abbreviate other deftypes). This also marks the point of departure
  for working with Functional Differential Geometry.


## [v0.9.8]

- This is the version that was current as of the talk @littleredcomputer gave at
  [Clojure/west 2017](2017.clojurewest.org), entitled "[Physics in
  Clojure](https://www.youtube.com/watch?v=7PoajCqNKpg)."
