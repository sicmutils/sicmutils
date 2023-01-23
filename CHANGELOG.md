# Changelog

## [unreleased]

## [0.23.0]

- #532:

  - Removes the `potemkin` dependency by importing only what we need directly
    into `emmy.util.def`. This makes sense since our versions add a `fork`
    call so that they work for ClojureScript as well.

  - Moves all `examples` into the tests so that we don't ship them with the
    library. These will eventually be converted to Clerk notebooks.

  - Removes the `hiccup` dependency.

  - Upgrades `test.chuck` and removes all `:include-macros true` calls for that
    library. Only `same/ish` requires them now!

  - Capitalizes the "Script" in ClojureScript everywhere it appears.

- #531:

  - Fixes a typo in one of the rules in
    `emmy.simplify.rules/expand-multiangle`, closing #530

  - Adds proper self-require forms to all `cljc` namespaces with macros; this
    removes the need for `:include-macros true` or `:refer-macros` and makes the
    life of consumers simpler.

  - Upgrades sci, test.check, core.match, timbre, clojurescript, shadow-cljs
    dependencies to remove various warnings.

  - Drops cljsjs dependencies in favor of `deps.cljs` entries

  - Forces the subexpression walk in `pattern.rule/try-subexpressions` with
    `doall`

  - Adds type hints to all remaining `Complex` calls in js, to fix issues with
    advanced compilation (thanks to @mhuebert for finding this)

  - Adds `emmy.structure/symbol-set`

  - Adds `IHash` implementations for `js/BigInt`, `goog.math.Long`,
    `goog.map.Integer` and `Ratio` types

- #515:

 - tidies up the `square` and `cube` speedups thanks to a tip from GJS

 - Converts more `(mul x x)` to `square` in the derivatives of the
   `emmy.generic` namespace.

 - fixes a bug in the `numeric-zero?` check of exponent's derivative.

- #514:

  - Modifies `emmy.calculus.derivative/taylor-series` to return a proper
    `PowerSeries` instance, which the user can call with some `dx` to get back
    the old behavior.

    The new version can take any number of arguments in addition to `f`.
    Supplying no arguments returns the expansion at 0; if you supply many
    arguments (totally fine!), you'll need to wrap your `dx` components in a
    vector before supplying them to the returned `PowerSeries`.

  - `emmy.series/function->` works the same way now, and functions
    identically, but with a different implementation. (previously it took a
    single expansion point under a keyword argument `:x0`.)

  - The new `emmy.calculus.derivative/symbolic-taylor-series` is a port of
    `Taylor-series-coefficients` from `scmutils`. It has the same contract as
    `taylor-series`, except that the full expansion is performed symbolically,
    and the original arguments are substituted in after expansion and
    simplification.

  - Other changes:

    - Installs `1` as the `one-like` and `identity-like` return values for
      structures and vectors. A true identity element would be an identity
      element compatible with all entries of the structure; but as defined now,
      `1` is a fine choice and matches the `scmutils` implementation.

    - new `emmy.differential/map-coefficients`, which makes `simplify`
      slightly more efficient by filtering terms .

    - more efficient `emmy.expression/variables-in`, maybe 30% faster for
      big expressions; this makes a difference in the simplifier!

    - `emmy.expression/substitute` now works for proper `Literal`
      instances. Before it only worked for unwrapped literals.

    - matrix walks made slightly faster by caching a row or column before
      traversal

- #512:

  - adds `emmy.mechanics.routhian`, with implementations of
    `Lagrangian->Routhian`, `Routh-equations`, `Routhian->acceleration`,
    `Routhian->state-derivative`, `Lagrangian-state->Routhian-state` and
    `Routhian-state->Lagrangian-state`.

  - adds missing `emmy.mechanics.{routhian,time-evolution,noether}` to
    `emmy.env.sci`

- #509:

  - Fixes a bug with `down*Matrix` multiplication, and adds tests for
    correctness.

  - Adds `emmy.matrix.{symmetric?,antisymmetric?}` predicates

  - The mechanics port continues with `emmy.mechanics.rigid`:

    - `T-rigid-body` moves to `T-body-Euler` with an alias back to its original
      name. Same situation for `Euler-state->L-body` => `L-body-Euler` and
      `Euler-state->L-space` => `L-space-Euler`.

    - New functions: `three-vector-components->antisymmetric`, `T-body`,
      `L-body`, `L-space`, `Euler->omega`, `Euler->omega-body`,
      `quaternion-state->omega-body`, `quaternion-state->omega-space`,
      `qw-state->L-body`, `qw-state->L-space`, `T-quaternion-state`

- #511 focuses on adding more rotations and efficiency to
  `emmy.quaternion`. Specifically:

  - `magnitude-sq` and `magnitude` are now more efficient.

  - New functions to get to and from quaternions and various matrix
    representations: `from-rotation-matrix`, `->rotation-matrix`,
    `from-complex-matrix`, `->complex-matrix`, `from-4x4-matrix`, `->4x4-matrix`

  - New instances `ONE-matrix`, `I-matrix`, `J-matrix`, `K-matrix`, matrix
    representations of the corresponding quaternion elements.

  - Similar to the matrix elements, we also now have `ONE-tensor`, `I-tensor`,
    `J-tensor`, `K-tensor`.

- #503:

  - Changes the default implementation of `square` and `cube` for differentials
    to use `(expt <> 2)` etc instead of `(* <> <>)`.

    This is a _big deal!_ For certain expressions there's a huge blowup when you
    square a big symbolic term, and taking the derivative of it TWICE is very
    messy.

    With this change, differentials use the chain rule to calculuate the
    derivative of $x^2$ as $2x*x'$, instead of using the product rule and
    achieving a SECOND differentiatation of the same form and another
    multiplication: $xx' + x'x$.

    Before a judicious `simplify` call I added, this change dropped the runtime
    of the `emmy.sicm.ch3-test` suite down by 6x. After the simplify change
    in `emmy.examples.top` the tests were still 40% faster in that
    namespace.

  - Fixes a bug where the `RationalFunction` cube implementation actually called
    `square`.

  - adds `emmy.mechanics.lagrange/Lagrangian` for building function
    signatures of Lagrangians.

  - adds the `emmy.mechanics.time-evolution` namespace

  - adds `emmy.mechanics.lagrange/L-axisymmetric-top`, more efficient than
    the version in `emmy.examples.top`

  - Fleshes out `emmy.mechanics.hamilton`:

    - New functions: `H-state?`, `compatible-H-state?`, `state->p`, `momenta`,
      `P`, `literal-Hamiltonian-state`, `L-state->H-state`, `H-state->L-state`,
      `H-state->matrix`, `matrix->H-state`, `make-Hamiltonian`, `D-phase-space`,
      `Hamiltonian->Lagrangian-procedure`, `Hamiltonian->Lagrangian`,
      `flow-derivative`, `flow-transform`, `standard-map-inverse`, `F->K`,
      `J-func`, `T-func`, `canonical-H?`, `canonical-K?`,
      `linear-function->multiplier`, `Phi`, `Phi*`, `qp-canonical?`,
      `polar-canonical-inverse`, `two-particle-center-of-mass` ,
      `two-particle-center-of-mass-canonical`, `transpose-function`,
      `multiplicative-transpose`, `symplectic-two-form`, `canonical-transform?`,
      `J-matrix`, `symplectic?`

    - `F->CH` moves to `F->CT` (`F->CT` is now an alias)

    - `Legendre-transform-fn` becomes `Legendre-transform-procedure` and gains
      more correctness tests, toggled on and off by the
      `*validate-Legendre-transform?*` dynamic variable.

- #508 adds `emmy.mechanics.noether` namespace, with `Noether-integral`.

- #506 tidies up the build by removing unneeded reader conditionals and
  replacing renames like `core-=` with a proper require of `clojure.core`.

- #502 begins the port of the remaining items in the scmutils `mechanics`
  package over the Clojure. This PR focuses on `emmy.mechanics.lagrange`,
  which contains functions from many files in the original `mechanics` folder.

  - `momentum-tuple` moves here from `emmy.mechanics.hamilton`

  - New functions `->L-state`, `->local`, `->state`,`state->n-dof`, `time`,
    `state->{q,qdot,qddot}`, `coordinates`, `velocities`, `accelerations`, `Q`,
    `Qdot`, `Qdotdot`, `literal-Lagrangian-state` `path->state-path` (alias for
    the existing `Gamma`), `Rayleigh-dissipation`, `qv->local-path`,
    `Lagrange-equations-first-order`, (with `Lagrange-equations-1` alias),
    `Lagrangian->power-loss`, `T3-spherical`, `L3-central`, `Dt-procedure` and
    the wrapping operator `Dt`, `Euler-lagrange-operator` (with
    `Lagrange-equations-operator` and `LE` aliases), `generalized-LE`.

  - Many of these are aliased into `emmy.env`. Ask if you think more should
    be there!

  - many new built-in Lagrangians: `L-Kepler-polar`, `L-coupled-harmonic`,
    `L-sliding-pend`, `L-pendulum`, `L-two-particle`

  - `Lagrange-equations`, `Lagrangian->acceleration`,
    `Lagrangian->state-derivative` now take a dissipation function

  - `local-state-derivative` aliases the 1-arity version of
    `Lagrangian->state-derivative`

  - New `rectangular->polar`, `polar->rectangular`, `spherical->rectangular` and
    `rectangular->spherical` that operate on coordinates, with associated `r->p`
    (new), `p->r`, `s->r` and `r->s` (new).

- #501 moves `elliptic-integrals` from `emmy.special.elliptical-test`
  `emmy.special.elliptical`, as it's needed by the upcoming
  `emmy.mechanics.pendulum` namespace.

## [0.22.0]

- #497:

  - `emmy.expression.compile/compile-state-fn` and its non-memoized version
    can now take an explicit `:mode` argument; this will override the
    dynamically bound `*mode*`.

    Invalid modes supplied via `:mode` will cause `compile-state-fn` to throw an
    exception.

  - Fixes a bug where non-numeric operations `up` and `down` were applied at
    compile time, throwing an error.

- #498: replace all long-form GPL headers with `"SPDX-License-Identifier:
  GPL-3.0"`.

- #496:

  - replaces the function values in `emmy.expression.compile` with symbols;
    I hadn't realized before that substituting in symbolic `Math/sqrt`, for
    example, was possible, vs a `#(Math/sqrt %)` function value. Compiled
    functions are now faster!

    A simulation run of the double pendulum example in the [clerk-demo
    repository](https://github.com/nextjournal/clerk-demo/blob/20a404a271bea29ef98ee4e60a05e54345aa43ba/notebooks/emmy.clj)
    now runs in 350ms vs the former 2.2 seconds, a major win.

  - Function compilation now pre-simplifies numerical forms encountered inside a
    function, like `(/ 1 2)`, instead of letting them be evaluated on every fn
    call.

  - All numerical forms encountered in function compilation are now converted to
    either `double` on the JVM or `js/Number` in javascript; this way no
    `BigInt` values etc are left around.

  - In `emmy.expression.compile`:

    - gains a new, validating `compiler-mode` function for fetching the compiler
      function.

    - `set-compiler-mode!` now actually works. It never did!

    - New `:source` compile mode that returns a source code form. You can either
      call `eval` on this or call `sci-eval` to get an SCI-evaluated function
      with all proper bindings in place.

    - `compile-state-fn` now takes an optional options map, with support for
      `:flatten?` and `:generic-params?` keywords. These can be used to tune the
      shape of the function returned by `compile-state-fn`.

- #485:

  - Bumps the JDK version for Github Actions to 17 from 8.

  - Adds a development dependency on Nextjournal's
    [Clerk](https://github.com/nextjournal/clerk) library, and begins the
    process of massaging various namespaces into proper literate essays
    display-able with Clerk. To run these, start a REPL and follow the
    instructions in `dev/user.clj`.

    - `emmy.calculus.derivative` and `emmy.differential` now render as
      proper literate essays, with all TeX bugs fixed.

  - Bumps the shadow-cljs dependency to version `2.17.4`, and the included
    `cljs` version to `1.11.4`. `emmy.collection` properly handles the new
    cljs `IntegerRange` class.

  - `emmy.polynomial.factor` now memoizes `poly->factored-expression` by
    default. If polynomial GCD fails inside that function the computation now
    proceeds with a warning instead of failing.

- #492 updates the `clj-kondo` linters to emit custom warnings with _all_
  metadata from the original token, not just `:row` and `:col`. This fixes the
  ability to override or ignore individual warnings.

- #490 adds `emmy.numerical.roots.bisect` with implementations of bisection
  search, secant search and a mixed method found in `scmutils`. These all live
  under a `bisect` function.

  The data structure returned is similar to the minimization functions in the
  `emmy.numeric.{unimin, multimin}` namespaces. As more root-finding
  methods come online this should all standardize nicely.

- #491 adds `emmy.mechanics.rotation/M->Euler`, for converting from a
  rotation matrix to a triple of Euler angles. Now we can successfully round
  trip.

- #489:

  - Installs `g/log` and `g/exp` for `js/BigInt` instances, enabling `g/log2` and
    `g/log10` in the mix.

  - Removes most `js*` calls using `coercive-=` and `js-mod`. This form is
    internal and should be avoided.

- #484 adds `emmy.polynomial/from-power-series`, for generating a
  polynomial instance from some prefix of a (univariate) power series.

## [0.21.1]

- #481:

  - fixes a long-standing (test-only) bug in `emmy.polynomial-test` around
    palindromic polynomials

  - Adds a new `x-degree` argument to `emmy.polynomial/univariate->dense`,
    for padding the result with zeros in the case that you want to guarantee a
    certain dense degree in the result.

- #480: `emmy.numerical.quadrature/definite-integral` now coerces the
  result of non-compiled integrands in cljs to double. This prevents the
  @kloimhardt bug where certain paths would produce `BigInt` instances and fail
  in quadrature calls.

- #477:

  - Adds tight integration with the
    [`clj-kondo`](https://github.com/clj-kondo/clj-kondo) linter via an exported
    clj-kondo configuration in the `resources` directory. All macros in the
    library now offer pleasant linting to users. This is especially helpful for
    the macros in `pattern.rule`, which now can offer live feedback to
    pattern-matching authors.

    See `doc/linting.md` for details on various warnings reported, and
    installation instructions for the clj-kondo config.

    Thanks to @borkdude for all of his help getting this working, and making
    this amazing project!

  - All linter errors and warnings are now addressed, fixed and silenced for the
    entire codebase, both `test` and `src` directories.

  - A new Github Action will run the linter for every PR and push to master, and
    annotate PRs with linter warnings and errors.

  - `com.gfredericks/test.chuck` dev dependency upgraded to `0.2.13` to grab its
    clj-kondo exported config.

  - I found the following bugs with the help of the linter:

    - Deleted the unused `emmy.differential/d:apply`.

    - Fixed a bug with `emmy.expression.render/->JavaScript` not using the
      second argument to `remainder`.

    - deleted `emmy.numerical.quadrature.common` in favor of
      `emmy.generic/infinite?`

    - Fixed a broken integrator in `emmy.numerical.quadrature.simpson38`,
      and fixed the tests to actually stress this code.

    - Fixed a bug where
      `emmy.numerical.quadrature.substitute/exponential-upper` was not
      actually using its input function!

    - unused `simplify` argument removed from
      `emmy.simplify.rules/non-negative-factors!` and all uses.

    - Bug fix in `emmy.special.elliptic/jacobi-elliptic-functions`; deep in
      the gnarly fn, one of the branches returned nil instead of its required
      values. Thank you, linter!

    - `pattern.rule` patterns can now handle spliced and unquote-spliced inputs in
      their symbol position.

    - `emmy.pattern/template` will no longer error in the 1-arity case when
      some form contains a binding entry like `(? (fn [m] ...))`. Instead, the
      function will be passed an empty map.

## [0.21.0]

- #474:

  - `g/quotient` now supports inexact inputs, as LONG as the inputs are equal up
    to sign. So `(g/quotient 1.2 -1.2)` now returns `-1` instead of throwing.

- #469:

  - `emmy.matrix` gains:

    - `literal-column-matrix`, `literal-row-matrix` for generating slightly
      tidier matrices of literal entries. (See `literal-matrix` for the prior
      option.)

    - `structure->matrix` converts 2 tensors into explicit matrices.

    - `s:solve-linear-left`, `s:solve-linear-right`, `s:divide-by-structure` act
      on 2 tensors. These live in the matrix namespace since they depend on
      conversions to and from tensors and matrices.

    - `make-diagonal` for generating diagonal matrices with a constant element
      along the diagonal.

    - `s->m`, `s:transpose` and `s:inverse` all gain new 2-arities
      that provides a sane default for `ls`.

    - More efficient matrix `invert` and `determinant` routines, plus functions
      to generate type specific custom matrix inversion and determinant routines
      via `classical-adjoint-formula`, `general-determinant`.

    - Linear equation solving via `solve`, `rsolve` and `cramers-rule`.

  - Implements new generics for matrices and structures:

    - diagonal matrices respond true to `v/=` with a scalar if all entries along
      the diagonal are equal to that scalar.

    - square matrices can now `g/+` and `g/-` with scalars; the scalar `c` is
      converted `(* c I)`, where `I` is an identity matrix of the same dimension
      as the square matrix.

    - `(g/acot M)` now expands the matrix `M` into a nice power series, more
      efficient than the previous default.

    - Thanks to `solve` and `cramers-rule`, the following `g/div` combinations
      now work: `matrix/scalar`, `scalar/square-matrix`,
      `column-matrix/square-matrix`, `row-matrix/square-matrix`,
      `up/square-matrix`, `down/square-matrix`, `matrix/square-matrix`.

    - new `solve-linear` implementations between square matrices and `up`
      `down`, row and column matrices, and between structures and scalars.

    - new `solve-linear-right` between row-matrix+square-matrix,
      down+square-matrix and scalar+structure.

  - Fixes an infinite loop with `emmy.matrix/some`.

  - Renames `square-structure->` to `two-tensor->`, and
    `square-structure-operation` to `two-tensor-operation`. These functions now
    work with rectangular 2 tensors, not just square.

  - `emmy.structure` gains `down-of-ups?`, `up-of-downs?`, `two-up?`,
    `two-down?`, `two-tensor?` and `two-tensor-info` for working with "2
    tensors", ie, structures that contain structural entries of matching
    orientation and size.

  - New `g/acot` generic method installed for Operator instances.

- #471:

  - installs the complex GCD implementation into the generic system and modifies
    it to work with real/complex pairs.

  - tweaks the default gcd implementation so that two identical values `x`, even
    if they are floating point, will return `x` from `(gcd x x)`. (default gcd
    in `emmy.euclid` can handle cases now where the terms are equal and of
    opposite sign.)

  - adds `exact-divide` handling of non-integral numbers when the inputs are
    either equal or of opposite sign.

- #398 adds a `emmy.generic/gcd` implementation for complex numbers,
  closing the long-standing #58. Thanks to @adamhaber for this!

- #461 adds `emmy.quaternion`, with a full arithmetic implementation and
  the beginnings of a rotation API. Quaternions are implemented like vectors of
  length 4, and implement all appropriate Clojure protocols. All arithmetic is
  compatible with all scalars and complex numbers.

  - Accessors: `get-r`, `real-part`, `get-i`, `get-j`, `get-k`, `complex-1`,
    `complex-2`, `->complex-pair`, `->vector`, `three-vector`

  - Predicates: `real?`, `zero?`, `one?`, `pure?`, `unit?`

  - Constants: `ZERO`, `ONE`, `I`, `J`, `K`

  - Reader literal `#sicm/quaternion` takes a 4-vector or a single entry.

  - Constructors: `make`, `from-complex`, `spherical`, `semipolar`,
    `multipolar`, `cylindrospherical`, `cylindrical`

  - More: `arity`, `evaluate`, `partial-derivative`, `magnitude-sq`,
    `normalize`, `commutator`

  - Transcendental functions: `exp` `log`, `cos`, `sin`, `tan` `sinh`, `cosh`,
    `tanh`. Many more transcendentals will work, thanks to their default
    implementations.

  - Arithmetic and generics: `+`, `-`, `*`, `/`, `dot-product`, `cross-product`,
    `conjugate`, `magnitude`, `expt`, `sqrt`, `simplify`, `infinite?`,
    `solve-linear-right`, `solve-linear`

  - Rotation-related: `from-angle-normal-axis`, `from-angle-axis`, `pitch`,
    `roll`, `yaw`, `->angle-axis`

- #468:

  - adds `emmy.polynomial/touchard`, implementing a constructor for the
    type of polynomial known as a "complete Bell polynomial" or ["Touchard
    polynomial"](https://en.wikipedia.org/wiki/Touchard_polynomials).

  - adds `emmy.special.factorial/bell` for computing the nth [Bell
    number](https://en.wikipedia.org/wiki/Bell_number)

  - `emmy.series/bell-series` returns an infinite sequence of bell numbers.

- #276 adds an `integration-opts` to
  `emmy.mechanics.lagrange/Lagrangian-action`. All options are passed on to
  `definite-integral`. By default, `parametric-path-action` passes `:compile?
  false`, since we do NOT want to compile the polynomial.

- #463 adds a new 1-arity to `emmy.matrix/characteristic-polynomial` that
  returns an actual polynomial instance. Creating this polynomial once and
  calling it many times is much more efficient. Closes #209.

  - `expt` called with a negative base and non-integral power now properly
    returns a complex number instead of `##NaN`.

  - symbolic `=` now behaves correctly and accumulates an expression of nested
    `and`s, vs before. The previous behavior would convert `(= 'a 'b 'c')` to
    `(= (= 'a 'b) 'c')`, which is NOT correct. (if `(= 'a 'b)` is true, then the
    expression evaluates to `false`, since `(= true 'c')` is false.)

  - adds `emmy.series/function->`, for generating a Maclaurin series from a
    function.

- #449:

  - All missing trigonometric functions have been filled in `emmy.generic`
    and aliased in `emmy.env`:

    - Inverse cotangent: `acot`
    - inverse secant: `asec`
    - inverse cosecant: `acsc`
    - hyperbolic (inverse hyperbolic) cotangent: `coth` and `acoth`
    - hyperbolic (and inverse hyperbolic) secant: `sech` and `asech`
    - hyperbolic (and inverse hyperbolic) cosecant: `csch` and `acsch`

    All of these have default implementations and derivatives defined. They'll
    work out of the box for all types with `atan` defined (and potentially
    `exp`, `sqrt` and `log`.)

    Thanks to John D Cook's ['Bootstrapping a minimal math
    library'](https://www.johndcook.com/blog/2021/01/05/bootstrapping-math-library/)
    for his inspiration on the defaults and implementation order of these new
    functions.

  - `expt` gains a new default implementation for non-native-integral powers,
    making `expt` work for any type with `exp`, `log` and `mul` defined.

  - `sqrt` gains a default implementation for all types implementing `exp`,
    `mul` and `log`.

  - All trig functions now have derivatives and docstrings.

  - New `sinc`, `tanc`, `sinhc`, `tanhc` functions live in `emmy.generic`
    and are aliased into `emmy.env`. These are generically defined as `(/
    (sin x) x)`, `(/ (tan x) x)` (and similar with `sinh` and `tanh`), with
    correct definitions for 0 and infinite-valued inputs.

    These functions all support derivatives as well.

  - New default `acot` implementation in `emmy.series`.

- #450:

  - Adds `emmy.series/harmonic-series`, the infinite series of [harmonic
    numbers](https://en.wikipedia.org/wiki/Harmonic_number)

  - moves `emmy.numerical.elliptic` to the `emmy.special` package, as
    `emmy.special.elliptic`.

  - New `emmy.special.factorial` namespace!
    `emmy.util.permute/factorial` moves here, and the forgotten duplicate
    `emmy.generic/factorial` is now gone.

    - New functions: `falling-factorial`, `rising-factorial`,
      `double-factorial`, `multi-factorial`, `subfactorial`,
      `binomial-coefficient`, `stirling-first-kind`, `stirling-second-kind`.

  - New `emmy.util.permute/multichoose` function, implementing the
    definition [described here](https://mathworld.wolfram.com/Multichoose.html).

  - better `number-of-combinations` impl in `emmy.util.permute`, using
    `emmy.special.factorial/falling-factorial`

  - sci bindings for`emmy.special.factorial`, `emmy.util.permute`.

- #458:

  - Default implementation of `g/negative?` returning `false` for literal
    numbers and symbols. This was required to get `g/abs` working for
    polynomials and rational functions with symbolic coefficients.

  - Polynomials and rational functions now correctly unwrap `Literal`
    coefficients in `->expression`. Without this, the resulting expressions
    would not correctly respond to `simplify` calls.

  - Slight efficiency improvement in
    `emmy.polynomial.gcd/->content+primitive`.

  - `emmy.rational-function/from-points` now correctly builds its function.
    Before, it was unhygienic; if `'x` appeared in the coefficients the results
    would be incorrect.

- #456:

  - `emmy.mechanics.lagrange/{Γ,Γ-bar}` are removed in favor of the
    existing `Gamma` and `Gamma-bar` functions. The `emmy.env` aliases are
    gone as well.

  - `emmy.mechanics.lagrange/Lagrange-interpolation-function` now returns
    an actual polynomial instance. Because polynomials support `IFn` and respond
    to the derivative operator `D`, this makes the `find-path` example on pages
    22/23 of SICM run about 5x faster.

  - Richardson extrapolation is now implemented as a functional fold. The
    exposition in `emmy.polynomial.richardson` discusses this; the
    namespaces gains `richardson-fold`, `richardson-sum` and `richardson-scan`.

- #455 makes `emmy.util.aggregate/scan` and
  `emmy.algebra.fold/fold->scan-fn` slightly more efficient by dropping the
  first element of the returned sequence before mapping the `present` function.

- #453:

  - Adds `emmy.polynomial/from-points` and
    `emmy.rational-function/from-points` for generating `Polynomial` and
    `RationalFunction` instances from sequences of points.

- #451:

  - new `emmy.algebra.fold` namespace:

    - New folds: `kahan-babushka-neumaier` (aliased as `kbn`),
      `kahan-babushka-klein` and and `kbk-n` macro for generating higher-order
      `kahan-babushka-klein` variants. `generic-sum-fold` folds using
      `emmy.generic/+`.

    - `emmy.util.aggregate/kahan-fold` now lives here, named `kahan`.

    - `fold->sum-fn` and `fold->scan-fn` generate functions like
      `emmy.util.aggregate.{sum,scan}` specialized to the supplied fold.
      See the docstrings for the multiple arities supported

    - fold primitives: `count`, `constant`, `min`, `max`.

    - fold combinator `join` allows compound folds to be built out of primitive
      folds.

  - Upgrades to `emmy.util.aggregate`:

    - `scanning-sum` renamed to `scan`

    - `halt-at` deleted in favor of the built-in `halt-when` that I didn't know
      about!

    - `scan` and `sum` now both use a dynamic binding, `*fold*`, to set the fold
      they use for aggregation. By default, this is set to the new
      `kahan-babushka-neumaier-fold`.

    - The three-arity version of `sum` now uses transducers, saving a pass over
      the input range.

    - `pairwise-sum` implements pairwise summation, an error-limiting technique
      for summing vectors. Use the dynamic binding `*cutoff*` to set where
      `pairwise-sum` bails out to normal summation.

  - Upgrades to `emmy.rational-function.polynomial`:

    - The folds in this namespace now follow the fold contract laid out in
      `emmy.algebra.fold`, implementing all three arities correctly.

    - I realized that the fold implementation here should /not/ return a full
      row every time it processes a previous row; a far better `present`
      implementation would return the best estimate so far. Then you could build
      a `scan` from that fold to see the estimates evolve lazily as new points
      are added. This has better performance, it turns out, than the original
      method!

    - added a bunch to the exposition to make the advantages clear.

  - Upgrades to `emmy.rational-function.interpolate`:

    - `fold` interface upgraded, similar to the polynomial interpolation notes.

    - New `bulirsch-stoer-fold`, `bulirsch-stoer-sum` and `bulirsch-stoer-scan`
      functions. These are similar to the `modified-**` versions but use the
      `bulirsch-stoer` algorithm, instead of `modified-bulirsch-stoer`.

    - `modified-bulirsch-stoer-fold-fn` renamed to
      `modified-bulirsch-stoer-fold`, to match the naming scheme of other
      "folds" in the library.

    - `modified-bulirsch-stoer-fold` renamed to `modified-bulirsch-stoer-sum`,
      to match the convention that "reducing a sequence with a fold" is called
      "summing" the sequence. I can see this changing down the road...

    See `context-opts` for instructions on how to enable
    `emmy.algebra.fold/kbk-n` in the SCI environment (you'll need to turn
    on access to `js/Math` or `java.lang.Math`).

  - Fixed a type inference warning in ClojureScript in `emmy.complex`.

  - Added support for `emmy.util.def` and its `fork` macro to the default
    SCI environment provided by Emmy. Helpful for macro-writing!

  - `emmy.numerical.quadrature.adaptive` now uses the dynamically bound
    `emmy.util.aggregate/*fold*` to accumulate its numerical integral
    pieces, instead of a hardcoded `kahan-sum`.

  - `emmy.numerical.quadrature.bulirsch-stoer` now uses the functional scan
    versions of polynomial and rational function interpolation, as these are a
    bit faster than the originals!

  - `emmy.util.stream/scan` deleted in favor of
    `emmy.util.aggregate/scan` with a dynamic binding for `*fold*` to
    customize.

- #448:

  - new `g/infinite?` generic with implementations for all numeric types,
    complex numbers, `differential` instances. Defaults to `false` for all other
    types. (Also aliased into `emmy.env/infinite?`).

  - The infix, TeX and JavaScript renderers (`->infix`, `->TeX` and
    `->JavaScript`) all properly render `##Inf` and `##-Inf`. Infix uses the
    Unicode symbol ∞, while `->TeX` uses the LaTeX command `\infty`.
    Javascript's `Infinity` stands in for `##Inf` in generated JS code.

  - Complex numbers now respond `true` to `g/negative?` if their imaginary
    component is zero and real component is negative, false otherwise.

  - `g/+`, `g/-`, `g//` now no longer short circuit if there is a NUMERIC zero
    on either side. This was causing bugs in cases where we allow, say, a scalar
    to be added to a quaternion, and auto-convert the scalar right there (so it
    adds only to the real part). OR in cases, like in the matrix PR, where we
    convert the scalar in addition to `<scalar>*I*`.

    - This caused some problems with `emmy.matrix` tests that were not well
      typed.

  - The default `expt` implementation is now available as a function to call
    directly (`emmy.generic/default-expt`) without going through the
    dispatch system.

- #447 contains a grab-bag of fixes and additions, many related to complex
  numbers:

  - Use `Math/E` instead of `(Math/exp 1)` for euler's constant in
    `emmy.env`.

  - Fix bug in `emmy.calculus.indexed`, in a case where either input was
    missing an `up` or `down`index type.

  - symbolic `dot-product` and `inner-product`

  - `inner-product` now defaults to `dot-product` for scalar instances. This is
    correct for all numeric types we currently have, since `complex` is the only
    tough case, and it has real coefficients.

  - simplify now does NOT freeze expressions before simplifying. This allows
    complex numbers to survive simplification, since they freeze to `(complex
    <re> <im>)`.

    - big rewrite in `emmy.simplify.rules`, to convert all of the frozen
      matchers like `(complex 1 2)` into matchers that actually bind to a
      complex number.

    - more rules in `complex-trig`, it can now handle bigger products inside of
      `sin` and `cos` multiplied by `I`.

  - Various improvements to `emmy.complex`:

    - complex implementations for `dot-product` between complex and real types

    - Fixed reflection warnings with `ComplexFormat`in complex parsing code

    - complex `zero?` now returns true for inputs like `(complex -0.0 -0.0)`,
      where a negative zero lives in the real or imaginary slots

    - new `emmy.complex/-I` binding, set to `(g/negate c/I)`

    - `g/expt` for complex numbers optimizes the inputs equal to `I` by
      returning exact 1, -1, `I` or `-I` depending on the input. This applies to
      `g/square` and `g/cube` as well.

- #445 fixes a bug where structures and other seq-able types were interpreted as
  sequence matchers.

  In `pattern.match` and all rules, things that respond true to `sequential?`
  but not `seq?` or `vector?` (many of the emmy types, like structures and
  the upcoming Quaternion type) were being converted to `seq` and treated as
  sequence matchers vs literal matchers. This no longer happens, and structures
  etc are treated as literal matchers.

- #443:

  - Implements `IKVReduce` and `Reversible` for structures. This enables `rseq`
    and `reduce-kv` to work with structures.

  - Removes a `reduced` shortcut condition in `emmy.generic/*` that was
    causing multiplications of the form `(* 0 0 (up 0 0))` to shortcut and
    return `0` instead of the appropriate structural form.

  - the `atan` implementation for symbolic numbers is now careful not to return
    a floating point number in the case of a 0 argument in the second position.
    Additionally, it now returns symbolic `pi` or 0 in the case of `0` in the y
    argument for positive and negative `x` argument, respectively, and symbolic
    `(/ pi 2)` or `(- (/ pi 2))` for a 0 `x` argument and respective positive or
    negative `y` argument.

- #442 fixes #441 by upgrading the implementations of
  `emmy.util.permute/{factorial,number-of-combinations}` to be able to
  handle large inputs. Thanks to @swapneils for the report.

- #440:

  - Modifies `(g/exp 0)` to return an exact 1, vs the previous `1.0`.

  - Fixes a bug in `emmy.rules/exp-contract` leftover from the port from
    Scheme. Thanks to @adamhaber for pointing this out!

- #438:

  - converts `doall` calls to `run!`, `dorun`, `doseq` or `mapv` where
    applicable. In cases where we were trying to force side effects (mostly in
    the tests), this change prevents the environment from retaining the full
    sequence. This will save memory!

  - adds missing tests from `connection.scm` to
    `emmy.calculus.connection-test`, stressing pages 205 - 213 from MTW,
    Gravitation.

- #434: allow pattern matching forms to successfully bind to `nil` or `false`.

- #397: `emmy.calculus.manifold/typical-coords` now returns generated
  coordinate symbols that start with the same symbol as the coordinate system's
  prototype, like:

```clj
(typical-coords R2-polar)
;;=> (up x065308 x165309)

(typical-coords
 (with-coordinate-prototype R2-polar (up 'r 'theta)))
;;=> (up r65312 theta65313)
```

## 0.20.1

- #396:

  - fixes a bug in the SCI version of `define-coordinates` which didn't allow
    any rebinding of manifolds.

  - Removes the `bindings` key from `emmy.env.sci/context-opts`.
    https://github.com/babashka/sci/issues/637 is a bug with variable rebinding
    that occurs when `:bindings` is in play. Instead of relying on this key,
    evaluate `(require '[emmy.env :refer :all])` against your SCI
    environment to get all bindings.

  - bumps the default version of SCI to 0.2.7.

## 0.20.0

- #348:

  - Adds a new single arity version of
    `emmy.util.permute/permutation-parity`, which returns the parity of a
    permutation relative to its sorted version.

  - `emmy.complex/complex` can now take a single string argument in both
    Clojure and ClojureScript.

  - Expands the complex number literal parser to take these forms, in addition
    to the previously-supported string argument:

```clj
#sicm/complex [1.2 3.6]    ;; 1.2+3.6i
#sicm/complex [1.2]        ;; 1.2
#sicm/complex 1.4          ;; 1.4
#sicm/complex "1.2 + 3.6i" ;; 1.2+3.6i
```

- #394 fixes a bug with derivatives of functions that returned a map... but
  where the map was actually meant to represent some other type, by holding a
  `:type` key. We do this for manifold families and manifold points, as two
  examples. Now, instead of recursing into the values, the system will correctly
  throw an error. (You can fix this by using a `defrecord` instead of a map and
  implementing `emmy.differential/IPerturbed`.)

- #393:

  - Forms like `(let-coordinates [(up x y) R2-rect] ...)` will now work even if
    `up` is not present in the environment. Previously this syntax was valid,
    but only if `up` had been imported.

  - Adds the `emmy.calculus.coordinate/define-coordinates` macro, also
    aliased into `emmy.env`. This macro allows you to write forms like

```clj
(define-coordinates (up t x y z) spacetime-rect)
(define-coordinates [r theta] R2-polar)
```

  and install set of bindings for a manifold's coordinate functions, basis
  vector fields and basis form fields into a namespace. This is used liberally
  in Functional Differential Geometry. (You might still prefer `let-coordinates`
  for temporary binding installation.)

  - Converts many of the `emmy.fdg` test namespaces to use the new
    `define-coordinates` macro, making for a presentation closer to the book's.

  - Fixes a ClojureScript warning in `emmy.util` warning due to
    redefinition of `clojure.core/uuid`

- #386:

  - Aliases `emmy.mechanics.hamilton/phase-space-derivative` into
    `emmy.env`, and adds `emmy.sr.frames/base-frame-maker`. The latter
    function makes it easier to write reference frames like `the-ether`, as with
    the `home` variable in chapter 11 of FDG.

  - Adds all code listings from chapters 10 and 11 of FDG as
    `emmy.fdg.{ch9,ch10}-test`.

- #384:

  - Adds `emmy.fdg.ch9-test`, with tests for all forms from FDG's 9th
    chapter.

  - Tests from `emmy.fdg.einstein-test` now all work, and quite fast. The
    functions in this namespace comprise some of the exercises from FDG chapter
    9. (Einstein's Field Equations hung until this PR... getting these working
    is a huge achievement for me, and, in some sense, the final milestone of the
    Big Port from scmutils.)

  - Adds `emmy.function/memoize`, a metadata-and-function-arity preserving
    version of `clojure.core/memoize`.

  - in `emmy.calculus.indexed`, `with-argument-types` and
    `with-index-types` now both correctly set the arity of the returned
    function, in addition to the argument types or indices.
    `emmy.function/arity` will now work correctly with indexed or typed
    functions.

  - Adds new `manifold?` and `manifold-family?` functions in `emmy.env` and
    `emmy.calculus.manifold`. These are enabled by new `:type
    :emmy.calculus.manifold/{manifold,manifold-family}` keys in the
    appropriate structures in the manifold namespace. Manifolds and manifold
    families will now respond with these keywords to `emmy.value/kind`.

  - The `emmy.calculus.manifold/ICoordinateSystem` now has a `uuid`
    function, for internal comparison of coordinate systems. This is here so
    that points can cache coordinate system representations by UUID. Before this
    change, changing the coordinate prototype, or attaching metadata to a
    coordinate system would break its cache entry in manifold points. (This was
    the killer for the Einstein Field Equations!)

  - `emmy.calculus.manifold/{coordinate-prototype,with-coordinate-prototype}`
     now store and retrieve the coordinate prototype from metadata. This plus
     the previous change allows manifold points to correctly cache their
     coordinate representations.

  - `emmy.calculus.manifold/manifold` acts as identity on manifolds now.
    Previously it only worked on coordinate systems.

- #382:

  - Makes the `name` argument to `emmy.operator/make-operator` optional.
    `name` now defaults to `'???`.

  - adds tests for all code forms in Chapter 8 of FDG.

- #376 adds more type hints to the `ratio.cljc` namespace. This fully solves the
  advanced compilation issues we were seeing.

- #374: Demos, thanks to @sigmaxipi!

- #379 fixes typos in a couple of the equations in `richardson.cljc`, closing
  #377. Thanks to @leifp for the report.

- Features, tests and bugfixes from #381:

  - `emmy.calculus.coordinate/generate` moves to
    `emmy.calculus.manifold/c:generate`; this supports a bugfix where
    1-dimensional manifolds like `R1-rect`, aka `the-real-line`, return a
    coordinate prototype of a single element like `t` instead of a structure
    with a single entry, like `(up t)`. Thanks to @phasetr for the bug report
    that led to this fix, and @gjs for finding and fixing the bug.

  - `same.ish/Approximate` implemented for `emmy.structure/Structure`,
    allowing `ish?` comparison of `up` and `down` structures with approximate
    entries. Require `emmy.generator` for this feature. (NOTE: because
    protocols are implemented for the LEFT argument, `(ish? <vector> (down
    ...))` will still return true if the values are approximately equal, even
    though a `<vector>` is technically an `up` and should NOT equal a `down`. Do
    an explicit conversion to `up` using `emmy.structure/vector->up` if
    this distinction is important.)

  - `same.ish/Approximate` now defers to `emmy.value/=` for equality
    between `Symbol` and other types. This lets `ish?` handle equality between
    symbols like `'x` and literal expressions that happen to wrap a single
    symbol.

  - `Cartan->Cartan-over-map` now does NOT compose `(differential map)` with its
    internal Cartan forms. This fixed a bug in a code listing in section 7.3 of
    FDG.

  - Section 7.3 of FDG implemented as tests in `emmy.fdg.ch7-test`.

  - Many new tests and explorations ported over from `covariant-derivative.scm`.
    These live in `emmy.calculus.covariant-test`.

  - timeout exceptions resulting from full GCD are now caught in tests using
    `emmy.simplify/hermetic-simplify-fixture`. Previously, setting a low
    timeout where simplification failed would catch and move on in normal work,
    but fail in tests where fixtures were applied.

## 0.19.2

Yet another incremental release, this time to bump the `Fraction.js` dependency.
The new `cljsjs` dependency has code compatible with advanced compilation.

- #372 bumps the `Fraction.js` dependency to `4.1.1`.

## 0.19.1

This is an incremental bugfix release to get ClojureScript advanced compilation
into shape.

- #371:

  - fixes a subtle bug with extern inference on `fraction.js/bigfraction.js`.
    Thanks to @sigmaxipi for this report!

  - removes overridden factory constructors like `->Polynomial`. I had
    originally done this for functions that held a metadata field, so that the
    user could leave it out and have it default to `nil`... but advanced Closure
    compilation can't understand the `ns-unmap` call, so it has to go.

  - Many unary functions on `Operator`, `Structure`, `Series`, `PowerSeries`,
    `Polynomial` and `RationalFunction` now preserve metadata. Binary functions
    between two instances of any of these still return a new object with
    metadata == `nil`.

## 0.19.0

> (If you have any questions about how to use any of the following, please ask us
> at our [Github Discussions](https://github.com/emmy/emmy/discussions)
> page!)

This release focused on improving the expressiveness and performance of the
three simplification engines in Emmy:

  - `emmy.polynomial` and `emmy.rational-function` are now quite well
    fleshed out, with full polynomial and rational function APIs and many
    generics.

  - The polynomial and rational function _simplifiers_ work by round-tripping
    expressions through these types, depending on each namespace to emit
    symbolic expressions in "canonical form". This process is now much faster!
    On one important Bianchi Identity benchmark in `emmy.fdg.bianchi-test`,
    one test that formerly took close to 30 minutes now runs in 30 seconds, and
    all see a 60-fold improvement.

  - By default, these simplifiers emit expressions with all terms multiplied
    out; the new `factor` function in `emmy.env` lets you factor
    expressions, overriding this default.

  - The rule-based simplifier is now based on a powerful pattern matching
    engine, implemented in `pattern.match` and `pattern.rule`.
    `emmy.simplify.rules` now contains every rule and possible
    customization from the original scmutils codebase.

There is a _lot_ in this release, all motivated by performance. Please read on
for the detailed notes, and enjoy version 0.19.0!

### Rule-Based Simplifier Overhaul

- #353 introduces a powerful new simplifier, ported from the `new-simplify`
  procedure in `simplify/rules.scm` of the scmutils library. There are now a
  BUNCH of new rulesets and rule simplifiers in `emmy.simplify.rules`!

  The next step with these is to massage them into separate bundles of rules
  that users can mix and match into custom simplifiers for objects like abstract
  matrices, abstract bra and ket structures, up and down, booleans (for
  representing equations and inequalities) and so on.

- #349 introduces a new pattern matching system, built out of matcher
  combinators. All of the rules in `emmy.simplify.rules` now use the new
  syntax offered by the library. Some notes:

  - `pattern.match` defines a number of "matcher combinators"; these are
    functions that take a map of bindings, a data input and a success
    continuation and either succeed by calling their continuation, or fail. Out
    of the box, the library provides `fail`, `pass`, `with-frame`,
    `update-frame`, `predicate`, `frame-predicate`, `eq`, `bind`, `match-when`,
    `match-if`, `or`, `and`, `not`, `segment` and `sequence`.

  - Additionally, any combinator that takes another combinator can ALSO take a
    pattern form like `'?x`. See `pattern.syntax` for the full, rich range of
    syntax allowed. These are all functions, so you'll have to quote your
    symbols at this stage.

  - Passing a matcher combinator to `pattern.match/matcher` to generate a
    matcher object. This is a function from some `data` input to a map of
    bindings on success, or an explicit `pattern.match/failure` object on
    failure. Test for failure with `pattern.match/failed?`.

  - A combination of a matcher and a "consequence function" is called a "rule".
    A consequence is a function that takes a binding map and either returns a
    new result or fails by returning `nil` or `false`. (Don't worry, you can
    succeed with these values too by wrapping them in `emmy.rule/succeed`.)

    Rules are the heart of the whole simplification mechanism in emmy! To
    learn about how to build these, see the documentation for `pattern*`,
    `pattern`, `consequence`, `template`, `rule*`and `rule`.

  - `pattern.rule` gives you some starter rules, and many combinators you can
    use to build more and more powerful and complex sets of rules. These are
    `pass`, `fail`, `predicate`, `return`, `branch`, `choice*`, `choice`,
    `pipe*`, `pipe`, `n-times`, `attempt`, `guard`, `iterated`, `while`,
    `until`, `fixed-point` and `trace`.

  - Rules are nice for rewriting entire expressions recursively, from the bottom
    up or top down. This is called "term rewriting". A big motivation for this
    rewrite was to make it easy to build custom term rewriters for types like
    abstract matrices or abstract up and down structures. You can use your rules
    to rewrite structures recursively with `bottom-up`, `top-down`,
    `iterated-bottom-up` and `iterated-top-down`. `ruleset*`, `ruleset`,
    `rule-simplifier` and `term-rewriting` capture some common patterns the
    library uses to go from rules => term rewriters.

  - If you want ideas about how to use the pattern matching library to rewrite
    expressions, see `emmy.simplify.rules` for many examples.

- #354 adds SCI support for all macros and functions in the new pattern matching
  namespaces, and adds these to the namespaces exposed via `emmy.env.sci`.

### Rational Function, Polynomial Simplifiers

- #341 takes on a large rewrite of the rational function and polynomial
  simplfiers. One goal of this project was to improve the performance of the
  Bianchi Identities in `emmy.fdg.bianchi-test`, and I'm happy to say that
  they are now a good bit faster than the original scmutils implementation.

  `emmy.polynomial` and `emmy.rational-function` are now solid data
  structures of their own, with many operations installed into the generic
  system. These are now valuable and useful outside of their role in the
  simplifier.

  This was a large project, and many small improvements and bugfixes snuck in.
  Here is the full list:

  - `v/kind` now works for `sorted-map` instances.

  - GCD in ClojureScript is now fast and efficient between all combinations of
    `js/BigInt` and `js/Number`, and in Clojure between all combinations of
    `clojure.lang.BigInt`, `BigInteger`, `Long` and `Integer`.

  - on the JVM, GCD now works properly with rational numbers. Previously
    anything non-integral would return `1`; now `(gcd 1/2 1/3)` properly returns
    `1/6`.

  - `g/exact-divide` now succeeds for all non-exact `::v/scalar` types (symbols,
    floats, etc) either if the denominator is zero, or if the two arguments are
    equal. Else, it throws, just like before.

  - A multi-arity call to `emmy.generic/*` now stops if it encounters a
    0, rather than attempting to multiply all remaining items by 0.

  - The default function for `emmy.generic/lcm` protects against overflow
    by dividing only a single one of its arguments `a` and `b` by `(gcd a b)`.

  - `(g/lcm 0 0)` now properly returns 0.

  - New `emmy.util.aggregate/{monoid,group}` functions let you build
    multi-arity aggregations out of binary combination functions, with an option
    to bail early at "annihilator" values, like 0 for multiplication.

  - New multi-arity `lcm` and `gcd` implementations for symbolic expressions
    appropriately handle `0` and `1` on either side, as well as the case where
    both arguments are equal.

  - In the `emmy.numsymb` namespace, thanks to `monoid` and `group`, the
    `'*`, `'/`, `'-`, `'+`, `'or`, `'and`, `'gcd`, `'lcm` and `'=` operations
    now have efficient multi-arity implementations that stop computing when they
    receive an annihilator, like `0` for multiplication or `true` for `or`.
    Access these via `(emmy.numsymb/symbolic-operator <symbol>)`.

  - `emmy.series/PowerSeries` gains `arg-scale` and `arg-shift` functions;
    these are identical to `emmy.function/arg-{scale,shift}`, but preserve
    the `PowerSeries` type. (#367 proposes making these functions generic.)

  - New `emmy.ratio/IRational` protocol, with `numerator` and `denominator`
    functions implemented for ratios and for the `RationalFunction` data type.
    These two are now exposed in `emmy.env`.

  - `emmy.simplify.rules/*divide-numbers-through-simplify?*` is now `true`
    by default; numbers in the denominator will now automatically pull up into
    the numerator. All tests now reflect this setting.

  - Any analyzer generated from `emmy.expression.analyze` can now act on
    both bare, unwrapped expressions (raw lists etc) and on
    `emmy.expression.Literal` instances. This means that you can now call
    `emmy.simplify/{*rf-simplify*,*poly-simplify*}` as functions and
    canonicalize some form with either simplifier without triggering a full
    simplification. A small win, but ice.

  - `emmy.polynomial.factor` got a major rewrite, and now exposes a few
    functions like `poly->factored-expression`, `factor-expression` and
    `factor`.

      - `factor` is _tremendously useful_! Call `factor` (it's aliased into
        `emmy.env`) on any expression to factor out all possible terms.
        This makes it much easier to see where there is some cancellation
        lurking, in, say, some expression you know should equal zero (a
        residual).

  - bugfix: `emmy.expression.Literal` instances now compare their contained
    expression via `emmy.value/=`.

  - `emmy.rules/constant-elimination` can now eliminate constants from
    expressions with any arity, not just binary forms.


  Now, the three big namespaces... `emmy.polynomial`,
  `emmy.rational-function` and `emmy.polynomial.gcd` all got a big
  overhaul.

  - `emmy.polynomial` notes:

    - `Polynomial` uses a new sparse representation for its "power product"
      term; this, plus an arithmetic rewrite, makes the whole system much faster
      for larger numbers of variables (for all #s, really).

    - `Polynomial` instances implement many more Clojure(script) protocols. They
      can hold metadata; they can be evaluated as functions of their
      indeterminates, and `seq` now returns a sequence of terms.

    - `Polynomial` extends `emmy.function/IArity` and
      `differential/IPerturbed`, so you can use `emmy.function/arity`, and
      take derivatives of functions that return polynomials.

    - In their arithmetic, `Polynomial` instances will drop down to bare
      coefficients whenever some multiplication or addition removes all
      indeterminates. All binary arithmetic exposed in the namespace can handle
      non-`Polynomial` instances on either or both sides, so this is fine.
      Coefficients are treated as constant polynomials.

    - The namespace holds many new functions. Some choice ones are:

      - constructors: `make`, `constant`, `linear`, `c*xn`, `identity`, and
        `new-variables`

      - accessor functions: `arity`, `degree`, `coefficients`, `leading-term`,
        `leading-coefficient`, `leading-exponents`, `leading-base-coefficient`,
        `trailing-coefficient`, `lowest-degree`

      - predicates: `monomial?`, `monic?`, `univariate?`, `multivariate?`,
        `negative?`

      - functions to generate new polynomials: `map-coefficients`,
        `map-exponents`, `scale`, `scale-l`, `normalize`, `reciprocal`,
        `drop-leading-term`, `contract` and `extend` alongside `contractible?`,
        `lower-arity`, `raise-arity`, `with-lower-arity`, `arg-scale`,
        `arg-shift`

      - arithmetic: `negate`, `abs`, `add`, `sub`, `mul`, `square`, `cube`,
        `expt`, `divide` along with `divisible?`, `evenly-divide`,
        `pseudo-remainder`, and _lots_ of functions installed into the generic
        arithmetic system.

      - different ways to evaluate polynomials: `evaluate`, `horner-with-error`

      - calculus! `partial-derivative` and `partial-derivatives` are alive and
        well, and work with the `D` operator.

      - Functions to get in and out of polynomials from other types:
        `univariate->dense`, `->power-series`, `expression->`, `->expression`

  - `emmy.polynomial.gcd` also got a rewrite; it's fairly clear to read
    now, and prepared for the eventual addition of the sparse multivariate GCD
    routine that scmutils uses. There are some efficiency gains here too that
    let us turn a number of tests back on, or demote them from `:long` markers.

  - `emmy.rational-function` notes:

    - `RationalFunction` instances implement many more Clojure(script)
      protocols. They can hold metadata; they can be evaluated as functions of
      their indeterminates, and `seq` now returns a pair of `numerator`,
      `denominator`.

    - `RationalFunction` extends `emmy.function/IArity` and
      `emmy.ratio/IRational`, so our generic `arity`, `numerator` and
      `denominator` work on these instances.

    - Here are some new functions from the `RationalFunction` namespace:

      - constructor: `make`, drops to polynomial or coefficient where needed
        just like `Polynomial` functions

      - functions to generate new rational functions: `arg-scale`, `arg-shift`

      - predicates: `negative?`

      - arithmetic: `negate`, `abs`, `add`, `sub`, `mul`, `square`, `cube`,
        `expt`, `invert`, `div`, `gcd`, and many functions installed into the
        generic arithmetic system.

      - evaluation via `evaluate`

      - calculus! `partial-derivative` and `partial-derivatives` are alive and
        well, and work with the `D` operator.

      - Functions to get in and out of rational functions from symbolic
        expressions: `expression->`, `->expression`.

### New Functions, Performance Improvements

- #358:

  - Adds a more efficient `literal-derivative` implementation to
    `emmy.abstract.function`, making the Bianchi identity benchmarks run
    40% faster.

  - In ClojureScript, `Range` instances now implement `emmy.value.Value`
    and `emmy.differential.IPerturbed`, allowing them to be returned from
    derivative-taking functions

  - Major, unexpected performance improvement - it turns out
    `emmy.value/number?` was quite slow in Clojure (less so in
    ClojureScript). Changing this function from an `isa?` check to a series of
    explicit `instance?` checks cut the build time in half. This makes the
    numeric tower less extensible... but it wasn't terribly extensible to start
    with, and needs some attention to make it so. A big win!

  - The Bianchi identity benchmarks have all been updated to reflect the big
    performance improvements achieved here, thanks to the wonderful
    [Tufte](https://github.com/ptaoussanis/tufte) profiling library from
    @ptaoussanis. The remaining very slow piece in the simplifier is the
    implementation of `g/add` for polynomial instances. #341 will improve this
    situation.

- #360 introduces a number of performance improvements to the
  `emmy.differential.Differential` implementation, primarily in `terms:+`
  and `terms:*`. thanks again to @ptaoussanis and the
  [Tufte](https://github.com/ptaoussanis/tufte) profiling library for helping me
  track these down.

- #357:

  - Adds the ability to do incremental simplification, every time an operation
    is performed involving a symbolic expression. Bind
    `emmy.numsymb/*incremental-simplifier*` to a function from raw
    expression -> raw expression, like `emmy.simplify/simplify-expression`
    or any of the rules in `emmy.simplify.rules` to enable this behavior.

  - Expands the `emmy.expression.analyze` API with the functions
    `default-simplifier`, `expression-simplifier`, `initializer`,
    `expression-analyzer` and `auxiliary-variable-fetcher`. See the [API
    documentation](https://cljdoc.org/d/emmy/emmy/CURRENT/api/emmy.expression.analyze)
    for detailed notes on how to do interactive expression analysis and
    simplification with these new tools.

  - by default, each simplification pass uses both rational function _and_
    polynomial canonicalization. This brings the simplifier into line with the
    scmutils simplifier.

- #353:

  - Adds a new `emmy.util.logic` namespace with an `assume!` function that
    allows rules to log assumptions when some simplification like `(sqrt (square
    x))` might have to choose one of multiple possible simplifications
    (`(non-negative? x)`, in this example).

    This function simply logs the assumption for now, instead of performing any
    checks. now. Turn off assumption logging with the dynamic variable
    `*log-assumptions?*` in that namespace.

  - new `emmy.value/almost-integral?` returns true if its argument is VERY
    close to an integral value, false otherwise.

- Efficient `symmetric-difference` implementation in `emmy.util.vector-set`
  (#346)

### Bug fixes, file moves, misc

- #369:

  - Removes JVM dependencies on Guava and nrepl.

  - Removes `emmy.env/emmy-repl-init`; this is only used by `lein
    repl`, and we now accomplish the same task with the `:repl-options` entry in
    `project.clj`.

  - Makes `emmy.polynomial.{factor,gcd}` available to SCI via the
    `emmy.env.sci` namespace

  - moves a few namespaces to more valid locations, now that the rational
    function and polynomial namespaces are tidied:

    - `emmy.numerical.interpolate.polynomial` ->
      `emmy.polynomial.interpolate`

    - `emmy.numerical.interpolate.richardson` ->
      `emmy.polynomial.richardson`

    - `emmy.numerical.interpolate.rational` ->
      `emmy.rational-function.interpolate`

- #358:

  - Converts the ClojureScript test build and REPL command from `lein-cljsbuild`
    to `shadow-cljs`. This enables more formerly-slow tests for ClojureScript;
    these are now fast enough to run, thanks to the performance improvements
    described below.

  - Upgrades our [Timbre](https://github.com/ptaoussanis/timbre) logging
    dependency to version 5.1.2, and [SCI](https://github.com/borkdude/sci) to
    0.2.5

- #353:

  - `expression->stream`, `expression->string`, `print-expression`, `pe` move
    from `emmy.simplify` to `emmy.expression`, and are now aliased in
    `emmy.env`.

  - `pattern.rule/guard` now fails if its rule argument fails; previously it
    wrapped the result in `attempt`, and would return its original input on
    failure.

  - fixed a heisenbug in `emmy.expression.analyze/make-analyzer` where, in
    ClojureScript, using expressions containing a `js/BigInt` as a hashmap key
    caused certain simplifications to fail. (This is vague, but the bug was
    _really_ subtle.) The fix was to make sure we freeze keys in the symbol
    cache. This is now noted in the function body.

## 0.18.0

> (If you have any questions about how to use any of the following, please ask us
> at our [Github Discussions](https://github.com/emmy/emmy/discussions)
> page!)

This release focused on porting over all of the material required to run every
piece of code from Sussman and Wisdom's ["Functional Differential
Geometry"](http://xahlee.info/math/i/functional_geometry_2013_sussman_14322.pdf).
The namespaces are lightly documented; the situation is better than the original
library, but will only get better as I work through the material and add
commentary.

There is a huge amount of functionality and material here! We can run many
examples from general and special relativity, and the tests are full of
exercises from the classic ["Gravitation" book by Misner, Thorne and Wheeler
(MTW)](https://www.amazon.com/Gravitation-Charles-W-Misner/dp/0691177791).

Notable changes from the rest of the library:

- `Operator` instances are slightly more efficient with their addition and
  multiplication, handling `zero?` and `one?` cases appropriately

- `Structure`s can now hold metadata

- We've extended the Emmy generics to Clojure's Map and Set data
  structures. These can now combine with `+`. Maps are treated as sparse
  infinite-dimensional vector spaces, and can multiply with symbolic or numeric
  scalars.

- `ModInt` instances are now correctly equal to numbers (when those numbers mod
  down to the `ModInt` instance's residue).

### What's next?

The next major change will be an overhaul of the simplifier to make it work fast
enough to solve Einstein's field equations in a reasonable amount of time, maybe
even in the browser. Polynomial GCD is slow, but
[#341](https://github.com/emmy/emmy/pull/341) will make it fast.

On to the detailed notes!

### Functional Differential Geometry

- From #339:

  - The new `emmy.calculus.covariant/Lie-D` can compute the Lie derivative
    for coordinates.

  - `emmy.calculus.frame` lets us create relativistic reference frames for
    investigating special relativity problems. This namespace aliases the
    following functions into `emmy.env`: 'frame?', `make-event`, `event?`,
    `claim`, `coords->event`, `event->coords`, `ancestor-frame`, `frame-name`,
    `frame-owner` and `frame-maker`.

  - `emmy.calculus.hodge-star` implements the Hodge star operator from
    chapter 10 of Functional Differential Geometry, plus Gram Schmidt
    orthonormalization. This namespace aliases the following functions into
    `emmy.env`: `Gram-Schmidt`, `orthonormalize` and `Hodge-star`.

  - `emmy.calculus.indexed` ports over the scmutils work on indexed objects
    and typed functions. This namespace aliases the following functions into
    `emmy.env`: `argument-types`, `with-argument-types`, `index-types`,
    `with-index-types`, `typed->indexed`, `indexed->typed`, `typed->structure`,
    `structure->typed`, `i:outer-product` and `i:contract`.

  - `emmy.calculus.manifold` gains `coordinate-system?`, which
    (predictably) returns true if its argument is a coordinate system, false
    otherwise. `chart` and `point` also take relativistic reference frames in
    addition to coordinate systems; the returned function converts to and from
    coordinates and events, rather than coordinates and manifold points.

  - `Div`, `Grad`, `Curl` and `Lap` move from `emmy.calculus.derivative` to
    `emmy.calculus.vector-calculus`. This namespace also contains versions
    of these operators from Functional Differential Geometry. This namespace
    aliases the following functions into `emmy.env`: `divergence`, `curl`,
    `gradient` and `Laplacian` (along with the others mentioned).

  - lots of new namespaces available in `emmy.env.sci`, soon to be deployed
    to Nextjournal: `emmy.calculus.{hodge-star, indexed, vector-calculus}`,
    and `emmy.sr.{boost,frames}`.

  - `emmy.sr.boost` describes boosts from special relativity, covered in
    chapter 11 of Functional Differential Geometry. This namespace aliases the
    following functions into `emmy.env`: `make-four-tuple`,
    `four-tuple->ct`, `four-tuple->space`, `proper-time-interval`,
    `proper-space-interval`, `general-boost`, `general-boost2` and
    `extended-rotation`.

  - `emmy.sr.frames` implements relativistic reference frames from special
    relativity, covered in chapter 11 of Functional Differential Geometry. This
    namespace aliases the following functions into `emmy.env`:
    `make-SR-coordinates`, `SR-coordinates?`, `SR-name`, `make-SR-frame`,
    `the-ether`, `boost-direction`, `v:c`, `coordinate-origin`, `add-v:cs` and
    `add-velocities`.

- From #338:

  - `emmy.fdg.bianchi-test` verifies the Bianchi identities; this was a
    challenge posed by GJS, and getting it working exposed a few bugs and
    triggered the rest of the work in this PR. Thank you, GJS!

  - `covariant-derivative` now properly handles the case of functions with
    argument types attached.

  - added `covariant-differential` to `emmy.calculus.covariant`.

  - aliased all functions from various namespaces in `emmy.calculus` into
    `emmy.env`.

  - adds `emmy.calculus.metric`, with the following functions exposed in
    `emmy.env`:

      - `coordinate-system->metric-components`, `coordinate-system->metric`,
        `coordinate-system->inverse-metric`, `literal-metric`,
        `components->metric`, `metric->components`,
        `metric->inverse-components`, `metric-over-map`, `lower`,
        `vector-field->oneform-field`, `drop1`, `raise`,
        `oneform-field->vector-field`, `raise1`, `drop2`, `raise2`,
        `trace2down`, `trace2up`, `sharpen`, `S2-metric`

      - `emmy.calculus.metric/invert` is exposed as `metric:invert` to
        match the scmutils naming scheme.

  - adds `emmy.calculus.connection`, with the following functions exposed
    in `emmy.env`:

    - `make-Christoffel-1`, `metric->Christoffel-1`, `metric->Christoffel-2`,
      `literal-Christoffel-1`, `literal-Christoffel-2`, `metric->connection-1`,
      `metric->connection-2`, `literal-Cartan`, `structure-constant`

- #337:

  - adds `emmy.calculus.curvature`, with these new functions and many tests
    from the classic "Gravitation" book: `Riemann-curvature`, `Riemann`,
    `Ricci`, `torsion-vector`, `torsion` and `curvature-components`

  - form fields now have NO identity operator, since they multiply by wedge, not
    composition.

- #328 adds many utilities for "Functional Differential Geometry".

  - vector fields, in `emmy.calculus.vector-field`:

    - new functions: `basis-components->vector-field`,
      `vector-field->basis-components`

    - vector fields now implement `v/zero?` and `v/zero-like` by returning
      proper vector fields.

  - form fields, in `emmy.calculus.vector-field`:

    - new functions: `nform-field?`, `basis-components->oneform-field`,
    `oneform-field->basis-components` and `function->oneform-field` (aliased as
    `differential-of-function`)

    - `Alt`, `alt-wedge` provide alternate wedge product definitions

    - form fields now implement `v/zero?` and `v/zero-like` by returning
      proper form fields that retain their rank.

    - form fields now correctly multiply via `*` by using
      `emmy.calculus.form-field/wedge`, instead of composition.

  - maps between manifolds, in `emmy.calculus.map`:

    - new function: `pushforward-function`

    - `differential` becomes `differential-of-map`, aliased back as `differential`

  - `emmy.calculus.covariant` gains new functions: `Cartan?`,
    `Christoffel?`, `Cartan->Christoffel`, `symmetrize-Christoffel`,
    `symmetrize-Cartan`, `Cartan->Cartan-over-map`, `geodesic-equation`,
    `parallel-transport-equation`.

  - `emmy.calculus.covariant/vector-field-Lie-derivative` can now handle
    structural inputs.

### New Functions, Functionality

- From #342:

  - Added `emmy.calculus.derivative/D-as-matrix` and
    `emmy.matrix/as-matrix`, ported from scmutils.

  - converted `emmy.modint.ModInt` to a `deftype`; this allows `ModInt`
    instances to be `=` to non-`ModInt` numbers on the right, if the right side
    is equal to the residue plus any integer multiple of the modulus. `v/=`
    gives us this behavior with numbers on the LEFT too, and `ModInt` on the
    right.

    - This change means that `:i` and `:m` won't return the residue and modulus
      anymore. `emmy.modint` gains new `residue` and `modulus` functions to
      access these attributes.

  - The JVM version of emmy gains more efficient `gcd` implementations
    for `Integer` and `Long` (in addition to the existing native `BigInteger`
    `gcd`), thanks to our existing Apache Commons-Math dependency.

  - `emmy.structure/dual-zero` aliases `compatible-zero` to match the
    scmutils interface. Both are now aliased into `emmy.env`.

  - `Structure` instances can now hold metadata (#339).

- From #339:

  - In `emmy.mechanics.rotation`:

    - gains aliases for `R{xyz}` in `rotate-x`, `rotate-y` and `rotate-z`.

    - `R{x,y,z}-matrix` now alias `rotate-{x,y,z}-matrix`.

    - Added new functions `angle-axis->rotation-matrix` and the mysterious,
      undocumented `wcross->w` from scmutils

    - `rotate-{x,y,z}-tuple` are now aliased into `emmy.env`.

  - `Operator` instances now ignore the right operator in operator-operator
    addition if the left operator passes a `v/zero?` test. Contexts are still
    appropriately merged.

  - in `emmy.simplify.rules`, the `sqrt-contract` ruleset now takes a
    simplifier argument and attempts to use it to simplify expressions internal
    to a square root. As an example, if two square roots in a product simplify
    to the same expression, we can drop the wrapping square root; otherwise
    multiplication is pushed under the root as before.

    - Added a missing rule in `simplify-square-roots` that handles roots of
      exponents with odd powers.

  - `emmy.matrix` changes:

    - `generate` has a new 2-arity version; if you supply a single dimension the
      returned matrix is square.

    - `diagonal?` returns true if its argument is a diagonal matrix, false
      otherwise.

  - A new namespace, `emmy.util.permute`:

    - `factorial` moved here from `emmy.generic`. It's still aliased into
      `emmy.env`.

    - new functions: `permutations`, `combinations`, `cartesian-product`,
      `list-interchanges`, `permutation-parity`, `permutation-interchanges`,
      `permute`, `sort-and-permute`, `subpermute`, `number-of-permutations`,
      `number-of-combinations`. See the tests for usage examples.

- From #338:

  - `(* <structure> <operator>)` multiplication pushes operator multiplication
    into the structure, rather than converting a structure into an operator.

- #337:

  - If you combine `Operator` instances with non-equal `:subtype` fields, the
    returned operator now keeps the parent subtype (or throws if one is not a
    subtype of the other).

  - `Operator` instances now ignore any `identity?`-passing operator on the left
    or right side of operator-operator multiplication. Contexts are still
    appropriately merged.

  - Similarly, `Operator` addition ignores `zero?` operators on the left or
    right side, and subtraction ignores `zero?` operators on the right right.

- #328:

  - Closes #249; operators now verify compatible contexts on multiplication.

  - `Operator` instances can now provides custom `zero?`, `one?`, `identity?`,
    `zero-like`, `one-like` and `identity-like` implementations by setting a
    function of a single (operator-typed) argument to a keyword like `:zero?` in
    their context. the identity operator returns `true` for `identity?`, and
    `false` for `one?` so that it isn't stripped by the `g/*` function.

  - structures implement the 0-arity case of IFn now.

- #335 implements `g/make-rectangular`, `g/make-polar` `g/real-part` and
  `g/imag-part` for clojure's Map data structure. Maps are treated as sparse
  vectors, any missing key on either side of `make-rectangular` or
  `make-polar`is treated as a 0 (rather than an error because the keys don't
  match, as in vectors).

- #334 adds implementations of `g/add` and the `emmy.value.Value` protocol
  for clojure's Set data structure. Addition is defined as set union, and
  `(zero-like <set>)` returns the empty set.

- #334 implements `g/add`, `g/negate` and `g/sub` for Clojure's Map data
  structure. Map addition is defined as a merge using `g/add` on clashing
  values; `g/sub` is the same, but any values on the right side not on the left
  side are negated.

  Maps can also be multiplied with scalars (commutatively) or divided (scalar on
  the right side only) by scalars. This, plus the commutative group property
  declared above, mean that Clojure's maps are sparse vector spaces over
  anything that responds true to `emmy.value/scalar?`... currently anything
  in the numeric tower up to complex, along with symbolic expressions and
  `Differential` instances.

## 0.17.0

> (If you have any questions about how to use any of the following, please ask us
> at our [Github Discussions](https://github.com/emmy/emmy/discussions)
> page!)

This release starts the work of porting all of GJS and JW's "Functional
Differential Geometry" to Emmy. The Differential Geometry section below
describes the many new manifolds, coordinate systems and functions for
interacting with these that we've gained.

The main big change in 0.17.0 that `simplify` no longer changes the type of its
input; simplified expressions _remain_ expressions.

`solve-linear`, `solve-linear-left` and `solve-linear-right` round out the
stable of generics ported from scmutils.

They're not fully installed yet, but we've laid the groundwork for a new literal
boolean type. This can represent equalities and inequalities, and will be
excellent for equation solving.

Enjoy the release!

### New Functions, Functionality

- #330 adds `g/real-part` and `g/imag-part` implementations for
  `emmy.structure.Structure` and `emmy.matrix.Matrix` instances. These
  pass through to the entries in the structure or matrix. #331 adds similar
  implementations for `g/make-rectangular` and `g/make-polar`.

- #327 adds `emmy.structure/sumr`, also aliased into `emmy.env` Given
  some function `f` and any number of isomorphic `structures`, `sumr` returns
  the sum of the results of applying `f` to each associated set of entries in
  each `structure`.

- #319 adds

  - symbolic boolean implementations for `sym:=`, `sym:and`, `sym:or` and
    `sym:not` with infix, latex and JavaScript renderers.
  - `sym:derivative`, for purely symbolic derivatives

  The boolean operators will act just like `=`, `and` and `or` on booleans, and
  appropriately respond if just one side is a boolean. If both sides are
  symbolic, These return a form like `(= a b)`, `(and a b)` or `(or a b)`.

  The functions currently live in `emmy.numsymb` only; access them via
  `(numsymb/symbolic-operator <sym>)`, where `<sym>` is one of `'=`, `'and`,
  `'or`, `'not` or `'derivative`.

- #304 aliases `emmy.operator/anticommutator`, `emmy.util/bigint?` and
  into `emmy.env`

  - implements `v/=` properly for sequences, `Differential`, `Complex`,
    `Structure` and `Matrix` instances

  - in `emmy.env`, `v/=` now overrides `clojure.core/=`. `v/=` should act
    identically to `clojure.core/=` everywhere; the difference is that its
    behavior is customizable, so we can make `Differential` instances equal to
    numbers, or complex numbers with a 0 imaginary part equal to real numbers
    with the same real part.

    `v/=` may not drop recursively down into, say, Clojure maps. Please open an
    issue if you find a case like this!

  - BIG CHANGE: `Literal` and `Structure` instances now KEEP their type under
    `g/simplify`. If you want to get the expression back out of its `Literal`
    wrapper, use `emmy.expression/expression-of`, also aliased into
    `emmy.env`.

    This means that you can no longer make comparisons like this:

```clojure
;; this worked before, and was used all over the tests (probably not in much
;; user code!)
(clojure.core/= '(* 3 x)
                (simplify (+ 'x 'x 'x)))
;;=> false
```

  Instead, use `v/=` (which is now aliased into `emmy.env`):

```clojure
;; `v/=` will do the right thing by unwrapping the literal expression on the
;; right:
(v/= '(+ x y) (+ 'x 'y))
;;=> true
```

- #305 adds `g/solve-linear` and `g/solve-linear-left` implementations between
  `emmy.structure/Structure` instances.

- #207:

  - adds missing implementations of `g/floor`, `g/ceiling`, `g/integer-part` and
    `g/fractional-part` for functions, both literal and abstract.

  - adds `g/solve-linear`, `g/solve-linear-left`, `g/solve-linear-right`.
    `(g/solve-linear-right a b)` returns `x` such that `a = x*b`, while
    `g/solve-linear` (and its alias, `g/solve-linear-left`) returns `x` such
    that `a*x = b`. These functions are implemented for:

    - `emmy.series.{Series, PowerSeries}`
    - all numeric types
    - functions, operators
    - `emmy.modint.ModInt`
    - `emmy.differential.Differential`, so you can differentiate through
      this operation

- #309: `emmy.util/bigint` is aliased as `emmy.env/bigint` in
  ClojureScript only. This is available natively in Clojure.

- #308 and #310 add:

  - `emmy.ratio/{numerator,denominator,ratio?,rationalize}` and are now
    aliased into `emmy.env` in ClojureScript. These are available natively
    in Clojure. `emmy.complex/complex?` is aliased into `emmy.env` for
    both platforms.

  - Proper superscript support in `->infix` and `->TeX` renderers.

- #306: Added the mathematical constants `phi` and `e` bound to, respectively,
  `emmy.env/{phi,euler}`.

### Differential Geometry

- #326 is a large PR that marks the start of a big push toward full
  implementation of the ideas in "Functional Differential Geometry". Here is the
  full list of changes:

  - `emmy.calculus.basis` gains a new `::coordinate-basis` type, along with
    `coordinate-basis?`, `basis->coordinate-system`, `basis->dimension`,
    `contract` and `make-constant-vector-field` from scmutils. More functions
    moved here.

  - In `emmy.calculus.coordinate`, `let-coordinates` and
    `using-coordinates` can now handle namespaced coordinate systems like
    `m/R2-rect` in their coordinate system position! Their docstrings are far
    better too.

  - `emmy.calculus.vector-field/coordinate-basis-vector-fields` was renamed
    to `coordinate-system->vector-basis`.

  - `emmy.calculus.form-field/coordinate-basis-oneform-fields` was renamed
    to `coordinate-system->oneform-basis`.

  - `emmy.calculus.manifold` gets a LOT of restructuring, and many new
    manifolds out of the box. Here's the full list of new functions:

    - `manifold-type`, `patch-names`, `coordinate-system-names`,
      `manifold-point?`, `typical-coords`, `typical-point`, `transfer-point`,
      `corresponding-velocities`
    - `zero-manifold-function`, `one-manifold-function`,
      `constant-manifold-function`

    And new manifolds and coordinate systems. Here's the full list of manifolds
    that are now present:

    - From the `Rn` family: `R1`, `R2`, `R3`, `R4`, `spacetime`
    - From `S2-type`: `S2`
    - From `Sn`: `S1`, `S2p`, `S3`
    - From `SO3-type`: `SO3`

    And coordinate systems, prefixed by their manifold in all cases:

    - `R1-rect`, `the-real-line` (alias for `R1-rect`)
    - `R2-rect`, `R2-polar`,
    - `R3-rect`, `R3-cyl`, `R3-spherical`,
    - `R4-rect`, `R4-cyl`,
    - `spacetime-rect`, `spacetime-sphere`

### Behavior changes, bug fixes

- #329 fixes a bug where the simplifier couldn't handle expressions like `(sqrt
  (literal-number 2))`, where literal numbers with no symbols were nested inside
  of symbolic expressions.

- #321 changes the default `TeX` rendering style for `down` tuples back to
  horizontal, undoing #283. @kloimhardt made a solid case that because `down`
  tuples represent row vectors, it's not helpful for building knowledge and
  intuition to only distinguish these with differently-shaped braces.
  Intuition-builders win!

- #320: `Operator` gains a new simplifier for its `name` field; the simplifier
  applies the associative rule to products and sums of operators, collapses
  (adjacent) products down into exponents, and removes instances of `identity`
  (the multiplicative identity for operators).

  `Operator` multiplication (function composition) is associative but NOT
  commutative, so the default simplifier is not appropriate.

  Before this change:

```clojure
emmy.env> (series/exp-series D)
#object[emmy.series.Series
  "(+ identity
      (* D identity)
      (* (/ 1 2) (* D (* D identity)))
      (* (/ 1 6) (* D (* D (* D identity)))) ...)"]
```

  After:

```clojure
emmy.env> (series/exp-series D)
#object[emmy.series.Series
  "(+ identity
      D
      (* (/ 1 2) (expt D 2))
      (* (/ 1 6) (expt D 3)) ...)"]
```

- #315: `g/log2` and `g/log10` on symbolic expressions now stay exact, instead
  of evaluating `(log 2)` or `(log 10)` and getting a non-simplifiable real
  number in the denominator:

```clojure
(g/log2 'x)
;;=> (/ (log x) (log 2))

(g/log10 'x)
;;=> (/ (log x) (log 10))
```

- #304:

  - implements `v/=` properly for sequences, `Differential`, `Complex`,
    `Structure` and `Matrix` instances

  - in `emmy.env`, `v/=` now overrides `clojure.core/=`. `v/=` should act
    identically to `clojure.core/=` everywhere; the difference is that its
    behavior is customizable, so we can make `Differential` instances equal to
    numbers, or complex numbers with a 0 imaginary part equal to real numbers
    with the same real part.

    `v/=` may not drop recursively down into, say, Clojure maps. Please open an
    issue if you find a case like this!

  - BIG CHANGE: `Literal` and `Structure` instances now KEEP their type under
    `g/simplify`. If you want to get the expression back out of its `Literal`
    wrapper, use `emmy.expression/expression-of`, also aliased into
    `emmy.env`.

    This means that you can no longer make comparisons like this:

```clojure
;; this worked before, and was used all over the tests (probably not in much
;; user code!)
(clojure.core/= '(* 3 x)
                (simplify (+ 'x 'x 'x)))
;;=> false
```

  Instead, use `v/=` (which is now aliased into `emmy.env`):

```clojure
;; `v/=` will do the right thing by unwrapping the literal expression on the
;; right:
(v/= '(+ x y) (+ 'x 'y))
;;=> true
```

- #207 fixes a bug where `emmy.function/compose` would fail when provided
  with no arguments. Now it appropriately returns `identity`.

- #310: `g/make-rectangular` and `g/make-polar` now return non-Complex numbers
  on the JVM if you pass `0` for the (respectively) imaginary or angle
  components. Previously this behavior only occurred on an integer 0; now it
  happens with 0.0 too, matching CLJS.

- #308 and #310: `->infix` now renders any symbol named as an upper and
  lowercase greek characters (`'alpha`, `'Phi` etc) as their proper unicode
  characters. `'ldots` renders to '...', and `'ell` renders to a pretty "ℓ",
  matching the TeX renderer.

## 0.16.0

> (If you have any questions about how to use any of the following, please ask us
> at our [Github Discussions](https://github.com/emmy/emmy/discussions)
> page!)

This release contains a few correctness fixes, a number of new
`emmy.generic` function implementations contributed by @pangloss, and a
large expansion of the namespaces available to SCI-hosted environments.

The themes of the release are:

- Many new functions and functionality for existing types
- Upgraded rendering for forms like nested partials
- Better and better documentation!
- Easier interop with interactive hosts via SCI

A major goal was to develop Emmy into an environment that could host all of
the exercises from the SICM textbook. We have many of those hosted at the
https://github.com/emmy/sicm-exercises repository, and they all work and
generate correctly rendered TeX!

The goals for the next release roll over from 0.15.0:

we'll focus on getting Emmy integrated with 2D and 3D rendering libraries
like [three.js](https://threejs.org), [babylon.js](https://www.babylonjs.com)
and [Quil](https://github.com/quil/quil). The long-term goal is for Emmy to
support the sort of workflow I described in ["The Dynamic
Notebook"](https://roadtoreality.substack.com/p/the-dynamic-notebook).

Out-of-the-box charting and animation primitives are missing and sorely needed.
Onward!

Detailed release notes:

### New Functions, Functionality

- #278 adds new generic `floor`, `ceiling`, `integer-part` and `fractional-part`
  generic functions, along with:

  - implementations for all types in the numeric tower - ratios, integers,
    reals, complex, and `Differential` (so derivatives of these functions work!)
  - symbolic expression implementations
  - symbolic implementations for `modulo` and `remainder`
  - new support for these four generics plus `modulo` and `remainder` in
    function compilation via `emmy.expression.compile` (#295)
  - rendering support by `->infix`, `->TeX`, `->Javascript` (#295)

  Thank you to @pangloss for this major contribution!

- division between two `Structure` instances `a` and `b` now (as of #297)
  returns a new structure instance `(/ a b)` that matches the contract `(= a (*
  b (/ a b)))`. Previously, the division would not necessarily contract with `b`
  to return `a`, resulting in problems with the double pendulum exercise of
  #296.

- #149 adds a `emmy.modint/modint?` predicate, and
  `emmy.modint/chinese-remainder`. The latter efficiently performs the
  [Chinese Remainder
  algorithm](https://en.wikipedia.org/wiki/Chinese_remainder_theorem) for
  solving systems of linear congruences. Available via
  `emmy.env/chinese-remainder`.

- `clojure.lang.Var` implements the `emmy.value/Value` protocol, allowing
  it to respond appropriately with its name to `v/freeze` (#298).

- Install `emmy.generic/{quotient,modulo,remainder,partial-derivative}`
  into `emmy.env` (#273). Thanks to @pangloss for pointing out that these
  were missing!

- #284 added:

  - new functions `emmy.mechanics.lagrange/acceleration-tuple` for creating
    the acceleration entry in a local tuple

  - `emmy.mechanics.lagrange/acceleration` for extracting the acceleration
    component of a local tuple

  - An upgraded `emmy.mechanics.lagrange/F->C` to handle local tuples of
    arbitrary length. This version of `F->C` is more general than the version
    from the textbook that was previously included.

  These are all aliased in `emmy.env`, along with a new `Γ-bar` alias for
  `emmy.mechanics.lagrange/Γ-bar`.

- #282 modifies the `emmy.value/freeze` implementation for Clojure vector
  to freeze vectors into the same representation as an `up` structure. This
  makes rendering these forms much more simple and matches the `scmutils`
  behavior.

- `emmy.structure.Structure` implements `clojure.lang.{Indexed, IReduce}`
  on the JVM, allowing it to act more like a vector (#282). (The CLJS
  implementation already did this.) `(vec (up 1 2 3))` now works correctly.

- `Series`, `PowerSeries` and `Operator` can hold metadata and respond properly
  to `meta` and `with-meta` (#265). `emmy.series/{->Series, ->PowerSeries}`
  and `emmy.operator/->Operator` all take a new arity for metadata.

### g/simplify changes

- `g/simplify` called with an argument `x` of type `Series`, `PowerSeries`,
  `Matrix`, `Operator`, `Complex` and `emmy.abstract.function/Function` now
  return an instance of type `x`, performing appropriate simplifications if
  possible. before #297 and #298, these operation would return bare symbols or
  sequences.

  A future release will make this change for `Structure` and `Literal` too, once
  #255 is resolved.

### Rendering, Docs

- #286 adds a batch of rules to `emmy.simplify.rules/canonicalize-partials`
  that act to gather up nested `partial` (derivative) applications into products
  and exponentiated partials. `->TeX` and `->infix` both produce better-looking
  forms with this change.

  This example shows how `g/simplify` can organize a nested application of many
  partial derivatives into a product:

```clojure
(let [f (literal-function 'f (-> (UP Real Real) Real))]
  (simplify
   (((partial 0)
     ((partial 1)
      ((partial 0) f))) (up 'x 'y))))
;;=> (((* (expt (partial 0) 2) (partial 1)) f) (up x y))
```

- #283 changes the default `TeX` rendering style for `down` tuples to vertical
  vs horizontal.

- Symbols like `'qprime` ending with `prime` or `primeprime` will render as `q'`
  or `q''` respectively in `TeX`, rather than the fully-spelled-out
  `\mathsf{qprime}` (#282).

- #280 adds a new `:equation` keyword argument to `emmy.render/->TeX`. If
  you pass a truthy value to `:equation`, the result will be wrapped in an
  equation environment. `:equation <string>` will insert a `\\label{<string>}`
  entry inside the equation environment.

    - `emmy.env/->tex-equation` is identical to `#(emmy.render/->TeX
      (g/simplify %) :equation true)`; If you pass a `:label` keyword argument
      to `->tex-equation` it will be forwarded to `->TeX`, creating the expected
      label entry.

- #279: Function aliases in `emmy.env` now properly mirror over docstrings
  and other `Var` metadata, thanks to
  [Potemkin](https://github.com/clj-commons/potemkin)'s `import-def`. This
  doesn't quite work in ClojureScript since we can't use `resolve` inside of a
  macro (special form!).

- Add a proper namespace to `demo.clj`, to make it easier to use outside of
  `lein repl` (#264).

### SCI Upgrades

- #289 adds many namespaces to `emmy.env.sci`:

  - `emmy.{complex,expression,modint,numsymb,polynomial,ratio,rational-function,util,value}`
  - `emmy.abstract.number`
  - `emmy.expression.analyze`
  - `emmy.numerical.elliptic`
  - `emmy.util.{aggregate,stream}`

  - #289 also introduces `emmy.function/*strict-arity-checks*` to allow the
    user to toggle whether or not to throw exceptions if the system thinks that
    arities are incompatible. It turns out that inside of an SCI environment,
    the usual tricks for detecting arities fail, causing errors in many
    expressions. To get around this, `*strict-arity-checks*` is FALSE by
    default.

### Behavior changes, bug fixes

- In JVM Clojure (as of #298), `emmy.expression.compile` defaults to
  `clojure.core/eval` to compile functions, while ClojureScript defaults to
  [SCI](https://github.com/borkdude/sci). The performance is much faster for
  numerical routines and worth the slightly different default behavior.

  To use to SCI compilation on the JVM, wrap your form in a binding:

  ```clojure
  (require '[emmy.expression.compile :as compile])

  (binding [compile/*mode* :sci]
    (my-compiler-triggering-function))
  ```

  To set the mode permanently, use `compile/set-compiler-mode!`:

  ```clojure
  (compile/set-compiler-mode! :sci)

  (my-compiler-triggering-function)
  ```

  The options allowed as of `0.16.0` are `:sci` and `:native`.

- #292 fixes a `StackOverflow` that would sometimes appear when comparing
  symbolic expressions to non-expressions. `(= (literal-number x) y)` now
  returns true if `(= x y)` (AND, in clj, if `y` is not a collection!), false
  otherwise. #255 is currently blocking pass-through equality with collections
  on the JVM. Thanks to @daslu for the report here!

- `emmy.modint/make` now verifies with a precondition that its two
  arguments are both `v/integral?` (#298). We need this constraint now that
  `g/modulo` is defined for more types.

- #285 fixes a bug that prevented `sin / cos` from simplifying into a `tan` in
  the numerator, and makes `seq:-` slightly more efficient (closing heisenbug
  #151).

## 0.15.0

> (If you have any questions about how to use any of the following, please ask us
> at our [Github Discussions](https://github.com/emmy/emmy/discussions)
> page!)

This release was focused on a small number of themes:

**Automatic differentiation**:

The goal for this cycle's automatic differentiation (AD) work was to expand the
set of functions and types that can play with AD. AD now works on functions that
return all Clojure sequences, maps, and vectors, in addition to Emmy types
like `Operator`, `Series`, `PowerSeries` and `Structure`. The system is now
fully extensible, so if you want to differentiate functions that return custom
records or Java collections, it's now no problem.

Emmy can now differentiate functions in ClojureScript that use comparison
operations like `<`, `=`, `<=` and friends. Clojure can't quite do this yet, but
you can differentiate through `v/compare` and `v/=` calls.

We can also differentiate functions that return other functions with no trouble;
only a few libraries can do this, and the behavior is subtle. Hold tight for
comprehensive docs describing this behavior.

New `Div`, `Grad`, `Curl` and `Lap` operators build on this foundation.

**SCI Integration**

To support safe execution inside of a browser-based Notebook or REPL
environment, Emmy now has full support for @borkdude's
[SCI](https://github.com/borkdude/sci), the Small Clojure Interpreter, via the
[emmy.sci](https://github.com/emmy/emmy/blob/master/src/emmy/env/sci.cljc)
namespace. Every function and macro in the library now works in SCI. (Thanks for
@borkdude and @mk for your help and contributions.

**Rendering**

@hcarvalhoalves made a number of contributions to the LaTeX and infix renderers;
`PowerSeries` and `Series` now render beautifully, as well as `=` and the
various inequality symbols. Expect a lot more action here as we move into more
browser-based notebook environments.

There's a lot more that went into this release; give the detailed notes below a
look for more details.

**What's coming next?**

The next release will focus on getting Emmy integrated with 2D and 3D
rendering libraries like [three.js](https://threejs.org),
[babylon.js](https://www.babylonjs.com) and
[Quil](https://github.com/quil/quil). The long-term goal is for Emmy to
support the sort of workflow I described in ["The Dynamic
Notebook"](https://roadtoreality.substack.com/p/the-dynamic-notebook). This will
require a big push on generic, pluggable representations for the various types
and expressions in the library.

Thanks again to @hcarvalhoalves and @mk for their contributions, and to @borkdude for
his help with SCI!

On to the detailed release notes:

### Automatic Differentiation

- New, literate `Differential` implementation lives at at
  `emmy.differential` (#221) (see [this
  page](https://samritchie.io/dual-numbers-and-automatic-differentiation/) for a
  readable version.) Notable changes to the original impl at
  `emmy.calculus.derivative` include:

  - We've changed our terminology from GJS's `finite-part`,
    `infinitesimal-part`, `make-x+dx` to the more modern `primal-part`,
    `tangent-part`, `bundle-element` that the Automatic Differentiation
    community has adopted. His comment is that he doesn't take terms from
    mathematics unless he's _sure_ that he's using it in the correct way; the
    safer way is to stick with his terms, but we go boldly forth with the
    masses.

  - A new `emmy.differential.IPerturbed` protocol makes it possible to
    extend the Automatic Differentiation (AD) system to be able to handle
    different Functor-shaped return values, like Java or JS lists and objects.
    See the [cljdoc page on Automatic
    Differentiation](https://cljdoc.org/d/emmy/emmy/CURRENT/doc/calculus/automatic-differentiation)
    for more detail.

    - #222 implements `d/IPerturbed` for Clojure maps, vectors and sequences;
      all are now valid return types for functions you pass to `D`.

    - #222 also implements `d/IPerturbed` for Emmy `Matrix`, `Structure`,
      `Series`, `PowerSeries` and `Operator`.

    - #223 implements `d/IPerturbed` for Clojure functions and multimethods,
      handling the attendant subtlety that fixes "Alexey's Amazing Bug".

  - `emmy.differential/{lift-1,lift-2,lift-n}` allow you to make custom
    operations differentiable, provided you can supply a derivative.

  - `Differential` implements `emmy.function/arity`, `IFn`, and can be
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
    "amazing" bug sections in `emmy.calculus.derivative-test` for proper
    exposition.

- #223 converts the implementation of `emmy.calculus.derivative/D` to use
  the new `Differential` type; this fixes "Alexey's Amazing Bug" and allows `D`
  to operate on higher order functions. For some function `f` that returns
  another function, `((D f) x)` will return a function that keeps `x` "alive"
  for the purposes of differentiation inside its body. See
  `emmy.calculus.derivative-test/amazing-bug` for an extended example.

- `emmy.generic/partial-derivative` gains a `Keyword` extension, so it can
  respond properly to `:name` and `:arity` calls (#221).

- `D` (or `emmy.generic/partial-derivative`) applied to a matrix of
  functions now takes the elementwise partials of every function in the matrix.
  (#218)

- #253 moves the derivative implementations (where relevant) onto the metadata
  of generic functions. You can access these by calling `(<generic-function>
  :dfdx)` or `(<generic-function> :dfdy)`, depending on whether the generic is
  unary or binary. #253 also changes the name of macro
  `emmy.generic/def-generic-function` to `emmy.generic/defgeneric`.

### Rendering

- `emmy.expression/Literal` instances now use `pr-str` to generate a string
  representation; this allows this type to wrap lazy-sequence expressions such
  as those returned from `g/simplify` (#259)

- `emmy.expression.render/->infix` and `emmy.expression.render/->TeX`
  now handle equality/inequality symbols (`=`, `>=`, `>`, ...) as infix (#257).

- `emmy.expression.render/*TeX-sans-serif-symbols*` binding to control if
  symbols longer than 1 char should have `\mathsf` applied (#258).

- `->infix`, `->TeX` and `->JavaScript` in `emmy.expression.render` can now
  accept unfrozen and unsimplified `Expression` instances (#241). This makes it
  a bit more convenient to use `->infix` and `->TeX` at the REPL, or in a
  Notebook environment. Additionally, the return values of renderers are always
  coerced to strings. (Previously, `(->infix 10)` would return a number
  directly.)

- `up` and `down` tuples from `emmy.structure` gain a proper `print-method`
  implementation (#229); these now render as `(up 1 2 3)` and `(down 1 2 3)`,
  instead of the former more verbose representation (when using `pr`.)

- `emmy.render/->infix` and `emmy.render/->TeX` will render `Series`
  and `PowerSeries` as an infinite sum (showing the first four terms).
  In the case of unnaplied `PowerSeries`, it will represent the unbound
  variable as `_` (#260).

### Performance Improvements

- `emmy.modint` gains more efficient implementations for `inverse`,
  `quotient`, `exact-divide` and `expt` on the JVM (#251).

### Comparison / Native Type Integration

- beefed up the Javascript numeric tower to allow objects like
  `emmy.differential/Differential`, `emmy.expression/Expression` and
  friends that WRAP numbers to compare properly using cljs-native `<`, `<=`,
  `=`, `>=` and `>` (#236)

- new `emmy.value/compare` function exposed in `emmy.env` returns a
  valid comparison bit between native numbers and numbers wrapped in
  `Differential` or `Expression` in both JVM Clojure and ClojureScript (#236).
  The behavior matches `clojure.core/compare` for all reals on the JVM; it
  doesn't in ClojureScript because native `compare` can't handle
  `goog.math.{Long,Integer}` or `js/BigInt`.

### Operator

- #219 introduces a number of changes to `Operator`'s behavior:

  - `Operator` is now a `deftype` (not a `defrecord`); the keyword lookup for
    its `:name`, `:arity`, `:context` and `:o` fields have been replaced by,
    respectively, `o/name`, `emmy.function/arity`, `o/context` and
    `o/procedure` functions. This change happened to allow `Operator` to
    implement protocols like `ILookup`.

  - Native `get` and `get-in` now act on `Operator`. Given an operator function
    `f`, `get` and `get-in` compose `#(get % k)`, or similar with `f`. This
    deferred action matches the effect of all emmy generics on functions.

  - Combining an operator and a non-operator via `+` and `-`, the non-operator
    was previously lifted into an operator that multiplied itself by the new
    operator's argument. As of #219, this "multiplication" uses the operator
    definition of multiplication - meaning, the new operator composes the
    non-operator with its argument. Where does this matter?

    Previously adding the non-operator `emmy.function/I` to the identity
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

  - `emmy.operator/identity-operator` has been renamed to
    `emmy.operator/identity`

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

### Additions

- #224 adds new `Div`, `Grad`, `Curl` and `Lap` operators in
  `emmy.calculus.derivative` and installs them into `emmy.env`. #224
  also removes the `g/transpose` implementation for `Operator` instances, and
  exposes `emmy.calculus.derivative/taylor-series` to `emmy.env`.

- #222 adds `v/Value` implementations for Clojure sequences and maps. Maps and
  vectors implement `f/Arity` and return `[:between 1 2]. `zero?` and
  `zero-like` work on sequence entries and map values. Maps can specify their
  `v/kind` return value with a `:type` key, and some of the calculus
  implementations do already make use of this feature. `g/partial-derivative` on
  a Clojure Map passes through to its values.

- As of #232, `emmy.expression.compile/compile-univariate-fn` is now
  `compile-fn` (same change for the non-cached `compile-fn*` in the same
  namespace). The new implementation can compile arguments of any arity, not
  just arity == 1. The new version takes an arity parameter `n` that defaults to
  `(emmy.function/arity f)`.

- `emmy.function/arity` is now a protocol method, under the
  `emmy.function/IArity` protocol (#218). In addition to functions, `arity`
  now correctly responds to:

    - `emmy.matrix/Matrix`: calling `arity` on a matrix assumes that the
      matrix has function elements; the returned arity is the most general arity
      that all functions will respond to.
    - `emmy.operator/Operator`: returns the arity of the operator's wrapped
      function.
    - `emmy.series/Series`: `arity` on a `Series` assumes that the series
      contains functions as entries, and returns, conservatively, the arity of
      the first element of the series.
   - `emmy.series/PowerSeries`: `arity` returns `[:exactly 1]`, since
     `PowerSeries` are currently single variable.
   - vectors, and `emmy.structure/Structure`: `arity` on these collections
     assumes that the collection contains functions as entries, and returns the
     most general arity that is compatible with all of the function elements.

- New single-arity case for `emmy.structure/opposite` returns an identical
  structure with flipped orientation (#220). acts as `identity` for
  non-structures.

- Added missing `identity?`, `identity-like` for complex and rational numbers
  (#236)

- `emmy.env/ref` now accepts function and operators (#219). `(ref f 0 1)`,
  as an example, returns a new function `g` that acts like `f` but calls `(ref
  result 0 1)` on the result.

- The slightly more general `emmy.env/component` replaces
  `emmy.structure/component` in the `emmy.env` namespace (#219).
  `((component 0 1) x) == (ref x 0 1)`.

- New functions `emmy.function/{get,get-in}` added that act like the
  `clojure.core` versions; but given a function `f`, they compose `#(get % k)`,
  or similar with `f`. This deferred action matches the effect of all emmy
  generics on functions. (#218)

- `emmy.function/I` aliases `clojure.core/identity` (#218). #219 exposes
  `I` in `emmy.env`.

- `emmy.env.sci` contains an SCI context and namespace mapping sufficient
  to evaluate all of emmy, macros and all, inside of an
  [SCI](https://github.com/borkdude/sci) environment (#216). Huge thanks to
  @borkdude for support and @mk for implementing this!

- `emmy.numerical.elliptic` gains a full complement of elliptic integral
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

### Fixes / Misc

- The operator returned by `emmy.calculus.derivative/partial` now has a
  proper name field like `(partial 0)`, instead of `:partial-derivative` (#223).

- #223 fixes a problem where `(operator * structure)` would return a structure
  of operators instead of an operator that closed over the multiplication.
  `::s/structure` is now properly a `::o/co-operator`, matching its status as a
  `::f/cofunction`.

- Fix a bug where `f/arity` would throw an exception with multiple-arity
  functions on the JVM (#240). It now responds properly with `[:between
  min-arity max-arity]`, or `[:at-least n]` if there is a variadic case too.

- #238 converts `emmy.abstract.function/Function` from a `defrecord` to a
  `deftype`, fixing a subtle bug where (empty f) was getting called in a nested
  derivative test.

- fixed bug with `g/dimension` for row and column matrices (#214). previously
  they returned `1` in both cases; now they return the total number of entries.

- #253 adds proper `:arglists` metadata for all generic functions.

## 0.14.0

- After the work below, `v/nullity?` renamed to `v/zero?`, and `v/unity?`
  renamed to `v/one?`
  ([#180](https://github.com/emmy/emmy/pull/180)). This
  affects the names listed in the CHANGELOG entries below.

### Miscellaneous

- expose `bootstrap-repl!` to ClojureScript, so that this is available in
  self-hosted CLJS (https://github.com/emmy/emmy/pull/157)

- modified `infix.cljc` to wrap forms in `displaystyle` and add proper carriage
  returns inside structures
  (https://github.com/emmy/emmy/pull/157)

- add `multidimensional-minimize` to the `emmy.env` namespace
  (https://github.com/emmy/emmy/pull/157)

- add more `sqrt` simplification rules to allow square roots to cancel out
  across a division boundary, with or without products in the numerator and
  denominator (https://github.com/emmy/emmy/pull/160)

- fix NPE bug that appears in nelder-mead, when callback isn't supplied
  (https://github.com/emmy/emmy/pull/162)

- Add `sqrt-expand` and `sqrt-contract`, to allow simplifications to push inside
  of square roots (https://github.com/emmy/emmy/pull/163)

- speed up power series multiplication by skipping work when either head term is
  zero (https://github.com/emmy/emmy/pull/166)

- File moves:
  - `emmy.polynomial-gcd`    => `emmy.polynomial.gcd`
  - `emmy.polynomial-factor` => `emmy.polynomial.factor`
  - `emmy.rules`             => `emmy.simplify.rules`
  - `emmy.analyze`           => `emmy.expression.analyze`
  - `emmy.infix`             => `emmy.expression.render`
  - `emmy.numerical.compile` => `emmy.expression.compile`

- `emmy.env/one?` now exposes/aliases `emmy.value/unity?`
  [#154](https://github.com/emmy/emmy/pull/154)

- Fixed [#93](https://github.com/emmy/emmy/issues/93) by
  adding an explicit `g/invert` implementation for polynomials in the rational
  fn namespace. The fix lives in
  [#169](https://github.com/emmy/emmy/pull/169).

- added `emmy.value/sqrt-machine-epsilon`
  ([#170](https://github.com/emmy/emmy/pull/170))

- fixed issues in `function.cljc` and `operator.cljc` where the ClojureScript
  `IFn` `-invoke` arguments shadowed either the `this` operator, or some
  parameter name in the deftype
  ([#169](https://github.com/emmy/emmy/pull/169))

- `g/sqrt` now maintains precision with ClojureScript's rational numbers.
  `(g/sqrt #sicm/ratio 9/4)` for example returns `#sicm/ratio 3/2`.
  ([#168](https://github.com/emmy/emmy/pull/168))

- `g/determinant` and `g/transpose` now act as identity for everything in the
  numeric tower, plus symbolic expressions
  ([#168](https://github.com/emmy/emmy/pull/168))

- `emmy.expression.Expression` is now `emmy.expression.Literal`; it
  has a new `meta` field, and is a `deftype` instead of a `defrecord`.
  ([#168](https://github.com/emmy/emmy/pull/168))
  - To get the internal expression, use `x/expression-of` instead of
    `:expression`.
  - to access the `type` field, use `x/literal-type` instead of `:type`

- 2-arity `g/atan`, `g/cross-product` and `g/gcd` now work for functions
  ([#168](https://github.com/emmy/emmy/pull/168))

- `Literal` now responds appropriately to `v/unity?` and `v/nullity?` if it
  wraps a numerical "0" or "1". `v/exact?` now returns true if the literal wraps
  an exact number ([#168](https://github.com/emmy/emmy/pull/168))

- `x/variables-in` now works with wrapped expressions; no more need to
  explicitly unwrap
  ([#168](https://github.com/emmy/emmy/pull/168))

- `x/walk-expression` renamed `x/evaluate`
  ([#168](https://github.com/emmy/emmy/pull/168))

- The new `x/substitute` performs substitutions on an _unwrapped_ expression
  ([#168](https://github.com/emmy/emmy/pull/168))

-  `x/compare` returns a comparator that works with unwrapped symbolic
   expression trees
   ([#168](https://github.com/emmy/emmy/pull/168)). The rules
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
  ([#171](https://github.com/emmy/emmy/pull/171))

- Moved all `literal-function` machinery and definitions to
  `emmy.abstract.function`
  ([#171](https://github.com/emmy/emmy/pull/171)).
  `emmy.function` now contains only the generic method implementations for
  clojure functions and multimethods.

- Switched inheritance order for functions;
  `:emmy.abstract.function/function` (used to be
  `:emmy.function/function`) now inherits from `::v/function` instead of
  the other way around.
  ([#171](https://github.com/emmy/emmy/pull/171))

- Enhanced the `g/simplify` behavior for core functions that overlap with
  generic functions (`+`, `-`, `*`, `/`, `mod`, `quot`, `rem`, `neg?`). These
  now freeze to the same symbols as their generic counterparts.
  ([#173](https://github.com/emmy/emmy/pull/173))

- Add support for the hyperbolic trig functions `sinh`, `cosh`, `tanh`, `atanh`,
  `asinh` and `acosh` to `emmy.expression.render/->Javascript`.
  ([#174](https://github.com/emmy/emmy/pull/174))

- Add support for the hyperbolic trig functions `atanh`, `asinh` and `acosh` to
  `emmy.expression.compile`.
  ([#175](https://github.com/emmy/emmy/pull/175))

- `matrix.cljc` gains `m/nth-col` and `m/diagonal`
  ([#178](https://github.com/emmy/emmy/pull/178) introduces:)

- As of [#178](https://github.com/emmy/emmy/pull/178)
  introduces:, we have three new kinds for matrices. Square matrices return
  `::m/square-matrix`, and columns and rows return `::m/column-matrix` and
  `::row-matrix` respectively. These all derive from `::m/matrix`. This makes it
  easier to register methods or test specifically for these cases. We've also
  added `m/column?` and `m/row?` predicates to check for these cases.

- [#185](https://github.com/emmy/emmy/pull/185) specializes
  all matrix operations that return power series (trig operations and `g/exp` to
  `::square-matrix`).

- [#184](https://github.com/emmy/emmy/pull/184) modifies
  `v/exact?` on functions; `((v/exact? f) x) == (v/exact? (f x))` now, instead
  of false as before. `literal-function` forms now have a correct `v/one-like`
  implementation.

- clojure Vars now respond to function algebra
  ([#184](https://github.com/emmy/emmy/pull/184)). All
  functions implement `g/negative?`, `g/abs`, `g/quotient`, `g/remainder`,
  `g/modulo`, `g/dimension` and `g/exact-divide`, responding to the appropriate
  arities.

- `emmy.complex/complex` can now take any real type in its constructor, vs
  only numbers
  ([#184](https://github.com/emmy/emmy/pull/184)).

- `modint` instances now implement `v/freeze?`: `(emmy.modint/make 1 2)`
  freezes to that `(modint 1 2)`.
  ([#185](https://github.com/emmy/emmy/pull/185)).

- `v/eq` renamed to `v/=`.
  ([#186](https://github.com/emmy/emmy/pull/186)).

- `v/zero-like` on matrices now fills entries with appropriate `v/zero-like`
  versions of their existing types
  ([#188](https://github.com/emmy/emmy/pull/188))

- `v/Value` gains `identity-like` and `identity`
  ([#188](https://github.com/emmy/emmy/pull/188)). These are
  aliased into `emmy.env`. Implementations are installed on:

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

- `emmy.complex/I` aliases `i`
  ([#189](https://github.com/emmy/emmy/pull/189))

- `matrix.cljc` has a new `by-cols` (analogous to `m/by-rows`), and `row` to
  generate a row matrix (analagous to `column`).
  [#197](https://github.com/emmy/emmy/pull/197) Also in
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

- `emmy.structure/up` and `emmy.structure/down` now have analogous
  `s/up*` and `s/down*` functions. These behave identically, but are
  non-variadic. If you already have a sequence you'd like to transform, prefer
  these ([#197](https://github.com/emmy/emmy/pull/197)).

- `emmy.value/kind-predicate` takes some item and returns a predicate that
  returns true if its argument has the same type (or inherits from it)
  ([#197](https://github.com/emmy/emmy/pull/197)).

- `emmy.function/arg-shift` and `emmy.function/arg-scale` take
  functions and return new functions that shift and scale their arguments
  (respectively) by the originally supplied shifts
  ([#197](https://github.com/emmy/emmy/pull/197)).

- `emmy.generic/factorial` computes the factorial of the supplied integer
  `n`.
  ([#197](https://github.com/emmy/emmy/pull/197)).

- Many new functions and constants exposed in `emmy.env` via
  [#197](https://github.com/emmy/emmy/pull/197):

  - `-pi` joins `pi` as a constant
  - `s:generate`, `m:generate`, `vector:generate` to generate matrices,
    structures and vectors
  - `constant-series`, from `series/constant`
  - `seq:print` and `seq:pprint`
  - `matrix-by-cols`, `row-matrix`, `v:make-basis-unit`
  - aliases for `emmy.function`'s `arity`, `arg-shift`, `arg-scale`
  - `dimension`, `factorial` aliased from `emmy.generic`
  - `derivative` aliased from `emmy.calculus.derivative`
  - `submatrix`, `up->column-matrix`, `down->row-matrix`,
    `row-matrix->{down,vector}`, `column-matrix->{up,vector}` aliased from
    `emmy.matrix`
  - `D-numeric` from `emmy.numerical.derivative`
  - `brent-min`, `brent-max`, `golden-section-min`, `golden-section-max`
  - `nelder-mead`
  - `sum` from `emmy.util.aggregate
  - `kind-predicate` from `emmy.value`

- Structures and matrices both gain the ability to do native `get-in`,
  `assoc-in` and `empty`. These work as expected, like a potentially nested
  vector. ([#193](https://github.com/emmy/emmy/pull/193))

- `matrix.cljc` gains `up->row-matrix`, `up->column-matrix`, `row-matrix->up`,
  `column-matrix->up`
  ([#193](https://github.com/emmy/emmy/pull/193))

- `structure.cljc` gains many features in
  ([#193](https://github.com/emmy/emmy/pull/193)):

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
  - `emmy.generators` now holds generators for `up`, `down`, and
    `structure` generators; these produce potentially deeply nested structures.
    `up1`, `down1` and `structure1` generate only one level deep. Mix and match!
    See `structure_test.cljc` for many examples of how to use these.

### Literals

- `literal-matrix` fn generates a symbolic matrix
  (https://github.com/emmy/emmy/pull/169)
- `literal`, `literal-up` and `literal-down` generate symbolic structures
  (https://github.com/emmy/emmy/pull/169)

### Numeric Tower Adjustments

This release (courtesy of
[#168](https://github.com/emmy/emmy/pull/168)) brings
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

- The `emmy.abstract.number` (I'll call this `an` here) namespace is
  responsible for installing generic implementations of all numeric methods for
  symbolic expressions and "literal numbers".

- the `an/literal-number` constructor promotes a number, symbol or symbolic
  expression up to `:xx/numeric`, which means that any operation you perform on
  it will pass it through the symbolic expressions defined in
  `emmy.numsymb`. A few notes on these expressions:

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
  `(an/literal-number 12)`, use `v/=`. In ClojureScript, this will work with
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

PR [#193](https://github.com/emmy/emmy/pull/193) brings:

- `g/dot-product`, for scalars, differentials, structures, functions and
  row/column matrices
- `g/inner-product` for scalars, structures, functions and row/column matrices
- `g/outer-product` for functions, structures of length 3 and matrices, between
  a row and a column only
- `g/cross-product` now works for row/column matrices in addition to structures
  (and functions that accept these)

PR https://github.com/emmy/emmy/pull/169 brings:

- `g/exp2`, `g/exp10` for exponents with base 2 and 10
- `g/log2`, for base 2 logarithms
- `g/log10` for base 10 logs
- `g/gcd` and `g/lcm` are now exposed in `emmy.env`

[#178](https://github.com/emmy/emmy/pull/178) introduces:

- `g/dimension` for scalars (always 1), structures and matrices (square, column
  and row)
- `g/trace` returns the trace for square matrices and square structures

We now expose the following additional trigonometric functions in
`emmy.generic` (courtesy of
https://github.com/emmy/emmy/pull/154):

- `cosh`: hyperbolic cosine
- `sinh`: hyperbolic sine
- `tanh`: hyperbolic tangent, ie sinh/cosh
- `sech`: hyperbolic secant, ie 1/cosh
- `csch`: hyperbolic secant, ie 1/sinh
- `acosh`: inverse hyperbolic cosine, ie, `(= x (cosh (acosh x)))`
- `asinh`: inverse hyperbolic sine, ie, `(= x (sinh (asinh x)))`
- `atanh`: inverse hyperbolic tangent, ie, `(= x (tanh (atanh x)))`

These three methods existed in `emmy.env`, but not as extensible generics.
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

Additionally, four methods that lived in `emmy.generic` are now exposed as
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
    [#168](https://github.com/emmy/emmy/pull/168)

- PR [#189](https://github.com/emmy/emmy/pull/189) introduces:

  - `g/make-rectangular`, (build a complex number from real and imaginary parts)
  - `g/make-polar` (build a complex number from radius and angle)
  - note that these work with real numbers, symbolic numbers, functions and any
    combination of these.

These work with functions, real numbers and symbolic expressions (and any mix of
the three).

## 0.13.0

The main announcement for this release is _ClojureScript Support!_. Implementing
this resulted in a few upgrades along the way:

- more powerful numerics, specifically `definite-integral` and native
  minimization routines
- a generic numeric tower for ClojureScript
- Many more tests! The test coverage was great before, and it's stayed high as
  we've added new implementations.
- added explicit code coverage metrics via Codecov: [![Codecov branch](https://img.shields.io/codecov/c/github/littleredcomputer/emmy/master.svg?maxAge=3600)](https://codecov.io/github/littleredcomputer/emmy)

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

### ClojureScript Support

Full conversion of Emmy to ClojureScript. All functionality from v0.12.1
now works in both Clojure and ClojureScript!

Most of the conversion was straightforward. The major missing piece was a
numeric tower implementation for ClojureScript (complex numbers, ratios) that
bring it up to parity with Clojure:

- Add the `bigfraction` implementation from
  [fraction.js](https://www.npmjs.com/package/fraction.js) emmy.ratio for
  cross-platform ratio support (#99)
- Adds CLJS complex number support through [complex.js](https://github.com/infusion/Complex.js) (#41)
- `js/BigInt`, `goog.math.Long` and `goog.math.Integer` implementations round
  out the numeric tower (#45)

### Numerical Routines

The numerical routines in Emmy depend heavily on Apache Commons, which of
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
    - [Riemann Sums](https://github.com/littleredcomputer/emmy/blob/master/src/emmy/numerical/quadrature/riemann.cljc), all the way up through efficient, incremental, "accelerated" versions of these easy-to-understand methods:
    - [Midpoint method](https://github.com/littleredcomputer/emmy/blob/master/src/emmy/numerical/quadrature/midpoint.cljc), same development but shorter since it reuses functional abstractions. Also incremental, efficient, accelerated
    - [Trapezoid Method](https://github.com/littleredcomputer/emmy/blob/master/src/emmy/numerical/quadrature/trapezoid.cljc), same idea but for closed intervals.

  - **Sequence Acceleration / Extrapolation Methods**
    - [Polynomial interpolation](https://github.com/littleredcomputer/emmy/blob/master/src/emmy/polynomial/interpolate.cljc): the general thing that "richardson extrapolation" is doing below. Historically cool and used to accelerate arbitrary integration sequences
    - [Rational Function extrapolation](https://github.com/littleredcomputer/emmy/blob/master/src/emmy/rational_function/interpolate.cljc): used in bulirsch-stoer integration and ODE solving.
    - "[Richardson extrapolation](https://github.com/littleredcomputer/emmy/blob/master/src/emmy/polynomial/richardson.cljc)" is a special case, where we get more efficient by assuming that the x values for the polynomial interpolation go 1, 1/2, 1/4... and that we're extrapolating to 0.

  - **Higher-order Calculus:**
    - [Numerical derivatives](https://github.com/littleredcomputer/emmy/blob/master/src/emmy/numerical/derivative.cljc): derivatives using three kinds of central difference formulas... accelerated using Richardson extrapolation, with a nice technique for guarding against underflow.
    - [Simpson's Method](https://github.com/littleredcomputer/emmy/blob/master/src/emmy/numerical/quadrature/simpson.cljc)... fit a parabola to every slice. OR, "accelerate" the trapezoid method with one step of Richarsdson extrapolation!
    - [Simpson's 3/8 Method](https://github.com/littleredcomputer/emmy/blob/master/src/emmy/numerical/quadrature/simpson38.cljc): Same idea, but accelerate a sequence that triples its slices every iteration.
    - [Boole's Rule](https://github.com/littleredcomputer/emmy/blob/master/src/emmy/numerical/quadrature/boole.cljc): trapezoid method plus two steps of Richardson extrapolation. (Are you starting to see the pattern??)
    - [Romberg Integration](https://github.com/littleredcomputer/emmy/blob/master/src/emmy/numerical/quadrature/romberg.cljc): midpoint OR trapezoid, with as many steps of Richardson extrapolation as we can take!
    - [Milne's Rule](https://github.com/littleredcomputer/emmy/blob/master/src/emmy/numerical/quadrature/milne.cljc), MIDPOINT method, one step of extrapolation!
    - [Bulirsch-Stoer integration](https://github.com/littleredcomputer/emmy/blob/master/src/emmy/numerical/quadrature/bulirsch_stoer.cljc)... midpoint or trapezoid, with rational function extrapolation, as many steps as we can handle AND some custom step sizes.

  - **Combinators**:
    - [Variable Substitutions](https://github.com/littleredcomputer/emmy/blob/master/src/emmy/numerical/quadrature/substitute.cljc): implemented as functional wrappers that take an integrator and return a modified integrator.
    - [Improper Integrals](https://github.com/littleredcomputer/emmy/blob/master/src/emmy/numerical/quadrature/infinite.cljc): a template for a combinator that enables infinite endpoints on any integrator, using variable substitution on an appropriate, tunable range.
    - [Adaptive Integration](https://github.com/littleredcomputer/emmy/blob/master/src/emmy/numerical/quadrature/adaptive.cljc): a combinator that turns any of the integrators above into an "adaptive" integrator that's able to focus in on difficult regions.
  - And finally, "[Numerical Quadrature](https://github.com/littleredcomputer/emmy/blob/master/src/emmy/numerical/quadrature.cljc)", the namespace/essay that ties it all together.

- `emmy.numerical.compile` uses [SCI](https://github.com/borkdude/sci), the
  Small Clojure Interpreter, to generate compiled numerical code (#133)

- Implemented ODE solving using @littleredcomputer's
  [odex-js](https://github.com/littleredcomputer/odex-js) library (#135)

### Reader Literals

[data_readers.cljc](https://github.com/littleredcomputer/emmy/blob/master/src/data_readers.cljc)
provides 3 new data reader literals:

- `#sicm/ratio`

Use this with a ratio literal, like `#sicm/ratio 1/2`, or with a string like
`#sicm/ratio "1/4"`. If the denominator is `1` this literal will return a
`js/BigInt` in ClojureScript, or a Long in Clojure.

- `#sicm/bigint`

Use with a number literal, like, `#sicm/bigint 10`, or a string like
`#sicm/bigint "10000012"` to generate a `js/BigInt` in ClojureScript, or a
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

## [v0.12.1]

- Getting Github releases up to parity with the most recent release to Clojars.

## [v0.10.0]

- Did some refactoring and one breaking rename (Struct became Structure, since
  we don't abbreviate other deftypes). This also marks the point of departure
  for working with Functional Differential Geometry.


## [v0.9.8]

- This is the version that was current as of the talk @littleredcomputer gave at
  [Clojure/west 2017](2017.clojurewest.org), entitled "[Physics in
  Clojure](https://www.youtube.com/watch?v=7PoajCqNKpg)."
