# Emmy

A Clojure(script) implementation of the
[scmutils](https://groups.csail.mit.edu/mac/users/gjs/6946/refman.txt) system
for math and physics investigations in the Clojure and ClojureScript languages.
Emmy provides facilities for

- [symbolic
  computation](https://cljdoc.org/d/emmy/emmy/CURRENT/doc/data-types/symbolic-expressions),
  including state of the art TeX rendering and [expression
  simplification](https://cljdoc.org/d/emmy/emmy/CURRENT/doc/simplification)
- [automatic](https://cljdoc.org/d/emmy/emmy/CURRENT/doc/calculus/automatic-differentiation),
  [numerical](https://cljdoc.org/d/emmy/emmy/CURRENT/doc/numerical-methods/numerical-derivative)
  and
  [symbolic](https://cljdoc.org/d/emmy/emmy/CURRENT/doc/calculus/automatic-differentiation)
  differentiation
- [numerical integration and
  optimization](https://cljdoc.org/d/emmy/emmy/CURRENT/doc/numerical-methods)
- investigations in [differential
  geometry](https://cljdoc.org/d/emmy/emmy/CURRENT/doc/textbooks/functional-differential-geometry)
  and [Lagrangian and Hamiltonian
  mechanics](https://cljdoc.org/d/emmy/emmy/CURRENT/doc/textbooks/structure-and-interpretation-of-classical-mechanics)

And implementations of many different [mathematical
objects](https://cljdoc.org/d/emmy/emmy/CURRENT/doc/data-types), all
built on a tower of [generic, extensible mathematical
operations](https://cljdoc.org/d/emmy/emmy/CURRENT/doc/basics/generics).

Scmutils is extensively used in the textbooks [The Structure and Interpretation
of Classical Mechanics][SICM] and [Functional Differential Geometry][FDG] by
G.J. Sussman and J. Wisdom.

> :wave: Need help getting started? Say hi on
> [Twitter](https://twitter.com/sritchie) or [Clojurians
> Slack](http://clojurians.net/) in
> [#emmy](https://clojurians.slack.com/archives/C01ECA9AA74).

[![Build Status](https://github.com/emmy/emmy/workflows/Clojure%20CI/badge.svg?branch=main)](https://github.com/emmy/emmy/actions?query=workflow%3A%22Clojure+CI%22)
[![License](https://img.shields.io/badge/license-GPLv3-brightgreen.svg)](https://github.com/emmy/emmy/blob/main/LICENSE)
[![Codecov branch](https://img.shields.io/codecov/c/github/emmy/emmy/main.svg?maxAge=3600)](https://codecov.io/github/emmy/emmy)
[![cljdoc badge](https://cljdoc.org/badge/emmy/emmy)](https://cljdoc.org/d/emmy/emmy/CURRENT)
[![Clojars Project](https://img.shields.io/clojars/v/emmy/emmy.svg)](https://clojars.org/emmy/emmy)

## Quickstart

> Emmy is best experienced in an interactive environment like the
> [REPL](https://clojure.org/guides/repl/introduction). We [support many
> environments](https://cljdoc.org/d/emmy/emmy/CURRENT/doc/basics/how-to-use-emmy)
> with rich support for [TeX](https://en.wikipedia.org/wiki/TeX) rendering and
> plotting.

Install Emmy into your Clojure(script) project using the instructions at
its Clojars page:

[![Clojars Project](https://img.shields.io/clojars/v/emmy/emmy.svg)](https://clojars.org/emmy/emmy)

Initialize the `emmy.env` "Batteries Included" environment at the REPL:

```clojure
(require '[emmy.env :as env])
(env/bootstrap-repl!)
```

See the [demo directory](./demo) for minimal examples of build configurations
that use the Emmy library.

Alternatively, visit the [Emmy Tutorial on
Nextjournal](https://nextjournal.com/try/samritchie/emmy) to try all of the
examples below in your browser with no setup required:

<img width="1152" alt="nje" src="https://user-images.githubusercontent.com/462255/109587851-9e1be280-7abc-11eb-9369-6d56519fb3cd.png">

Math works as expected (see
[Generics](https://cljdoc.org/d/emmy/emmy/CURRENT/doc/basics/generics)
for the full menu of operations), but notice that the numeric tower includes
[complex
numbers](https://cljdoc.org/d/emmy/emmy/CURRENT/doc/data-types/complex),
and proper ratios in ClojureScript:

```clojure
(- (* 7 (/ 1 2)) 2)
;;=> 3/2

(asin -10)
;;=> #sicm/complex [-1.5707963267948966 2.9932228461263786]
```

Symbols are interpreted as abstract complex numbers, and arithmetic on them
generates symbolic expressions. You can render these with
[`->TeX`](https://cljdoc.org/d/emmy/emmy/CURRENT/api/emmy.expression.render#->TeX)
and
[`->infix`](https://cljdoc.org/d/emmy/emmy/CURRENT/api/emmy.expression.render#->infix):

```clojure
(def render (comp ->infix simplify))

(square (sin (+ 'a 3)))
;;=> (expt (sin (+ a 3)) 2)

(render (square (sin (+ 'a 3))))
;;=> "sin²(a + 3)"
```

Use the
[`D`](https://cljdoc.org/d/emmy/emmy/CURRENT/api/emmy.calculus.derivative#D)
operator to perform [forward-mode automatic
differentiation](https://cljdoc.org/d/emmy/emmy/CURRENT/doc/calculus/automatic-differentiation)
and
[`simplify`](https://cljdoc.org/d/emmy/emmy/CURRENT/doc/simplification)
to collapse symbolic expressions into tidy form:

```clojure
((D cube) 'x)
;;=>  (+ (* x (+ x x)) (* x x))

(simplify ((D cube) 'x))
;;=> (* 3 (expt x 2))

(->infix
 (simplify ((D cube) 'x)))
;;-> "3 x²"
```

Emmy is based on the engine behind [The Structure and Interpretation of
Classical Mechanics][SICM], and has a built-in API for exploring Lagrangian and
Hamiltonian mechanics.

Define a [Lagrangian](https://en.wikipedia.org/wiki/Lagrangian_mechanics) for a
central potential `U` acting on a particle with mass `m`:

```clojure
(defn L-central-polar [m U]
  (fn [[_ [r] [rdot thetadot]]]
    (- (* 1/2 m
          (+ (square rdot)
             (square (* r thetadot))))
       (U r))))
```

and generate the two [Euler-Lagrange equations of
motion](https://en.wikipedia.org/wiki/Lagrangian_mechanics#Euler–Lagrange_equations_and_Hamilton's_principle)
for the `r` and `theta` coordinates:

```clojure
(let [potential-fn (literal-function 'U)
      L     (L-central-polar 'm potential-fn)
      state (up (literal-function 'r)
                (literal-function 'theta))]
  (render
   (((Lagrange-equations L) state) 't)))
;;=> "down(- m r(t) (Dθ(t))² + m D²r(t) + DU(r(t)), m (r(t))² D²θ(t) + 2 m r(t) Dr(t) Dθ(t))"
```

There is so much more! This is a dense library, and lots of documentation
remains to be written. Some suggested next steps, for now:

- Open up the live, interactive [Emmy tutorial on
Nextjournal](https://nextjournal.com/try/samritchie/emmy), play with the
examples above and start to explore on your own.
- Read the [Emmy Reference Manual][REFMAN] ("refman") for inspiration. All
  of the code snippets in the refman will work in the [Nextjournal
  environment](https://nextjournal.com/try/samritchie/emmy). Use the two
  together.
- Visit our [CLJDocs][CLJDOCS] page for an introduction and detailed
  documentation
- Watch Colin's ["Physics in Clojure"][PHYSICS_IN_CLOJURE] talk for an overview
  of Emmy and its implementation
- Visit the HTML version of [Structure and Interpretation of Classical
  Mechanics](https://tgvaughan.github.io/sicm/). Many of the SICM exercises have
  been worked using Emmy; they live at [this Nextjournal
  page](https://nextjournal.com/sicm).

## Background

[SICM][SICM] and [FDG][FDG] can be thought of as spiritual successors to [The
Structure and Interpretation of Computer Programs][SICP], a very influential
text—as I can attest, since carefully reading this book in my 30s changed my
life as a programmer. To see the same techniques applied to differential
geometry and physics is an irresistible lure.

Scmutils is an excellent system, but it is written in an older variant of LISP
(Scheme) and is tied to a particular implementation of Scheme—MIT/GNU Scheme.
(There is a [port to Guile][GSCM], but due to the fact that Guile does not
support MIT Scheme's [apply
hooks](https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref/Application-Hooks.html)
some glue code is required to run examples from the book in that environment.)

Having the system in Clojure offers a number of advantages. It is not necessary
to obtain or prepare a MIT/GNU Scheme executable to execute: only a Java runtime
is required. It does not require the X Window System for graphics, as MIT Scheme
does. All of the standard tooling for Java and Clojure become available, and
this is a lot compared to what we get with MIT/GNU scheme. Clojure support is
now extensive in any number of editors and IDEs. Even better, you can interact
with the system in the context of a [Jupyter notebook](./jupyter).

You can invoke the system from within Java or Javascript code or use any Java or
JS packages you like together with the mathematics system. It's my hope that
continuing this project will extend the reach of SICM and FDG by allowing
experimentation and collaboration with them in modern environments.

## Citing Emmy

To cite this repository, see the "Cite this Repository" link on the top right of
the Github page. Citation information is generated from
[`CITATION.cff`](CITATION.cff).

Here is the generated BibTeX entry:

```
@software{Ritchie_Emmy_Functional_Computer_2016},
author = {Ritchie, Sam and Smith, Colin},
license = {GPL-3.0},
month = {4},
title = {{Emmy: Functional Computer Algebra in Clojure}},
url = {https://github.com/emmy/emmy},
version = {0.23.0},
year = {2016}
```

In the above BibTeX entry, the version number is intended to be that from
[project.clj](./project.clj), and the year corresponds to the project's
open-source release.

## License

[GPL v3](LICENSE).

[CLJDOCS]: https://cljdoc.org/d/emmy/emmy/CURRENT
[SICM]: https://mitpress.mit.edu/books/structure-and-interpretation-classical-mechanics-second-edition
[FDG]: http://mitpress.mit.edu/books/functional-differential-geometry
[SICP]: http://mitpress.mit.edu/sicp/
[GSCM]: http://www.cs.rochester.edu/~gildea/guile-scmutils/
[REFMAN]: https://cljdoc.org/d/emmy/emmy/CURRENT/doc/reference-manual
[PHYSICS_IN_CLOJURE]: https://www.youtube.com/watch?v=7PoajCqNKpg

Copyright © 2016 Colin Smith
