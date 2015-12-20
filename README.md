# math 
[![Build Status](https://travis-ci.org/littleredcomputer/math.svg?branch=master)](https://travis-ci.org/littleredcomputer/math) [![License](https://img.shields.io/badge/license-GPLv3-brightgreen.svg)](https://github.com/littleredcomputer/math/blob/master/LICENSE)



A reimplementation of the Scmutils system for math and physics
investigations in the Clojure language. Scmutils is extensively used
in the textbooks
[The Structure and Interpretation of Classical Mechanics][SICM] and
[Functional Differential Geometry][FDG] by G.J. Sussman and J. Wisdom.

These books can be thought of as spiritual successors to
[The Structure and Interpretation of Computer Programs][SICP], a very
influential text--as I can attest, since carefully reading this book
in my 30s changed my life as a programmer. To see the same techniques
applied to differential geometry and physics is an irresistible lure.

Scmutils is an excellent system, but it is written in an older variant
of LISP (Scheme) and is tied to a particular implementation of
Scheme--MIT/GNU Scheme--for a variety of reasons, and to my knowledge
has never successfully been executed on any other LISP-like system.

[I have seen [a port to guile][guile-scmutils], haven't examined it extensivey yet.  *AG* 20/12/15]

Having the system in Clojure offers a number of advantages. It is not
necessary to obtain or prepare a MIT/GNU Scheme executable to execute:
only a Java runtime is required. It does not require the X Window
System for graphics, as MIT Scheme does. All of the standard tooling
for Java and Clojure become available, and this is a lot compared to
what get with MIT/GNU scheme: for example, in MIT you pretty much have
to use an old clone of Emacs called edwin to interact with it.

[MIT-Scheme with SCMUTILS will run happily under Emacs, Emacs24, XEmacs, etc. 
as an inferior process, offering an acceptable REPL.  
I assume edwin could also be thus configured.  *AG* 20/12/15]

Clojure support is now extensive in any number of editors and IDEs.
The MIT/Scheme distribution (apparently) must be rebuilt if you wish
to hack on the Scheme code providing the mathematics implementation;
whereas in this Clojure implementation, you can test out new ideas
and enhancements directly in the REPL (or a REPL server) at will.

You can invoke the system from within Java code or use any Java
packages you like together with the mathematics system. It's my hope
that continuing this project will extend the reach of SICM and PDG by
allowing experimentation and collaboration with them in modern
environments.

## Status

Rather than just quasi-mechanically translate the Scheme to Clojure, I
have studied the implementation of the system before bringing it to
Clojure, and have used TDD throughout the project (which turned out to
be absolutely essential as I considered various approaches to problems
posed by the Scheme code base). At this writing there are over 500
unit tests, and there easily ought to be twice as many.

The implementation is far from complete. My goal was to create a
system that could execute the example code in SICM and FDG directly
from the book, to the extent possible. I started with SICM, as the
requirements seemed the lesser; FDG code is written at a higher level
of abstraction. Starting with nothing, I tried to push the frontier of
the new code ever closer to approaching being able to execute the book
examples.

Naturally, this was harder than I thought. I began with the generic
operation system. Fortunately the
[lecture notes](http://groups.csail.mit.edu/mac/users/gjs/6.945/) GJS
provides for his 6.945 class in Symbolic Programming provided some
clues as to where to begin. With this implemented, some simple algebra
over symbols was possible. Handing structured objects (up and down
tuples, the system's analogs for contra- and covariant vectors) was
fun. What was less fun was understanding how the simplifier works with
the polynomial systems: that took a while! Finally I got
differentiation working, and then some of the book examples began to
work.

Much remains to be done (see below).

## What's "working" now

```scheme
; Scheme
(define ((L-central-polar m U) local)
  (let ((q (coordinate local))
        (qdot (velocity local)))
    (let ((r (ref q 0)) (phi (ref q 1))
          (rdot (ref qdot 0)) (phidot (ref qdot 1)))
      (- (* 1/2 m
           (+ (square rdot)
              (square (* r phidot))) )
         (U r)))))
```

```clojure
; Clojure
(defn L-central-polar [m U]
  (fn [[_ [r _] [rdot φdot]]] ;; [r _] corresponds to [r φ]
    (- (* 1/2 m
          (+ (square rdot)
             (square (* r φdot))))
       (U r))))
```

We can see a few things from this example. `L-central-polar` wants to
compute a Lagrangian for a point mass `m` in a potential field `U`. In
Scheme, it's possible to specify currying at the site of a function's
definition: `(L-central-polar m U)` returns a function of the `local`
tuple (a sequence of time, generalized coordinates, and generalized
velocities). We don't have that syntax in Clojure, but instead have
something even more useful: argument destructuring. We can pick out
exactly the coordinates we want out of the local tuple components
directly.

While function definitions cannot be typed directly from the book,
function applications in Clojure and Scheme are the same. The
following works in both systems:

```clojure
(((Lagrange-equations (L-central-polar 'm (literal-function 'U)))
  (up (literal-function 'r)
      (literal-function 'φ)))
  't)
```
yielding

```clojure
(down
 (+
  ((D U) (r t))
  (* -1N (r t) m (expt ((D φ) t) 2))
  (* (((expt D 2) r) t) m))
 (+
  (* (((expt D 2) φ) t) m (expt (r t) 2))
  (* 2N (r t) ((D r) t) ((D φ) t) m)))
```

Which, modulo a few things, is what Scmutils would give. From later
in [SICM][SICM] (pp. 81-2) we have, in Scheme:

```scheme
(define ((T3-spherical m) state)
  (let ((t (time state))
        (q (coordinate state))
        (qdot (velocity state)))
    (let ((r (ref q 0))
          (theta (ref q 1))
          (phi (ref q 2))
          (rdot (ref qdot 0))
          (thetadot (ref qdot 1))
          (phidot (ref qdot 2)))
      (* 1/2 m
         (+ (square rdot)
            (square (* r thetadot))
            (square (* r (sin theta) phidot)))))))

(define (L3-central m Vr)
  (define (Vs state)
    (let ((r (ref (coordinate state) 0)))
      (Vr r)))
  (- (T3-spherical m) Vs))

(((partial 1) (L3-central ’m (literal-function ’V)))
  (up ’t
      (up ’r ’theta ’phi)
      (up ’rdot ’thetadot ’phidot)))
```

And in Clojure, using a couple of simplifying definitions:

```clojure
(def V (literal-function 'V))
(def spherical-state (up 't
                         (up 'r 'θ 'φ)
                         (up 'rdot 'θdot 'φdot)))
(defn T3-spherical [m]
  (fn [[t [r θ φ] [rdot θdot φdot]]]
    (* 1/2 m (+ (square rdot)
                (square (* r θdot))
                (square (* r (sin θ) φdot))))))

(defn L3-central [m Vr]
  (let [Vs (fn [[_ [r]]] (Vr r))]
    (- (T3-spherical m) Vs)))

(((pd 1) (L3-central 'm V)) spherical-state)
```
yielding
```clojure
(down
 (* m rdot)
 (* m θdot (expt r 2))
 (* m φdot (expt r 2) (expt (sin θ) 2)))
```

Which again agrees with Scmutils modulo notation. (These results are
examples of "down tuples", or covariant vectors, since they represent
derivatives of objects in primal space.) The partial derivative operation
is called `partial` in Scmutils, but `pd` in Clojure (where `partial`
has already been defined to mean partial function application). Given
the frequent use of functions returning functions of state in SICM, I've
decided to leave the Clojure definition of `partial` exposed.

## What's not there yet

### Rational function and rule-based simplification

The Scmutils simplifier has three polynomial libraries, FPF, PCF, and
RCF. The first, FPF or "flat polynomial form", is used to analyze an
expression from the point of view of a polynomial over a commutative ring.
PCF ("polynomial canonical form") works with polynomials over fields, and RCF
("rational canonical form") works with quotients of PCFs. These polynomial
libraries are used as simplification engines, especially for the grouping
of like terms. The Clojure code only implements FPF at this point;
expressions run through this simplifier are considerably simpler than the
raw expressions computed by the system would be, but there are many
simplifications (like canceling in fractions) that will require the RCF
form at some point.

While the rule simplifier has been implemented, it has not been linked to
the REPL and FPF-based simplifications: this will probably be the next
thing I work on.

### TeX

Scmutils can export its expressions in TeX format; this is very handy,
but I haven't started that work yet as the necessary extensions to the
simplification library to make that useful haven't happened yet.

### Numerical Methods

The barest minimum of numerical methods are provided; enough to support
the investigation of path action minimization early in Chapter 1. For
one-variable integration, I use old-fashioned Simpson's rule; the Brent
univariate minimizer for functions of one variable is provided as well.
The missing pieces are a multivariate minimizer (which is only used once
in the book) and a numerical ODE solver. The latter is more central to the
effort of getting all the code in the book working, so it would take
priority.

### Matrix methods

The current implementation of arithmetic on `up` and `down` tuples is
fairly complete; they can be nested and all the arithmetic rules for
them are implemented except for those where nested structures are to
be treated as matrices (i.e., for finding inverses, transposes, etc.)

Scmutils provides a separate matrix "type" for these activities; in this
work my plan is to see if it can all be done without introducing a separate
type, forcing all matrix work to be "variance-labeled" by the use of
`up` and `down` correctly at each level.

### General operators and functions

In Scmutils, any object can be made callable by using a MIT/GNU Scheme
feature called `apply-hook`s. They are used extensively. In the Clojure
implementation, certain objects are defined to extend the interface
`IFn` to achieve this. This works, but it is different enough to what
Scmutils does that I have not carried the technique out very far, since
I'm not certain I've hit on exactly the right way to organize these
objects. That said, univariate functions and operators upon them work
well enough, but I have not done a lot of work for functions of higher
arity: caveat emptor.

### Coordinates, patches, k-forms and all that

I think the code here has the potential to quickly evolve to cover the
techniques of [PDG], but at the moment, I have not gotten to the point
of implementing any of the groundwork for that yet, as working through
[SICM] has been challenge enough for me.

### Derivatives of nested functions

Or, what is described as "Alexey's Amazing Bug" in the Scmutils source
code (and further described by Oleksandr Manzyuk [here][OM]). This should
be straightforward to fix but hasn't been necessary for the work so far.

### Quaternions, Power Series...

The Scmutils library is vast, and I don't pretend to have covered
anywhere near all of it. The breadth-first approach I have used to get
the textbook examples working has left many corners of the source
library unexplored as of this writing.

## The experience of Clojure

This is my first real attempt at a project in Clojure, so I can't say
that everything I've done is idiomatic or the best possible. On the
other hand, I've written a fair amount of Scheme, so it has been a lot
of fun to consider how to best use Clojure's strengths in implementing
this. Of course, none of this project should be considered original
work of my own; it is the brainchild of G.J. Sussman and his
collaborators. But allow me take a moment to describe some of the
pleasant surprises I experienced while doing this.

First of all, a rich set of persistent data structures makes all the
difference. What few examples of mutability in Scmutils where present
have all been removed. Many instances of using association lists and
other `O(n)` data structures have been replaced by maps and sets, and
the sorted variants of those.

Using `defrecord` and `deftype` simplified the handling of a lot of
Scmutils objects that were simply the `cons` of a type tag and a
value.  Such types can implement `IFn` when they need to be callable,
allowing me to completely dodge the `apply-hook` technique used in
MIT/GNU Scheme, which is one of the main reasons the code would not
have been easy to port to other variants of Scheme, let alone any
other flavor of LISP.

The Scmutils code is essentially a monolith, given that Scheme has no
module scoping. This Clojure implementation partitions the codebase
into a variety of namespaces, with unit tests for each. This adds a
bit of sanity to the code absent in the original, where it is not easy
to see exactly how the user's environment is constructed. Clojure's
namespace facility allows us to prepare the environment in which
primitive operations like `+` and `*` are replaced with their
namespace-qualified generic equivalents exactly at the point where
this is necessary; but also allows the generic operations to be mixed
with the native operations when this is useful.

## Running the code

Installation is simple if you have leiningen; this tool will arrange
to retrieve everything else you need. On Mac OS, for example,

~~~ sh
$ brew install leiningen
~~~

ought to get you started. Clone the repo. Then there are several ways
you can run the code. For example, to run the demonstration script in
`demo.clj`, you can:

~~~ sh
$ lein run -m math.repl < demo.clj
~~~

If you want it to run faster, you can

~~~ sh
$ lein uberjar
$ java -jar target/uberjar/math-0.0.1-SNAPSHOT-standalone.jar < demo.clj
~~~

To run the test suite:

~~~ sh
$ lein test
~~~

## License

The work this is based on GPL code, and so carries the GPL v3 license.

[SICM]: http://mitpress.mit.edu/books/structure-and-interpretation-classical-mechanics
[FDG]: http://mitpress.mit.edu/books/functional-differential-geometry
[SICP]: http://mitpress.mit.edu/sicp/
[OM]: http://oleksandrmanzyuk.files.wordpress.com/2012/04/paper.pdf
[guile-scmutils]: http://www.cs.rochester.edu/~gildea/guile-scmutils/

Copyright © 2014 Colin Smith
