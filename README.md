# math

A reimplementation of the Scmutils system for math and physics
investigations in the Clojure language. Scmutils is extensively used
in the textbooks
[The Structure and Interpretation of Classical Mechanics][SICM] and
[Functional Differential Geometry][FDG] by G.J. Sussman and J. Wisdom.

Scmutils is an excellent system, but it is written in an older variant
of LISP (Scheme) and is tied to a particular implementation of
Scheme--MIT/GNU Scheme--for a variety of reasons, and to my knowledge
has never successfully been executed on any other system.

Having the system in Clojure offers a number of advantages. It is not
necessary to obtain or prepare a MIT/GNU Scheme executable to execute:
only a Java runtime is required. It does not require the X Window
System for graphics, as MIT Scheme does. All of the standard tooling
for Java and Clojure become available, and this is a lot compared to
what get with MIT/GNU scheme: for example, in MIT you pretty much have
to use an old clone of Emacs called edwin to interact with it.
Clojure support is now extensive in any number of editors and IDEs.

You can invoke the system from within Java code or use any Java
packages you like together with the mathematics system. It's my hope
that continuing this project will extend the reach of SICM and PDG by
allowing experimentation with them in modern environments.

## Status

Rather than just quasi-mechanically translate the Scheme to Clojure, I
have studied the implementation of the system before bringing it to
Clojure, and have used TDD throughout the project (which turned out to
be absolutely essential as I considered various approaches to problems
posed by the Scheme code base.  At this writing there are over 500
unit tests, and there easily ought to be twice as many.

The implementation is far from complete. My goal was to create a
system that could execute the example code in SICM and PDG directly
from the book, to the extent possible. I started with SICM, as the
requirements seemed the lesser; PDG code is written at a higher level
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
(define ((L-central-polar m U) local)
  (let ((q (coordinate local))
        (qdot (velocity local)))
    (let ((r (ref q 0)) (phi (ref q 1))
          (rdot (ref qdot 0)) (phidot (ref qdot 1)))
      (- (* 1/2 m
           (+ (square rdot)
              (square (* r phidot))) )
              (U r)))))```

```clojure
(defn L-central-polar [m U]
  (fn [[_ [r] [rdot φdot]]]
    (- (* 1/2 m
          (+ (square rdot)
             (square (* r φdot))))
       (U r))))```

We can see a few things from this example. `L-central-polar` wants to compute
a Lagrangian for a point mass `m` in a potential field `U`. In Scheme, it's
possible to specify currying at the function site: `(L-central-polar m U)`
returns a function of the `local` tuple (a sequence of time, generalized
coordinates, and generalized velocities). We don't have that syntax in Clojure,
but instead have something even more useful: argument destructuring: we can
pick out exactly the coordinates of the local tuple components directly.


## Usage

FIXME: explanation

    $ java -jar math-0.1.0-standalone.jar [args]

## Options

FIXME: listing of options this app accepts.

## Examples

...

### Bugs

...

### Any Other Sections
### That You Think
### Might be Useful

## License

[SICM]: http://mitpress.mit.edu/books/structure-and-interpretation-classical-mechanics
[FDG]: http://mitpress.mit.edu/books/functional-differential-geometry

Copyright © 2014 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
