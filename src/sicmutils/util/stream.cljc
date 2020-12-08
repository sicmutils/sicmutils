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

(ns sicmutils.util.stream
  "This namespace contains various standard sequences, as well as utilities for
  working with strict and lazy sequences."
  (:require [clojure.pprint :as pp]
            [sicmutils.generic :as g]
            [sicmutils.value :as v]))

(defn pprint
  "Realizes and pretty-prints `n` elements from the supplied sequence `xs`."
  [n xs]
  (doseq [x (take n xs)]
    (pp/pprint x)))

(defn powers
  "Returns an infinite sequence of `x * n^i`, starting with i == 0. `x` defaults
  to 1."
  ([n] (powers n 1))
  ([n x] (iterate #(* n %) x)))

(defn zeno
  "Returns an infinite sequence of x / n^i, starting with i == 0. `x` defaults to
  1."
  ([n] (zeno n 1))
  ([n x] (iterate #(/ % n) x)))

(defn scan
  "Returns a function that accepts a sequence `xs`, and performs a scan by:

  - Aggregating each element of `xs` into
  - the initial value `init`
  - transforming each intermediate result by `present` (defaults to `identity`).

  The returned sequence contains every intermediate result, after passing it
  through `present` (not `init`, though).

  This is what distinguishes a scan from a 'fold'; a fold would return the final
  result. A fold isn't appropriate for aggregating infinite sequences, while a
  scan works great.

  Arities:

  - the three-arity version takes `init` value, `f` fold function and `present`.
  - the two arity version drops `init`, and instead calls `f` with no arguments.
  The return value is the initial aggregator.
  - The 1-arity version only takes the `merge` fn and defaults `present` to
  `identity`.
    "
  [f & {:keys [present init]
        :or {present identity}}]
  (let [init (or init (f))]
    (fn [xs]
      (->> (reductions f init xs)
           (map present)
           (rest)))))

;; ## Convergence Tests
;;
;; This convergence tester comes from Gerald Sussman's "Abstraction in Numerical
;; Methods":
;; https://dspace.mit.edu/bitstream/handle/1721.1/6060/AIM-997.pdf?sequence=2
;;
;; We're planning on adding a number of these here and consolidating all of the
;; available ideas about relative and maximum tolerance so we can share (and
;; combine) them across different stream functions.

(defn- close-enuf?
  "relative closeness, transitioning to absolute closeness when we get
  significantly smaller than 1."
  [tolerance]
  (fn [h1 h2]
    (<= (g/abs (- h1 h2))
        (* 0.5 tolerance (+ 2 (g/abs h1) (g/abs h2))))))

;; I have a dream that a function like `seq-limit` could service most of the
;; numerical methods in this library. Function minimization, root finding,
;; definite integrals and numerical derivatives can all be expressed as
;; successive approximations, with convergence tests (or other stopping
;; conditions) checked and applied between iterations.
;;
;; As of 10.2020 we use this exclusively for various numerical integration
;; routines. But it has more promise than this!

(defn seq-limit
  "Accepts a sequence, iterates through it and returns a dictionary of this form:

  {:converged? <boolean>
   :terms-checked <int>
   :result <sequence element>}

  `:converged?` is true if the sequence reached convergence by passing the tests
  described below, false otherwise.

  `:terms-checked` will be equal to the number of items examined in the
  sequence.

  `:result` holds the final item examined in the sequence.

  ## Optional keyword args:

  `:convergence-fn` user-supplied function of two successive elements in `xs`
  that stops iteration and signals convergence if it returns true.

  `:fail-fn` user-supplied function of two successive elements in `xs` that
  stops iteration and signals NO convergence (failure!) if it returns true.

  `:minterms` `seq-limit` won't return until at least this many terms from the
  sequence have been processed.

  `:maxterms` `seq-limit` will return (with `:converged? false`) after
  processing this many elements without passing any other checks.

  `:tolerance` A combination of relative and absolute tolerance. defaults to
  `sqrt(machine epsilon)`."
  ([xs] (seq-limit xs {}))
  ([xs {:keys [minterms
               maxterms
               tolerance
               convergence-fn
               fail-fn]
        :or {minterms       2
             tolerance      v/sqrt-machine-epsilon
             convergence-fn (close-enuf? tolerance)}}]
   (if (empty? xs)
     {:converged? false
      :terms-checked 0
      :result        nil}
     (let [stop? (if maxterms
                   (fn [i] (>= i maxterms))
                   (constantly false))]
       (loop [[x1 & [x2 :as more]] xs
              terms-checked 1]
         (if (empty? more)
           {:converged?    false
            :terms-checked terms-checked
            :result        x1}
           (let [terms-checked (inc terms-checked)
                 converged?    (convergence-fn x1 x2)]
             (if (and (>= terms-checked minterms)
                      (or converged?
                          (stop? terms-checked)))
               {:converged?    converged?
                :terms-checked terms-checked
                :result        x2}
               (recur more terms-checked)))))))))
