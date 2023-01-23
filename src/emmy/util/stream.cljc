#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.util.stream
  "This namespace contains various standard sequences, as well as utilities for
  working with strict and lazy sequences."
  (:require [clojure.pprint :as pp]
            [emmy.generic :as g]
            [emmy.value :as v]))

(defn seq-print
  "Realizes, simplifies and prints `n` elements from the supplied sequence `xs`."
  [n xs]
  (doseq [x (take n xs)]
    (prn (g/simplify x))))

(defn pprint
  "Realizes, simplifies and pretty-prints `n` elements from the supplied sequence
  `xs`."
  [n xs]
  (doseq [x (take n xs)]
    (pp/pprint
     (g/simplify x))))

(defn powers
  "Returns an infinite sequence of `x * n^i`, starting with `i == 0`. `x` defaults
  to 1."
  ([n] (powers n 1))
  ([n x] (iterate #(* n %) x)))

(defn zeno
  "Returns an infinite sequence of `x / n^i`, starting with `i == 0`. `x` defaults
  to 1."
  ([n] (zeno n 1))
  ([n x] (iterate #(/ % n) x)))

(defn vector:generate
  "Generates a new vector of length `n` by applying the function `f` to integers
  in the range $[0,n)$."
  [n f]
  (mapv f (range n)))

(defn separatev
  "Returns a pair of vectors:

  - the first contains the items in coll for which (pred item) returns true
  - the second contains the items for which (pred item) returns false

  pred must be free of side-effects."
  [pred coll]
  (let [[ts fs] (reduce (fn [[t f] o] (if (pred o)
                                       [(conj! t o) f]
                                       [t (conj! f o)]))
                        [(transient []) (transient [])]
                        coll)]
    [(persistent! ts) (persistent! fs)]))

;; ## Convergence Tests
;;
;; This convergence tester comes from Gerald Sussman's "Abstraction in Numerical
;; Methods":
;; https://dspace.mit.edu/bitstream/handle/1721.1/6060/AIM-997.pdf?sequence=2
;;
;; We're planning on adding a number of these here and consolidating all of the
;; available ideas about relative and maximum tolerance so we can share (and
;; combine) them across different stream functions.

(defn ^:no-doc close-enuf?
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
               convergence-fn]
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
