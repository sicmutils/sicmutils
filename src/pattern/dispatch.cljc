;;
;; Copyright © 2021 Sam Ritchie.
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

(ns pattern.dispatch
  "NOTE that this code is based on
  https://github.com/axch/rules/blob/master/pattern-dispatch.scm."
  (:require [sicmutils.util :as u]
            [pattern.rule :as r])
  #?(:clj
     (:import (clojure.lang IFn IObj))))

;; ## Pattern Dispatch
;;
;; A pattern directed operator is a collection of rules, one of which
;; is expected to match any datum that the operator may be given.
;; The operator tries the rules in order until the first matches, and
;; returns the value given by that one; if none match, it errors out.

(declare try-rules)

(deftype PatternOperator [rules m]
  #?@(:clj
      [IObj
       (meta [_] m)
       (withMeta [_ new-m] (PatternOperator. rules new-m))

       IFn
       (invoke [_]
               (try-rules rules []))
       (invoke [_ a]
               (try-rules rules [a]))
       (invoke [_ a b]
               (try-rules rules [a b]))
       (invoke [_ a b c]
               (try-rules rules [a b c]))
       (invoke [_ a b c d]
               (try-rules rules [a b c d]))
       (invoke [_ a b c d e]
               (try-rules rules [a b c d e]))
       (invoke [_ a b c d e f]
               (try-rules rules [a b c d e f]))
       (invoke [_ a b c d e f g]
               (try-rules rules [a b c d e f g]))
       (invoke [_ a b c d e f g h]
               (try-rules rules [a b c d e f g h]))
       (invoke [_ a b c d e f g h i]
               (try-rules rules [a b c d e f g h i]))
       (invoke [_ a b c d e f g h i j]
               (try-rules rules [a b c d e f g h i j]))
       (invoke [_ a b c d e f g h i j k]
               (try-rules rules [a b c d e f g h i j k]))
       (invoke [_ a b c d e f g h i j k l]
               (try-rules rules [a b c d e f g h i j k l]))
       (invoke [_ a b c d e f g h i j k l m-arg]
               (try-rules rules [a b c d e f g h i j k l m-arg]))
       (invoke [_ a b c d e f g h i j k l m-arg n]
               (try-rules rules [a b c d e f g h i j k l m-arg n]))
       (invoke [_ a b c d e f g h i j k l m-arg n o]
               (try-rules rules [a b c d e f g h i j k l m-arg n o]))
       (invoke [_ a b c d e f g h i j k l m-arg n o p]
               (try-rules rules [a b c d e f g h i j k l m-arg n o p]))
       (invoke [_ a b c d e f g h i j k l m-arg n o p q]
               (try-rules rules [a b c d e f g h i j k l m-arg n o p q]))
       (invoke [_ a b c d e f g h i j k l m-arg n o p q r]
               (try-rules rules [a b c d e f g h i j k l m-arg n o p q r]))
       (invoke [_ a b c d e f g h i j k l m-arg n o p q r s]
               (try-rules rules [a b c d e f g h i j k l m-arg n o p q r s]))
       (invoke [_ a b c d e f g h i j k l m-arg n o p q r s t]
               (try-rules rules [a b c d e f g h i j k l m-arg n o p q r s t]))
       (invoke [_ a b c d e f g h i j k l m-arg n o p q r s t rest]
               (try-rules rules (into [a b c d e f g h i j k l m-arg n o p q r s t] rest)))
       (applyTo [_ args] (try-rules rules args))]

      :cljs
      [IMeta
       (-meta [_] m)

       IWithMeta
       (-with-meta [_ new-m] (PatternOperator. rules new-m))

       IFn
       (-invoke [_]
                (try-rules rules []))
       (-invoke [_ a]
                (try-rules rules [a]))
       (-invoke [_ a b]
                (try-rules rules [a b]))
       (-invoke [_ a b c]
                (try-rules rules [a b c]))
       (-invoke [_ a b c d]
                (try-rules rules [a b c d]))
       (-invoke [_ a b c d e]
                (try-rules rules [a b c d e]))
       (-invoke [_ a b c d e f]
                (try-rules rules [a b c d e f]))
       (-invoke [_ a b c d e f g]
                (try-rules rules [a b c d e f g]))
       (-invoke [_ a b c d e f g h]
                (try-rules rules [a b c d e f g h]))
       (-invoke [_ a b c d e f g h i]
                (try-rules rules [a b c d e f g h i]))
       (-invoke [_ a b c d e f g h i j]
                (try-rules rules [a b c d e f g h i j]))
       (-invoke [_ a b c d e f g h i j k]
                (try-rules rules [a b c d e f g h i j k]))
       (-invoke [_ a b c d e f g h i j k l]
                (try-rules rules [a b c d e f g h i j k l]))
       (-invoke [_ a b c d e f g h i j k l m-arg]
                (try-rules rules [a b c d e f g h i j k l m-arg]))
       (-invoke [_ a b c d e f g h i j k l m-arg n]
                (try-rules rules [a b c d e f g h i j k l m-arg n]))
       (-invoke [_ a b c d e f g h i j k l m-arg n o]
                (try-rules rules [a b c d e f g h i j k l m-arg n o]))
       (-invoke [_ a b c d e f g h i j k l m-arg n o p]
                (try-rules rules [a b c d e f g h i j k l m-arg n o p]))
       (-invoke [_ a b c d e f g h i j k l m-arg n o p q]
                (try-rules rules [a b c d e f g h i j k l m-arg n o p q]))
       (-invoke [_ a b c d e f g h i j k l m-arg n o p q r]
                (try-rules rules [a b c d e f g h i j k l m-arg n o p q r]))
       (-invoke [_ a b c d e f g h i j k l m-arg n o p q r s]
                (try-rules rules [a b c d e f g h i j k l m-arg n o p q r s]))
       (-invoke [_ a b c d e f g h i j k l m-arg n o p q r s t]
                (try-rules rules [a b c d e f g h i j k l m-arg n o p q r s t]))
       (-invoke [_ a b c d e f g h i j k l m-arg n o p q r s t rest]
                (try-rules rules (concat [a b c d e f g h i j k l m-arg n o p q r s t] rest)))]))

(defn operator? [op]
  (instance? PatternOperator op))

(defn- get-rules [op]
  (.-rules ^PatternOperator op))

(defn pattern-dispatch
  "Constructor!"
  [& rules]
  (->PatternOperator (into [] rules) nil))

(defn attach-rule [op rule]
  {:pre [(operator? op)]}
  (->PatternOperator (conj (get-rules op) rule)
                     (meta op)))

(defn try-rules
  ([rules data]
   (letfn [(fail []
             (u/illegal
              (str "No applicable operations: " rules data)))]
     (try-rules rules data fail)))
  ([rules data fail]
   (let [result ((r/rule-list rules) data r/sentinel)]
     (if (= result r/sentinel)
       (fail)
       result))))

(comment
  (define-test (quad-test)
    (interaction
     (define quad
       (pattern-dispatch
        (rule
         `((? a) (? b) (? c) (? x))
         (+ (* a (expt x 2))
            (* b x)
            c))

        (rule
         `((? a) (? x) (? x) + (? b) (? x) + (? c))
         (+ (* a (expt x 2))
            (* b x)
            c))))

     (quad 1 2 3 4)
     (produces 27)

     (quad 1 4 4 '+ 2 4 '+ 3)
     (produces 27)))

  (define-test (frob-test)
    (interaction
     (define frob
       (pattern-dispatch))

     (attach-rule! frob
                   (rule
                    '(a (?? x) (?? y) (?? x) c)
                    (and (<= (length y) 2)
	                       y)))

     (apply frob '(a b b b b b b c))
     (produces '(b b))))

  (def factorial
    (pattern-dispatch
     (r/rule [0] 1)
     (r/rule [(:? n pos?)]
             (fn [frame]
               (* (frame 'n) (factorial (dec (frame 'n))))))))

  (define-test (factorial-1)
    (interaction
     (define factorial (make-pattern-operator '()))

     (attach-rule! factorial (rule '(0) 1))

     (attach-rule! factorial
		               (rule `((? n ,positive?))
			                   (* n (factorial (- n 1)))))

     (factorial 10)
     (produces 3628800)))

  (define-test (factorial-2)
    (interaction
     (define factorial (make-pattern-operator '()))

     (attach-rule! factorial
		               (make-rule '(0) (lambda () 1)))

     (attach-rule! factorial
		               (make-rule '((? n))
			                        (lambda (n) (* n (factorial (- n 1))))))

     (factorial 10)
     (produces 3628800))))
