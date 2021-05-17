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

(ns sicmutils.rational-function
  #?(:clj
     (:refer-clojure
      :rename {denominator core-denominator
               numerator core-numerator}))
  (:require [clojure.set :as set]
            [sicmutils.complex :refer [complex?]]
            [sicmutils.expression.analyze :as a]
            [sicmutils.expression :as x]
            [sicmutils.function :as f]
            [sicmutils.generic :as g]
            [sicmutils.numsymb :as sym]
            [sicmutils.polynomial :as p]
            [sicmutils.polynomial.gcd :as poly]
            [sicmutils.ratio :as r]
            [sicmutils.util :as u]
            [sicmutils.util.aggregate :as ua]
            [sicmutils.value :as v])
  #?(:clj
     (:import (clojure.lang AFn IFn IObj))))

(declare evaluate rf:=)

;; TODO check arity on CONSTRUCTION; make sure that either `u` or `v` is a
;; scalar AND we match the other, or that we are passing arities. But you can
;; totally pass scalars as either side.
(deftype RationalFunction [arity u v m]
  v/Value
  (zero? [_] (v/zero? u))
  (one? [_] (and (v/one? u) (v/one? v)))
  (identity? [_] (and (v/identity? u) (v/one? v)))

  (zero-like [_]
    (RationalFunction. arity (v/zero-like u) (v/one-like v) m))

  (one-like [_]
    (RationalFunction. arity (v/one-like u) (v/one-like v) m))

  (identity-like [_]
    (RationalFunction. arity (v/identity-like u) (v/one-like v) m))

  (freeze [_] (list '/ (v/freeze u) (v/freeze v)))
  (kind [_] ::rational-function)

  f/IArity
  ;; TODO we CAN actually evaluate this thing with less... so it's always really
  ;; between 0 and arity, right??
  (arity [_] [:exactly arity])

  #?@(:clj
      [Object
       (toString [p] (str u " : " v))
       (equals [this that] (rf:= this that))

       IObj
       (meta [_] m)
       (withMeta [_ m] (RationalFunction. arity u v m))

       IFn
       (invoke [this a]
               (evaluate this [a]))
       (invoke [this a b]
               (evaluate this [a b]))
       (invoke [this a b c]
               (evaluate this [a b c]))
       (invoke [this a b c d]
               (evaluate this [a b c d]))
       (invoke [this a b c d e]
               (evaluate this [a b c d e]))
       (invoke [this a b c d e f]
               (evaluate this [a b c d e f]))
       (invoke [this a b c d e f g]
               (evaluate this [a b c d e f g]))
       (invoke [this a b c d e f g h]
               (evaluate this [a b c d e f g h]))
       (invoke [this a b c d e f g h i]
               (evaluate this [a b c d e f g h i]))
       (invoke [this a b c d e f g h i j]
               (evaluate this [a b c d e f g h i j]))
       (invoke [this a b c d e f g h i j k]
               (evaluate this [a b c d e f g h i j k]))
       (invoke [this a b c d e f g h i j k l]
               (evaluate this [a b c d e f g h i j k l]))
       (invoke [this a b c d e f g h i j k l m-arg]
               (evaluate this [a b c d e f g h i j k l m-arg]))
       (invoke [this a b c d e f g h i j k l m-arg n]
               (evaluate this [a b c d e f g h i j k l m-arg n]))
       (invoke [this a b c d e f g h i j k l m-arg n o]
               (evaluate this [a b c d e f g h i j k l m-arg n o]))
       (invoke [this a b c d e f g h i j k l m-arg n o p]
               (evaluate this [a b c d e f g h i j k l m-arg n o p]))
       (invoke [this a b c d e f g h i j k l m-arg n o p q]
               (evaluate this [a b c d e f g h i j k l m-arg n o p q]))
       (invoke [this a b c d e f g h i j k l m-arg n o p q r]
               (evaluate this [a b c d e f g h i j k l m-arg n o p q r]))
       (invoke [this a b c d e f g h i j k l m-arg n o p q r s]
               (evaluate this [a b c d e f g h i j k l m-arg n o p q r s]))
       (invoke [this a b c d e f g h i j k l m-arg n o p q r s t]
               (evaluate this [a b c d e f g h i j k l m-arg n o p q r s t]))
       (invoke [this a b c d e f g h i j k l m-arg n o p q r s t rest]
               (evaluate this [a b c d e f g h i j k l m-arg n o p q r s t rest]))
       (applyTo [this xs] (AFn/applyToHelper this xs))]

      :cljs
      [Object
       (toString [p] (str u " : " v))

       IEquiv
       (-equiv [this that] (rf:= this that))

       IMeta
       (-meta [_] m)

       IWithMeta
       (-with-meta [_ m] (RationalFunction. arity u v m))

       IFn
       (-invoke [this a]
                (evaluate this [a]))
       (-invoke [this a b]
                (evaluate this [a b]))
       (-invoke [this a b c]
                (evaluate this [a b c]))
       (-invoke [this a b c d]
                (evaluate this [a b c d]))
       (-invoke [this a b c d e]
                (evaluate this [a b c d e]))
       (-invoke [this a b c d e f]
                (evaluate this [a b c d e f]))
       (-invoke [this a b c d e f g]
                (evaluate this [a b c d e f g]))
       (-invoke [this a b c d e f g h]
                (evaluate this [a b c d e f g h]))
       (-invoke [this a b c d e f g h i]
                (evaluate this [a b c d e f g h i]))
       (-invoke [this a b c d e f g h i j]
                (evaluate this [a b c d e f g h i j]))
       (-invoke [this a b c d e f g h i j k]
                (evaluate this [a b c d e f g h i j k]))
       (-invoke [this a b c d e f g h i j k l]
                (evaluate this [a b c d e f g h i j k l]))
       (-invoke [this a b c d e f g h i j k l m-arg]
                (evaluate this [a b c d e f g h i j k l m-arg]))
       (-invoke [this a b c d e f g h i j k l m-arg n]
                (evaluate this [a b c d e f g h i j k l m-arg n]))
       (-invoke [this a b c d e f g h i j k l m-arg n o]
                (evaluate this [a b c d e f g h i j k l m-arg n o]))
       (-invoke [this a b c d e f g h i j k l m-arg n o p]
                (evaluate this [a b c d e f g h i j k l m-arg n o p]))
       (-invoke [this a b c d e f g h i j k l m-arg n o p q]
                (evaluate this [a b c d e f g h i j k l m-arg n o p q]))
       (-invoke [this a b c d e f g h i j k l m-arg n o p q r]
                (evaluate this [a b c d e f g h i j k l m-arg n o p q r]))
       (-invoke [this a b c d e f g h i j k l m-arg n o p q r s]
                (evaluate this [a b c d e f g h i j k l m-arg n o p q r s]))
       (-invoke [this a b c d e f g h i j k l m-arg n o p q r s t]
                (evaluate this [a b c d e f g h i j k l m-arg n o p q r s t]))
       (-invoke [this a b c d e f g h i j k l m-arg n o p q r s t rest]
                (evaluate this [a b c d e f g h i j k l m-arg n o p q r s t rest]))

       IPrintWithWriter
       (-pr-writer
        [x writer _]
        (write-all writer
                   "#object[sicmutils.structure.RationalFunction \""
                   (.toString x)
                   "\"]"))]))

(do (ns-unmap 'sicmutils.rational-function '->RationalFunction)
    (defn ->RationalFunction
      "Positional factory function for [[RationalFunction]].

  The final argument `m` defaults to nil if not supplied."
      ([arity u v]
       (RationalFunction. arity u v nil))
      ([arity u v m]
       (RationalFunction. arity u v m))))

(defn rational-function?
  "Returns true if the supplied argument is an instance of [[RationalFunction]],
  false otherwise.

  TODO make a `bare-rf?` that does this, then `rational-function?` that is true
  if it's this OR a polynomial. OR do that with the dispatch hierarchy.

  TODO should we actually do the derive thing? A coef IS a polynomial?? test!"
  [r]
  (instance? RationalFunction r))

(defn bare-arity [^RationalFunction rf]
  (.-arity rf))

;; TODO should these return identity and 1? or... should the ACTUAL numerator
;; and denominator functions work here, the same ones as work for ratio??

(defn numerator
  "Returns the numerator of the supplied [[RationalFunction]] instance `rf`.

  TODO handle polynomial too??"
  [^RationalFunction rf]
  (.-u rf))

(defn denominator
  "Returns the denominator of the supplied [[RationalFunction]] instance `rf`.

  TODO handle polynomial too??"
  [^RationalFunction rf]
  (.-v rf))

(defn rf:=
  "TODO check the equal case where we have a 1 in the denom."
  [^RationalFunction this that]
  (cond (instance? RationalFunction that)
        (let [that ^RationalFunction that]
          (and (= (bare-arity this) (bare-arity that))
               (v/= (numerator this) (numerator that))
               (v/= (denominator this) (denominator that))))

        (v/one? (denominator this))
        (v/= (numerator this) that)

        :else false))

(defn- rf:arity
  "TODO verify that this is what we want..."
  [u v]
  (let [ua (p/arity u)
        va (p/arity v)]
    (cond (p/coeff? u) va
          (p/coeff? v) ua
          (= ua va) ua
          :else (u/illegal (str "Unequal arities: " u ", " v)))))

(defn- coef-sgn [x]
  (cond (v/real? x)
        (if (g/negative? x) -1 1)
        ;; This is a kludge, needed so that rational
        ;; functions can be canonicalized, even if
        ;; coefficients are complex.
        (complex? x)
        (if (g/negative? (g/real-part x)) -1 1)

        :else 1))

(defn- make-reduced
  "NOTE: IF you've already reduced it yourself, call this."
  [arity u v]
  (if (v/one? v)
    u
    (->RationalFunction arity u v)))

(defn make
  "Make the fraction of the two polynomials p and q, after dividing
  out their greatest common divisor."
  [u v]
  (when (v/zero? v)
    (u/arithmetic-ex "Can't form rational function with zero denominator"))
  (let [xform (comp (distinct)
                    (filter r/ratio?)
                    (map r/denominator))
        coefs  (concat
                (p/coefficients v)
                (p/coefficients u))
        factor (transduce xform (completing g/lcm) 1 coefs)
        factor (if (= 1 (coef-sgn
                         (p/lead-coefficient v)))
                 factor
                 (g/negate factor))
        [u' v'] (if (v/one? factor)
                  [u v]
                  [(g/* factor u)
                   (g/* factor v)])
        g   (poly/gcd u' v')
        u'' (p/evenly-divide u' g)
        v'' (p/evenly-divide v' g)]
    (make-reduced (rf:arity u'' v'') u'' v'')))

;; ## RF Arithmetic
;;
;; Rational arithmetic is from Knuth vol 2 section 4.5.1
;;
;; The notation here is from Knuth (p. 291). In various places we take the gcd
;; of two polynomials and then use quotient to reduce those polynomials.

(defn- binop
  "Returns a function of two rf, poly or coefficient args... TODO describe args.
  This is mostly for building a janky version of the generic fn.

  THIS is here for accumulations, if we drop down to coef arithmetic."
  [u*v u*rf rf*v rf*rf]
  (fn [u v]
    (if (rational-function? u)
      (if (rational-function? v)
	      (rf*rf u v)
	      (rf*v u v))
      (if (rational-function? v)
	      (u*rf u v)
	      (u*v u v)))))

(defn rf:+
  "Add the [[RationalFunction]] instances `r` and `s`."
  [r s]
  {:pre [(rational-function? r)
         (rational-function? s)
         (= (bare-arity r) (bare-arity s))]}
  (let [a  (bare-arity r)
        u  (numerator r)
        u' (denominator r)
        v  (numerator s)
        v' (denominator s)]
    (if (v/= u' v')
	    (let [n (p/poly:+ u v)
            g (poly/gcd u' n)]
	      (if (v/one? g)
		      (make-reduced a n u')
		      (make-reduced a
                        (p/evenly-divide n g)
                        (p/evenly-divide u' g))))
	    (let [d1 (poly/gcd u' v')]
	      (if (v/one? d1)
		      (make-reduced a
                        (p/poly:+ (p/poly:* u v')
                                  (p/poly:* u' v))
			                  (p/poly:* u' v'))
		      (let [u':d1 (p/evenly-divide u' d1)
                v':d1 (p/evenly-divide v' d1)
			          t (p/poly:+ (p/poly:* u v':d1)
				                    (p/poly:* u':d1 v))]
		        (if (v/zero? t)
		          0
		          (let [d2 (poly/gcd t d1)]
                ;; TODO this branch seems a little pedantic. Check and remove IF
                ;; in fact evenly-divide can easily catch this and be fast.
			          (if (v/one? d2)
			            (make-reduced a t (p/poly:* u':d1 v'))
			            (make-reduced a
			                          (p/evenly-divide t d2)
			                          (p/poly:* u':d1
				                                  (p/evenly-divide v' d2))))))))))))

(defn rf+other
  "Add a rational function to a polynomial."
  [u v]
  {:pre [(rational-function? u)]}
  (if (v/zero? v)
    u
    (let [n (numerator u)
          d (denominator u)]
      (make (p/poly:+ n (p/poly:* d v))
            d))))

(defn other+rf
  "Add a rational function to a polynomial.

  TODO these little guards continue to feel weird. Why not check both sides?"
  [u v]
  {:pre [(rational-function? v)]}
  (if (v/zero? u)
    v
    (let [n (numerator v)
          d (denominator v)]
      (make (p/poly:+ (p/poly:* u d) n)
            d))))

(def ^{:private true
       :doc "TODO figure out where to stash this."}
  full-add
  (binop p/poly:+ other+rf rf+other rf:+))

(defn negate [r]
  (if (rational-function? r)
    (->RationalFunction (bare-arity r)
                        (p/negate (numerator r))
                        (denominator r))
    (p/negate r)))

(defn rf:- [r s]
  (rf:+ r (negate s)))

(defn rf-other [u v]
  {:pre [(rational-function? u)]}
  (if (v/zero? v)
    u
    (let [n (numerator u)
          d (denominator u)]
      (make (p/poly:- n (p/poly:* d v))
            d))))

(defn other-rf [u v]
  {:pre [(rational-function? v)]}
  (if (v/zero? u)
    (negate v)
    (let [n (numerator v)
          d (denominator v)]
      (make (p/poly:- (p/poly:* u d) n)
            d))))

;; TODO continue here.

#_
(fn [u u' v v']
  (let [d1 (poly/gcd u v')
	      d2 (poly/gcd u' v)]
	  (if (v/one? d1)
	    (if (v/one? d2)
		    (make-rcf (p/poly:* u v) (p/poly:* u' v'))
		    (make-rcf (p/poly:* u (p/evenly-divide v d2))
			            (p/poly:* (p/evenly-divide u' d2) v')))
	    (if (v/one? d2)
		    (make-rcf (p/poly:* (p/evenly-divide u d1) v)
			            (p/poly:* u' (p/evenly-divide v' d1)))
		    (make-rcf (p/poly:* (p/evenly-divide u d1)
				                    (p/evenly-divide v d2))
			            (p/poly:* (p/evenly-divide u' d2)
				                    (p/evenly-divide v' d1)))))))

(defn rf:* [r s]
  {:pre [(rational-function? r)
         (rational-function? s)
         (= (bare-arity r) (bare-arity s))]}
  (let [a (bare-arity r)
        u (numerator r)
        u' (denominator r)
        v (numerator s)
        v' (denominator s)]
    (cond (v/zero? r) r
          (v/zero? s) s
          (v/one? r) s
          (v/one? s) r
          :else (let [d1 (poly/gcd u v')
                      d2 (poly/gcd u' v)
                      u'' (p/poly:* (p/evenly-divide u d1) (p/evenly-divide v d2))
                      v'' (p/poly:* (p/evenly-divide u' d2) (p/evenly-divide v' d1))]
                  (if (and (p/coeff? u'')
                           (p/coeff? v''))
                    (g/div u'' v'')
                    (make-reduced a u'' v''))))))

(defn rf*other [u v]
  #_
  (let [u (numerator r)
        v (denominator r)
        a (bare-arity r)]
    (cond (v/zero? p) 0
          (v/one? p)  r
          :else (let [d (poly/gcd v p)]
                  (if (v/one? d)
                    (make-reduced a (p/poly:* u p) v)
                    (make-reduced a
                                  (p/poly:* u (p/evenly-divide p d))
                                  (p/evenly-divide v d))))))

  #_
  (fn [u u' v]
    (cond (v/zero? v) rcf:zero
	        (v/one? v) u:u'
	        :else
	        (let [d (poly/gcd u' v)]
	          (if (v/one? d)
		          (make-rcf (p/poly:* u v) u')
		          (make-rcf (p/poly:* u (p/evenly-divide v d))
			                  (p/evenly-divide u' d)))))))

(defn other*rf [u v]
  ;; ratio on left:
  #_
  (make (g/mul (numerator r)
               (r/numerator a))
        (g/mul (denominator r)
               (r/denominator a)))

  ;; just coeff on left:
  #_
  (make (g/mul c (numerator r))
        (denominator r))

  #_
  (let [u (numerator r)
        v (denominator r)
        a (bare-arity r)]
    (cond (v/zero? p) 0
          (v/one? p)  r
          :else (let [d (poly/gcd p v) ]
                  (if (v/one? d)
                    (->RationalFunction a (p/poly:* p u) v)
                    (->RationalFunction a
                                        (p/poly:* (p/evenly-divide p d) u)
                                        (p/evenly-divide v d))))))

  #_
  (fn [u v v']
    (cond (v/zero? u) rcf:zero
	        (v/one? u) v:v'
	        :else
	        (let [d (poly/gcd u v')]
	          (if (v/one? d)
		          (make-rcf (p/poly:* u v) v')
		          (make-rcf (p/poly:* (p/evenly-divide u d) v)
			                  (p/evenly-divide v' d)))))))

(defn expt [r n]
  {:pre [(rational-function? r)
         (v/integral? n)]}
  (let [u (numerator r)
        v (denominator r)
        [top bottom e] (if (g/negative? n)
                         [v u (g/negate n)]
                         [u v n])]
    (->RationalFunction (bare-arity r)
                        (p/expt top e)
                        (p/expt bottom e))))

(defn square [r]
  (if (rational-function? r)
    (->RationalFunction (bare-arity r)
                        (p/square (numerator r))
                        (p/square (denominator r)))
    (p/square r)))

(defn cube [r]
  (if (rational-function? r)
    (->RationalFunction (bare-arity r)
                        (p/cube (numerator r))
                        (p/cube (denominator r)))
    (p/cube r)))

(defn invert [r]
  ;; use make so that the - sign will get flipped if needed.
  ;;
  ;; TODO this is expensive, we can do better AND just flip the negative sign
  ;; ourselves.
  (make (denominator r)
        (numerator r)))

(defn rf:div [u v]
  (g/mul #_rf:* u (invert v)))

(defn rf-div-other [u v]
  (g/mul #_rf*other u (g/invert v)))

(defn other-div-rf [u v]
  (g/mul #_other*rf u (invert v)))

(defn rf:gcd [u v]
  (let [d1 (poly/gcd (numerator u)
                     (numerator v))
	      d2 (poly/gcd (denominator u)
                     (denominator v))]
	  (make d1 d2)))

(defn rf-gcd-other [u v]
  (cond (v/zero? v) u
	      (v/one? v)  1
	      :else (poly/gcd (numerator u) v)))

(defn other-gcd-rf [u v]
  (cond (v/zero? u) v
	      (v/one? u)  1
	      :else (poly/gcd u (numerator v))))

;; TODO don't use `g/div`, build the `div` function!

(defn arg-scale [r points]
  (if (rational-function? r)
    (g/div (p/arg-scale (numerator r) points)
	         (p/arg-scale (denominator r) points))
    (p/arg-scale r points)))

(defn arg-shift [r points]
  (if (rational-function? r)
    (g/div (p/arg-shift (numerator r) points)
	         (p/arg-shift (denominator r) points))
    (p/arg-shift r points)))

(defn evaluate [r xs]
  (if (rational-function? r)
    (g/div (p/evaluate (numerator r) xs)
	         (p/evaluate (denominator r) xs))
    (p/evaluate r xs)))

(defn compose
  "only plugs r2 in for the principal indeterminate."
  [r1 r2]
  (if (rational-function? r2)
    (let [nr1 (numerator r1)
          nr2 (numerator r2)
	        dr1 (denominator r1)
          dr2 (denominator r2)
          dn  (p/degree nr1)
	        dd  (p/degree dr1)
	        narity (+ (p/arity dr1) 1)
          nnr1 (p/extend 1 (p/principal-reverse nr1))
          ndr1 (p/extend 1 (p/principal-reverse dr1))
          scales [(second (p/new-variables narity)) 1]
          pn (p/evaluate (p/principal-reverse
				                  (p/arg-scale nnr1 scales))
				                 [nr2 dr2])
          pd (p/evaluate (p/principal-reverse
				                  (p/arg-scale ndr1 scales))
				                 [nr2 dr2])]
	    (cond (> dn dd) (g/div pn (p/poly:* (p/expt dr2 (- dn dd)) pd))
		        (< dn dd) (g/div (p/poly:* (p/expt dr2 (- dd dn)) pn) pd)
		        :else (g/div pn pd)))
    (g/div (p/evaluate (numerator r1) r2)
	         (p/evaluate (denominator r1) r2))))

(defn partial-derivative [r i]
  (if (rational-function? r)
    (let [u (numerator r)
          v (denominator r)]
	    (g/div (p/poly:- (p/poly:* (p/partial-derivative u i) v)
		                   (p/poly:* u (p/partial-derivative v i)))
	           (p/poly:* v v)))
    (p/partial-derivative r i)))

;; I don't know if this stuff is ever important...GJS

(defn assoc-accumulation [rat:op poly:op rat:identity]
  (letfn [(operate [rats]
            (cond (empty? rats) rat:identity

                  (empty? (rest rats)) (first rats)

                  (rational-function? (first rats))
	                (cond (rational-function? (second rats))
		                    (operate
                         (cons (rat:op (first rats)
                                       (second rats))
				                       (drop 2 rats)))

		                    (empty? (drop 2 rats))
                        (rat:op (first rats)
                                (second rats))

		                    (not (rational-function? (nth rats 2)))
		                    (operate
                         (cons (first rats)
				                       (cons (poly:op (second rats)
                                              (nth rats 2))
				                             (drop 3 rats))))

		                    :else (operate
                               (cons (rat:op (first rats)
                                             (second rats))
				                             (drop 2 rats))))

                  (rational-function? (second rats))
	                (operate
                   (cons (rat:op (first rats)
                                 (second rats))
			                   (drop 2 rats)))
	                :else
	                (operate
                   (cons (poly:op (first rats)
                                  (second rats))
			                   (drop 2 rats)))))]
    (fn [& xs]
      (operate xs))))

(def +$rf (assoc-accumulation rf:+ p/poly:+ 0))
(def *$rf (assoc-accumulation rf:* p/poly:* 1))

(defn assoc-inverse-accumulation [rat:inv-op rat:op rat:invert poly:op rat:identity]
  (let [direct-op (assoc-accumulation rat:op poly:op rat:identity)]
    (fn operator
      ([] rat:identity)
      ([r] (rat:invert r))
      ([r & rs]
       (rat:inv-op r (apply direct-op rs))))))

(def -$rcf
  (assoc-inverse-accumulation rf:- rf:+ negate p/poly:+ 0))

(def div-$rcf
  (assoc-inverse-accumulation rf:div rf:* invert p/poly:* 1))

;; TODO use accumulation etc to make these tidy.
;;
;; TODO make a note that this operator table can handle polynomials,
;; coefficients AND rational functions, nothing else.
;;
;; TODO this is not true yet, but we want it to be!

;; TODO REMOVE `g` form from all of these!

(def ^:private operator-table
  {'+ (ua/accumulation g/add 0)
   '- (ua/inverse-accumulation g/sub g/add g/negate 0)
   '* (ua/accumulation g/mul 1)
   '/ (ua/inverse-accumulation g/div g/mul g/invert 1)
   'negate negate
   'invert g/invert
   'expt g/expt
   'square square
   'cube cube
   'gcd g/gcd})

(def operators-known
  (u/keyset operator-table))

(deftype RationalFunctionAnalyzer [poly-analyzer]
  a/ICanonicalize
  (expression-> [this expr cont]
    (a/expression-> this expr cont compare))

  (expression-> [this expr cont v-compare]
    ;; Convert an expression into Rational Function canonical form. The
    ;; expression should be an unwrapped expression, i.e., not an instance
    ;; of the Literal type, nor should subexpressions contain type
    ;; information. This kind of simplification proceeds purely
    ;; symbolically over the known Rational Function operations;;  other
    ;; operations outside the arithmetic available R(x...) should be
    ;; factored out by an expression analyzer before we get here. The
    ;; result is a RationalFunction object representing the structure of
    ;; the input over the unknowns."
    (let [expression-vars (sort v-compare
                                (set/difference (x/variables-in expr)
                                                operators-known))
          arity (count expression-vars)
          sym->var (zipmap expression-vars (p/new-variables arity))
          expr' (x/evaluate expr sym->var operator-table)]
      (cont expr' expression-vars)))

  (->expression [_ r vars]
    ;; This is the output stage of Rational Function canonical form simplification.
    ;; The input is a RationalFunction, and the output is an expression
    ;; representing the evaluation of that function over the
    ;; indeterminates extracted from the expression at the start of this
    ;; process."
    (if (rational-function? r)
      ((sym/symbolic-operator '/)
       (a/->expression poly-analyzer (numerator r) vars)
       (a/->expression poly-analyzer (denominator r) vars))
      (a/->expression poly-analyzer r vars)))

  (known-operation? [_ o]
    (contains? operators-known o)))

;; ## Generic Method Implementations
;;
;; TODO figure out MORE methods to install here... cos etc?
;;
;; `exact-divide`, `abs`, `partial-derivative`, `simplify`,
;; `solve-linear-right`, `solve-linear`

;; TODO can I make them inherit... and then just do the top one ONLY?

(defmethod v/= [::rational-function ::rational-function] [u v] (rf:= u v))
(defmethod v/= [::rational-function ::polynomial] [u v] (rf:= u v))
(defmethod v/= [::rational-function ::coeff] [u v] (rf:= u v))
(defmethod v/= [::polynomial ::rational-function] [u v] (rf:= v u))
(defmethod v/= [::p/coeff ::rational-function] [u v] (rf:= v u))

(defmethod g/add [::rational-function ::rational-function] [u v] (rf:+ u v))
(defmethod g/add [::rational-function ::p/polynomial] [u v] (rf+other u v))
(defmethod g/add [::rational-function ::p/coeff] [u v] (rf+other u v))
(defmethod g/add [::p/polynomial ::rational-function] [u v] (other+rf u v))
(defmethod g/add [::p/coeff ::rational-function] [u v] (other+rf u v))

(defmethod g/negate [::rational-function] [a] (negate a))

(defmethod g/sub [::rational-function ::rational-function] [a b] (rf:- a b))
(defmethod g/sub [::rational-function ::p/polynomial] [u v] (rf-other u v))
(defmethod g/sub [::rational-function ::p/coeff] [u v] (rf-other u v))
(defmethod g/sub [::p/polynomial ::rational-function] [u v] (other-rf u v))
(defmethod g/sub [::p/coeff ::rational-function] [u v] (other-rf u v))

(defmethod g/mul [::rational-function ::rational-function] [u v] (rf:* u v))
#_(defmethod g/mul [::rational-function ::p/polynomial] [u v] (rf*other u v))
#_(defmethod g/mul [::rational-function ::p/coeff] [u v] (rf*other u v))
#_(defmethod g/mul [::p/polynomial ::rational-function] [u v] (other*rf u v))
#_(defmethod g/mul [::p/coeff ::rational-function] [u v] (other*rf u v))


;; TODO fold the next six in...
(defmethod g/mul [::p/polynomial ::rational-function] [c r]
  (make (g/mul c (numerator r))
        (denominator r)))

(defmethod g/mul [::p/coeff ::rational-function] [c r]
  (make (g/mul c (numerator r))
        (denominator r)))

(defmethod g/mul [::rational-function ::p/polynomial] [r c]
  (make (g/mul (numerator r) c)
        (denominator r)))

(defmethod g/mul [::rational-function ::p/coeff] [r c]
  (make (g/mul (numerator r) c)
        (denominator r)))

;; Ratio support

(defmethod g/mul [::rational-function r/ratiotype] [r a]
  (make (g/mul (numerator r)
               (r/numerator a))
        (g/mul (denominator r)
               (r/denominator a))))

(defmethod g/mul [r/ratiotype ::rational-function] [a r]
  (make (g/mul (r/numerator a)
               (numerator r))
        (g/mul (r/denominator a)
               (denominator r))))

(defmethod g/invert [::p/polynomial] [p]
  (->RationalFunction (p/bare-arity p) 1 p))

(defmethod g/div [::rational-function ::rational-function] [u v] (rf:div u v))
(defmethod g/div [::rational-function ::p/polynomial] [u v] (rf-div-other u v))
(defmethod g/div [::p/polynomial ::rational-function] [u v] (other-div-rf u v))

(defmethod g/div [::p/polynomial ::p/polynomial] [p q]
  (let [g (poly/gcd p q)]
    (make (p/evenly-divide p g)
          (p/evenly-divide q g))))

(defmethod g/div [::rational-function ::p/coeff] [r c]
  (make (numerator r)
        (g/mul c (denominator r))))

(defmethod g/div [::p/coeff ::rational-function] [c r]
  (g/divide (p/make-constant (bare-arity r) c)
            r))

(defmethod g/div [::p/coeff ::p/polynomial] [c p]
  (make (p/make-constant (p/bare-arity p) c)
        p))

(defmethod g/expt [::rational-function ::v/integral] [b x]
  (expt b x))

(defmethod g/gcd [::rational-function ::rational-function] [u v] (rf:gcd u v))
(defmethod g/gcd [::p/polynomial ::rational-function] [u v] (other-gcd-rf u v))
(defmethod g/gcd [::p/coeff ::rational-function] [u v] (other-gcd-rf u v))
(defmethod g/gcd [::rational-function ::p/polynomial] [u v] (rf-gcd-other u v))
(defmethod g/gcd [::rational-function ::p/coeff] [u v] (rf-gcd-other u v))
