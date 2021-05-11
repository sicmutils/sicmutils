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

(comment
  (defn rcf:binary-operator
    [u:u* v:v* int*int int*rat rat*int rat*rat]
    (if (rational-function? u:u*)
      (if (rational-function? v:v*)
	      (rat*rat (ratform-numerator u:u*)
		             (ratform-denominator u:u*)
		             (ratform-numerator v:v*)
		             (ratform-denominator v:v*))
	      (rat*int (ratform-numerator u:u*)
		             (ratform-denominator u:u*)
		             v:v*))
      (if (rational-function? v:v*)
	      (int*rat u:u*
		             (ratform-numerator v:v*)
		             (ratform-denominator v:v*))
	      (int*int u:u* v:v*))))

  (defn rcf:+
    "TODO see how different this is from what we have below."
    [u:u* v:v*]
    (rcf:binary-operator
     u:u* v:v*
     p/poly:+
     (fn [u v v*]
       (if (v/zero? u)
	       v:v*
	       (make-rcf (poly:+ (poly:* u v*) v) v*)))
     (fn [u u* v]
       (if (poly:zero? v)
	       u:u*
	       (make-rcf (poly:+ u (poly:* u* v)) u*)))
     (fn [u u* v v*]
       (if (poly:= u* v*)
	       (let* ((n (poly:+ u v)) (g (poly:gcd u* n)))
	         (if (poly:one? g)
		         (make-rcf n u*)
		         (make-rcf (poly:quotient n g) (poly:quotient u* g))))
	       (let [d1 (poly:gcd u* v*)]
	         (if (poly:one? d1)
		         (make-rcf (poly:+ (poly:* u v*) (poly:* u* v))
			                 (poly:* u* v*))
		         (let [u*:d1 (poly:quotient u* d1)
			             t (poly:+ (poly:* u (poly:quotient v* d1))
				                     (poly:* u*:d1 v))]
		           (if (poly:zero? t)
		             rcf:zero
		             (let [d2 (poly:gcd t d1)]
			             (if (poly:one? d2)
			               (make-rcf t (poly:* u*/d1 v*))
			               (make-rcf
			                (poly:quotient t d2)
			                (poly:* u*/d1
				                      (poly:quotient v* d2))))))))))))))

(defn rf:+
  "Add the rational functions r and s."
  [r s]
  {:pre [(rational-function? r)
         (rational-function? s)
         (= (bare-arity r) (bare-arity s))]}
  (let [a  (bare-arity r)
        u  (numerator r)
        u' (denominator r)
        v  (numerator s)
        v' (denominator s)
        d1 (poly/gcd u' v')]
    (if (v/one? d1)
      (make-reduced a (p/poly:+ (p/poly:* u v')
                                (p/poly:* u' v))
                    (p/poly:* u' v'))
      (let [t (p/poly:+ (p/poly:* u (p/evenly-divide v' d1))
                        (p/poly:* v (p/evenly-divide u' d1)))
            d2 (poly/gcd t d1)]
        (make-reduced a
                      (p/evenly-divide t d2)
                      (p/poly:* (p/evenly-divide u' d1)
                                (p/evenly-divide v' d2)))))))

;; TODO add one for the poly on LEFT etc and install it in generics.
;; TODO this is exactly what the binop is doing.
(defn addp
  "Add a rational function to a polynomial."
  [r p]
  (if (v/zero? p)
    r
    (let [v (denominator r)]
      (-> (p/poly:+ (numerator r) (p/poly:* v p))
          (make v)))))

(defn negate [r]
  {:pre [(rational-function? r)]}
  (->RationalFunction (bare-arity r)
                      (p/negate (numerator r))
                      (denominator r)))

(comment
  (define rcf:- [u:u* v:v*]
    (rcf:binary-operator
     u:u* v:v*
     poly:-
     (lambda (u v v*)
             (if (poly:zero? u)
	             (make-ratform (poly:negate v) v*)
	             (make-rcf (poly:- (poly:* u v*) v) v*)))
     (fn [u u* v]
       (if (poly:zero? v)
	       u:u*
	       (make-rcf (poly:- u (poly:* u* v)) u*)))
     (fn [u u* v v*]
       (if (poly:= u* v*)
	       (let* ((n (poly:- u v)) (g (poly:gcd u* n)))
	         (if (poly:one? g)
		         (make-rcf n u*)
		         (make-rcf (poly:quotient n g) (poly:quotient u* g))))
	       (let [d1 (poly:gcd u* v*)]
	         (if (poly:one? d1)
		         (make-rcf (poly:- (poly:* u v*) (poly:* u* v))
			                 (poly:* u* v*))
		         (let [u*:d1 (poly:quotient u* d1)
			             t (poly:- (poly:* u (poly:quotient v* d1))
				                     (poly:* u*:d1 v))]
		           (if (poly:zero? t)
		             rcf:zero
		             (let [d2 (poly:gcd t d1)]
			             (if (poly:one? d2)
			               (make-rcf t (poly:* u*:d1 v*))
			               (make-rcf
			                (poly:quotient t d2)
			                (poly:* u*:d1
				                      (poly:quotient v* d2))))))))))))))

(defn rf:- [r s]
  (rf:+ r (negate s)))

;; TODO add one for the poly on LEFT etc and install it in generics.
(defn subp
  "Subtract polynomial `p` from rational function `r`."
  [r p]
  {:pre [(rational-function? r)]}
  (if (v/zero? p)
    r
    (let [v (denominator r)]
      (-> (p/poly:- (numerator r)
                    (p/poly:* v p))
          (make v)))))

(comment
  (define (rcf:* u:u* v:v*)
    (rcf:binary-operator
     u:u* v:v*
     poly:*
     (fn [u v v*]
       (cond (poly:zero? u) rcf:zero
	           (poly:one? u) v:v*
	           :else
	           (let [d (poly:gcd u v*)]
	             (if (poly:one? d)
		             (make-rcf (poly:* u v) v*)
		             (make-rcf (poly:* (poly:quotient u d) v)
			                     (poly:quotient v* d))))))
     (fn [u u* v]
       (cond (poly:zero? v) rcf:zero
	           (poly:one? v) u:u*
	           :else
	           (let [d (poly:gcd u* v)]
	             (if (poly:one? d)
		             (make-rcf (poly:* u v) u*)
		             (make-rcf (poly:* u (poly:quotient v d))
			                     (poly:quotient u* d))))))
     (fn [u u* v v*]
       (let [d1 (poly:gcd u v*)
	           d2 (poly:gcd u* v)]
	       (if (poly:one? d1)
	         (if (poly:one? d2)
		         (make-rcf (poly:* u v) (poly:* u* v*))
		         (make-rcf (poly:* u (poly:quotient v d2))
			                 (poly:* (poly:quotient u* d2) v*)))
	         (if (poly:one? d2)
		         (make-rcf (poly:* (poly:quotient u d1) v)
			                 (poly:* u* (poly:quotient v* d1)))
		         (make-rcf (poly:* (poly:quotient u d1)
				                       (poly:quotient v d2))
			                 (poly:* (poly:quotient u* d2)
				                       (poly:quotient v* d1))))))))))

;; TODO move the other multipliers up!!

(defn rf:*
  [r s]
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
                  (make-reduced a u'' v'')))))

(comment
  ;; pretty sure this is identical to what we do, deferring to polynomial.
  (defn rcf:expt [base exponent]
    (letfn [(expt-iter [x count answer]
              (if (fix:zero? count)
	              answer
	              (if (even? count)
	                (expt-iter (rcf:square x) (fix:quotient count 2) answer)
	                (expt-iter x (fix:-1+ count) (rcf:* x answer)))))]
      (if (g/negative? exponent)
	      (rcf:invert (expt-iter base (int:negate exponent) rcf:one))
	      :else (expt-iter base exponent rcf:one)))))

(defn expt [r n]
  {:pre [(rational-function? r) (v/integral? n)]}
  (let [u (numerator r)
        v (denominator r)
        [top bottom e] (if (g/negative? n)
                         [v u (g/negate n)]
                         [u v n])]
    (->RationalFunction (bare-arity r)
                        (p/expt top e)
                        (p/expt bottom e))))

(defn square [r]
  {:pre [(rational-function? r)]}
  (let [u (numerator r)
        v (denominator r)]
    (->RationalFunction (bare-arity r)
                        (p/square u)
                        (p/square v))))

(defn cube [r]
  {:pre [(rational-function? r)]}
  (let [u (numerator r)
        v (denominator r)]
    (->RationalFunction (bare-arity r)
                        (p/cube u)
                        (p/cube v))))

(defn invert [r]
  ;; use make so that the - sign will get flipped if needed.
  ;;
  ;; TODO seems expensive, we can do better!
  (make (denominator r)
        (numerator r)))

;; TODO if we want this to be more generic we need to get `rf:*` to handle more
;; stuff...
(defn div [r s]
  (g/mul r (invert s)))

(comment
  (defn rcf:gcd [u/u* v/v*]
    (rcf:binary-operator u/u* v/v*
                         poly:gcd
                         (lambda (u v v*)
                                 (cond ((poly:zero? u) v/v*)
	                                     ((poly:one? u) poly:one)
	                                     (else (poly:gcd u v))))
                         (lambda (u u* v)
                                 (cond ((poly:zero? v) u/u*)
	                                     ((poly:one? v) poly:one)
	                                     (else (poly:gcd u v))))
                         (lambda (u u* v v*)
                                 (let ((d1 (poly:gcd u v))
	                                     (d2 (poly:gcd u* v*)))
	                                 (make-rcf d1 d2)))))

  (define (rcf:arg-scale r points)
    (if (ratform? r)
      (div (apply poly:arg-scale (ratform-numerator r) points)
	         (apply poly:arg-scale (ratform-denominator r) points))
      (apply poly:arg-scale r points)))

  (define (rcf:arg-shift r points)
    (if (ratform? r)
      (div (apply poly:arg-shift (ratform-numerator r) points)
	         (apply poly:arg-shift (ratform-denominator r) points))
      (apply poly:arg-shift r points)))

  ;; TODO call this `evaluate`, since we define that name above.
  (define (rcf:value r points)
    (if (ratform? r)
      (div (apply poly:value (ratform-numerator r) points)
	         (apply poly:value (ratform-denominator r) points))
      (apply poly:value r points)))

  ;; The following only plugs r2 in for the principal indeterminate.
  ;; TODO add poly:principal-reverse
  (defn rcf:compose [r1 r2]
    (if (ratform? r2)
      (let [nr1 (numerator r1)
            nr2 (numerator r2)
	          dr1 (denominator r1)
            dr2 (denominator r2)
            dn (p/degree nr1)
	          dd (p/degree dr1)
	          narity (+ (p/arity dr1) 1)
            nnr1 (poly:extend 1 (poly:principal-reverse nr1))
            ndr1 (poly:extend 1 (poly:principal-reverse dr1))
            scales (list (cadr (poly:new-variables narity)) 1)
            pn (poly:value (poly:principal-reverse
				                    (poly:arg-scale nnr1 scales))
				                   nr2
				                   dr2)
            pd (poly:value (poly:principal-reverse
				                    (poly:arg-scale ndr1 scales))
				                   nr2
				                   dr2)]
	      (cond (> dn dd) (div pn (poly:* (poly:expt dr2 (fix:- dn dd)) pd))
		          (< dn dd) (div (poly:* (poly:expt dr2 (fix:- dd dn)) pn) pd)
		          :else (div pn pd)))
      (div (poly:value (numerator r1) r2)
	         (poly:value (denominator r1) r2))))

  (defn rcf:derivative [r varnum]
    (if (ratform? r)
      (let [u (numerator r)
            v (denominator r)]
	      (div (poly:- (poly:* (poly:derivative u varnum) v)
		                 (poly:* u (poly:derivative v varnum)))
	           (poly:* v v)))
      (poly:derivative r varnum))))

(comment
  ;; I don't know if this stuff is ever important...GJS

  (defn assoc-accumulation [rat:op poly:op rat:identity]
    (letfn [(operate [rats]
              (cond (null? rats) rat:identity
	                  (null? (cdr rats)) (car rats)
	                  (ratform? (car rats))
	                  (cond (ratform? (cadr rats))
		                      (operate (cons (rat:op (car rats) (cadr rats))
				                                 (cddr rats)))
		                      (null? (cddr rats)) (rat:op (car rats) (cadr rats))
		                      (not (ratform? (caddr rats)))
		                      (operate (cons (car rats)
				                                 (cons (poly:op (cadr rats) (caddr rats))
				                                       (cdddr rats))))
		                      :else (operate (cons (rat:op (car rats) (cadr rats))
				                                       (cddr rats))))
	                  (ratform? (cadr rats))
	                  (operate (cons (rat:op (car rats) (cadr rats))
			                             (cddr rats)))
	                  :else
	                  (operate (cons (poly:op (car rats) (cadr rats))
			                             (cddr rats)))))]
      (fn [& xs]
        (operate rats))))

  (def +$rf (assoc-accumulation rf:+ p/poly:+ rf:zero))
  (def *$rf (assoc-accumulation rf:* p/poly:* rf:one))

  (defn assoc-inverse-accumulation [rat:inv-op rat:op rat:invert poly:op rat:identity]
    (let [direct-op (assoc-accumulation rat:op poly:op rat:identity)]
      (fn operator [& rats]
        (cond (null? rats) rat:identity
	            (null? (cdr rats)) (rat:invert (car rats))
	            :else (rat:inv-op (car rats) (apply direct-op (cdr rats)))))))

  (def -$rcf
    (assoc-inverse-accumulation rcf:- rcf:+ rcf:negate poly:+ rcf:zero))

  (def div-$rcf
    (assoc-inverse-accumulation div rcf:* rcf:invert poly:* rcf:one)))

;; TODO use accumulation etc to make these tidy.
;;
;; TODO make a note that this operator table can handle polynomials,
;; coefficients AND rational functions, nothing else.
(def ^:private operator-table
  {'+ #(reduce g/add %&)
   '- (fn [arg & args]
        (if (some? args) (g/sub arg (reduce g/add args)) (g/negate arg)))
   '* (fn
        ([] 1)
        ([x] x)
        ([x y] (g/mul x y))
        ([x y & more]
         (reduce g/mul (g/mul x y) more)))
   '/ (fn [arg & args]
        (if (some? args) (g/div arg (reduce g/mul args)) (g/invert arg)))
   'negate negate
   'invert invert
   'expt g/expt
   'square square
   'cube cube
   'gcd g/gcd
   })

(def operators-known
  (u/keyset operator-table))

(deftype RationalFunctionAnalyzer [polynomial-analyzer]
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
          arity    (count expression-vars)
          sym->var (zipmap expression-vars (p/new-variables arity))
          expr'    (x/evaluate expr sym->var operator-table)]
      (cont expr' expression-vars)))

  (->expression [_ r vars]
    ;; This is the output stage of Rational Function canonical form simplification.
    ;; The input is a RationalFunction, and the output is an expression
    ;; representing the evaluation of that function over the
    ;; indeterminates extracted from the expression at the start of this
    ;; process."
    (cond (rational-function? r)
          ((sym/symbolic-operator '/)
           (a/->expression polynomial-analyzer (numerator ^RationalFunction r) vars)
           (a/->expression polynomial-analyzer (denominator ^RationalFunction r) vars))

          (p/polynomial? r)
          (a/->expression polynomial-analyzer r vars)

          :else r))

  (known-operation? [_ o]
    (contains? operators-known o)))

;; ## Generic Method Implementations
;;
;; TODO figure out MORE methods to install here... cos etc?

(defmethod g/add [::rational-function ::rational-function] [a b]
  (rf:+ a b))

(defmethod g/add [::rational-function ::p/polynomial] [r p]
  (addp r p))

(defmethod g/add [::p/polynomial ::rational-function] [p r]
  (addp r p))

(defmethod g/add [::rational-function ::p/coeff] [a b]
  (addp a (p/make-constant (bare-arity a) b)))

(defmethod g/add [::p/coeff ::rational-function] [b a]
  (addp a (p/make-constant (bare-arity a) b)))

(defmethod g/negate [::rational-function] [a]
  (negate a))

(defmethod g/sub [::rational-function ::rational-function] [a b]
  (rf:- a b))

(defmethod g/sub [::rational-function ::p/polynomial] [r p]
  (subp r p))

;; TODO why just integral??
(defmethod g/sub [::rational-function ::v/integral] [^RationalFunction r c]
  (let [u (numerator r)
        v (denominator r)]
    (make (p/poly:- (g/mul c v) u) v)))

;; TODO make these NOT necessarily commute,
(defmethod g/sub [::rational-function ::p/polynomial] [r p]
  (addp r (g/negate p)))

(defmethod g/sub [::p/polynomial ::rational-function] [p r]
  (addp (g/negate r) p))

(defmethod g/mul [::rational-function ::rational-function] [a b]
  (rf:* a b))

;; Multiply the rational function r = u/v by the polynomial p.

(defmethod g/mul [::rational-function ::p/polynomial] [r p]
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
                                  (p/evenly-divide v d)))))))

;; Multiply the polynomial p by the rational function r = u/v.
;;
;; TODO why not make-reduced?

(defmethod g/mul [::p/polynomial ::rational-function] [p r]
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
                                        (p/evenly-divide v d)))))))

(defmethod g/div [::rational-function ::rational-function] [a b]
  (div a b))

(defmethod g/div [::rational-function ::p/polynomial] [r p]
  (make (numerator r)
        (p/poly:* (denominator r) p)))

(defmethod g/div [::p/polynomial ::rational-function] [p r]
  (make (p/poly:* p (denominator r))
        (numerator r)))

(defmethod g/div [::p/polynomial ::p/polynomial] [p q]
  (let [g (poly/gcd p q)]
    (make (p/evenly-divide p g)
          (p/evenly-divide q g))))

(defmethod g/invert [::p/polynomial] [p]
  (make (p/make-constant (p/bare-arity p) 1) p))

;; ### (coeff, RF)

(defmethod g/mul [::p/coeff ::rational-function] [c ^RationalFunction r]
  (make (g/mul c (numerator r))
        (denominator r)))

(defmethod g/mul [::rational-function ::p/coeff] [^RationalFunction r c]
  (make (g/mul (numerator r) c)
        (denominator r)))

;; Ratio support for Clojure.
(defmethod g/mul [::rational-function r/ratiotype] [^RationalFunction r a]
  (make (g/mul (numerator r)
               (r/numerator a))
        (g/mul (denominator r)
               (r/denominator a))))

(defmethod g/mul [r/ratiotype ::rational-function] [a r]
  (make (g/mul (r/numerator a)
               (numerator r))
        (g/mul (r/denominator a)
               (denominator r))))

(defmethod g/expt [::rational-function ::v/integral] [b x]
  (expt b x))

(defmethod g/div [::rational-function ::p/coeff] [r c]
  (make (numerator r)
        (g/mul c (denominator r))))

(defmethod g/div [::p/coeff ::rational-function] [c r]
  (g/divide (p/make-constant (bare-arity r) c)
            r))

(defmethod g/div [::p/coeff ::p/polynomial] [c p]
  (make (p/make-constant (p/bare-arity p) c)
        p))

;; TODO shouldn't this move to the `gcd` namespace??
(defmethod g/gcd [::p/polynomial ::p/polynomial] [p q]
  (poly/gcd p q))

(defmethod g/gcd [::p/polynomial ::rational-function] [p u]
  (poly/gcd p (numerator u)))

(defmethod g/gcd [::rational-function ::p/polynomial] [u p]
  (poly/gcd (numerator u) p))

(defmethod g/gcd [::rational-function ::rational-function] [u v]
  (make (poly/gcd (numerator u)
                  (numerator v))
        (poly/gcd (denominator u)
                  (denominator v))))

(defmethod g/gcd [::p/polynomial ::v/integral] [p a]
  (poly/primitive-gcd (cons a (p/coefficients p))))

(defmethod g/gcd [::v/integral ::p/polynomial] [a p]
  (poly/primitive-gcd (cons a (p/coefficients p))))

(defmethod g/gcd [::p/polynomial r/ratiotype] [p a]
  (poly/primitive-gcd (cons a (p/coefficients p))))

(defmethod g/gcd [r/ratiotype ::p/polynomial] [a p]
  (poly/primitive-gcd (cons a (p/coefficients p))))
