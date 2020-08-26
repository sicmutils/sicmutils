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

(ns sicmutils.ratio
  #?(:clj
     (:refer-clojure :rename {rationalize core-rationalize
                              ratio? core-ratio?
                              denominator core-denominator
                              numerator core-numerator}))
  (:require [sicmutils.expression :as x]
            [sicmutils.generic :as g]
            [sicmutils.util :as u]
            [sicmutils.value :as v]
            #?(:cljs ["fraction.js/bigfraction.js" :as Fraction]))
  #?(:clj (:import [clojure.lang BigInt Ratio])))

(def ratiotype #?(:clj Ratio :cljs Fraction))
(derive ratiotype ::v/number)

(def ratio?
  #?(:clj core-ratio?
     :cljs (fn [r] (instance? Fraction r))))

(def numerator
  #?(:clj core-numerator
     :cljs (fn [^Fraction x]
             (if (pos? (.-s x))
               (.-n x)
               (- (.-n x))))))

(def denominator
  #?(:clj core-denominator
     :cljs (fn [^Fraction x] (.-d x))))

(defn rationalize
  "Construct a ratio."
  ([x]
   #?(:cljs (if (v/integral? x)
              x
              (Fraction. x))
      :clj (core-rationalize x)))
  ([n d]
   #?(:cljs (if (v/unity? d)
              n
              (Fraction. n d))
      :clj (core-rationalize (/ n d)))))

(defn parse-ratio
  "TODO Experimental. Parser for the #sicm/frac data literal."
  [x]
  (cond #?@(:clj
            [(ratio? x)
             `(sicmutils.ratio/rationalize
               ~(long (numerator x))
               ~(long (denominator x)))])

        (sequential? x)
        (let [[op n d] x]
          (cond (= op '/) `(sicmutils.ratio/rationalize ~n ~d)
                (nil? d)  `(sicmutils.ratio/rationalize ~op ~n)
                :else (u/illegal (str "Invalid: " x))))

        (v/number? x) `(sicmutils.ratio/rationalize ~x)

        :else (u/illegal (str "Invalid: " x))))

#?(:clj
   (extend-type Ratio
     v/Value
     (nullity? [c] (zero? c))
     (unity? [c] (= 1 c))
     (zero-like [_] 0)
     (one-like [_] 1)
     (freeze [x] (let [n (numerator x)
                       d (denominator x)]
                   (if (v/unity? d)
                     n
                     `(~'/ ~n ~d))))
     (exact? [c] true)
     (numerical? [_] true)
     (kind [_] Ratio))

   :cljs
   (let [ZERO (Fraction. 0)
         ONE  (Fraction. 1)]
     (extend-type Fraction
       v/Value
       (nullity? [c] (.equals c ZERO))
       (unity? [c] (.equals c ONE))
       (zero-like [_] 0)
       (one-like [_] 1)
       (freeze [x] (let [n (numerator x)
                         d (denominator x)]
                     (if (v/unity? d)
                       (js/Number n)
                       `(~'/
                         ~(js/Number n)
                         ~(js/Number d)))))
       (exact? [c] true)
       (numerical? [_] true)
       (kind [x] Fraction)

       IEquiv
       (-equiv [this other]
         (cond (ratio? other) (.equals this other)

               (v/integral? other)
               (and (v/unity? (denominator this))
                    (v/eq (numerator this) other))

               ;; Enabling this would work, but would take us away from
               ;; Clojure's behavior.
               #_(v/number? other)
               #_(.equals this (rationalize other))

               :else false))

       IComparable
       (-compare [this other]
         (if (or (number? other)
                 (ratio? other))
           (.compare this other)
           (.compare this (rationalize other))))

       Object
       (toString [r] (str (v/freeze r)))

       IPrintWithWriter
       (-pr-writer [x writer opts]
         (let [x (v/freeze x)]
           (if (number? x)
             (write-all writer x)
             (write-all writer "#sicm/ratio " (str x))))))))

#?(:clj
   (doseq [[op f] [[g/exact-divide /]
                   [g/quotient quot]
                   [g/remainder rem]
                   [g/modulo mod]]]
     (defmethod op [Ratio Ratio] [a b] (f a b))
     (defmethod op [Ratio ::v/integral] [a b] (f a b))
     (defmethod op [::v/integral Ratio] [a b] (f a b)))

   :cljs
   (do
     ;; The -equiv implementation handles equality with any number, so flip the
     ;; arguments around and invoke equiv.
     (defmethod v/eq [::v/number Fraction] [l r] (= r l))

     (defn promote [x]
       (if (v/unity? (denominator x))
         (numerator x)
         x))

     (defmethod g/add [Fraction Fraction] [a b] (promote (.add a b)))
     (defmethod g/sub [Fraction Fraction] [a b] (promote (.sub a b)))
     (defmethod g/mul [Fraction Fraction] [a b] (promote (.mul a b)))
     (defmethod g/div [Fraction Fraction] [a b] (promote (.div a b)))
     (defmethod g/exact-divide [Fraction Fraction] [a b] (promote (.div a b)))

     ;; TODO this does NOT work. We can actually only take integral exponents! To match Clojure, we want to:
     ;;
     ;; - handle integral exponents, just like the implementation allows.
     ;; - if the pow is NOT integral, convert both toValue then proceed.
     ;; - below, downcast fraction if we find it in the exponent.
     ;; - from nt/expt, 'return an exact number if the base is an exact number
     ;; - and the power is an integer, otherwise returns a double'

     (defmethod g/expt [Fraction Fraction] [a b] (promote (.pow a b)))

     (defmethod g/negate [Fraction] [a] (promote (.neg a)))
     (defmethod g/negative? [Fraction] [a] (neg? (.-s a)))
     (defmethod g/invert [Fraction] [a] (promote (.inverse a)))
     (defmethod g/square [Fraction] [a] (promote (.mul a a)))
     (defmethod g/cube [Fraction] [a] (promote (.pow a 3)))
     (defmethod g/abs [Fraction] [a] (promote (.abs a)))
     (defmethod g/magnitude [Fraction] [ a] (promote (.abs a)))
     (defmethod g/gcd [Fraction Fraction] [a b] (promote (.gcd a)))
     (defmethod g/lcm [Fraction Fraction] [a b] (promote (.lcm a)))

     (defmethod g/quotient [Fraction Fraction] [a b]
       (promote
        (let [^Fraction x (.div a b)]
          (if (pos? (.-s x))
            (.floor x)
            (.ceil x)))))

     (defmethod g/remainder [Fraction Fraction] [a b]
       (promote (.mod a b)))

     ;; Cross-compatibility with numbers in CLJS.
     (defn downcast-fraction
       "Anything that `upcast-number` doesn't catch will hit this and pull a floating
  point value out of the ratio."
       [op]
       (defmethod op [Fraction ::v/number] [a b]
         (op (.valueOf a) b))

       (defmethod op [::v/number Fraction] [a b]
         (op a (.valueOf b))))

     (defn upcast-number
       "Integrals can stay exact, so they become ratios before op."
       [op]
       (defmethod op [Fraction ::v/integral] [a b]
         (op a (Fraction. b 1)))

       (defmethod op [::v/integral Fraction] [a b]
         (op (Fraction. a 1) b)))

     ;; An exact number should become a ratio rather than erroring out, if one
     ;; side of the calculation is already rational (but not if neither side
     ;; is).
     (upcast-number g/exact-divide)

     (doseq [op [g/add g/mul g/sub g/gcd g/expt g/remainder g/quotient g/div]]
       (upcast-number op)
       (downcast-fraction op))))
