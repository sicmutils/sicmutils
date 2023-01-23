#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.ratio
  "This namespace provides a number of functions and constructors for working
  with ratios in Clojure and ClojureScript.

  [[clojure.lang.Ratio]] is native in Clojure. The ClojureScript implementation
  uses [Fraction.js](https://github.com/infusion/Fraction.js/).

  For other numeric extensions, see [[emmy.numbers]]
  and [[emmy.complex]]."
  (:refer-clojure :exclude [ratio? numerator denominator rationalize])
  (:require #?(:clj [clojure.core :as core])
            #?(:clj [clojure.edn] :cljs [cljs.reader])
            #?(:cljs [goog.array :as garray])
            #?(:cljs [goog.object :as obj])
            #?(:cljs [emmy.complex :as c])
            [emmy.generic :as g]
            [emmy.util :as u]
            [emmy.value :as v]
            #?(:cljs ["fraction.js/bigfraction.js" :as Fraction]))
  #?(:clj (:import (clojure.lang Ratio))))

(def ^:no-doc ratiotype
  #?(:clj Ratio :cljs Fraction))

(derive ratiotype ::v/real)

(def ratio?
  #?(:clj core/ratio?
     :cljs (fn [r] (instance? Fraction r))))

(defprotocol IRational
  (numerator [_])
  (denominator [_]))

(extend-protocol IRational
  #?(:clj Object :cljs default)
  (numerator [x] x)
  (denominator [_] 1)

  #?@(:clj
      [Ratio
       (numerator [r] (core/numerator r))
       (denominator [r] (core/denominator r))]

      :cljs
      [Fraction
       (numerator
        [x]
        (if (pos? (obj/get x "s"))
          (obj/get x "n")
          (- (obj/get x "n"))))
       (denominator
        [x]
        (obj/get x "d"))]))

#?(:cljs
   (defn- promote [x]
     (if (v/one? (denominator x))
       (numerator x)
       x)))

(defn rationalize
  "Construct a ratio."
  ([x]
   #?(:cljs (if (v/integral? x)
              x
              (Fraction. x))
      :clj (core/rationalize x)))
  ([n d]
   #?(:cljs (if (v/one? d)
              n
              (promote (Fraction. n d)))
      :clj (core/rationalize (/ n d)))))

(def ^:private ratio-pattern #"([-+]?[0-9]+)/([0-9]+)")

(defn matches? [pattern s]
  (let [[match] (re-find pattern s)]
    (identical? match s)))

(defn ^:private match-ratio
  [s]
  (let [m (vec (re-find ratio-pattern s))
        numerator   (m 1)
        denominator (m 2)
        numerator (if (re-find #"^\+" numerator)
                    (subs numerator 1)
                    numerator)]
    `(rationalize
      (u/bigint ~numerator)
      (u/bigint ~denominator))))

(defn parse-ratio
  "Parser for the `#sicm/ratio` literal."
  [x]
  (cond #?@(:clj
            [(ratio? x)
             `(rationalize
               (u/bigint ~(str (numerator x)))
               (u/bigint ~(str (denominator x))))])

        (v/number? x) `(emmy.ratio/rationalize ~x)
        (string? x)    (if (matches? ratio-pattern x)
                         (match-ratio x)
                         (recur
                          #?(:clj  (clojure.edn/read-string x)
                             :cljs (cljs.reader/read-string x))))
        :else (u/illegal (str "Invalid ratio: " x))))

#?(:clj
   (extend-type Ratio
     v/Numerical
     (numerical? [_] true)

     v/Value
     (zero? [c] (zero? c))
     (one? [c] (= 1 c))
     (identity? [c] (= 1 c))
     (zero-like [_] 0)
     (one-like [_] 1)
     (identity-like [_] 1)
     (freeze [x] (let [n (numerator x)
                       d (denominator x)]
                   (if (v/one? d)
                     n
                     `(~'/ ~n ~d))))
     (exact? [_] true)
     (kind [_] Ratio))

   :cljs
   (let [ZERO (Fraction. 0)
         ONE  (Fraction. 1)]
     (extend-type Fraction
       v/Numerical
       (numerical? [_] true)

       v/Value
       (zero? [c] (.equals c ZERO))
       (one? [c] (.equals c ONE))
       (identity? [c] (.equals c ONE))
       (zero-like [_] 0)
       (one-like [_] 1)
       (identity-like [_] 1)
       (freeze [x] (let [n (numerator x)
                         d (denominator x)]
                     (if (v/one? d)
                       (v/freeze n)
                       `(~'/
                         ~(v/freeze n)
                         ~(v/freeze d)))))
       (exact? [_] true)
       (kind [_] Fraction)

       IEquiv
       (-equiv [this other]
         (cond (ratio? other) (.equals this other)
               (v/integral? other)
               (and (v/one? (denominator this))
                    (v/= (numerator this) other))

               ;; Enabling this would work, but would take us away from
               ;; Clojure's behavior.
               #_(v/number? other)
               #_(.equals this (rationalize other))

               :else false))

       IComparable
       (-compare [this other]
         (if (ratio? other)
           (.compare this other)
           (let [o-value (.valueOf other)]
             (if (v/real? o-value)
               (garray/defaultCompare this o-value)
               (throw (js/Error. (str "Cannot compare " this " to " other)))))))

       IHash
       (-hash [this]
         (bit-xor
          (-hash (numerator this))
          (-hash (denominator this))))

       Object
       (toString [r]
         (let [x (v/freeze r)]
           (if (number? x)
             x
             (let [[_ n d] x]
               (str n "/" d)))))

       IPrintWithWriter
       (-pr-writer [x writer opts]
         (let [n (numerator x)
               d (denominator x)]
           (if (v/one? d)
             (-pr-writer n writer opts)
             (write-all writer "#sicm/ratio \""
                        (str n) "/" (str d)
                        "\"")))))))

#?(:clj
   (do
     (defmethod g/gcd [Ratio ::v/integral] [a b]
       (g/div (.gcd (core/numerator a)
                    (biginteger b))
              (core/denominator a)))

     (defmethod g/gcd [::v/integral Ratio] [a b]
       (g/div (.gcd (biginteger a)
                    (core/numerator b))
              (core/denominator b)))

     (defmethod g/gcd [Ratio Ratio] [a b]
       (g/div (.gcd (core/numerator a)
                    (core/numerator b))
              (g/lcm (core/denominator a)
                     (core/denominator b))))

     (defmethod g/infinite? [Ratio] [_] false)

     (doseq [[op f] [[g/exact-divide /]
                     [g/quotient quot]
                     [g/remainder rem]
                     [g/modulo mod]]]
       (defmethod op [Ratio Ratio] [a b] (f a b))
       (defmethod op [Ratio ::v/integral] [a b] (f a b))
       (defmethod op [::v/integral Ratio] [a b] (f a b))))

   :cljs
   (do
     (defn- pow [r m]
       (let [n (numerator r)
             d (denominator r)]
         (if (neg? m)
           (rationalize (g/expt d (g/negate m))
                        (g/expt n (g/negate m)))
           (rationalize (g/expt n m)
                        (g/expt d m)))))

     ;; The -equiv implementation handles equality with any number, so flip the
     ;; arguments around and invoke equiv.
     (defmethod v/= [::v/real Fraction] [l r] (= r l))

     (defmethod g/add [Fraction Fraction] [a b] (promote (.add ^js a b)))
     (defmethod g/sub [Fraction Fraction] [a b] (promote (.sub ^js a b)))

     (defmethod g/mul [Fraction Fraction] [a b]
       (promote (.mul ^js a b)))

     (defmethod g/div [Fraction Fraction] [a b]
       (promote (.div ^js a b)))

     (defmethod g/exact-divide [Fraction Fraction] [a b]
       (promote (.div ^js a b)))

     (defmethod g/negate [Fraction] [a] (promote (.neg ^js a)))
     (defmethod g/negative? [Fraction] [a] (neg? (obj/get a "s")))
     (defmethod g/infinite? [Fraction] [_] false)
     (defmethod g/invert [Fraction] [a] (promote (.inverse ^js a)))
     (defmethod g/square [Fraction] [a] (promote (.mul ^js a a)))
     (defmethod g/cube [Fraction] [a] (promote (.pow ^js a 3)))
     (defmethod g/abs [Fraction] [a] (promote (.abs ^js a)))
     (defmethod g/magnitude [Fraction] [a] (promote (.abs ^js a)))

     (defmethod g/gcd [Fraction Fraction] [a b]
       (promote (.gcd ^js a b)))

     (defmethod g/lcm [Fraction Fraction] [a b]
       (promote (.lcm ^js a b)))

     (defmethod g/expt [Fraction ::v/integral] [a b] (pow a b))
     (defmethod g/sqrt [Fraction] [a]
       (if (neg? a)
         (g/sqrt (c/complex (.valueOf a)))
         (g/div (g/sqrt (u/double (numerator a)))
                (g/sqrt (u/double (denominator a))))))

     (defmethod g/modulo [Fraction Fraction] [a b]
       (promote
        (.mod (.add (.mod ^js a b) b) b)))

     ;; Only integral ratios let us stay exact. If a ratio appears in the
     ;; exponent, convert the base to a number and call g/expt again.
     (defmethod g/expt [Fraction Fraction] [a b]
       (if (v/one? (denominator b))
         (promote (.pow ^js a (numerator b)))
         (g/expt (.valueOf a)
                 (.valueOf b))))

     (defmethod g/quotient [Fraction Fraction] [a b]
       (promote
        (let [x (.div ^js a b)]
          (if (pos? (obj/get x "s"))
            (.floor ^js x)
            (.ceil ^js x)))))

     (defmethod g/remainder [Fraction Fraction] [a b]
       (promote (.mod ^js a b)))

     ;; Cross-compatibility with numbers in CLJS.
     (defn- downcast-fraction
       "Anything that `upcast-number` doesn't catch will hit this and pull a floating
  point value out of the ratio."
       [op]
       (defmethod op [Fraction ::v/real] [a b]
         (op (.valueOf ^js a) b))

       (defmethod op [::v/real Fraction] [a b]
         (op a (.valueOf ^js b))))

     (defn- upcast-number
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

     ;; We handle the cases above where the exponent connects with integrals and
     ;; stays exact.
     (downcast-fraction g/expt)

     (doseq [op [g/add g/mul g/sub g/gcd g/lcm
                 g/modulo g/remainder
                 g/quotient g/div]]
       (upcast-number op)
       (downcast-fraction op))))
