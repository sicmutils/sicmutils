(ns sicmutils.generators
  "test.check generators for the various types in the sicmutils project."
  (:refer-clojure :rename {bigint core-bigint
                           biginteger core-biginteger
                           double core-double
                           long core-long}
                  #?@(:cljs [:exclude [bigint double long]]))
  (:require [clojure.test.check.generators :as gen]
            [same :refer [zeroish?]]
            [same.ish :as si]
            [sicmutils.complex :as c]
            [sicmutils.generic :as g]
            [sicmutils.matrix :as m]
            [sicmutils.ratio :as r]
            [sicmutils.util :as u]
            [sicmutils.value :as v])
  #?(:clj
     (:import [org.apache.commons.math3.complex Complex])))

(def bigint
  "js/BigInt in cljs, clojure.lang.BigInt in clj."
  #?(:cljs
     (gen/fmap u/bigint gen/large-integer)
     :clj
     gen/size-bounded-bigint))

(def biginteger
  #?(:cljs bigint
     :clj (gen/fmap u/biginteger bigint)))

(def native-integral
  "non-floating-point integers on cljs, Long on clj."
  gen/small-integer)

(def long
  "goog.math.Long in cljs,
  java.lang.Long in clj."
  #?(:clj gen/large-integer
     :cljs (gen/fmap u/long gen/large-integer)))

(def integer
  "goog.math.Integer in cljs, java.lang.Integer in clj."
  (gen/fmap u/int gen/small-integer))

(defn reasonable-double
  ([] (reasonable-double {}))
  ([{:keys [min max]
     :or {min -10e5
          max 10e5}}]
   (let [[excluded-lower excluded-upper] [-1e-4 1e-4]]
     (gen/one-of [(gen/double* {:infinite? false
                                :NaN? false
                                :min min
                                :max excluded-lower})
                  (gen/double* {:infinite? false
                                :NaN? false
                                :min excluded-upper
                                :max max})]))))

(defn inexact-double
  ([] (inexact-double {}))
  ([opts]
   (->> (reasonable-double opts)
        (gen/fmap (fn [x]
                    (if (v/exact? x)
                      (+ x 0.5)
                      x))))))

(def any-integral
  (gen/one-of [native-integral
               bigint
               long
               integer]))

(def real
  (gen/one-of [any-integral (reasonable-double)]))

(defn reasonable-real [bound]
  (let [bound    (core-long bound)
        integral (gen/fmap
                  #(g/remainder % bound)
                  any-integral)]
    (gen/one-of [integral (reasonable-double
                           {:min (- bound)
                            :max bound})])))

(def complex
  (gen/let [r (reasonable-double)
            i (reasonable-double)]
    (c/complex r i)))

(def ratio
  "Generates a small ratio (or integer) using gen/small-integer. Shrinks
  toward simpler ratios, which may be larger or smaller."
  (gen/fmap
   (fn [[a b]] (r/rationalize a b))
   (gen/tuple gen/small-integer (gen/fmap inc gen/nat))))

(def big-ratio
  (gen/let [n bigint
            d bigint]
    (let [d (if (v/zero? d)
              (u/bigint 1)
              d)]
      (r/rationalize n d))))

(defn square-matrix
  ([n] (square-matrix n ratio))
  ([n entry-gen]
   (gen/fmap #(apply m/by-rows %)
             (gen/vector (gen/vector entry-gen n) n))))

(defn- eq-delegate
  "Takes a real number `this` on the left, and checks it for approximate equality
  with:

  - complex numbers by comparing to the real part and checking that `that`'s
    imaginary part is roughly ~0
  - real numbers with the default approximate check behavior
  - falls through to `=` for all other types.

  In CLJS, `this` can be `js/BigInt`, `google.math.Long`, `goog.math.Real`,
  `Fraction` or any of the other types in the numeric tower."
  [this that]
  (cond (c/complex? that) (and (si/*comparator* 0.0 (g/imag-part that))
                               (si/*comparator*
                                (u/double this)
                                (g/real-part that)))
        (v/real? that)    (si/*comparator*
                           (u/double this)
                           (u/double that))
        :else             (= this that)))

(extend-protocol si/Approximate
  #?@(:cljs
      [r/ratiotype
       (ish [this that] (eq-delegate this that))

       u/inttype
       (ish [this that] (eq-delegate this that))

       u/longtype
       (ish [this that] (eq-delegate this that))

       js/BigInt
       (ish [this that] (eq-delegate this that))

       number
       (ish [this that] (eq-delegate this that))])

  #?@(:clj
      [Double
       (ish [this that] (eq-delegate this that))

       Float
       (ish [this that] (eq-delegate this that))

       Number
       (ish [this that] (eq-delegate this that))])

  #?(:cljs c/complextype :clj Complex)
  (ish [this that]
    (cond (c/complex? that)
          (and (si/*comparator* (g/real-part this)
                                (g/real-part that))
               (si/*comparator* (g/imag-part this)
                                (g/imag-part that)))
          (v/real? that)
          (and (si/*comparator* 0.0 (g/imag-part this))
               (si/*comparator*
                (g/real-part this) (core-double that)))
          :else (= this that))))
