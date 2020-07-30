;
; Copyright © 2017 Colin Smith.
; This work is based on the Scmutils system of MIT/GNU Scheme:
; Copyright © 2002 Massachusetts Institute of Technology
;
; This is free software;  you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3 of the License, or (at
; your option) any later version.
;
; This software is distributed in the hope that it will be useful, but
; WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
; General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this code; if not, see <http://www.gnu.org/licenses/>.
;

(ns sicmutils.complex
  (:require [sicmutils.generic :as g]
            [sicmutils.value :as v]
            [sicmutils.util :as u]
            #?(:cljs [cljsjs.complex :as Complex]))
  #?(:clj
     (:import [org.apache.commons.math3.complex Complex])))

(def ZERO #?(:clj Complex/ZERO :cljs (.-ZERO Complex)))
(def ONE #?(:clj Complex/ONE :cljs (.-ONE Complex)))

(defn complex
  "Construct a complex number from real, or real and imaginary, components."
  ([re]
   (Complex. re))
  ([re im]
   (Complex. re im)))

(defn complex?
  [a]
  (instance? Complex a))

(defn conjugate [^Complex a] (.conjugate a))
(defn real-part [^Complex a] (#?(:clj .getReal :cljs .-re) a))
(defn imag-part [^Complex a] (#?(:clj .getImaginary :cljs .-im) a))
(defn angle [^Complex a] (#?(:clj .getArgument :cljs .arg) a))

(derive ::complex :sicmutils.expression/numerical-expression)

#?(:cljs
   (extend-type Complex
     IEquiv
     (-equiv [this other]
       (.equals this other))))

(extend-type Complex
  v/Value
  (nullity? [c] #?(:clj (= ZERO c) :cljs (.isZero c)))
  (unity? [c] (= ONE c))
  (zero-like [_] ZERO)
  (one-like [_] ONE)
  (freeze [c] (list 'complex (real-part c) (imag-part c)))
  (exact? [c] (and (v/exact? (real-part c))
                   (v/exact? (imag-part c))))
  (numerical? [_] true)
  (kind [_] ::complex))

(defmethod g/add [::complex ::complex] [^Complex a ^Complex b] (.add a b))
(defmethod g/add [::complex u/numtype] [^Complex a n] (.add a (double n)))
(defmethod g/add [u/numtype ::complex] [n ^Complex a] (.add a (double n)))

(defmethod g/expt [::complex ::complex] [^Complex a ^Complex b] (.pow a b))
(defmethod g/expt [::complex u/numtype] [^Complex a n] (.pow a (double n)))
(defmethod g/expt [u/numtype ::complex] [n ^Complex a] (.pow ^Complex (complex n) a))

(defmethod g/abs [::complex] [^Complex a] (.abs a))
(defmethod g/exp [::complex] [^Complex a] (.exp a))
(defmethod g/log [::complex] [^Complex a] (.log a))
(defmethod g/sqrt [::complex] [^Complex a] (.sqrt a))
(defmethod g/sin [::complex] [^Complex a] (.sin a))
(defmethod g/cos [::complex] [^Complex a] (.cos a))
(defmethod g/tan [::complex] [^Complex a] (.tan a))
(defmethod g/asin [::complex] [^Complex a] (.asin a))
(defmethod g/acos [::complex] [^Complex a] (.acos a))
(defmethod g/atan [::complex] [^Complex a] (.atan a))
(defmethod g/magnitude [::complex] [^Complex a] (.abs a))

;;The remaining methods have different names in the Clojure vs JS
;;implementations.
#?(:clj
   (do
     (defmethod g/sub [::complex ::complex] [^Complex a ^Complex b] (.subtract a b))
     (defmethod g/sub [::complex Number] [^Complex a n] (.subtract a (double n)))
     (defmethod g/sub [Number ::complex] [n ^Complex a] (.add (.negate a) (double n)))

     (defmethod g/mul [::complex ::complex] [^Complex a ^Complex b] (.multiply a b))
     (defmethod g/mul [::complex Number] [^Complex a n] (.multiply a (double n)))
     (defmethod g/mul [Number ::complex] [n ^Complex a] (.multiply a (double n)))

     (defmethod g/div [::complex ::complex] [^Complex a ^Complex b] (.divide a b))
     (defmethod g/div [::complex Number] [^Complex a n] (.divide a (double n)))
     (defmethod g/div [Number ::complex] [n ^Complex a] (.multiply (.reciprocal a) (double n)))

     (defmethod g/negate [::complex] [^Complex a] (.negate a))
     (defmethod g/invert [::complex] [^Complex a] (.reciprocal a))
     (defmethod g/square [::complex] [^Complex a] (.multiply a a))
     (defmethod g/cube [::complex] [^Complex a] (.pow a 3.0)))

   :cljs
   (do
     (defmethod g/sub [::complex ::complex] [a b] (.sub a b))
     (defmethod g/sub [::complex js/Number] [a n] (.sub a (double n)))
     (defmethod g/sub [js/Number ::complex] [n a] (.add (.neg a) (double n)))

     (defmethod g/mul [::complex ::complex] [a b] (.mul a b))
     (defmethod g/mul [::complex js/Number] [a n] (.mul a (double n)))
     (defmethod g/mul [js/Number ::complex] [n a] (.mul a (double n)))

     (defmethod g/div [::complex ::complex] [a b] (.div a b))
     (defmethod g/div [::complex js/Number] [a n] (.div a (double n)))
     (defmethod g/div [js/Number ::complex] [n a] (.mul (.inverse a) (double n)))

     (defmethod g/negate [::complex] [a] (.neg a))
     (defmethod g/invert [::complex] [a] (.inverse a))
     (defmethod g/square [::complex] [a] (.mul a a))
     (defmethod g/cube [::complex] [a] (.pow a 3.0))))
