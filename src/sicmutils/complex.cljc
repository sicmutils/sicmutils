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
  (:require [sicmutils.expression :as x]
            [sicmutils.generic :as g]
            [sicmutils.value :as v]
            #?(:cljs ["complex.js" :as Complex]))
  #?(:clj
     (:import [org.apache.commons.math3.complex Complex ComplexFormat])))

(def ZERO #?(:clj Complex/ZERO :cljs (.-ZERO Complex)))
(def ONE #?(:clj Complex/ONE :cljs (.-ONE Complex)))

(def complextype Complex)

(derive ::complex ::x/numerical-expression)

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

(def ^{:doc "Parser that converts a string representation of a complex number,
  like `1 + 3i`, into a Complex number object in clj or cljs."}
  parse-complex
  #?(:clj (let [cf (ComplexFormat.)]
            (fn [s]
              (let [v (.parse cf s)]
                `(complex ~(real-part v)
                          ~(imag-part v)))))

     :cljs (fn [s] `(complex ~s))))

#?(:cljs
   (extend-type Complex
     IEquiv
     (-equiv [this other]
       (.equals this other))

     IPrintWithWriter
     (-pr-writer [x writer opts]
       (write-all writer "#sicm/complex \"" (.toString x) "\""))))

#?(:clj
   ;; Clojure implementation of a printer that will emit items that can
   ;; round-trip via #sicm/complex.
   (let [cf (ComplexFormat.)]
     (defmethod print-method Complex [^Complex v ^java.io.Writer w]
       (.write w (str "#sicm/complex \""
                      (.format cf v)
                      "\"")))))

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
(defmethod g/add [::complex v/numtype] [^Complex a n] (.add a (double n)))
(defmethod g/add [v/numtype ::complex] [n ^Complex a] (.add a (double n)))

(defmethod g/expt [::complex ::complex] [^Complex a ^Complex b] (.pow a b))
(defmethod g/expt [::complex v/numtype] [^Complex a n] (.pow a (double n)))
(defmethod g/expt [v/numtype ::complex] [n ^Complex a] (.pow ^Complex (complex n) a))

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

(defmethod g/cosh [::complex] [^Complex a] (.cosh a))
(defmethod g/sinh [::complex] [^Complex a] (.sinh a))
(defmethod g/tanh [::complex] [^Complex a] (.tanh a))

#?(:cljs
   ;; These are all defined explicitly in Complex.js.
   (do
     (defmethod g/cot [::complex] [^Complex a] (.cot a))
     (defmethod g/sec [::complex] [^Complex a] (.sec a))
     (defmethod g/csc [::complex] [^Complex a] (.csc a))
     (defmethod g/tanh [::complex] [^Complex a] (.tanh a))
     (defmethod g/sech [::complex] [^Complex a] (.sech a))
     (defmethod g/csch [::complex] [^Complex a] (.csch a))
     (defmethod g/acosh [::complex] [^Complex a] (.acosh a))
     (defmethod g/asinh [::complex] [^Complex a] (.asinh a))
     (defmethod g/atanh [::complex] [^Complex a] (.atanh a))))

;;The remaining methods have different names in the Clojure vs JS
;;implementations.
#?(:clj
   (do
     (defmethod g/sub [::complex ::complex] [^Complex a ^Complex b] (.subtract a b))
     (defmethod g/sub [::complex v/numtype] [^Complex a n] (.subtract a (double n)))
     (defmethod g/sub [v/numtype ::complex] [n ^Complex a] (.add (.negate a) (double n)))

     (defmethod g/mul [::complex ::complex] [^Complex a ^Complex b] (.multiply a b))
     (defmethod g/mul [::complex v/numtype] [^Complex a n] (.multiply a (double n)))
     (defmethod g/mul [v/numtype ::complex] [n ^Complex a] (.multiply a (double n)))

     (defmethod g/div [::complex ::complex] [^Complex a ^Complex b] (.divide a b))
     (defmethod g/div [::complex v/numtype] [^Complex a n] (.divide a (double n)))
     (defmethod g/div [v/numtype ::complex] [n ^Complex a] (.multiply (.reciprocal a) (double n)))

     (defmethod g/negate [::complex] [^Complex a] (.negate a))
     (defmethod g/invert [::complex] [^Complex a] (.reciprocal a))
     (defmethod g/square [::complex] [^Complex a] (.multiply a a))
     (defmethod g/cube [::complex] [^Complex a] (.pow a 3.0)))

   :cljs
   (do
     (defmethod g/sub [::complex ::complex] [^Complex a ^Complex b] (.sub a b))
     (defmethod g/sub [::complex v/numtype] [^Complex a n] (.sub a (double n)))
     (defmethod g/sub [v/numtype ::complex] [n ^Complex a] (.add (.neg a) (double n)))

     (defmethod g/mul [::complex ::complex] [^Complex a ^Complex b] (.mul a b))
     (defmethod g/mul [::complex v/numtype] [^Complex a n] (.mul a (double n)))
     (defmethod g/mul [v/numtype ::complex] [n ^Complex a] (.mul a (double n)))

     (defmethod g/div [::complex ::complex] [^Complex a ^Complex b] (.div a b))
     (defmethod g/div [::complex v/numtype] [^Complex a n] (.div a (double n)))
     (defmethod g/div [v/numtype ::complex] [n ^Complex a] (.mul ^Complex (.inverse a) (double n)))

     (defmethod g/negate [::complex] [^Complex a] (.neg a))
     (defmethod g/invert [::complex] [^Complex a] (.inverse a))
     (defmethod g/square [::complex] [^Complex a] (.mul a a))
     (defmethod g/cube [::complex] [^Complex a] (.pow a 3.0))))
