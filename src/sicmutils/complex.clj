;
; Copyright (C) 2016 Colin Smith.
; This work is based on the Scmutils system of MIT/GNU Scheme.
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
  (:require [sicmutils
             [value :as v]
             [generic :as g]])
  (:import [org.apache.commons.math3.complex Complex]))

(extend-type Complex
  v/Value
  (nullity? [c] (= Complex/ZERO c))
  (unity? [c] (= Complex/ONE c))
  (zero-like [_] Complex/ZERO)
  (one-like [_] Complex/ONE)
  (freeze [c] (list 'complex (.getReal c) (.getImaginary c)))
  (exact? [_] false)
  (numerical? [_] true)
  (kind [_] ::complex))

(defn complex
  "Construct a complex number from real, or real and imaginary, components."
  ([re]
   (Complex. re))
  ([re im]
   (Complex. re im)))

(defn complex?
  [a]
  (instance? Complex a))

(defmethod g/add [::complex ::complex] [^Complex a ^Complex b] (.add a b))
(defmethod g/add [::complex Number] [^Complex a n] (.add a (double n)))
(defmethod g/add [Number ::complex] [n ^Complex a] (.add a (double n)))
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
(defmethod g/abs [::complex] [^Complex a] (.abs a))
(defmethod g/exp [::complex] [^Complex a] (.exp a))
(defmethod g/log [::complex] [^Complex a] (.log a))
(defmethod g/square [::complex] [^Complex a] (.multiply a a))
(defmethod g/cube [::complex] [^Complex a] (.pow a 3.0))
(defmethod g/sqrt [::complex] [^Complex a] (.sqrt a))
(defmethod g/sin [::complex] [^Complex a] (.sin a))
(defmethod g/cos [::complex] [^Complex a] (.cos a))
(defmethod g/tan [::complex] [^Complex a] (.tan a))
(defmethod g/asin [::complex] [^Complex a] (.asin a))
(defmethod g/acos [::complex] [^Complex a] (.acos a))
(defmethod g/atan [::complex] [^Complex a] (.atan a))
(defmethod g/expt [::complex ::complex] [^Complex a ^Complex b] (.pow a b))
(defmethod g/expt [::complex Number] [^Complex a n] (.pow a (double n)))
(defmethod g/expt [Number ::complex] [n ^Complex a] (.pow ^Complex (complex n) a))
(defmethod g/magnitude [::complex] [^Complex a] (.abs a))

(defn conjugate [^Complex a] (.conjugate a))
(defn real-part [^Complex a] (.getReal a))
(defn imag-part [^Complex a] (.getImaginary a))
(defn angle [^Complex a] (.getArgument a))


(derive ::complex :sicmutils.expression/numerical-expression)
