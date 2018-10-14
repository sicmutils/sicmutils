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
  (:require [sicmutils
             [generic :as g]])
  (:import [org.apache.commons.math3.complex Complex]))

(defn complex
  "Construct a complex number from real, or real and imaginary, components."
  ([re]
   (Complex. re))
  ([re im]
   (Complex. re im)))

(defn complex?
  [a]
  (instance? Complex a))

(extend-type Complex
  g/INumericType
  (zero? [c] (= c Complex/ZERO))
  (one? [c] (= c Complex/ONE)))

(defmethod g/add [Complex Complex] [^Complex a ^Complex b] (.add a b))
(defmethod g/add [Complex :sicmutils.numsymb/native-numeric-type] [^Complex a n] (.add a (double n)))
(defmethod g/add [:sicmutils.numsymb/native-numeric-type Complex] [n ^Complex a] (.add a (double n)))
(defmethod g/sub [Complex Complex] [^Complex a ^Complex b] (.subtract a b))
(defmethod g/sub [Complex :sicmutils.numsymb/native-numeric-type] [^Complex a n] (.subtract a (double n)))
(defmethod g/sub [:sicmutils.numsymb/native-numeric-type Complex] [n ^Complex a] (.add (.negate a) (double n)))
(defmethod g/mul [Complex Complex] [^Complex a ^Complex b] (.multiply a b))
(defmethod g/mul [Complex :sicmutils.numsymb/native-numeric-type] [^Complex a n] (.multiply a (double n)))
(defmethod g/mul [:sicmutils.numsymb/native-numeric-type Complex] [n ^Complex a] (.multiply a (double n)))
(defmethod g/div [Complex Complex] [^Complex a ^Complex b] (.divide a b))
(defmethod g/div [Complex :sicmutils.numsymb/native-numeric-type] [^Complex a n] (.divide a (double n)))
(defmethod g/div [:sicmutils.numsymb/native-numeric-type Complex] [n ^Complex a] (.multiply (.reciprocal a) (double n)))
(defmethod g/negate [Complex] [^Complex a] (.negate a))
(defmethod g/invert [Complex] [^Complex a] (.reciprocal a))
(defmethod g/abs [Complex] [^Complex a] (.abs a))
(defmethod g/exp [Complex] [^Complex a] (.exp a))
(defmethod g/log [Complex] [^Complex a] (.log a))
(defmethod g/square [Complex] [^Complex a] (.multiply a a))
(defmethod g/cube [Complex] [^Complex a] (.pow a 3.0))
(defmethod g/sqrt [Complex] [^Complex a] (.sqrt a))
(defmethod g/sin [Complex] [^Complex a] (.sin a))
(defmethod g/cos [Complex] [^Complex a] (.cos a))
(defmethod g/tan [Complex] [^Complex a] (.tan a))
(defmethod g/asin [Complex] [^Complex a] (.asin a))
(defmethod g/acos [Complex] [^Complex a] (.acos a))
(defmethod g/atan [Complex] [^Complex a] (.atan a))
(defmethod g/expt [Complex Complex] [^Complex a ^Complex b] (.pow a b))
(defmethod g/expt [Complex :sicmutils.numsymb/native-numeric-type] [^Complex a n] (.pow a (double n)))
(defmethod g/expt [:sicmutils.numsymb/native-numeric-type Complex] [n ^Complex a] (.pow ^Complex (complex n) a))
(defmethod g/magnitude [Complex] [^Complex a] (.abs a))
(defmethod g/freeze [Complex] [^Complex c] (list 'complex (.getReal c) (.getImaginary c)))
(defmethod g/numerical? [Complex] [_] true)

(derive Complex :sicmutils.numsymb/numeric-type)

(defn conjugate [^Complex a] (.conjugate a))
(defn real-part [^Complex a] (.getReal a))
(defn imag-part [^Complex a] (.getImaginary a))
(defn angle [^Complex a] (.getArgument a))
