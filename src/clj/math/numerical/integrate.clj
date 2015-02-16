;; Copyright (C) 2015 Colin Smith.
;; This work is based on the Scmutils system of MIT/GNU Scheme.
;;
;; This is free software;  you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or (at
;; your option) any later version.

;; This software is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this code; if not, see <http://www.gnu.org/licenses/>.

(ns math.numerical.integrate
  (:import (org.apache.commons.math3.analysis UnivariateFunction)
           (org.apache.commons.math3.analysis.integration RombergIntegrator)))

;; simple Simpson's rule to get things off the ground

(defn integrate [f a b]
  (.integrate (RombergIntegrator.) 10000 (proxy [UnivariateFunction] []
                                           (value [x] (f x))) a b))
