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

(ns sicmutils.numerical.integrate
  (:require [clojure.tools.logging :as log]
            [sicmutils.numerical.compile :refer :all]
            [sicmutils.numbers])
  (:import (org.apache.commons.math3.analysis UnivariateFunction)
           (com.google.common.base Stopwatch)
           (org.apache.commons.math3.analysis.integration RombergIntegrator
                                                          MidPointIntegrator
                                                          IterativeLegendreGaussIntegrator)))

(def ^:private method->integrator
  {:romberg #(RombergIntegrator.)
   :midpoint #(MidPointIntegrator.)
   :legendre-gauss #(IterativeLegendreGaussIntegrator. 64 1 256)})

(defn definite-integral [f a b & {:keys [compile max-evaluations method method-args]
                                  :or {compile false,
                                       max-evaluations 10000,
                                       method :romberg
                                       metohd-args []
                                       }}]
  {:pre [(contains? method->integrator method)]}
  (let [total-time (Stopwatch/createStarted)
        evaluation-count (atom 0)
        evaluation-time (Stopwatch/createUnstarted)
        integrand (if compile (compile-univariate-function f) f)
        value (.integrate ((method->integrator method))
                          max-evaluations
                          (reify UnivariateFunction
                            (value [_ x]
                              (.start evaluation-time)
                              (swap! evaluation-count inc)
                              (let [fx (integrand x)]
                                (.stop evaluation-time)
                                fx)))
                          a b)]
    (.stop total-time)
    (log/info "#" @evaluation-count "total" (str total-time) "f" (str evaluation-time))
    value))
