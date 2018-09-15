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
           (org.apache.commons.math3.analysis.integration UnivariateIntegrator
                                                          RombergIntegrator
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
        integrator ^UnivariateIntegrator ((method->integrator method))
        value (.integrate integrator
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

(defn carlson-rf [x y z]
  (let [errtol 0.08
        tiny 1.5e-38
        big 3e37
        third (/ 3.)
        c1 (/ 24.)
        c2 0.1
        c3 (/ 3. 44.)
        c4 (/ 14.)]
    (when (or (< (min x y z) 0)
              (< (min (+ x y) (+ x z) (+ y z)) tiny)
              (> (max x y z) big))
      (throw (IllegalArgumentException.)))
    (loop [xt x
           yt y
           zt z]
      (let [sqrtx (Math/sqrt xt)
            sqrty (Math/sqrt yt)
            sqrtz (Math/sqrt zt)
            alamb (+ (* sqrtx
                        (+ sqrty sqrtz))
                     (* sqrty sqrtz))
            xt' (* 0.25 (+ xt alamb))
            yt' (* 0.25 (+ yt alamb))
            zt' (* 0.25 (+ zt alamb))
            ave (* third (+ xt' yt' zt'))
            delx (/ (- ave xt') ave)
            dely (/ (- ave yt') ave)
            delz (/ (- ave zt') ave)]
        (if (> (max (Math/abs delx) (Math/abs dely) (Math/abs delz)) errtol)
          (recur xt' yt' zt')
          (let [e2 (- (* delx dely) (* delz delz))
                e3 (* delx dely delz)]
            (/ (+ 1
                  (* (- (* c1 e2)
                        c2
                        (* c3 e3))
                     e2)
                  (* c4 e3))
               (Math/sqrt ave))))))))

(defn elliptic-f [phi k]
  (let [s (Math/sin phi)]
    (* s (carlson-rf (Math/pow (Math/cos phi) 2)
                     (* (- 1 (* s k))
                        (+ 1 (* s k)))
                     1))))
