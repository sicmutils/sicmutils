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
            [sicmutils.numerical.compile :refer :all])
  (:import (org.apache.commons.math3.analysis UnivariateFunction)
           (com.google.common.base Stopwatch)
           (org.apache.commons.math3.analysis.integration UnivariateIntegrator
                                                          RombergIntegrator
                                                          MidPointIntegrator
                                                          IterativeLegendreGaussIntegrator)))

(defn ^:private select-integrator
  [method relative-accuracy absolute-accuracy min-iterations max-iterations points]
  (case method
    :romberg (RombergIntegrator. relative-accuracy
                                 absolute-accuracy
                                 min-iterations
                                 max-iterations)
    :midpoint (MidPointIntegrator. relative-accuracy
                                   absolute-accuracy
                                   min-iterations
                                   max-iterations)
    :legendre-gauss (IterativeLegendreGaussIntegrator. points
                                                       relative-accuracy
                                                       absolute-accuracy
                                                       min-iterations
                                                       max-iterations)))
(defn definite-integral [f a b & {:keys [compile
                                         method
                                         max-evaluations
                                         relative-accuracy
                                         absolute-accuracy
                                         min-iterations
                                         max-iterations
                                         points]
                                  :or {compile false,
                                       method :romberg,
                                       max-evaluations 32768,
                                       relative-accuracy 1e-6,
                                       absolute-accuracy 1e-15,
                                       min-iterations 3
                                       max-iterations 32
                                       points 16}
                                  :as options}]
  (let [total-time (Stopwatch/createStarted)
        evaluation-count (atom 0)
        evaluation-time (Stopwatch/createUnstarted)
        integrand (if compile (compile-univariate-function f) f)
        ^UnivariateIntegrator integrator (select-integrator method
                                                            relative-accuracy
                                                            absolute-accuracy
                                                            min-iterations
                                                            max-iterations points)
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
  "From W.H. Press, Numerical Recipes in C++, 2ed. NR::rf from section 6.11"
  (let [errtol 0.0025
        tiny 1.5e-38
        big 3.0e37
        third (/ 3.)
        c1 (/ 24.)
        c2 0.1
        c3 (/ 3. 44.)
        c4 (/ 14.)]
    (when (or (< (min x y z) 0)
              (< (min (+ x y) (+ x z) (+ y z)) tiny)
              (> (max x y z) big))
      (throw (IllegalArgumentException. "Carlson R_F")))
    (loop [xt x
           yt y
           zt z]
      (let [sqrtx (Math/sqrt xt)
            sqrty (Math/sqrt yt)
            sqrtz (Math/sqrt zt)
            alamb (+ (* sqrtx (+ sqrty sqrtz))
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
  "Legendre elliptic integral of the first kind F(φ, k).
   See W.H. Press, Numerical Recipes in C++, 2ed. eq. 6.11.19"
  (let [s (Math/sin phi)]
    (* s (carlson-rf (Math/pow (Math/cos phi) 2)
                     (* (- 1 (* s k))
                        (+ 1 (* s k)))
                     1))))
