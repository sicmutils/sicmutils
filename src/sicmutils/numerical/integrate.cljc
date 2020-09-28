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
  (:require [sicmutils.util :as u]
            [sicmutils.util.stopwatch :as us]
            [sicmutils.numerical.compile :as c]
            [taoensso.timbre :as log]))

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
      (u/illegal "Carlson R_F"))
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
        (if (> (max (Math/abs delx)
                    (Math/abs dely)
                    (Math/abs delz))
               errtol)
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

(defn elliptic-f
  "Legendre elliptic integral of the first kind F(φ, k).
   See W.H. Press, Numerical Recipes in C++, 2ed. eq. 6.11.19"
  [phi k]
  (let [s (Math/sin phi)]
    (* s (carlson-rf (Math/pow (Math/cos phi) 2)
                     (* (- 1 (* s k))
                        (+ 1 (* s k)))
                     1))))


(defn definite-integral
  "Evaluate the definite integral of f over [a, b].

  See: https://en.wikipedia.org/wiki/Adaptive_Simpson%27s_method"
  [f a b & {:keys [compile
                   epsilon]
            :or {compile false,
                 epsilon 1e-9}}]

  (letfn
      [(simpson-1
         [a fa b fb]
         (let [m (/ (+ a b) 2)
               fm (f m)]
           [m fm (* (/ (- b a) 6) (+ fa (* 4 fm) fb))]))
       (trapezoid-1
         [a fa b fb]
         (let [m (/ (+ a b) 2)
               fm (f m)]
           [m fm (* (/ (- b a) 4) (+ fa (* 2 fm) fb))]))
       (adaptive-simpson-recurse
         [a fa b fb epsilon whole m fm]
         (let [[lm flm l] (simpson-1 a fa m fm)
               [rm frm r] (simpson-1 m fm b fb)
               delta (- (+ l r) whole)]
           (if (<= (u/compute-abs delta) (* 15 epsilon))
             (+ l r (/ delta 15))
             (let [half-epsilon (/ epsilon 2)]
               (+ (adaptive-simpson-recurse a fa m fm half-epsilon l lm flm)
                  (adaptive-simpson-recurse m fm b fb half-epsilon r rm frm))))))]
    (let [fa (f a)
          fb (f b)
          [m fm whole] (simpson-1 a fa b fb)]
      (adaptive-simpson-recurse a fa b fb epsilon whole m fm))))
