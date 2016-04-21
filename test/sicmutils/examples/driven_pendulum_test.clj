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

(ns sicmutils.examples.driven-pendulum-test
  (:refer-clojure :exclude [+ - * / zero? partial ref])
  (:require [sicmutils.env :refer :all]
            [sicmutils.mechanics.lagrange :refer :all]
            [sicmutils.examples.driven-pendulum :as driven]
            [clojure.test :refer :all]))

(deftest equations
  (with-literal-functions
    [θ y]
    (let [state (up 't 'θ 'θdot)
          V (driven/V-pend 'm 'l 'g y)
          T (driven/T-pend 'm 'l 'g y)
          L (driven/L-pend 'm 'l 'g y)]
      (is (= '(+ (* -1 (cos θ) g l m) (* (y t) g m))
             (simplify (V state))))
      (is (= '(+
               (* ((D y) t) (sin θ) l m θdot)
               (* 1/2 (expt l 2) m (expt θdot 2))
               (* 1/2 (expt ((D y) t) 2) m))
             (simplify (T state))))
      (is (= '(+
               (* ((D y) t) (sin θ) l m θdot)
               (* 1/2 (expt l 2) m (expt θdot 2))
               (* (cos θ) g l m)
               (* 1/2 (expt ((D y) t) 2) m)
               (* -1 (y t) g m))
             (simplify (L state))))
      (is (= '(+ (* -1 (cos (+ (* t ω) φ)) (sin (θ t)) a l m (expt ω 2))
                 (* (sin (θ t)) g l m)
                 (* (((expt D 2) θ) t) (expt l 2) m))
             (simplify (((Lagrange-equations
                           (driven/L-pend 'm 'l 'g (driven/periodic-drive 'a 'ω 'φ)))
                          θ)
                         't))))
      (let [o (atom [])
            observe (fn [t q] (swap! o conj [t q]))]
        (do
          (driven/evolver {:t 3/60 :dt 1/60 :observe observe})
          (is (= 4 (count @o))))))))

(deftest as-javascript
  (let [eq (simplify
            ((driven/state-derivative 'm 'l 'g 'a 'omega 'phi)
             (up 't 'theta 'thetadot)))]
    ;; Interesting. This isn't exactly what we want. We want the fixed parameters
    ;; to be the parameters of a javascript outer function.
    (is (= (str "function(t, theta, thetadot) {\n"
                "  var _1 = Math.sin(theta);\n"
                "  return [1, thetadot, (_1 * Math.cos(omega * t + phi) * a * Math.pow(omega, 2) - _1 * g) / l];\n}")
           (->JavaScript eq :parameter-order '[t theta thetadot])))))
