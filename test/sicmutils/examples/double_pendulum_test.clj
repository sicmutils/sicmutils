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

(ns sicmutils.examples.double-pendulum-test
  (:refer-clojure :exclude [+ - * / zero? partial ref])
  (:require [sicmutils.env :refer :all]
            [sicmutils.mechanics.lagrange :refer :all]
            [sicmutils.examples.double-pendulum :as double]
            [clojure.test :refer :all]))

(deftest equations
  (let [state (up 't (up 'θ 'φ) (up 'θdot 'φdot))
        V (double/V-double-pend 'm1 'm2 'l1 'l2 'g)
        T (double/T-double-pend 'm1 'm2 'l1 'l2 'g)
        L (double/L-double-pend 'm1 'm2 'l1 'l2 'g)]
    (is (= '(+ (* -1 (cos θ) g l1 m1)
               (* -1 (cos θ) g l1 m2)
               (* -1 (cos φ) g l2 m2))
           (simplify (V state))))
    (is (= '(+
             (* (cos (+ θ (* -1 φ))) l1 l2 m2 θdot φdot)
             (* 1/2 (expt l1 2) m1 (expt θdot 2))
             (* 1/2 (expt l1 2) m2 (expt θdot 2))
             (* 1/2 (expt l2 2) m2 (expt φdot 2)))
           (simplify (T state))))
    (is (= '(+
             (* (cos (+ θ (* -1 φ))) l1 l2 m2 θdot φdot)
             (* 1/2 (expt l1 2) m1 (expt θdot 2))
             (* 1/2 (expt l1 2) m2 (expt θdot 2))
             (* 1/2 (expt l2 2) m2 (expt φdot 2))
             (* (cos θ) g l1 m1)
             (* (cos θ) g l1 m2)
             (* (cos φ) g l2 m2))
           (simplify (L state))))
    (with-literal-functions [θ φ]
      (is (= '(down (+ (* (expt ((D φ) t) 2) (sin (+ (θ t) (* -1 (φ t)))) l1 l2 m2)
                       (* (((expt D 2) φ) t) (cos (+ (θ t) (* -1 (φ t)))) l1 l2 m2)
                       (* (((expt D 2) θ) t) (expt l1 2) m1)
                       (* (((expt D 2) θ) t) (expt l1 2) m2)
                       (* (sin (θ t)) g l1 m1)
                       (* (sin (θ t)) g l1 m2))
                    (+ (* -1 (expt ((D θ) t) 2) (sin (+ (θ t) (* -1 (φ t)))) l1 l2 m2)
                       (* (((expt D 2) θ) t) (cos (+ (θ t) (* -1 (φ t)))) l1 l2 m2)
                       (* (((expt D 2) φ) t) (expt l2 2) m2) (* (sin (φ t)) g l2 m2)))
             (simplify (((Lagrange-equations
                          (double/L-double-pend 'm1 'm2 'l1 'l2 'g))
                         (up θ φ))
                        't)))))
    (let [o (atom [])
          observe (fn [t q] (swap! o conj [t q]))]
      (do
        (double/evolver {:t 3/60 :dt 1/60 :observe observe})
        (is (= 4 (count @o)))))))
