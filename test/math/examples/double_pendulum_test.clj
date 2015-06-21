(ns math.examples.double-pendulum-test
  (:refer-clojure :exclude [+ - * /])
  (:require [math.env :refer :all]
            [math.mechanics.lagrange :refer :all]
            [math.examples.double-pendulum :as double]
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
    (is (= '(+ (* (cos (+ θ (* -1 φ))) l1 l2 m2 θdot φdot)
               (* 1/2 (expt l1 2) m1 (expt θdot 2))
               (* 1/2 (expt l1 2) m2 (expt θdot 2))
               (* 1/2 (expt l2 2) m2 (expt φdot 2)))
           (simplify (T state))))
    (is (= '(+ (* (cos (+ θ (* -1 φ))) l1 l2 m2 θdot φdot)
               (* 1/2 (expt l1 2) m1 (expt θdot 2))
               (* 1/2 (expt l1 2) m2 (expt θdot 2))
               (* 1/2 (expt l2 2) m2 (expt φdot 2))
               (* (cos θ) g l1 m1) (* (cos θ) g l1 m2) (* (cos φ) g l2 m2))
           (simplify (L state))))
    (with-literal-functions [θ φ]
                            (is (= '(down (+ (* (expt ((D φ) t) 2) (sin (+ (θ t) (* -1 (φ t)))) l1 l2 m2)
                                             (* (((expt D 2) φ) t) (cos (+ (θ t) (* -1 (φ t)))) l1 l2 m2)
                                             (* (((expt D 2) θ) t) (expt l1 2) m1)
                                             (* (((expt D 2) θ) t) (expt l1 2) m2)
                                             (* (sin (θ t)) g l1 m1)
                                             (* (sin (θ t)) g l1 m2))
                                          (+ (* -1N (expt ((D θ) t) 2) (sin (+ (θ t) (* -1 (φ t)))) l1 l2 m2)
                                             (* (((expt D 2) θ) t) (cos (+ (θ t) (* -1 (φ t)))) l1 l2 m2)
                                             (* (((expt D 2) φ) t) (expt l2 2) m2)
                                             (* (sin (φ t)) g l2 m2)))
                                   (simplify (((Lagrange-equations
                                                 (double/L-double-pend 'm1 'm2 'l1 'l2 'g))
                                                (up θ φ))
                                               't)))))
    (is (double/evolver 1 1/60 9.8 1 0.4 1 0 2 0.6 -1 0))))
