(ns math.examples.driven-pendulum-test
  (:refer-clojure :exclude [+ - * /])
  (:require [math.env :refer :all]
            [math.mechanics.lagrange :refer :all]
            [math.examples.driven-pendulum :as driven]
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
      (is (= '(+ (* ((D y) t) (sin θ) l m θdot)
                 (* 1/2 (expt l 2) m (expt θdot 2))
                 (* 1/2 (expt ((D y) t) 2) m))
             (simplify (T state))))
      (is (= '(+ (* ((D y) t) (sin θ) l m θdot)
                 (* 1/2 (expt l 2) m (expt θdot 2))
                 (* (cos θ) g l m)
                 (* 1/2 (expt ((D y) t) 2) m)
                 (* -1 (y t) g m))
             (simplify (L state))))
      (is (= '(+ (* -1N (cos (+ (* t ω) φ)) (sin (θ t)) A l m (expt ω 2))
                 (* (sin (θ t)) g l m)
                 (* (((expt D 2) θ) t) (expt l 2) m))
             (simplify (((Lagrange-equations
                           (driven/L-pend 'm 'l 'g (driven/periodic-drive 'A 'ω 'φ)))
                          θ)
                         't))))
      (is (driven/evolver 1 1/60 0.2 3 9.8 1.57 0)))))
