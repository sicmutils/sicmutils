(ns net.littleredcomputer.math.examples.central-potential-test
  (:refer-clojure :exclude [+ - * /])
  (:require [net.littleredcomputer.math.env :refer :all]
            [net.littleredcomputer.math.mechanics.lagrange :refer :all]
            [net.littleredcomputer.math.examples.central-potential :as central]
            [clojure.test :refer :all]))

(deftest equations
  (with-literal-functions
    [x y]
    (let [state (up 't (up 'x 'y) (up 'xDot 'yDot))
          L (central/L-central 'M 'm 0 0)]
      (is (= '(+ (* 1/2 M (expt xDot 2))
                 (* 1/2 M (expt yDot 2))
                 (* -1 (/ (* -1 M m) (sqrt (+ (expt x 2) (expt y 2))))))
             (simplify (L state))))
      (is (= '(+ (* 1/2 m1 (expt x1' 2))
                 (* 1/2 m1 (expt y1' 2))
                 (* 1/2 m2 (expt x2' 2))
                 (* 1/2 m2 (expt y2' 2)))
             (simplify ((central/T) (up 0 (up 'm1 'x1 'y1 'm2 'x2 'y2) (up 0 'x1' 'y1' 0 'x2' 'y2'))))))
      (is (= '(* -1 (/ (* m1 m2)
                       (sqrt (+ (expt x1 2) (expt y1 2)))))
             (simplify ((central/V) (up 0 (up 'm1 'x1 'y1 'm2 0 0) (up 0 'x1' 'y1' 0 'x2' 'y2'))))))
      (is (= '(down
               (+ (* -2 (x t)
                     (/ 1 (* 2 (sqrt (+ (expt (x t) 2) (expt (y t) 2)))))
                     (/ (* -1 M m) (+ (expt (x t) 2) (expt (y t) 2))))
                  (* (((expt D 2) x) t) M))
               (+ (* -2 (y t)
                     (/ 1 (* 2 (sqrt (+ (expt (y t) 2) (expt (x t) 2)))))
                     (/ (* -1 M m) (+ (expt (y t) 2) (expt (x t) 2))))
                  (* (((expt D 2) y) t) M)))
             (simplify (((Lagrange-equations L) (up x y)) 't))))
      (is (central/evolver 1 1/60 1 20 20 -2 0)))))
