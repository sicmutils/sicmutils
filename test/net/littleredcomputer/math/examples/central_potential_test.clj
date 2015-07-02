(ns net.littleredcomputer.math.examples.central-potential-test
  (:refer-clojure :exclude [+ - * /])
  (:require [net.littleredcomputer.math.env :refer :all]
            [net.littleredcomputer.math.mechanics.lagrange :refer :all]
            [net.littleredcomputer.math.examples.central-potential :as central]
            [clojure.test :refer :all]))

(deftest equations
  (with-literal-functions
    [m M x y X Y]
    (let [state (up 't (up 'x 'y) (up 'xDot 'yDot))
          state2 (up 't (up 'm 'x 'y 'M 'X 'Y) (up 'dm 'dx 'dy 'dM 'dX 'dY))
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
      (is (= '(+ (* 1/2 m1 (expt x1' 2))
                 (* 1/2 m1 (expt y1' 2))
                 (* 1/2 m2 (expt x2' 2))
                 (* 1/2 m2 (expt y2' 2))
                 (/ (* m1 m2)
                    (sqrt (+ (expt x1 2) (expt y1 2)))))
             (simplify ((central/L) (up 0 (up 'm1 'x1 'y1 'm2 0 0) (up 0 'x1' 'y1' 0 'x2' 'y2'))))))
      ;; haven't checked these; we're still debugging this
      (let [state (up 0 (up 'm1 'x1 'y1 'm2 0 0) (up 'm1' 'x1' 'y1' 'm2' 'x2' 'y2'))
            F ((∂ 1) (central/L))
            P ((∂ 2) (central/L))
            A ((∂ 2) P)]
        (is (= '(down
                 (+ (* (/ 1 (sqrt (+ (expt x1 2) (expt y1 2)))) m2) (* 1/2 (expt x1' 2)) (* 1/2 (expt y1' 2)))
                 (* -2 (/ 1 (* 2 (sqrt (+ (expt x1 2) (expt y1 2))))) (/ (* m1 m2) (+ (expt x1 2) (expt y1 2))) x1)
                 (* -2 (/ 1 (* 2 (sqrt (+ (expt x1 2) (expt y1 2))))) (/ (* m1 m2) (+ (expt x1 2) (expt y1 2))) y1)
                 (+ (* (/ 1 (sqrt (+ (expt x1 2) (expt y1 2)))) m1) (* 1/2 (expt x2' 2)) (* 1/2 (expt y2' 2)))
                 (* 2 (/ 1 (* 2 (sqrt (+ (expt x1 2) (expt y1 2))))) (/ (* m1 m2) (+ (expt x1 2) (expt y1 2))) x1)
                 (* 2 (/ 1 (* 2 (sqrt (+ (expt x1 2) (expt y1 2))))) (/ (* m1 m2) (+ (expt x1 2) (expt y1 2))) y1))
               (simplify (F state))))
        (is (= '(down 0 (* m1 x1') (* m1 y1') 0 (* m2 x2') (* m2 y2'))
               (simplify (P state))))
        ;; well, this is certainly a singular matrix. Adding mass derivatives doesn't help.
        ;;
        (is (= '(down
                 (down 0 0 0 0 0 0)
                 (down 0 m1 0 0 0 0)
                 (down 0 0 m1 0 0 0)
                 (down 0 0 0 0 0 0)
                 (down 0 0 0 0 m2 0)
                 (down 0 0 0 0 0 m2))
               (simplify (A state)))))
      #_(is (= 'foo
               (simplify ((central/state-derivative) state2))))
      ;; weird. Should we let M, m enter the equations like this? What happens if we do?
      (is (= '(down
               (+ (* -1/2 (expt ((D x) t) 2))
                  (* -1/2 (expt ((D y) t) 2))
                  (* -1 (M t) (/ 1 (sqrt (+ (expt (x t) 2) (expt (y t) 2))))))
               (+ (* 2 (x t)
                     (/ 1 (* 2 (sqrt (+ (expt (x t) 2) (expt (y t) 2)))))
                     (/ (* (m t) (M t)) (+ (expt (x t) 2) (expt (y t) 2))))
                  (* (m t) (((expt D 2) x) t))
                  (* ((D m) t) ((D x) t)))
               (+ (* 2 (y t) (/ 1 (* 2 (sqrt (+ (expt (y t) 2) (expt (x t) 2)))))
                     (/ (* (m t) (M t)) (+ (expt (y t) 2) (expt (x t) 2))))
                  (* (m t)
                     (((expt D 2) y) t))
                  (* ((D m) t) ((D y) t)))
               (* -1 (m t) (/ 1 (sqrt (+ (expt (x t) 2) (expt (y t) 2)))))
               (* -2 (x t) (/ 1 (* 2 (sqrt (+ (expt (x t) 2) (expt (y t) 2))))) (/ (* (m t) (M t)) (+ (expt (x t) 2) (expt (y t) 2))))
               (* -2 (y t) (/ 1 (* 2 (sqrt (+ (expt (y t) 2) (expt (x t) 2))))) (/ (* (m t) (M t)) (+ (expt (y t) 2) (expt (x t) 2)))))
             (simplify (((Lagrange-equations (central/L))
                         (up m x y M (constantly 0) (constantly 0))) 't))))
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
