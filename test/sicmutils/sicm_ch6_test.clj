(ns sicmutils.sicm-ch6-test
  (:refer-clojure :exclude [+ - * / zero? ref partial])
  (:require [clojure.test :refer :all]
            [sicmutils
             [value :as v]
             [numsymb]
             [env :refer :all]
             [series :as series]
             [operator :as o]
             [simplify :refer [hermetic-simplify-fixture]]]
            [sicmutils.mechanics.hamilton :refer :all]))

(deftest section-6-2
  (let [H0 (fn [alpha]
             (fn [[_ _ ptheta]]
               (/ (square ptheta) (* 2 alpha))))
        H1 (fn [beta]
             (fn [[_ theta _]]
               (* -1 beta (cos theta))))
        H-pendulum-series (fn [alpha beta epsilon]
                            (series (H0 alpha) (* epsilon (H1 beta))))
        W (fn [alpha beta]
            (fn [[_ theta ptheta]]
              (/ (* -1 alpha beta (sin theta)) ptheta)))
        a-state (up 't 'theta 'p_theta)
        L (Lie-derivative (W 'α 'β))
        H (H-pendulum-series 'α 'β 'ε)
        E (((exp (* 'ε L)) H) a-state)]
    (is (= 0 (simplify ((+ ((Lie-derivative (W 'alpha 'beta)) (H0 'alpha))
                           (H1 'beta))
                        a-state))))
    (is (= '[(/ (expt p_theta 2) (* 2 α))
             0
             (/ (* (expt (sin theta) 2) α (expt β 2) (expt ε 2)) (* 2N (expt p_theta 2)))
             0
             0]
           (simplify (series/take 5 E))))
    (is (= '(/ (+ (* (expt (sin theta) 2) (expt α 2) (expt β 2) (expt ε 2)) (expt p_theta 4))
               (* 2N (expt p_theta 2) α))
           (simplify (series:sum E 2))))))
