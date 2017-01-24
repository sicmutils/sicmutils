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
        L (Lie-derivative (W 'alpha 'beta))
        H (H-pendulum-series 'alpha 'beta 'ε)
        E (((exp (* 'ε L)) H) a-state)
        ]


    (is (= 0 (simplify ((+ ((Lie-derivative (W 'alpha 'beta)) (H0 'alpha))
                           (H1 'beta))
                        a-state))))
    (is (= 0 (simplify
              (series/take 4 E))))
    (is (= 0 (simplify (series:sum E 2))))

    (let [a (H a-state)
          b (((* 'ε L) H) a-state)
          c (((* 1/2 (expt (* 'ε L) 2)) H) a-state)
          d (((* 1/6 (expt (* 'ε L) 3)) H) a-state)]
      (is (= 0 (simplify (series/take 4 (+ a b c d)))))
      (is (= 0 (simplify (series:sum (+ a b c d) 2)))))))
