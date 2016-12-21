(ns sicmutils.sicm-ch6-test
  (:refer-clojure :exclude [+ - * / zero? ref partial])
  (:require [clojure.test :refer :all]
            [sicmutils
             [numsymb]
             [env :refer :all]
             [series :as series]
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
        a-state (up 't 'theta 'p_theta)]
    (is (= 0 (simplify ((+ ((Lie-derivative (W 'alpha 'beta)) (H0 'alpha))
                           (H1 'beta))
                        a-state))))

    #_(is (= 'foo (type
                 (((exp (* 'epsilon (Lie-derivative (W 'alpha 'beta))))
                   (H-pendulum-series 'alpha 'beta 'epsilon))
                  a-state))))
    #_(is (= 0 (simplify
              (series:sum
               (((exp (* 'epsilon (Lie-derivative (W 'alpha 'beta))))
                 (H-pendulum-series 'alpha 'beta 'epsilon))
                a-state)
               2))))
    ))
