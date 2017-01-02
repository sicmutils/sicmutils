(ns sicmutils.sicm-ch6-test
  (:refer-clojure :exclude [+ - * / zero? ref partial])
  (:require [clojure.test :refer :all]
            [sicmutils
             [value :as v]
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
    (is (= '((/ (expt p_theta 2) (* 2 a)) (* -1 (cos theta) b e) 0 0 0 0)
           (simplify
            (series/take 6
                         ((H-pendulum-series 'a 'b 'e) a-state)))))

    (let [H (H-pendulum-series 'a 'b 'e)
          L (Lie-derivative (W 'a 'b))]
      (is (= '[(/ (expt p_theta 2) (* 2 a))
               (* -1 (cos theta) b e)
               0]
             (simplify (series/take 3 (H a-state)))))
      (is (= [:exactly 0] (v/arity H)))
      (is (= [:exactly 1] (v/arity L)))
      (is (= [:exactly 1] (v/arity (L H))))
      (is (= '[(* (cos theta) b)
               (/ (* (expt (sin theta) 2) a (expt b 2) e) (expt p_theta 2))]
             (simplify
                (series/take 2
                             ((L H) a-state)))))
      )
    #_(is (= 0 (simplify
              (series:sum
               (((exp (* 'epsilon (Lie-derivative (W 'alpha 'beta))))
                 (H-pendulum-series 'alpha 'beta 'epsilon))
                a-state)
               2))))
    ))
