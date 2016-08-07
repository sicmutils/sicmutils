(ns sicmutils.sicm-ch5-test
  (:refer-clojure :exclude [+ - * / zero? ref partial])
  (:require [clojure.test :refer :all]
            [sicmutils.env :refer :all]
            [sicmutils.mechanics.lagrange :refer :all]
            [sicmutils.mechanics.hamilton :refer :all]
            [sicmutils.simplify :refer [hermetic-simplify-fixture]]))

(deftest section-5-1
  (testing "central field"
    (is (= '(/
             (+
              (* 2 (V r) m (expt r 2))
              (* (expt p_r 2) (expt r 2))
              (expt p_phi 2))
             (* 2 m (expt r 2)))
           (simplify ((compose (H-central 'm (literal-function 'V))
                               (F->CT p->r))
                      (up 't
                          (up 'r 'phi)
                          (down 'p_r 'p_phi))))))))

(deftest section-5-2
  (testing "canonical"
    (is (= '(up 0 (up 0 0) (down 0 0))
           (simplify
            ((compositional-canonical?
              (F->CT p->r)
              (H-central 'm (literal-function 'V)))
             (up 't
                 (up 'r 'phi)
                 (down 'p_r 'p_phi))))))
    ;; We've moved the simplifier frontier back to this point.
    ;; This should give (up 0 0 0). This is currently blocked
    ;; on teaching the simplifier to reduce square roots of
    ;; perfect squares.
    #_(is (= '(up 0
                (/ (+ (* (sqrt (* 4N (expt I 2))) G__4770) (* -2N G__4770 I)) (sqrt (* 4N (expt I 2))))
                (/ (+ (* -1N (sqrt (* 4N (expt I 2))) G__4769) (* 2N G__4769 I)) (sqrt (* 4N (expt I 2)))))
           (simplify
            ((time-independent-canonical? (polar-canonical 'alpha))
             (up 't 'theta 'I)))))))
