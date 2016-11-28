(ns sicmutils.sicm-ch5-test
  (:refer-clojure :exclude [+ - * / zero? ref partial])
  (:require [clojure.test :refer :all :exclude [function?]]
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
    (is (= '(up 0 0 0)
           (simplify
            ((time-independent-canonical? (polar-canonical 'alpha))
             (up 't 'theta 'I)))))
    (let [a-non-canonical-transform (fn [[t theta p]]
                                      (let [x (* p (sin theta))
                                            p_x (* p (cos theta))]
                                        (up t x p_x)))]
      (is (not= '(up 0 0 0)
             (simplify
              ((time-independent-canonical? a-non-canonical-transform)
               (up 't 'theta 'p))))))
    (let [J-func (fn [[_ dh1 dh2]] (up 0 dh2 (- dh1)))]
      (is (= '(matrix-by-rows [0 0 0 0 0]
                              [0 0 0 1 0]
                              [0 0 0 0 1]
                              [0 -1 0 0 0]
                              [0 0 -1 0 0])
             (simplify (let [s (up 't (up 'x 'y) (down 'px 'py))
                             s* (compatible-shape s)]
                         (s->m s* ((D J-func) s*) s*)))))
      (let [symplectic? (fn [C]
                          (fn [s]
                            (let [s* (compatible-shape s)
                                  J (s->m s* ((D J-func) s*) s*)
                                  DCs (s->m s* ((D C) s) s)]
                              (- J (* DCs J (m:transpose DCs))))))]
        (is (= '(matrix-by-rows [0 0 0 0 0]
                                [0 0 0 0 0]
                                [0 0 0 0 0]
                                [0 0 0 0 0]
                                [0 0 0 0 0])
               (simplify
                ((symplectic? (F->CT p->r))
                 (up 't
                     (up 'r 'varphi)
                     (down 'p_r 'p_varphi))))))))))
