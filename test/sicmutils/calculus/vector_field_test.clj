(ns sicmutils.calculus.vector-field-test
  (:require [clojure.test :refer :all]
            [sicmutils
             [generic :as g]
             [structure :as s]]
            [sicmutils.calculus.manifold :as m]
            [sicmutils.calculus.coordinate :as c]
            [sicmutils.calculus.vector-field :refer :all]))

(deftest vectorfield
  (testing "literal"
    (let [f (m/literal-manifold-function 'f-rect m/R2-rect)
          v (literal-vector-field 'b m/R2-rect)
          R2-rect-chi-inverse (m/point m/R2-rect)
          p (R2-rect-chi-inverse (s/up 'x0 'y0))]
      (is (= '(+ (* (((∂ 0) f-rect) (up x0 y0)) (b↑0 (up x0 y0)))
                 (* (((∂ 1) f-rect) (up x0 y0)) (b↑1 (up x0 y0))))
             (g/simplify ((v f) p))))
      (is (= '(up (b↑0 (up x0 y0)) (b↑1 (up x0 y0)))
             (g/simplify ((v (m/chart m/R2-rect)) p))))))
  (testing "exponentiation"
    (c/let-coordinates [[x y] m/R2-rect]
      (let [circular (g/- (g/* x d:dy) (g/* y d:dx))]
        (is (= '(up (+ (* -1/720 (expt a 6)) (* 1/24 (expt a 4)) (* -1/2 (expt a 2)) 1)
                    (+ (* 1/120 (expt a 5)) (* -1/6 (expt a 3)) a))
               (g/simplify
                 ((((evolution 6) 'a circular) (m/chart m/R2-rect))
                     ((m/point m/R2-rect) (s/up 1 0)))))))   )))


