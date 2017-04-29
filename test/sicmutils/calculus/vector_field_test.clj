(ns sicmutils.calculus.vector-field-test
  (:require [clojure.test :refer :all]
            [sicmutils
             [generic :as g]
             [structure :as s]]
            [sicmutils.calculus.manifold :as m]
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
             (g/simplify ((v (m/chart m/R2-rect)) p)))))))

