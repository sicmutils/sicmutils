(ns math.sicm-test
  (:refer-clojure :exclude [+ - * / zero?])
  (:require [clojure.test :refer :all]
            [math.generic :refer :all]
            [math.structure :refer :all]
            [math.expression :refer :all]
            [math.function :refer :all]
            [math.calculus.derivative :refer :all]))

(defn velocity [local] (nth 2 local))

(defn L-free-particle [mass]
  (fn [local]
    (let [v (velocity local)]
      (* 1/2 mass (square v)))))

(def q
  (up (literal-function 'x)
      (literal-function 'y)
      (literal-function 'z)))

(defn Γ [q]
  (fn [t]
    (up t (q t) ((derivative q) t))))

(deftest sicm
  (testing "apply-struct"
    (is (= (up (literal-number '(x t))
               (literal-number '(y t))
               (literal-number '(z t)))
           (q 't)))
    ;; not quite there! need a way to differentiate
    ;; a literal function before this can work.
    ;;(is (= 'bar ((derivative q) 't)))
    ;;(is (= 'foo ((Γ q) 't)))
    ))
