(ns math.sicm-test
  (:refer-clojure :exclude [+ - * / zero?])
  (:require [clojure.test :refer :all]
            [math.generic :refer :all]
            [math.structure :refer :all]
            [math.expression :refer :all]
            [math.function :refer :all]
            [math.calculus.derivative :refer :all]))

(defn velocity [local] (nth 2 local))
(defmacro mx [x] `(make '~x))

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
    (up t (q t) ((D q) t))))

(deftest sicm
  (testing "apply-struct"
    (is (= (up (literal-number '(x t))
               (literal-number '(y t))
               (literal-number '(z t)))
           (q 't)))
    (is (= (up (mx ((D x) t))
               (mx ((D y) t))
               (mx ((D z) t)))
           ((D q) 't)))
    ;; need to get exponentiation of operators before we can
    ;; do this.
    ;; (is (= (up (literal-number (((expt D 2) 'x) 't))
    ;;            (literal-number (((expt D 2) 'y) 't))
    ;;            (literal-number (((expt D 2) 'z) 't)))
    ;;        ((D (D q)) 't)))
    (is (= (up 't
               (up (mx (x t))
                   (mx (y t))
                   (mx (z t)))
               (up (mx ((math.generic/D x) t))
                   (mx ((math.generic/D y) t))
                   (mx ((math.generic/D z) t))))
           ((Γ q) 't)))
    ))
