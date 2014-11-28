(ns math.sicm-test
  (:refer-clojure :exclude [+ - * / zero?])
  (:require [clojure.test :refer :all]
            [math.generic :refer :all]
            [math.structure :refer :all]
            [math.expression :refer :all]
            [math.numerical.integrate :refer :all]
            [math.function :refer :all]
            [math.calculus.derivative :refer :all]))

(defn velocity [local] (nth local 2))
(defmacro mx [x] `(make '~x))

(defn L-free-particle [mass]
  (fn [local]
    (let [v (velocity local)]
      (* 1/2 mass (square v)))))

(def q
  (up (literal-function 'x)
      (literal-function 'y)
      (literal-function 'z)))

(defn Γ
  [q]
  (fn [t]
    (up t (q t) ((D q) t))))

(defn Lagrangian-action
  [L q t1 t2]
  (integrate (comp L (Γ q)) t1 t2))

(defn test-path
  [t]
  (up (+ (* 4 t) 7)
      (+ (* 3 t) 5)
      (+ (* 2 t) 1)))

(deftest sicm
  (testing "apply-struct"
    (is (= '(up (x t)
               (y t)
               (z t))
           (freeze-expression (q 't))))
    (is (= '(up ((D x) t)
                ((D y) t)
                ((D z) t))
           (freeze-expression ((D q) 't))))
    ;; need to get exponentiation of operators before we can
    ;; do this.
    ;; (is (= (up (literal-number (((expt D 2) 'x) 't))
    ;;            (literal-number (((expt D 2) 'y) 't))
    ;;            (literal-number (((expt D 2) 'z) 't)))
    ;;        ((D (D q)) 't)))
    (is (= '(up t
                (up (x t)
                    (y t)
                    (z t))
                (up ((D x) t)
                    ((D y) t)
                    ((D z) t)))
           (freeze-expression ((Γ q) 't))))
    ;; this is beginning to take shape... the simplifier is not
    ;; deployed yet; the text has (expt ((D x) t) 2) where we have
    ;; literal multiplication.
    (is (= '(* 1/2 m (+
                      (* ((D z) t)
                         ((D z) t))
                      (* ((D x) t)
                         ((D x) t))
                      (* ((D y) t)
                         ((D y) t))))
           (freeze-expression ((comp (L-free-particle 'm) (Γ q)) 't))))
    ;; at this point in the text we should be able to show-expression
    ;; in TeX form XXX.
    (is (= 435.0 (Lagrangian-action (L-free-particle 3.0) test-path 0.0 10.0)))
    ))
