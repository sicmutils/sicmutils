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

#_(deftest the-basics
  (let [F (series (literal-function 'f)
                  (literal-function 'g)
                  (literal-function 'h))
        G (literal-function 'G)
        H0 (fn [alpha]
             (fn [[_ _ ptheta]]
               (/ (square ptheta) (* 2 alpha))))
        H1 (fn [beta]
             (fn [[_ theta _]]
               (* -1 beta (cos theta))))
        I (o/make-operator
           (fn [f]
             (fn [x]
               (println "I received" x)
               (f x)))
           'I)
        H-series (series (H0 'alpha) (* 'epsilon (H1 'beta)))

        ]
    (is (= '[(f x) (g x) (h x) 0] (simplify (series/take 4 (F 'x)))))
    ;;(is (= 'foo (simplify (series/take 4 (G (F 'x))))))
    (is (= 'foo (simplify (series/take 4 (((* I 'e) F) 'x)))))
    (is (= 'foo (simplify (series/take 4 (((* 'e I) F) 'x)))))
    (is (= 'foo (simplify (series/take 4 (H-series (up 't 'theta 'ptheta))))))
    (is (= 'foo (simplify (series/take 4 (((* 'epsilon I) H-series) (up 't 'theta 'ptheta))))))
    (is (= 'foo (simplify (series/take 4 (((* I 'epsilon) H-series) (up 't 'theta 'ptheta))))))

    (is (= 'foo (simplify (series/take 4 (((exp (* 'e D)) (literal-function 'f)) 'x)))))
    (is (= 'foo (simplify (series/take 4 (((exp (* 'e D)) H-series) (up 't 'theta 'ptheta))))))
    ))


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
      (is (= '((/ (expt p_theta 2) (* 2 a)) (* -1 (cos theta) b ε) 0 0 0 0)
             (simplify
              (series/take 6
                           ((H-pendulum-series 'a 'b 'ε) a-state)))))

    ;; F (series (literal-function 'f)
    ;; (literal-function 'g)
    ;; (literal-function 'h))
    (let [H (H-pendulum-series 'a 'b 'ε)
          L (Lie-derivative (W 'a 'b))]
      (is (= '[(/ (expt p_theta 2) (* 2 a))
               (* -1 (cos theta) b ε)
               0]
             (simplify (series/take 3 (H a-state)))))
      (is (= [:exactly 0] (v/arity H)))
      (is (= [:exactly 1] (v/arity L)))
      (is (= [:exactly 1] (v/arity (L H))))
      (is (= [:exactly 1] (v/arity (W 'a 'b))))
      (is (= '[(/ (expt p_theta 2) (* 2 a))
               (* -1 (cos theta) b ε)
               0
               0] (simplify (series/take 4 (H a-state)))))
      (is (= '[(* (cos theta) b)
               (/ (* (expt (sin theta) 2) a (expt b 2) ε) (expt p_theta 2))
               0
               0]
             (simplify
                (series/take 4
                             ((L H) a-state)))))
      ;;(is (= 'foo (simplify (series/take 4 (((* 'ε L) F) 'x)))))
      (is (= 'A1 ((L ((o/number->operator 'ε) H)) a-state)))
      (is (= 'A2 (((o/number->operator 'ε) (L H)) a-state)))
      (is (= '[(* (cos theta) b ε )
               (/ (* (expt (sin theta) 2) a (expt b 2) (expt ε 2) ) (expt p_theta 2))
               0
               0]
             (simplify (series/take 4 (((* 'ε L) H) a-state)))))
      (is (= '[(* (cos theta) b ε )
               (/ (* (expt (sin theta) 2) a (expt b 2) (expt ε 2) ) (expt p_theta 2))
               0
               0]
             (simplify (series/take 4 (((* L 'ε) H) a-state)))))
      #_(let [F (series/starting-with (literal-function 'f)
                                    (literal-function 'g))]
        (is (= '[(f x) (g x) 0 0] (simplify (series/take 4 (F 'x)))))
        (is (= '[(* 2 (f x)) (* 2 (g x)) 0 0] (simplify (series/take 4 ((* 2 F) 'x)))))
        (is (= '[(* 2 (f x)) (* 2 (g x)) 0 0] (simplify (series/take 4 ((* F 2) 'x))))))
      #_(is (= '((/ (expt p_theta 2) a) (* -2 (cos theta) b ε) 0 0)
             (simplify (series/take 4 ((* 2 H) a-state)))))
      #_(is (= '((/ (expt p_theta 2) a) (* -2 (cos theta) b ε) 0 0)
             (simplify (series/take 4 ((* H 2) a-state)))))
      #_(is (= '((* (cos theta) b ε)
               (/
                (* (expt (sin theta) 2) a (expt b 2) (expt ε 2))
                (expt p_theta 2))
               0
               0)
             (simplify (series/take 4 (((* 'ε L) H) a-state)))))
      ;; (is (= 'foo (simplify (series/take 4 (((* L 'ε) H) a-state)))))
      ;; (is (= 'foo  (* L 'e)))
      ;; (is (= 'bar  (* 'e L)))
      ;; (is (= 'qux (simplify (series/take 4 (* 'ε ((L H) a-state))))))
      ;; (is (= 'quux (simplify (series/take 4 ((L (fn [s] (H s))) a-state)))))
      ;;(is (= 'quux1 (simplify (series/take 4 ((L H) a-state)))))
      ;; (is (= 'quux2 (simplify (series/take 4 ((L (fn [s] (* 'ε (H s)))) a-state)))))
      ;; (is (= 'baaaz (simplify (series/take 4  (H a-state)))))
      ;; (is (= 'quuux (simplify (series/take 4  (* 'ε (H a-state))))))

      ;; (is (= 'foo  ((* L 'e) H)))
      ;; (is (= 'bar  ((* 'e L) H)))
      ;; (is (= 'foo  (((* L 1) H) a-state)))
      ;; (is (= 'bar  (series/take 4 (((* 'e L) H) a-state))))


      ;; (is (= 'foo (simplify (series/take 4 ((* 'ε H) a-state)))))
      ;; (is (= 'foo (simplify (series/take 4 ((* H 'ε) a-state)))))
      ;; (is (= 'foo (simplify (series/take 4 ((* L (* 'ε H)) a-state)))))
      ;; (is (= 'foo (simplify (series/take 4 ((* L (* H 'ε)) a-state)))))
      ;; (is (= 'foo (simplify (series/take 4 (((* L 'ε) H) a-state)))))
      ;; (is (= 'foo (* 'ε L)))
      ;; (is (= 'foo (* L 'ε)))
      (is (= 'foo (simplify (series/take 4 ((* 'ε H) a-state)))))
      (is (= 'foo (simplify (series/take 4 ((* H 'ε) a-state)))))
      #_(let [T1 (H a-state)
            T2 (((* 'ε L) H) a-state)


            T3 (((* 1/2 (square 'ε) (* L L)) H) a-state)]
        ;; the first term of the Lie series:
        (is (= '[(/ (expt p_theta 2) (* 2 a))
                 (* -1 (cos theta) b ε)
                 0
                 0]
               (simplify (series/take 4 T1))))
        ;; the second term of the Lie series:
        (is (= '[(* (cos theta) b ε)
                 (/ (* (expt (sin theta) 2) a (expt b 2) (expt ε 2)) (expt p_theta 2))
                 0
                 0]
               (simplify (series/take 4 T2))))

        (is (= 'foo
               (simplify (series/take 4 T3))))
        (is (= 'foo (simplify (series/take 4 (+ T1 T2))))))


      )
    ;; (is (= 'foo (simplify (series/take 4 (((partial 0) (H-pendulum-series 'alpha 'beta 'ε)) a-state)))))
    ;; (is (= 'foo (simplify (series/take 4 (((partial 1) (H-pendulum-series 'alpha 'beta 'ε)) a-state)))))
    ;; (is (= 'foo (simplify (series/take 4 (((partial 2) (H-pendulum-series 'alpha 'beta 'ε)) a-state)))))
    ;; (is (= 'foo (simplify (series/take 4 (((partial 2) (fn [s] ((H-pendulum-series 'alpha 'beta 'ε) s))) a-state)))))
    ;; (is (= 'foo (simplify (series/take 4 ((D (series (literal-function 'f) (literal-function 'g))) 'x)))))
    #_(is (= 0 (simplify
              (series/take 4 (((exp (* 'ε (Lie-derivative (W 'alpha 'beta))))
                               (H-pendulum-series 'alpha 'beta 'ε))
                              a-state)))))
    #_(is (= 0 (simplify
              (series:sum
               (((exp (* 'ε (Lie-derivative (W 'alpha 'beta))))
                 (H-pendulum-series 'alpha 'beta 'ε))
                a-state)
               2))))
    ))

;; Where we left off: for operators, (* e D) and (* D e) don't seem to be the same
;; thing, and they should be when e commutes with D.
