(ns math.simplify-test
  (require [clojure.test :refer :all]
           [math.simplify :refer :all]
           [math.expression :as x]
           [math.generic :as g]
           [math.structure :as s]
           [math.numbers]
           [math.mechanics.lagrange :refer :all]
           [math.function :as f]
           [math.poly :as poly]))

(deftest generator
  (let [g (symbol-generator "k%d")
        a (for [_ (range 5)] (g))
        b (for [_ (range 5)] (g))
        h (symbol-generator "k%d")
        c (for [_ (range 5)] (h))]
    (is (= '(k0 k1 k2 k3 k4) a))
    (is (= '(k5 k6 k7 k8 k9) b))
    (is (= '(k0 k1 k2 k3 k4) c))
    ))

(deftest analyzer-test
  (let [new-analyzer (fn [] (analyzer (symbol-generator "k%d")
                                      poly/expression->
                                      poly/->expression
                                      poly/operators-known))
        A (fn [x]
            (x/print-expression ((new-analyzer) x)))]
    (is (= '(+ 1 x) (A `(g/+ 1 x))))
    (is (= '(+ 1 x) (A `[g/+ 1 x])))
    (is (= 'x (A `(g/* 1/2 (g/+ x x)))))
    (is (= '(* (sin y) (cos (+ 1 (expt (sin y) 4) (* 2 (sin y)))) y)
           (A `(g/* y (g/sin y) (g/cos (g/+ 1 (g/sin y) (g/sin y) (g/expt (g/sin y) 4)))))))
    (is (= '(+ ((D U) (r t)) (* (((expt D 2) r) t) m) (* -1 (r t) m (expt ((D phi) t) 2)))
           (A `(g/- (g/* 1/2 m (g/+ (((g/expt D 2) r) t) (((g/expt D 2) r) t)))
                                    (g/+ (g/* 1/2 m (g/+ (g/* ((D phi) t) ((D phi) t) (r t))
                                                         (g/* ((D phi) t) ((D phi) t) (r t))))
                                         (g/* -1 ((D U) (r t))))))))

    ))

(deftest trivial-simplifications
  (is (= 1 (g/simplify 1)))
  (is (= 1.0 (g/simplify 1.0)))
  (is (= 'foo (g/simplify 'foo)))
  (is (= 3 (g/simplify (g/+ 1 2))))
  (is (= 6 (g/simplify (g/+ 1 2 3))))
  (is (= nil (g/simplify nil)))
  (is (= '(* 2 x) (x/print-expression (g/simplify (g/+ 'x 'x)))))
  (is (= '(+ 1 x) (x/print-expression (g/simplify (g/+ 1 'x)))))
  )

(deftest equations
  (let [pes #(-> % g/simplify x/print-expression)
        xy (s/up (f/literal-function 'x) (f/literal-function 'y))
        xyt (xy 't)
        U (f/literal-function 'U)
        xyt2 (g/square xyt)
        Uxyt2 (U xyt2)
        ]
    (is (= '(up x y) (pes xy)))
    (is (= '(up (x t) (y t)) (pes xyt)))
    (is (= '(+ (expt (x t) 2) (expt (y t) 2)) (pes xyt2)))
    (is (= '(U (+ (expt (x t) 2) (expt (y t) 2))) (pes Uxyt2)))))

(deftest lagrange-equations-test
  (let [pe x/print-expression
        xy (s/up (f/literal-function 'x) (f/literal-function 'y))
        LE (((Lagrange-equations (L-central-rectangular 'm (f/literal-function 'U))) xy) 't)]
    (is (= '(up x y) (pe xy)))
    (is (= '(down (-
                    (* 1/2 m (+ (((expt D 2) x) t) (((expt D 2) x) t)))
                    (* -1 ((D U) (sqrt (+ (* (y t) (y t)) (* (x t) (x t))))) (/ 1 (* 2 (sqrt (+ (* (y t) (y t)) (* (x t) (x t)))))) (+ (x t) (x t))))
                  (-
                    (* 1/2 m (+ (((expt D 2) y) t) (((expt D 2) y) t)))
                    (* -1 ((D U) (sqrt (+ (* (x t) (x t)) (* (y t) (y t))))) (/ 1 (* 2 (sqrt (+ (* (x t) (x t)) (* (y t) (y t)))))) (+ (y t) (y t)))))
           (pe LE)))
    (is (= '(down (+
                   (* -1N (((expt D 2) x) t) m)
                   (* -2 (x t) ((D U) (sqrt (+ (expt (x t) 2) (expt (y t) 2)))) (/ 1 (* 2 (sqrt (+ (expt (x t) 2) (expt (y t) 2)))))))
                 (+ (* -1N (((expt D 2) y) t) m)
                    (* -2 (y t) ((D U) (sqrt (+ (expt (y t) 2) (expt (x t) 2)))) (/ 1 (* 2 (sqrt (+ (expt (y t) 2) (expt (x t) 2))))))))
           (pe (g/simplify LE))))
    ))