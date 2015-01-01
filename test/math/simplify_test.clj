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
  (let [g (symbol-generator "k")
        a (for [_ (range 5)] (g))
        b (for [_ (range 5)] (g))
        h (symbol-generator "k")
        c (for [_ (range 5)] (h))]
    (is (= '(k0 k1 k2 k3 k4) a))
    (is (= '(k5 k6 k7 k8 k9) b))
    (is (= '(k0 k1 k2 k3 k4) c))
    ))

(deftest analyzer-test
  (let [new-analyzer (fn [] (analyzer (symbol-generator "k")
                                      poly/expression->
                                      poly/->expression
                                      poly/operators-known))
        A (fn [x]
            (x/print-expression ((new-analyzer) x)))]
    (is (= '(+ 1 x) (A `(g/+ 1 x))))
    (is (= '(+ 1 x) (A `[g/+ 1 x])))
    (is (= 'x (A `(g/* 1/2 (g/+ x x)))))
    (is (= '(* y (cos (+ 1 (expt (sin y) 4) (* 2 (sin y)))) (sin y))
           (A `(g/* y (g/sin y) (g/cos (g/+ 1 (g/sin y) (g/sin y) (g/expt (g/sin y) 4)))))))
    (is (= '(+ ((D U) (r t)) (* m (((expt D 2) r) t)) (* -1N m (r t) (expt ((D phi) t) 2)))
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
  (is (= '(* 2 x) (x/print-expression (g/simplify (g/+ 'x 'x)))))
  (is (= '(+ 1 x) (x/print-expression (g/simplify (g/+ 1 'x)))))
  )

#_(deftest simplifier-test
  (let [eqs (((Lagrange-equations (L-central-rectangular 'm (f/literal-function 'U)))
               (s/up (f/literal-function 'x)
                     (f/literal-function 'y)))
              't)
        simp-eqs (g/simplify eqs)]
    ))