(ns math.simplify-test
  (require [clojure.test :refer :all]
           [math.simplify :refer :all]
           [math.generic :as g]
           [math.expression :as x]
           [math.structure :as s]
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
  (let [pe x/print-expression
        new-analyzer (fn [] (analyzer (symbol-generator "k")
                                      poly/expression->
                                      poly/->expression
                                      poly/operators-known))
        A (fn [x]
            (let [[simplified _ env] ((new-analyzer) x)
                  ps (x/print-expression simplified)
                  penv (into (empty env) (for [[k v] env] [k (pe v)]))]
              [ps penv]))
        x1 (g/* 'y (g/sin 'y) (g/cos (g/+ 1 (g/sin 'y) (g/sin 'y) (g/expt (g/sin 'y) 4))))
        x2 `(g/* 1/2 (g/+ 'x 'x))
        x3 `(g/- (g/* 1/2 m (g/+ (((g/expt D 2) r) t) (((g/expt D 2) r) t)))
                 (g/+ (g/* 1/2 m (g/+ (g/* ((D phi) t) ((D phi) t) (r t))
                                      (g/* ((D phi) t) ((D phi) t) (r t))))
                      (g/* -1 ((D U) (r t)))))
        x3 0 #_(*
             1/2
             m
             (+
               (*
                 (r t)
                 (+ (* ((D r) t) ((D phi) t)) (* (r t) (((expt D 2) phi) t))))
               (* ((D r) t) (r t) ((D phi) t))
               (* ((D r) t) (r t) ((D phi) t))
               (*
                 (+ (* ((D r) t) ((D phi) t)) (* (r t) (((expt D 2) phi) t)))
                 (r t))))]
    #_(is (= '[(* y k0 k1)
             {k0 (sin y)
              k1 (cos (+ 1 (expt k0 4) (* 2 k0)))}] (A x1)))
    #_(is (= 'foo (A x2)))

    ; but it's wrong to have (+ k5 k5) here! it should have been simplified.
    ; so there's a recursive step we're missing.


    #_(is (= '[(- (* 1/2 k0 (+ k5 k5))
                (+ (* 1/2 k0 (+ (* k8 k11 k12) (* k15 k18 k19))) (* -1 k23)))
             {k0 (quote m),
              k1 (quote D),
              k2 (quote r),
              k3 ((expt k1 2) k2),
              k4 (quote t),
              k5 (k3 k4),
              k6 (quote phi),
              k7 (k1 k6)}] (A x3)))
    ))

(deftest simplifier-test
  (let [eqs (((Lagrange-equations (L-central-rectangular 'm (f/literal-function 'U)))
               (s/up (f/literal-function 'x)
                     (f/literal-function 'y)))
              't)]))