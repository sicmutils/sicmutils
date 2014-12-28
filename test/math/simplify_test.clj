(ns math.simplify-test
  (require [clojure.test :refer :all]
           [math.simplify :refer :all]))

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

(deftest poly-analyzer
  (let [known '#{+ - *}
        g (symbol-generator "k")
        A (analyzer g (fn [x y] x) identity known)
        x1 '(* y (sin y) (cos (+ 1 (sin y) (sin y) (expt (sin y) 4))))]
    (let [[simplifed _ env] (A x1)]
      (is (= '(* y k0 k2) simplifed))
      (is (= '{k0 (sin y)
               k1 (expt k0 4)
               k2 (cos (+ 1 k0 k0 k1))} env)))))

