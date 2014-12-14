(ns math.calculus.derivative-test
  (:refer-clojure :exclude [+ - * / zero?])
  (:require [clojure.test :refer :all]
            [math.calculus.derivative :refer :all]
            [math.function :refer :all]
            [math.generic :refer :all]
            [math.value :as v]
            [math.numbers]
            [math.expression :refer :all]
            [math.structure :refer :all]
            ))


(def ^:private q
  (up (literal-function 'x)
      (literal-function 'y)
      (literal-function 'z)))

(defn- δ
  [η]
  (fn [f]
    ;; Define g(ε) as in Eq. 1.22; then δ_η f[q] = Dg(0)
    (fn [q]
      (let [g (fn [ε]
                (f (+ q (* ε η))))]
        ((D g) 0)))))

(deftest diff-test-1
  (testing "add, mul differentials"
    (let [zero_ (make-differential-term [] 0)
          one_ (make-differential-term [] 1)
          dx_ (make-differential-term [0] 1)
          dy_ (make-differential-term [1] 1)
          dz_ (make-differential-term [2] 1)
          dxdy_ (make-differential-term [0 1] 1)
          one (make-differential [one_])
          dx (make-differential [dx_])
          dy (make-differential [dy_])
          dz (make-differential [dz_])
          dx-plus-dy (make-differential [dx_ dy_])
          dx-plus-dz (make-differential [dx_ dz_])
          ]
      (is (= 1 one))
      (is (= dx-plus-dy (+ dx dy)))
      (is (= dx-plus-dz (+ dx dz)))
      (is (= (make-differential [(make-differential-term [0] 0)]) (dx*dy dx 0)))
      (let [b (dx+dy 0 (dx*dy dx 0))
            c (dx*dy 0 dx)]
        (is (= (make-differential [(make-differential-term [0] 0)
                                   (make-differential-term [] 0)])
               b))

        (is (= (make-differential [(make-differential-term [0] 0)])
               c))
        (is (= 0 (dx+dy b c)))
        )
      (is (= [dxdy_] (dxs*dys [dx_] [dy_])))
      (is (= [] (dxs*dys [dx_] [dx_])))
      (is (= [] (dx*dys dx_ [dx_])))
      (is (= [dxdy_] (dx*dys dx_ [dy_])))

      (is (= (make-differential [zero_ dxdy_]) (* dx dy)))
      (is (= 0 (* dx dx)))
      ))
  (testing "some simple functions"
    (is (= 2 ((D #(* 2 %)) 1)))
    (is (= 2 ((D #(* 2 %)) 'y)))
    (is (= (+ 'y 'y) ((D #(* % %)) 'y)))
    (is (= (* 3 (expt 'y 2))
           ((D #(expt % 3)) 'y)))
    (is (= (* 2 (cos (* 2 'y))) ((D #(sin (* 2 %))) 'y)))
    (is (= (up 2 (+ 't 't)) ((D #(up (* 2 %) (* % %))) 't)))
    (is (= (up (* -1 (sin 't)) (cos 't)) ((D #(up (cos %) (sin %))) 't)))
    )
  (testing "chain rule"
    (let [s (fn [t] (sqrt t))
          u (fn [t] (expt (- (* 3 (s t)) 1) 2/3))
          y (fn [t] (/ (+ (u t) 2) (- (u t) 1)))

          ]
      (is ((v/within 1e-6) (/ -1 18.) ((D y) 9))))
    )

  (testing "structural-functions"
    (is (= '(up (cos t) (* -1 (sin t))) (freeze-expression ((D (up sin cos)) 't)))))

  (testing "partial derivatives"
    (let [f (fn [x y] (+ (* x x) (* y y)))]
      (is (= 4 (((pd 0) f) 2 3)))
      (is (= 6 (((pd 1) f) 2 3)))))
  )

(deftest diff-test-2
  (testing "delta-eta-test"
    (let [η (literal-function 'η)
          q (literal-function 'q)
          I (fn [q] (fn [t] (q t)))
          f (literal-function 'f)
          g (literal-function 'g)
          F (fn [q] (fn [t] (f (q t))))
          G (fn [q] (fn [t] (g (q t))))
          q+εη (+ q (* 'ε η))
          g (fn [ε] (+ q (* ε η)))
          δη (δ η)
          δηI (δη I)
          δηIq (δηI q)
          δηFq ((δη F) q)
          ]
      (is (= '(+ (q t) (* ε (η t))) (freeze-expression (q+εη 't))))
      (is (= '(+ (q t) (* ε (η t))) (freeze-expression ((g 'ε) 't))))
      (is (= '(η a) (freeze-expression (((D g) 'dt) 'a))))
      (is (= '(η t) (freeze-expression (δηIq 't))))
      (is (= '(f (q t)) (freeze-expression ((F q) 't))))
      (is (= '(* ((D f) (q t)) (η t)) (freeze-expression (δηFq 't))))
      ;; product rule for variation: δ(FG) = δF G + F δG
      (is (= (freeze-expression (+ (* (((δη F) q) 't) ((G q) 't))
                                   (* ((F q) 't) (((δη G) q) 't))))
             (freeze-expression (((δη (* F G)) q) 't))))
      )
    ))
