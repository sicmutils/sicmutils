(ns math.calculus.derivative-test
  (:refer-clojure :exclude [+ - * /])
  (:require [clojure.test :refer :all]
            [math.calculus.derivative :refer :all]
            [math.generic :refer :all]
            [math.structure :refer :all]
            ))

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
          dxdy (make-differential [(make-differential-term [0 1] 1)])
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

  (testing "partial derivatives"
    (let [f (fn [x y] (+ (* x x) (* y y)))]
      (is (= 4 (((pd 0) f) 2 3)))
      (is (= 6 (((pd 1) f) 2 3)))))
  )
