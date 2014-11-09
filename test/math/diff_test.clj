(ns math.diff-test
  (:require [clojure.test :refer :all]
            [math.diff :refer :all]
            [math.generic :as g]
            [math.struct :as s]
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
      (is (= dx-plus-dy (g/+ dx dy)))
      (is (= dx-plus-dz (g/+ dx dz)))
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

      (is (= (make-differential [zero_ dxdy_]) (g/* dx dy)))
      (is (= 0 (g/* dx dx)))
      ))
  (testing "some simple functions"
    (is (= 2 ((derivative #(g/* 2 %)) 1)))
    (is (= 2 ((derivative #(g/* 2 %)) 'y)))
    (is (= (g/+ 'y 'y) ((derivative #(g/* % %)) 'y)))
    (is (= (g/* 3 (g/expt 'y 2))
           ((derivative #(g/expt % 3)) 'y)))
    (is (= (g/* 2 (g/cos (g/* 2 'y))) ((derivative #(g/sin (g/* 2 %))) 'y)))
    ;; hm. pretty quickly we find that canonicalization is
    ;; important. Why are the expressions produced by derivative not
    ;; canonical? Probably because they are built in pieces. We have reached the
    ;; frontier where simplification will be necessary to determine whether two
    ;; expressions are equivalent. The good news is that differentiation is
    ;; working modulo canonicalization.
    ;;(is (= (g/* (g/cos (g/cos 'x)) -1 (g/sin 'x)) ((derivative #(g/sin (g/cos %))) 'x)))
    (is (= (s/up 2 (g/+ 't 't)) ((derivative #(s/up (g/* 2 %) (g/* % %))) 't)))
    (is (= (s/up (g/* -1 (g/sin 't)) (g/cos 't)) ((derivative #(s/up (g/cos %) (g/sin %))) 't)))
    ))
