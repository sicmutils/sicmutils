(ns math.diff-test
  (:require [clojure.test :refer :all]
            [math.diff :refer :all]
            [math.generic :as g]
            ))

(deftest diff-test-1
  (testing "add-dtl"
    (let [dx_ (make-differential-term [0] 1)
          dy_ (make-differential-term [1] 1)
          dz_ (make-differential-term [2] 1)
          dx (make-differential [dx_])
          dy (make-differential [dy_])
          dz (make-differential [dz_])
          dxdy (make-differential [(make-differential-term [0 1] 1)])
          dx+dy (make-differential [dx_ dy_])
          dx+dz (make-differential [dx_ dz_])
          ]
      (is (= dx+dy (g/+ dx dy)))
      (is (= dx+dz (g/+ dx dz)))
      (is (= dxdy (g/* dx dy)))
      (is (zero? (g/* dx dx)))
      )
    ))
