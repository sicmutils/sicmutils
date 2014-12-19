(ns math.numerical.minimize
  (:import [math.numerical Brent Brent$Result]))

(defn minimize
  [f a b]
  (let [^Brent$Result m (Brent/minimize a b f 1e-5)]
    [(.x m) (.fx m) (.iter m)]))