(ns math.numerical.minimize
  (:require [math.value :as v]))

(defn- sign
  [a b]
  (if (>= b 0)
    (if (>= a 0) a (- a))
    (if (>= a 0) (- a) a)))

(defn- brent-minimize
  "Given a function f, and given a bracketing triplet of abscissas ax, bx, cx
  (such that bx is between ax and cs, and (f bx) is less than both (f ax) and
  (f cx)), this function isolates the minimum to a fractional precision
  of about tol using Brent's method. The return value is [xmin, fmin];
  the minimum abscissa and function value there. [Numerical Recipes in
  C++: the art of scientific computing. Press [et al.] 2ed 2002]"
  [ax bx cx f tol]
  (let [itmax 100
        cgold 0.3819660
        zeps (* v/machine-epsilon 1.0e-3)
        fx (f bx)]
    (loop [iter 0
           a (if (< ax cx) ax cx)
           b (if (> ax cx) ax cx)
           d 0.0
           e 0.0
           v bx
           w bx
           x bx
           fv fx
           fw fx
           fx fx
           ]
      (let [xm (* 0.5 (+ a b))
            tol1 (+ zeps (* tol (Math/abs x)))
            tol2 (* 2.0 tol1)
            ]
        (cond (<= (Math/abs (- x xm)) (- tol2 (* 0.5 (- b a))))
              [x fx iter]
              (> iter itmax)
              (throw (RuntimeException. "minimization failed to converge"))
              :else

              (let [[d e] (if (> (Math/abs e) tol1)
                            (let [r (* (- x w) (- fx fv))
                                  q (* (- x v) (- fx fw))
                                  p (- (* (- x v) q) (* (- x w) r))
                                  q (* 2.0 (- q r))
                                  p (if (> q 0.0) p (- p))
                                  q (Math/abs q)]
                              (if (or (>= (Math/abs p) (Math/abs (* 0.5 q e)))
                                      (<= p (* q (- a x)))
                                      (>= p (* q (- b x))))
                                (let [e (if (>= x xm) (- a x) (- b x))]
                                  [(* cgold e) e])
                                (let [d (/ p q)
                                      u (+ x d)
                                      d (if (or (< (- u a) tol2)
                                                (< (- b u) tol2))
                                          (sign tol1 (- xm x))
                                          d)]
                                  [d e])))
                            (let [e (if (>= x xm) (- a x) (- b x))]
                              [(* cgold e) e]))]
                ;; now we have updated d & e.
                ;; so we have reached the starred line in the text.
                (let [u (if (> (Math/abs d) tol1) (+ x d) (+ x (sign tol1 d)))
                      fu (f u)]
                  (if (<= fu fx)
                    (recur (inc iter)
                           (if (>= u x) x a)
                           (if (< u x) x b)
                           d e
                           w x u
                           fw fx fu)
                    (let [[new-a new-b] (if (< u x) [u b] [a u])
                          [new-v new-w new-fv new-fw] (cond (or (<= fu fw) (= w x))
                                                            [w u fw fu]
                                                            (or (<= fu fv) (= v x) (= v w))
                                                            [u w fu fw]
                                                            :else
                                                            [v w fv fw])]
                      (recur (inc iter)
                             new-a
                             new-b
                             d e
                             new-v new-w x
                             new-fv new-fw fx))))))))))

(defn minimize
  [f a b]
  (brent-minimize a (* (+ a b) 0.5) b f 1e-6))
