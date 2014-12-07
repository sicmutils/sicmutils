(ns math.numerical.minimize
  (:require [math.value :as v]))

(defn- sign
  [a b]
  (if (>= b 0)
    (if (>= a 0) a (- a))
    (if (>= a 0) (- a) a)))

(defn- ^double abs [^double x]
  (if (< x 0) (- x) x))

(defn- brent-minimize
  "Given a function f, and given a bracketing triplet of abscissas ax, bx, cx
  (such that bx is between ax and cs, and (f bx) is less than both (f ax) and
  (f cx)), this function isolates the minimum to a fractional precision
  of about tol using Brent's method. The return value is [xmin, fmin];
  the minimum abscissa and function value there. [Numerical Recipes in
  C++: the art of scientific computing. Press [et al.] 2ed 2002]"
  [^double ax ^double cx f ^double tol]
  (let [itmax 100
        bx (* (+ ax cx) 0.5)
        fx (f bx)
        cgold 0.3819660
        zeps (* v/machine-epsilon 1.0e-3)
        ]
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
           fx fx]
      #_(prn "brent-step" iter a b d e v w x fv fw fx)
      (let [xm (* 0.5 (+ a b))
            tol1 (+ zeps (* tol (abs x)))
            tol2 (* 2.0 tol1)]
        (cond (<= (abs (- x xm)) (- tol2 (* 0.5 (- b a))))
              [x fx iter]
              (> iter itmax)
              (throw (RuntimeException. "minimization failed to converge"))
              :else
              ;; returning d & e in a vector is forcing their type
              ;; back to Object, when we would prefer a primitive
              ;; double. TODO: figure out a way around this!
              (let [[d e] (if (> (abs e) tol1)
                            (let [r (* (- x w) (- fx fv))
                                  q (* (- x v) (- fx fw))
                                  p (- (* (- x v) q) (* (- x w) r))
                                  q (* 2.0 (- q r))
                                  p (if (> q 0.0) (- p) p)
                                  q (abs q)
                                  etemp e
                                  e d]
                              (if (or (>= (abs p) (abs (* 0.5 q etemp)))
                                      (<= p (* q (- a x)))
                                      (>= p (* q (- b x))))
                                (let [e (if (>= x xm) (- a x) (- b x))]
                                  [(* cgold e) e])
                                (let [d (/ p q)
                                      u (+ x d)
                                      d1 (if (or (< (- u a) tol2)
                                                 (< (- b u) tol2))
                                          (sign tol1 (- xm x))
                                          d)]
                                  [d1 e])))
                            (let [e (if (>= x xm) (- a x) (- b x))]
                              [(* cgold e) e]))]
                (let [u (if (> (abs d) tol1) (+ x d) (+ x (sign tol1 d)))
                      fu (f u)]
                  (if (<= fu fx)
                    (recur (inc iter)
                           (if (>= u x) x a)
                           (if (< u x) x b)
                           d e
                           w x u
                           fw fx fu)
                    (let [u<x (< u x)
                          fu<fw (< fu fw)
                          new-a (if u<x u a)
                          new-b (if u<x b u)
                          iter' (inc iter)]
                      (cond (or fu<fw (= w x))
                            (recur iter' new-a new-b d e w u x fw fu fx)
                            (or (<= fu fv) (= v x) (= v w))
                            (recur iter' new-a new-b d e u w x fu fw fx)
                            :else
                            (recur iter' new-a new-b d e v w x fv fw fx)))))))))))

(defn minimize
  [f a b]
  (brent-minimize a b f 1e-5))
