(ns sicmutils.numerical.ode.runge-kutta
  (:require [clojure.core.reducers :as r]
            [sicmutils.generic :as g]
            [sicmutils.structure :as s]
            [sicmutils.value :as v]))

(comment
  ;; META REVIEWER NOTE:
  ;; Not sure how much of this we want in this file. Initially
  ;; I thought we could build up a sequence of RK functions leading
  ;; toward the last one, but I decided not to do that (and also
  ;; I have resisted the temptation to "make it switchable" whether
  ;; we have adaptive stepsize and dense output: I consider these
  ;; necessary.)  So you may find this "big comment" unhelpful: LMK

;; The following implementation is guided by
;; "Solving Ordinary Differential Equations I:
;; Nonstiff Problems," Second Revised Edition, by
;; E. Hairer, S.P. Nørsett and G. Wanner [Springer, 1993],
;; chapter II.1 et seq., hereafter referred to as "HNW."
;;
;;
;; In the Euler method for solving differential
;; equations (i.e., recovering a function y given
;; its derivative f), we use the function f to
;; obtain the slope of the line through the initial
;; point to estimate the new y value after
;; a small interval. Let h be the step we would
;; like to take. Then,
;;
;; y(x_0 + h) ~= y_0 + h f(x_0, y_0)
;;
;; This is known to have convergence of order h.
;; We know we can do better than that for integration
;; problems, where f is a funciton of x alone, and
;; this is explored in detail elsewhere in this package
;; [INCLUDE LINK].
;;
;; One possibility would be to imitate the midpoint
;; rule for integration:
;;
;; $$y(x_0 + h) ~= y_0 + h y(x_0 + h\over 2)$$
;;
;; This has order $h^2$, which would be a big improvement.
;; For the ODE case we would write:
;;
;; $$y(x_0 + h) ~= y_0 + h f(x_0 + h\over 2, y(x_0 + h\over 2))$$
;;
;; but $y(x_0 + h/2)$ is not known! We could use an Euler
;; step to estimate it though, yielding the following
;; 2-stage process:
;;
;; $$
;; k_1 = f(x_0, y_0)\cr
;; k_2 = f(x_0 + h\over 2, y_0 + h\over 2 k_1)\cr
;; y_1 = y_0 + h k_2
;; $$
;;
;; While the Euler step is of order h by itself, its
;; use in the computation of $y_1$ has an $h$ factor applied
;; to it, resulting in an order of $h^2$, which is what we
;; wanted.
;;
;; Runge Kutta methods consist of using methods like this
;; but add additional stages in an effort to gain even higher
;; powers of $h$. The general form of a Runge-Kutta method is
;;
;; $$
;; k_1 = f(x_0 + c_1 h, y_0)\cr
;; k_2 = f(x_0 + c_2 h, y_0 + h a_{21} k_1)\cr
;; k_3 = f(x_0 + c_3 h, y_0 + h(a_{31} k_1 + a_{32} k_2))\cr
;; \cdots
;; k_s = f(x_0 + c_s h, y_0 + h\sum{i=1}{s-1} a_{si} k_i)
;; y_1 = y_0 + h\sum{i=1}{s} b_i k_i
;; $$
;;
;; Following Butcher's 1963 paper the constants in such methods
;; are displayed in a tableau form, in which the $c_i$ values
;; are in the left column, the $b_i$ values on the bottom row, and
;; the $a$ vectors, whose lengths are one less than the stage number,
;; are arranged in the middle triangle. The simple two-stage RK
;; method detailed above has the following small tableau:
;;
;; Runge, order 2
;;
;;   0  |
;;  1/2 | 1/2
;; -----+--------
;;      |  0   1
;;
;; We observe that the stages in an RK method refer only to
;; constants corresponding to the current stage, and all previous
;; values of $k$; therefore the computation can be organized as
;; a fold which builds the $k$ array step by step and finally
;; "combines" the final value by taking the dot product of $b$ and
;; $k$. For our tableau representation, we use a form in which
;; the values of $c_i, b_i$ and $a_i$ are in the $i$-th element
;; of the tableau:

  (def runge-2
    {:tableau [[0   0 []]
               [1/2 1 [1/2]]]})

;; We enumerate some other methods that are commonly used.

;; Runge, order 3
;;
;;   0  |
;;  1/2 | 1/2
;;   1  |  0   1
;;   1  |  0   0   1
;; -----+----------------
;;      | 1/6 2/3  0  1/6

  #_(def runge-3
    {:tableau [[0   1/6 []]
               [1/2 2/3 [1/2]]
               [1   0   [0 1]]
               [1   1/6 [0 0 1]]]})

;; Heun, order 3
;;
;;   0  |
;;  1/3 | 1/3
;;  2/3 |  0  2/3
;; -----+-------------
;;      | 1/4  0  3/4

  #_(def ^:private heun-3
    {:tableau [[0   1/4  []]
               [1/3 0    [1/3]]
               [2/3 3/4  [0 2/3]]]})

  #_(def ^:private the-runge-kutta
    {:tableau [[0   1/6 []]
               [1/2 2/6 [1/2]]
               [1/2 2/6 [0 1/2]]
               [1   1/6 [0 0 1]]]})

  #_(def ^:private three-eighths-rule
    {:tableau [[0   1/8 []]
               [1/3 3/8 [1/3]]
               [2/3 3/8 [-1/3 1]]
               [1   1/8 [1 -1 1]]]})

  ;; At this point we can realize the Runge-Kutta procedure as a
  ;; canonical closure reducing operator, which consumes a row
  ;; of the tableau to update $y_s$ and append a row to the $b$ array.

  #_(defn- rk-step
    [f x0 y0 h]
    (let [z (v/zero-like y0)]
      (fn
        ([] [y0 []])
        ([[y k] [c b a]]
         (let [k' (f (+ x0 (* c h)) (g/+ y0 (g/* h (reduce g/+ z (map g/* a k)))))]
           [(g/+ y (g/* (* h b) k'))
            (conj k k')]))))))

;; For the ODE solver for this package, however, we will want
;; adaptive stepsize and dense output capabiity, so we will be
;; extending the example tableaux above with these capabilities.
;;
;; A common method of step size selection via error estimation is
;; to form both an order $p$ and an order $p-1$ estimate of $y$;
;; if there is very little diffference between these two, then
;; the step size is small enough (the success of a weaker
;; approximation suggests that the stronger approximation is
;; not only good enough but is not near the error margin).
;;
;; In Runge Kutta methods, there's a convenient way to get the
;; lower order estimate almost for free: supply another row of
;; $b$ values, which we call $\hat{b}$, which supply a second
;; estimate of y generated from values already present in the
;; $k$ tableau. For tableaux which possess the $\hat{b}$ values,
;; we can enrich the reducing operator to compute the estimate
;; $\hat{y}$ at the same time as $y$:

(defn- rk-step-with-error-control
  [f x0 y0 h]
  (let [z (v/zero-like y0)]
    (fn
      ([] [y0 y0 []])
      ([[y yhat k] [c b bhat a]]
       (let [k' (f (+ x0 (* c h)) (g/+ y0 (g/* h (reduce g/+ z (map g/* a k)))))]
         [(g/+ y (g/* h b k'))
          (g/+ yhat (g/* h bhat k'))
          (conj k k')])))))

(defn- scaled-norm
  "HNW define this norm in equation 4.11 (p.168), but analysis of their
   Fortran implementation shows that the factor of $1/n$ in that equation
   is not used except when measuring the distance between $y$ and $\\hat{y}$.
   We use the term `scaled-norm` for the step size computation cases, and
   `scaled-error` for the step rejection application, which is consistent
   with the Fortran and allows it to serve as a reference implementation."
  [y sc]
  (Math/sqrt (transduce (map #(Math/pow % 2)) + (s/elementwise / y sc))))

(defn- initial-step-size
  "Gather a quick and dirty estimate of the magnitude of first and
   second derivatives of the function being integrated and use this
   to solve for a step size consistent with the tolerance desired and
   the order of the tableau in use. The calculation here is done before
   we have taken any official Runge-Kutta steps, so we have less information
   than we will have when we consider adapting the step size after each
   step is taken."
  [f y0 x0 atol rtol order]
  (let [sc (g/+ atol (s/elementwise g/* (s/mapr g/abs y0) rtol))
        f0 (f x0 y0)
        d0 (scaled-norm y0 sc)
        d1 (scaled-norm f0 sc)
        h0 (if (or (<= d0 1e-5)
                   (<= d1 1e-5))
             1e-6
             (/ d0 d1 100))
        y1 (g/+ y0 (g/* h0 f0))
        f1 (f (+ x0 h0) y1)
        d2 (scaled-norm (g/- f1 f0) sc)
        maxd (max d1 d2)
        h1 (if (<= maxd 1e-15)
             (max 1e-6 (* h0 1e-3))
             (g/expt (g// 0.01 maxd) (/ (inc order))))]
    (min (* 100 h0) h1)))

(defn- scaled-error
  "See scaled norm: this is equation 4.11 of HNW as written, used to measure
   the scaled difference between the two estimates of $y$ generated by
   the tableau."
  [y sc]
  (g/sqrt (/ (transduce (map #(Math/pow % 2)) + (s/elementwise / y sc))
             (count sc))))

(defn- consider-step
  "Implement the step-size control algorithm of HNW Chapter II.4 (p. 167).
   `y0` and `y1` represent integrated function values at two points separated
   by `h`, and `yhat` is an estimate of different (usually lower) order supplied
   as a check on the accuracy of `y1` (i.e., if the step size is sufficiently
   small, then `y1` and `yhat` cannot differ by a large amount). Returns a
   pair [accept, h-new] where accept is true if the current step is acceptable,
   and h-new represents the new estimate of the ideal step size."
  [y0 y1 yhat h atol rtol order]
  (let [fac-min (/ 3)
        fac-max 6
        fac-damp (/ 4 5)
        sc (g/+ atol (s/elementwise g/*
                                    (s/elementwise #(max (Math/abs %1) (Math/abs %2)) y0 y1) rtol))
        err (scaled-error (g/- y1 yhat) sc)
        h-factor (Math/pow err (/ -1. order))
        h-new (* h (min fac-max (max fac-min (* fac-damp h-factor))))]
    [(< err 1) h-new]))

(defn- make-interpolation-fn
  "Given coefficents of for a fourth degree interpolating polynomial in
   the alternating variables $\\theta, 1-\\theta$, produce a function
   which will receive an argument in the range $[x_0, x_0+h]$. That
   argument will be mapped to $0\\le\\theta\\«le 1$ and will return the
   value of the polynomial there."
  [x0 h [r1 r2 r3 r4 r5]]
  (fn [x]
    (let [theta (/ (- x x0) h)
          theta' (- 1 theta)]
      (assert (and (>= theta 0)
                   (<= theta 1))
              "interpolation requested out of range")
      (-> r5
          (g/* theta')
          (g/+ r4)
          (g/* theta)
          (g/+ r3)
          (g/* theta')
          (g/+ r2)
          (g/* theta)
          (g/+ r1)))))

(defn- runge-kutta
  "Generate a lazy sequence of solution intervals for the differential
   equation y' = f, y(x0) = y0, with precision epsilon, starting at x0.
   Each solution object contains x and y values, an interpolation
   function, and an interval h: the interpolation function will produce
   accurate values of y within the interval [x,x+h]. The sequence of solution
   objects will be ordered and connected, but the interval size will vary
   as the solution evolves."
  [tableau epsilon f x0 y0]
  (let [tolerance (vec (repeat (count y0) epsilon))
        p0 (apply min (:order tableau))
        p (first (:order tableau))
        h0 (initial-step-size f y0 x0 tolerance tolerance p0)
        inner-step (partial rk-step-with-error-control f)
        accept-step (fn
                      ;; Given $x, y(x)$ and a candidate inteval $h$, perform the Runge-Kutta
                      ;; tableau fold to obtain an accepted step size $h_1$ and $y(x+h_1)$.
                      ;; Return an association containing the solution interval and an interpolation
                      ;; function valid within it, and the recommended stepsize for the next interval
                      [x y h0]
                      (loop [h h0]
                        (let [[y1 yhat ks] (r/reduce
                                            (inner-step x y h)
                                            (:tableau tableau))
                              [accept h'] (consider-step y y1 yhat h tolerance tolerance p)]
                          (if accept
                            ;; step is accepted. Prepare dense-output data and return a solution
                            ;; interval object.
                            (let [y1-y (g/- y1 y)
                                  rs [y
                                      y1-y
                                      (g/- (g/* h (nth ks 0)) y1-y)
                                      (g/- (g/* 2 y1-y) (g/* h (g/+ (nth ks 0) (nth ks 6))))
                                      (g/* h (g/dot-product (:dense tableau) ks))]]
                              [{:x0 x :y0 y :x1 (+ x h) :y1 y1 :h h :f (make-interpolation-fn x h rs)} h'])
                            ;; step is rejected; stay at current x,y point and redo with smaller step
                            (recur h')))))
        step (fn step
               ;; Build the lazy sequence of solutions. Pass the initial known $y_0 = y(x_0)$
               ;; data to the step finder, and arrange to return the step plus the sequence of
               ;; following steps, propagating the suggested new step size from this step to
               ;; the next.
               [x y h]
               (lazy-seq
                (let [[solution h'] (accept-step x y h)]
                  (cons solution
                        (step (:x1 solution) (:y1 solution) h')))))]
    (step x0 y0 h0)))

;; Note: Clojurescript doesn't support rational literals without a reader
;; macro prefix. Since there are a great many rational numbers in this tableau,
;; it would be noisy to annotate each of them with #sicm/ratio. Instead we
;; write the rationals as (/ a b), since we might as well convert them to
;; floating point now rather than every time we use the tableau.

(def ^:private dormand-prince-5-tableau
  {:order [5 4]
   :tableau [[0 (/ 35 384) (/ 5179 57600) []]
             [(/ 1 5) 0 0 [(/ 1 5)]]
             [(/ 3 10) (/ 500 1113) (/ 7571 16695) [(/ 3 40) (/ 9 40)]]
             [(/ 4 5) (/ 125 192) (/ 393 640) [(/ 44 45) (/ -56 15) (/ 32 9)]]
             [(/ 8 9) (/ -2187 6784) (/ -92097 339200) [(/ 19372 6561) (/ -25360 2187) (/ 64448 6561) (/ -212 729)]]
             [1 (/ 11 84) (/ 187 2100) [(/ 9017 3168) (/ -355 33) (/ 46732 5247) (/ 49 176) (/ -5103 18656)]]
             [1 0 (/ 1 40) [(/ 35 384) 0 (/ 500 1113) (/ 125 192) (/ -2187 6784) (/ 11 84)]]]
   :dense [(/ -12715105075 11282082432)
           0
           (/ 87487479700 32700410799)
           (/ -10690763975 1880347072)
           (/ 701980252875 199316789632)
           (/ -1453857185 822651844)
           (/ 69997945 29380423)]})

(def dormand-prince-5 (partial runge-kutta dormand-prince-5-tableau))

(defn apply-solution-stream
  "Given an ordered, connected, sequence of differential equation solution
   intervals (the sort of thing produced by, e.g., dormand-prince-5), and
   an increasing sequence of x values, return a lazy sequence of y(x) values."
  [solutions xs]
  (when-first [x xs]
    (lazy-seq
     (let [solutions (drop-while #(> x (:x1 %)) solutions)]
       (cons
        [x ((:f (first solutions)) x)]
        (apply-solution-stream solutions (rest xs)))))))

(comment
  (defn brusselator [_x [y1 y2]] [(+ 1 (* y1 y1 y2) (* -4 y1)) (- (* 3 y1) (* y1 y1 y2))])
  (defn sincos [_x [y1 y2]] [y2 (- y1)])
  (defn expo [_x [y]] [y])

  (defn grid [f x0 y0 dx epsilon]
    (apply-solution-stream
     (dormand-prince-5 epsilon f x0 y0)
     (iterate #(+ % dx) x0)))

  (take 5 (dormand-prince-5 1e-4 sincos 0 [0. 1.]))
  (take 63 (grid sincos 0 [0 1] 0.1 1e-6))
  (take 11 (grid expo 0 (s/up 1) 0.1 1e-6))
  (take 5 (dormand-prince-5 1e-5 expo 0 (s/up 1))))

;; ## References:
;;
;; - Hairer, Ernst; Nørsett, Syvert Paul; Wanner, Gerhard (1993), Solving ordinary differential equations I: Nonstiff problems
;; - Butcher, John C. (1963), "Coefficients for the study of Runge-Kutta integration processes", Journal of the Australian Mathematical Society, 3 (2): 185–201
;; - Wikipedia: https://en.wikipedia.org/wiki/Runge–Kutta_methods
