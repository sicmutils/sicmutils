;;
;; Copyright © 2021 Sam Ritchie.
;; This work is based on the Scmutils system of MIT/GNU Scheme:
;; Copyright © 2002 Massachusetts Institute of Technology
;;
;; This is free software;  you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or (at
;; your option) any later version.
;;
;; This software is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this code; if not, see <http://www.gnu.org/licenses/>.

(ns sicmutils.fdg.ch9-test
  (:refer-clojure :exclude [+ - * / zero? ref partial])
  (:require [clojure.test :refer [is deftest testing use-fixtures]]
            [sicmutils.env :as e :refer [+ - * / square sin expt
                                         zero?
                                         compose D d freeze partial
                                         up down
                                         point chart
                                         R2-rect R3-rect S2-spherical
                                         let-coordinates]
             #?@(:cljs [:include-macros true])]
            [sicmutils.calculus.curvature-test :refer [S2-Christoffel]]
            [sicmutils.calculus.vector-field :as vf]
            [sicmutils.operator :as o]
            [sicmutils.value :as v]
            [sicmutils.simplify :refer [hermetic-simplify-fixture]]
            [taoensso.tufte :as tufte :refer [defnp p profiled profile]]))

(tufte/add-basic-println-handler! {})

(use-fixtures :each hermetic-simplify-fixture)

(def simplify
  (comp v/freeze e/simplify))

(let-coordinates [[theta phi] S2-spherical]
  (def S2-basis (e/coordinate-system->basis S2-spherical))
  (def S2C (S2-Christoffel S2-basis theta))
  (def sphere-Cartan (e/Christoffel->Cartan S2C))

  (defn g-sphere [R]
    (fn [u v]
      (* (e/square R)
         (+ (* (dtheta u) (dtheta v))
            (* (e/compose (square sin) theta)
               (dphi u)
               (dphi v)))))))

(defn metric->Lagrangian [metric coordsys]
  (fn L [[_ q qd]]
    (let [v (e/components->vector-field (fn [_] qd) coordsys)]
      ((* (/ 1 2) (metric v v)) ((point coordsys) q)))))

(defn Lagrange-explicit [L]
  (let [P ((partial 2) L)
        F ((partial 1) L)]
    (/ (- F (+ ((partial 0) P)
               (* ((partial 1) P) e/velocity)))
       ((partial 2) P))))

(deftest ch9-tests
  ;; `lower` and `raise` live in metric.cljc. TODO note that raise in the book
  ;; does not return a legit procedure->vector-field. Probably will break the
  ;; scheme.


  ;; TODO all setup here is missing, or assumed to come from the previous
  ;; section again.
  (testing "computing Christoffel, page 136"
    (is (= '(down
             (down (down 0 0)
                   (down 0 (* (expt R 2) (cos theta0) (sin theta0))))
             (down
              (down 0 (* (expt R 2) (cos theta0) (sin theta0)))
              (down (* -1 (expt R 2) (cos theta0) (sin theta0))
                    0)))
           (simplify
            ((e/Christoffel->symbols
              (e/metric->Christoffel-1 (g-sphere 'R) S2-basis))
             ((point S2-spherical) (up 'theta0 'phi0)))))
        "Computing Christoffel coefficients, page 136"))

  (is (= '(down (down (up 0 0)
                      (up 0 (/ (cos theta0) (sin theta0))))
                (down (up 0 (/ (cos theta0) (sin theta0)))
                      (up (* -1 (cos theta0) (sin theta0)) 0)))
         (simplify
          ((e/Christoffel->symbols
            (e/metric->Christoffel-2 (g-sphere 'R) S2-basis))
           ((point S2-spherical) (up 'theta0 'phi0)))))
      "page 137")

  (is (= '(down (down (up 0 0 0) (up 0 0 0) (up 0 0 0))
                (down (up 0 0 0) (up 0 0 0) (up 0 0 0))
                (down (up 0 0 0) (up 0 0 0) (up 0 0 0)))
         (simplify
          (let [metric (e/literal-metric 'g R3-rect)
                q (e/typical-coords R3-rect)
                L2 (metric->Lagrangian metric R3-rect)]
            (+ (* (/ 1 2)
                  (((expt (partial 2) 2) (Lagrange-explicit L2))
                   (up 't q (e/corresponding-velocities q))))
               ((e/Christoffel->symbols
                 (e/metric->Christoffel-2 metric
                                          (e/coordinate-system->basis R3-rect)))
                ((point R3-rect) q))))))
      "page 138"))

(def L2
  (metric->Lagrangian (e/literal-metric 'm R2-rect)
                      R2-rect))
(defn L1 [state]
  (e/sqrt (* 2 (L2 state))))

(deftest arc-length-tests
  (testing "page 140"
    (is (= '(+ (* (m_00 (up x y)) (m_11 (up x y)))
               (* -1 (expt (m_01 (up x y)) 2)))
           (simplify
            (e/determinant
             (((partial 2) ((partial 2) L2))
              (up 't (up 'x 'y) (up 'vx 'vy))))))
        "mass matrix of L_2 is nonsingular")

    (is (zero?
         (simplify
          (e/determinant
           (((partial 2) ((partial 2) L1))
            (up 't (up 'x 'y) (up 'vx 'vy))))))
        "mass matrix of L_1 has determinant 0")

    (testing "page 141"
      (letfn [(L1 [state]
                (e/sqrt (square (e/velocity state))))]
        (is (= '(down
                 (/ (+ (* -1 ((D x) t) ((D y) t) (((expt D 2) y) t))
                       (* (((expt D 2) x) t) (expt ((D y) t) 2)))
                    (+ (* (expt ((D x) t) 2) (sqrt (+ (expt ((D x) t) 2) (expt ((D y) t) 2))))
                       (* (expt ((D y) t) 2) (sqrt (+ (expt ((D x) t) 2) (expt ((D y) t) 2))))))
                 (/ (+ (* (expt ((D x) t) 2) (((expt D 2) y) t))
                       (* -1 ((D x) t) (((expt D 2) x) t) ((D y) t)))
                    (+ (* (expt ((D x) t) 2) (sqrt (+ (expt ((D x) t) 2) (expt ((D y) t) 2))))
                       (* (expt ((D y) t) 2) (sqrt (+ (expt ((D x) t) 2) (expt ((D y) t) 2)))))))
               (simplify
                (((e/Lagrange-equations L1)
                  (up (e/literal-function 'x) (e/literal-function 'y)))
                 't)))
            "NOTE that the simplifier here and in the current version of
             scmutils can't figure out that the denominators are to the 3/2
             power.")))

    (e/with-literal-functions [x y f]
      (let [E1 (e/Euler-Lagrange-operator L1)]
        (is (= '(down 0 0)
               (simplify
                ((- (compose E1
                             (e/Gamma (up (compose x f)
                                          (compose y f))
                                      4))
                    (* (compose E1
                                (e/Gamma (up x y) 4)
                                f)
                       (D f)))
                 't)))
            "page 142"))

      (let [q (up x y)]
        (is (= '(down (+ (* (m_00 (up (x (f t)) (y (f t)))) ((D x) (f t)) (((expt D 2) f) t))
                         (* (m_01 (up (x (f t)) (y (f t)))) ((D y) (f t)) (((expt D 2) f) t)))
                      (+ (* ((D x) (f t)) (m_01 (up (x (f t)) (y (f t)))) (((expt D 2) f) t))
                         (* ((D y) (f t)) (m_11 (up (x (f t)) (y (f t)))) (((expt D 2) f) t))))
               (simplify
                ((- (compose (e/Euler-Lagrange-operator L2)
                             (e/Gamma (compose q f) 4))
                    (* (compose (e/Euler-Lagrange-operator L2)
                                (e/Gamma q 4)
                                f)
                       (expt (D f) 2)))
                 't)))
            "page 143")))))

;; TODO the system did not say to install spacetime coordinates!

(let-coordinates [[t x y z] e/spacetime-rect]
  ;; TODO this was missing!
  (def spacetime-rect-basis
    (e/coordinate-system->basis spacetime-rect))

  (defn Newton-metric [M G c V]
    (let [a (+ 1 (* (/ 2 (square c))
                    (compose V (up x y z))))]
      (fn g [v1 v2]
        (let [ret (+ (* -1 (square c) a (dt v1) (dt v2))
                     (* (dx v1) (dx v2))
                     (* (dy v1) (dy v2))
                     (* (dz v1) (dz v2)))]
          (fn [& args]
            (p :newton/inner (apply ret args))))))))

(defn Newton-connection [M G c V]
  (e/Christoffel->Cartan
   (e/metric->Christoffel-2 (Newton-metric M G c V)
                            spacetime-rect-basis)))

(def nabla
  (e/covariant-derivative
   (Newton-connection
    'M 'G 'c
    (e/literal-function 'V '(-> (UP Real Real Real) Real)))))

(deftest general-relativity-tests
  ;; TODO this should just use `spacetime-rect-basis` from above.
  #_(let-coordinates [[t x y z] e/spacetime-rect]
      (simplify
       (((e/Ricci nabla spacetime-rect-basis)
         d:dt d:dt)
        ((point spacetime-rect) (up 'x 'y 'z 't)))))

  (testing "OKAY, here we go, this is a clue! This one takes 158 seconds. Why is
  this slow? It only takes 135 seconds on my old machine. At least in this case
  we get the correct answer."
    (profile {}
             (let-coordinates [[t x y z] e/spacetime-rect]
               (p :overall
                  (simplify
                   (p :inner-call
                      (((e/Ricci nabla spacetime-rect-basis)
                        d:dt d:dt)
                       ((point spacetime-rect) (up 't 'x 'y 'z))))))))))


;; HUH! That is a very interesting clue!!

;; here is the current stuff for the d:dt d:dt call, WITH a memoize which makes it possible, but no memoize on the dt dx dy etc funcitons.

;; with memoize on those functions...

;; :coord-basis-oneform-field-procedure/outer  3,118,452     2.83μs     3.65μs     5.08μs     5.70μs     7.94μs     8.55s     24.53μs ±168%     1.27m    232%
;; :coordinate-basis                           3,177,912     2.79μs     3.62μs     5.17μs     6.17μs    60.39μs     2.59s     10.90μs ±126%    34.64s    105%
;; :overall                                            1    32.90s     32.90s     32.90s     32.90s     32.90s     32.90s     32.90s    ±0%    32.90s    100%
;; :inner-call                                         1    32.82s     32.82s     32.82s     32.82s     32.82s     32.82s     32.82s    ±0%    32.82s    100%
;; :coordinate-basis/inner-f                   3,177,912   166.00ns   250.00ns   304.00ns   339.00ns    56.83μs     2.59s      6.56μs ±189%    20.84s     63%
;; :newton/inner                                 369,664    31.63μs    36.00μs    58.58μs    66.29μs    90.96μs   261.72ms    53.65μs  ±48%    19.83s     60%
;; :s/mapr                                       308,753   750.00ns     1.46μs    92.67μs   331.21μs   367.83μs   130.48ms    46.72μs ±132%    14.42s     44%
;; :point-application                          3,177,913   708.00ns   875.00ns     1.24μs     1.42μs     1.82μs    64.98ms     1.01μs  ±22%     3.22s     10%
;; :tufte/compaction                                  28    46.82ms    63.11ms   197.26ms   255.17ms   261.60ms   261.60ms    87.92ms  ±50%     2.46s      7%
;; :coord-basis-oneform-field-procedure/inner  3,118,944    83.00ns   167.00ns   209.00ns   244.00ns   367.00ns   197.27ms   352.02ns  ±97%     1.10s      3%
;; :chart-application                          6,296,856     0.00ns    42.00ns    84.00ns    90.00ns   154.00ns   175.34ms    90.87ns  ±88%   572.21ms     2%
;; :basis/contract                                     1    81.53ms    81.53ms    81.53ms    81.53ms    81.53ms    81.53ms    81.53ms   ±0%    81.53ms     0%
;; :Ricci/inner                                        4    12.98ms    18.07ms    34.09ms    34.09ms    34.09ms    34.09ms    20.29ms  ±34%    81.15ms     0%
;; :internal-mapr                                  1,364    17.25μs    28.25μs    79.79μs    94.67μs   135.88μs   759.50μs    40.78μs  ±54%    55.62ms     0%
;; :partial-derivative                             2,304     1.92μs     3.08μs     4.92μs     5.71μs     7.75μs    98.54μs     3.53μs  ±28%     8.13ms     0%

;; Accounted                                                                                                                                    3.99m    728%
;; Clock32.91s    100

;; More memoize applied

;; pId                                            nCalls        Min      50% ≤      90% ≤      95% ≤      99% ≤        Max       Mean   MAD      Clock  Total

;; :coord-basis-oneform-field-procedure/outer  2,655,924    41.00ns   125.00ns   225.00ns   391.00ns     2.52μs     2.36s      6.66μs ±193%    17.69s    196%
;; :overall                                            1     9.04s      9.04s      9.04s      9.04s      9.04s      9.04s      9.04s    ±0%     9.04s    100%
;; :inner-call                                         1     8.98s      8.98s      8.98s      8.98s      8.98s      8.98s      8.98s    ±0%     8.98s     99%
;; :coordinate-basis                           2,715,384     0.00ns    84.00ns   183.00ns   350.00ns     2.58μs   960.68ms     2.03μs ±186%     5.51s     61%
;; :coordinate-basis/inner-f                      27,816   167.00ns   292.00ns   500.00ns     3.83μs    77.33μs   960.63ms   177.25μs ±197%     4.93s     55%
;; :newton/inner                                 311,872     5.33μs     6.04μs    24.04μs    25.29μs    29.88μs   111.63ms    10.62μs  ±70%     3.31s     37%
;; :s/mapr                                       247,625   750.00ns     1.29μs     2.29μs     6.42μs     8.29μs    51.63ms     2.13μs  ±72%   527.21ms     6%
;; :tufte/compaction                                   7    51.52ms    62.98ms    87.29ms    89.14ms    89.14ms    89.14ms    66.83ms  ±18%   467.81ms     5%
;; :basis/contract                                     1    49.74ms    49.74ms    49.74ms    49.74ms    49.74ms    49.74ms    49.74ms   ±0%    49.74ms     1%
;; :Ricci/inner                                        4    11.38ms    12.73ms    13.14ms    13.14ms    13.14ms    13.14ms    12.41ms   ±4%    49.63ms     1%
;; :point-application                             27,817   916.00ns     1.13μs     1.67μs     1.92μs     3.38μs   400.75μs     1.32μs  ±25%    36.67ms     0%
;; :internal-mapr                                  1,364    12.88μs    17.63μs    55.58μs    59.38μs    81.58μs   235.92μs    26.73μs  ±54%    36.46ms     0%
;; :coord-basis-oneform-field-procedure/inner     26,688   125.00ns   250.00ns   292.00ns   625.00ns     3.67μs    18.17μs   388.93ns  ±72%    10.38ms     0%
;; :chart-application                             54,504     0.00ns    83.00ns   125.00ns   167.00ns     2.96μs    76.79μs   167.18ns ±113%     9.11ms     0%
;; :partial-derivative                             2,272     1.75μs     2.29μs     3.33μs     4.00μs     7.13μs   108.04μs     2.67μs  ±26%     6.06ms     0%

;; Accounted                                                                                                                                   50.65s    560%
;; Clock                                                                                                                                        9.04s    100

;; So the dx, dy, etc functions are doing a shitload of work. what about
;; functions returned by d:dt etc? Probably the same! Why? Same work for
;; scmutils? How are they dodging the issues?
;;
;; Hmmm...
;;
;; TODO try and directly memoize the vector field?


;; A good improvement:
;;
;; :coord-basis-oneform-field-procedure/outer    166,708    41.00ns     1.96μs    11.29μs    14.17μs    15.42μs     1.53s     67.22μs ±188%    11.21s    198%
;; :overall                                            1     5.65s      5.65s      5.65s      5.65s      5.65s      5.65s      5.65s    ±0%     5.65s    100%
;; :inner-call                                         1     5.65s      5.65s      5.65s      5.65s      5.65s      5.65s      5.65s    ±0%     5.65s    100%
;; :coordinate-basis                              63,276    42.00ns     2.21μs    11.54μs    14.54μs    58.71μs   499.70ms    50.11μs ±181%     3.17s     56%
;; :coordinate-basis/inner-f                       5,404   250.00ns   375.00ns    52.13μs    96.42μs   177.08μs   499.66ms   533.45μs ±195%     2.88s     51%
;; :s/mapr                                       195,681   791.00ns     1.42μs     6.96μs     7.33μs     7.88μs   317.63μs     1.99μs  ±61%   388.82ms     7%
;; :newton/inner                                     720     5.50μs    70.75μs   138.00μs   180.04μs   442.00μs   686.54μs    84.43μs  ±52%    60.79ms     1%
;; :basis/contract                                     1    18.45ms    18.45ms    18.45ms    18.45ms    18.45ms    18.45ms    18.45ms   ±0%    18.45ms     0%
;; :Ricci/inner                                        4     4.16ms     4.61ms     5.37ms     5.37ms     5.37ms     5.37ms     4.60ms   ±8%    18.39ms     0%
;; :point-application                              5,405   833.00ns     1.08μs     1.58μs     1.83μs     3.46μs    30.50μs     1.25μs  ±24%     6.73ms     0%
;; :internal-mapr                                     56    39.08μs   106.67μs   130.96μs   143.83μs   163.75μs   318.25μs   109.82μs  ±17%     6.15ms     0%
;; :coord-basis-oneform-field-procedure/inner      4,756   166.00ns   250.00ns   375.00ns   791.00ns     3.33μs    62.21μs   401.72ns  ±75%     1.91ms     0%
;; :chart-application                             10,160    41.00ns    42.00ns   125.00ns   167.00ns     2.54μs    61.79μs   176.97ns ±120%     1.80ms     0%
;; :partial-derivative                               244     3.42μs     4.29μs     5.50μs     6.00μs    10.42μs    16.58μs     4.59μs  ±15%     1.12ms     0%

;; Accounted                                                                                                                                   29.07s    514%
;; Clock                                                                                                                                        5.65s    100%
