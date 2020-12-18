;;
;; Copyright © 2020 Sam Ritchie.
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
;;

(ns sicmutils.numerical.bessel
  (:require [sicmutils.generic :as g]
            [sicmutils.util :as u]
            [sicmutils.util.stream :as us]
            [sicmutils.series :as s]
            [sicmutils.value :as v]))

(defn coef
  "This inefficiently implements `(n, m)` from section 4 (Asymptotic Expansions)
  of the paper, just to check that we have the right idea."
  [n m]
  (let [num (reduce (fn [acc i]
                      (* acc (- (* 4 (* n n))
                                (g/square
                                 (dec (* 2 (inc i)))))))
                    1
                    (range 0 m))
        den (* (g/expt 2 (* 2 m))
               (g/factorial m))]
    (/ num den)))

(comment
  ;; next, these are the example expansions for $P_0(x)$ and $Q_0(x)$, just to
  ;; verify:
  (let [p0-series (-> (s/power-series*
                       (map (fn [m]
                              (* (g/expt (- 1) m)
                                 (/ (coef 0 (* 2 m))
                                    (g/expt 2 (* 2 m)))))
                            (range)))
                      (s/inflate 2))
        q0-series (-> (s/power-series*
                       (map (fn [m]
                              (* (g/expt (- 1) m)
                                 (/ (coef 0 (inc (* 2 m)))
                                    (g/expt 2 (inc (* 2 m))))))
                            (range)))
                      (s/inflate 2))]

    (= '(1
         0
         (/ -9 128)
         0
         (/ 3675 32768)
         0
         (/ -2401245 4194304)
         0
         (/ 13043905875 2147483648))
       (v/freeze (take 9 p0-series)))

    (= '((/ -1 8)
         0
         (/ 75 1024)
         0
         (/ -59535 262144)
         0
         (/ 57972915 33554432)
         0)
       (v/freeze (take 8 q0-series)))))

(defn updater
  "Returns a function that generates $(n, m+2)$ given a triple of the numerator,
  denominator and `m` from the previous evaluation.

  (Returns a triple of the same form.)"
  [n]
  (let [four*n**2 (* 4 n n)]
    (fn [[num denom m]]
      (let [two-m     (* 2 m)
            num'      (* num
                         (- four*n**2 (g/square (+ two-m 1)))
                         (- four*n**2 (g/square (+ two-m 3))))
            den'      (* denom
                         16
                         (+ m 1)
                         (+ m 2))]
        [num' den' (+ 2 m)]))))

(defn nm-seq
  "Given a value of `n` and an optional initial `m` (defaults to 0, only 0 or 1
  accepted), returns an infinite sequence of:

  [(n, m), (n, m+2), (n, m+4), ...]

  Using the definition of (n, m) from the paper."
  ([n]
   (raw-coef** n 0))
  ([n m]
   {:pre [(#{0 1} m)]}
   (let [num (if (zero? m)
               1
               (- (* 4 n n) (g/square (- (* 2 m) 1))))
         den (g/expt 2 (* 2 m))
         m   (u/bigint m)]
     (iterate (updater n) [num den m]))))

(defn p-series
  "Returns the power series $P_n(x)$ from section 4 of the paper."
  [n]
  (-> (s/power-series*
       (map-indexed
        (fn [i [num den two-m]]
          (* (g/expt (- 1) i)
             (/ num (* den (g/expt 2 two-m)))))
        (nm-seq n 0)))
      (s/inflate 2)))

(defn q-series
  "Returns the power series $Q_n(x)$ from section 4 of the paper.

  TODO this uses a goofy `cons 0` to do an efficient multiplication by `x`...
  bake this into the power series library."
  [n]
  (let [unshifted (-> (s/power-series*
                       (map-indexed
                        (fn [i [num den two-m+1]]
                          (* (g/expt (- 1) i)
                             (/ num (* den (g/expt 2 two-m+1)))))
                        (raw-coef** n 1)))
                      (s/inflate 2))]
    (s/power-series*
     (cons 0 unshifted))))

(defn alpha-series
  "Returns the power series $\\alpha(n)$ defined in the 'modified expansions'
  section of the paper."
  [n]
  (s/compose s/atan-series
             (g// (g/- (q-series n))
                  (p-series n))))

(defn beta-series
  "Returns the power series $\\beta(n)$ defined in the 'modified expansions'
  section of the paper."
  [n]
  (g/sqrt (g/+ (g/square (p-series n))
               (g/square (q-series n)))))

(comment
  ;; Once again, validate that our efficient version calculates the correct
  ;; series from the paper.
  (= '(1
       0
       (/ -9 128)
       0
       (/ 3675 32768)
       0
       (/ -2401245 4194304)
       0
       (/ 13043905875 2147483648))
     (v/freeze (take 9 (p-series 0))))

  (= '((/ -1 8)
       0
       (/ 75 1024)
       0
       (/ -59535 262144)
       0
       (/ 57972915 33554432)
       0)
     (v/freeze
      (take 8 (q-series 0)))))

;; ## Locating the Zeros
;;
;; This doesn't QUITE read like the paper because those power series are in
;; $\frac{1}{x}$, so we need to be a bit careful when trying to multiply both
;; sides by $x$. `identity` here stands for $\frac{1}{x}$.

(def x-factor
  (g/+ 1 (g/* s/identity (g/- (alpha-series 0)))))

(def p-factor
  (g/invert x-factor))

(def one-over-p
  (g/* s/identity p-factor))

(def one-over-x
  (s/revert one-over-p))

(def p-over-x
  (g// one-over-x s/identity))

(def x-over-p
  (g/invert p-over-x))

(defn x-as-fn-of-inv-p
  "This is a little wonky; because the series is in 1/p, we have to evaluate it
  first then add the final p term back on."
  [p]
  (g/+ (s/series 0 p)
       ((g// (g/- x-over-p 1) s/identity)
        (g/invert p))))

(comment
  (= (0 (/ (+ (* 8N (expt p 2)) 1) (* 8N p))
        0
        (/ -31N (* 384N (expt p 3)))
        0
        (/ 3779N (* 15360N (expt p 5)))
        0
        (/ -6277237N (* 3440640N (expt p 7)))
        0
        (/ 2092163573N (* 82575360N (expt p 9))))
     ((g/simplify (take 10 (x-as-fn-of-inv-p 'p))))))
