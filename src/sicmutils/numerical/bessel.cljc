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
            [sicmutils.series :as s]
            [sicmutils.value :as v]))

(defn raw-coef
  "This is the raw thing..."
  [m n]
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
  ;; Here we have the series from the paper...
  (= '(1
       0
       (/ -9 128)
       0
       (/ 3675 32768)
       0
       (/ -2401245 4194304)
       0
       (/ 13043905875 2147483648))
     (v/freeze
      (take 9
            (-> (s/power-series*
                 (map (fn [m]
                        (* (g/expt (- 1) m)
                           (/ (raw-coef (* 2 m) 0)
                              (g/expt 2 (* 2 m)))))
                      (range)))
                (s/inflate 2)))))

  (= '((/ -1 8)
       0
       (/ 75 1024)
       0
       (/ -59535 262144)
       0
       (/ 57972915 33554432)
       0)
     (v/freeze
      (take 8
            (-> (s/power-series*
                 (map (fn [m]
                        (* (g/expt (- 1) m)
                           (/ (raw-coef (inc (* 2 m)) 0)
                              (g/expt 2 (inc (* 2 m))))))
                      (range)))
                (s/inflate 2))))))

(defn updater
  "Returns an updater that updates two steps."
  [n]
  (fn [[num denom m]]
    (let [four*n**2 (* 4 n n)
          two-m     (* 2 m)
          num'      (* num
                       (- four*n**2 (g/square (+ two-m 1)))
                       (- four*n**2 (g/square (+ two-m 3))))
          den'      (* denom 16 (inc m) (+ m 2))]
      [num' den' (+ 2 m)])))

(defn raw-coef**
  "This is the thing that updates ONE tick at a time!"
  ([n]
   (raw-coef** n 0))
  ([n init-num init-denom init-m]
   (iterate (updater n)
            [init-num init-denom init-m])))

(defn p-series [n]
  (-> (s/power-series*
       (map-indexed
        (fn [i [num den two-m]]
          (* (g/expt (- 1) i)
             (/ num (* den (g/expt 2 two-m)))))
        (raw-coef** n 1 1 0)))
      (s/inflate 2)))

(defn q-series
  "TODO this is janky, fix the shift."
  [n]
  (s/power-series*
   (cons 0
         (-> (s/power-series*
              (map-indexed
               (fn [i [num den two-m+1]]
                 (* (g/expt (- 1) i)
                    (/ num (* den (g/expt 2 two-m+1)))))
               (raw-coef** n -1 4 1)))
             (s/inflate 2)))))

(defn alpha-series [n]
  (s/compose s/atan-series
             (g// (g/- (q-series n))
                  (p-series n))))

(def beta-series
  (g/sqrt (g/+ (g/square p-series)
               (g/square q-series))))

(comment
  ;; Here we have the series from the paper...
  (= '(1
       0
       (/ -9 128)
       0
       (/ 3675 32768)
       0
       (/ -2401245 4194304)
       0
       (/ 13043905875 2147483648))
     (v/freeze
      (take 9 p-series)))

  (= '((/ -1 8)
       0
       (/ 75 1024)
       0
       (/ -59535 262144)
       0
       (/ 57972915 33554432)
       0)
     (v/freeze
      (take 8 q-series))))
