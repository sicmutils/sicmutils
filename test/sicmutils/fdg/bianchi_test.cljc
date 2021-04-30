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
;;

(ns sicmutils.fdg.bianchi-test
  (:refer-clojure :exclude [+ - * /])
  (:require [clojure.test :refer [is deftest testing use-fixtures]]
            [sicmutils.env :as e :refer [+ - * /]]
            [sicmutils.simplify :refer [hermetic-simplify-fixture]]
            [sicmutils.value :as v]))

(use-fixtures :each hermetic-simplify-fixture)

(require '[taoensso.tufte :as tufte :refer [defnp p profiled profile]])

(def simplify
  (comp v/freeze e/simplify))

(defn cyclic-sum [f]
  (fn [x y z]
    (+ (f x y z)
       (f y z x)
       (f z x y))))

(defn check-bianchi [coordsys f]
  (let [omega (e/literal-oneform-field 'omega coordsys)
        X (e/literal-vector-field 'X coordsys)
        Y (e/literal-vector-field 'Y coordsys)
        Z (e/literal-vector-field 'Z coordsys)
        V (e/literal-vector-field 'V coordsys)]
    (f omega X Y Z V)))

(defn torsion-symmetric
  "FDG, section 8.4, page 129: A system with a symmetric connection is
  torsion-free. The returned expression should simplify to zero."
  [coordsys]
  (let [C (e/symmetrize-Cartan
           (e/literal-Cartan 'C coordsys))
        del (e/covariant-derivative C)
        R (e/Riemann del)]
    (check-bianchi
     coordsys
     (fn [omega X Y Z _]
       (((e/torsion del) omega X Y)
        (e/typical-point coordsys))))))

(require '[taoensso.tufte :as tufte :refer [defnp p profiled profile]])
(tufte/add-basic-println-handler! {})

;;; Let's define a couple dummy fns to simulate doing some expensive work
(defn get-x [] (Thread/sleep 500)             "x val")
(defn get-y [] (Thread/sleep (rand-int 1000)) "y val")

(defn Bianchi1-symmetric
  "FDG, equation 8.32, page 130 - first Bianchi identity with a
  symmetric (torsion-free) connection."
  [coordsys]
  (let [C (e/symmetrize-Cartan
           (e/literal-Cartan 'C coordsys))
        del (e/covariant-derivative C)
        R (e/Riemann del)]
    (let [n (:dimension
             (sicmutils.calculus.manifold/manifold coordsys))]
      (check-bianchi
       coordsys
       (fn [omega X Y Z _]
         (p :outer
            (((cyclic-sum
               (fn [x y z]
                 (R omega x y z)))
              X Y Z)
             (e/typical-point coordsys))))))))

(comment
  (profile ; Profile any `p` forms called during body execution
   {} ; Profiling options; we'll use the defaults for now
   (dotimes [_ 5]
     (Bianchi1-symmetric e/R2-rect))))

(defn Bianchi1-general
  "[Bianchi's first
  identity](https://en.wikipedia.org/wiki/Torsion_tensor#Curvature_and_the_Bianchi_identities)
  with a general (not necessarily torsion-free) connection."
  [coordsys]
  (let [C   (e/literal-Cartan 'C coordsys)
        del (e/covariant-derivative C)
        R   (e/Riemann-curvature del)
        T   (e/torsion-vector del)
        TT  (e/torsion del)]
    (check-bianchi
     coordsys
     (fn [omega X Y Z _]
       (p :outer (((cyclic-sum
                    (fn [x y z]
                      (- (omega ((R x y) z))
                         (+ (omega (T (T x y) z))
                            (((del x) TT) omega y z)))))
                   X Y Z)
                  (e/typical-point coordsys)))))))

(defn Bianchi2-symmetric
  "FDG, equation 8.33, page 130 - second Bianchi identity with a
  symmetric (torsion-free) connection."
  [coordsys]
  (let [C (e/symmetrize-Cartan
           (e/literal-Cartan 'C coordsys))
        del (e/covariant-derivative C)
        R (e/Riemann del)]
    (check-bianchi
     coordsys
     (fn [omega X Y Z V]
       (let [R  (e/Riemann del)]
         (p :outer
            (((cyclic-sum
               (fn [x y z]
                 (((del x) R) omega V y z)))
              X Y Z)
             (e/typical-point coordsys))))))))

(defn Bianchi2-general
  "[Bianchi's second
  identity](https://en.wikipedia.org/wiki/Torsion_tensor#Curvature_and_the_Bianchi_identities)
  with a general (not necessarily torsion-free) connection."
  [coordsys]
  (let [C   (e/literal-Cartan 'C coordsys)
        del (e/covariant-derivative C)
        R   (e/Riemann del)
        T   (e/torsion-vector del)
        TT  (fn [omega x y]
              (omega (T x y)))]
    (check-bianchi
     coordsys
     (fn [omega X Y Z V]
       (p :outer
          (((cyclic-sum
             (fn [x y z]
               (+ (R omega V (T x y) z)
                  (((del x) R) omega V y z))))
            X Y Z)
           (e/typical-point coordsys)))))))

(deftest bianchi-identities
  ;; The default test suite comments out the second Bianchi identity for both
  ;; cases, since it runs slow (even in the R2 case!). Feel free to run it
  ;; yourself below.
  ;;
  ;; Feel free to run these with different coordinate systems! The chart below
  ;; contains timing information for R2-rect, R3-rect and R4-rect. These are
  ;; much slower than the scmutils implementation, probably because our
  ;; polynomial GCD routine is not very fast.
  ;;
  ;;
  ;; With a (torsion-free) symmetric connection:
  ;;
  ;; +----------+----------+-----------+
  ;; |          |Bianchi 1 | Bianchi 2 |
  ;; +----------+----------+-----------+
  ;; |R2        |300ms, 500ms | 6.5s, 8.5s |
  ;; +----------+----------+-----------+
  ;; |R3        | 1.5s, 4.4s  | 2.64m, 19.62m |
  ;; +----------+----------+-----------+
  ;; |R4        |  6s, 45s    |44m, ??? |
  ;; +----------+----------+-----------+
  ;;
  ;; With a general connection (with torsion):
  ;;
  ;; +----------+----------+-----------+
  ;; |          |Bianchi 1 | Bianchi 2 |
  ;; +----------+----------+-----------+
  ;; |R2        |860ms, 1.3s  |7.65s, 12.7s |
  ;; +----------+----------+-----------+
  ;; |R3        |5.8s, 14.6s | 117s, old with simple: 1521s (25m) |
  ;; +----------+----------+-----------+
  ;; |R4        |26s, 180s |???        |
  ;; +----------+----------+-----------+

  (testing "A system with a symmetric connection is torsion-free."
    (is (= 0 (simplify
              (torsion-symmetric e/R2-rect)))))

  (testing "Bianchi identities with symmetric (torsion-free) connection"
    (testing "First bianchi identity"
      (is (= 0 (simplify
                (Bianchi1-symmetric e/R2-rect)))))

    (comment
      (testing "Second bianchi identity"
        (is (= 0 (simplify
                  (Bianchi2-symmetric e/R2-rect)))))))

  (testing "Bianchi identities with general (not-torsion-free) connection"
    (testing "First (general) bianchi identity"
      (is (= 0 (simplify
                (Bianchi1-general e/R2-rect)))))

    (comment
      (testing "Second (general) bianchi identity"
        (is (= 0 (simplify
                  (Bianchi2-general e/R2-rect))))))))
