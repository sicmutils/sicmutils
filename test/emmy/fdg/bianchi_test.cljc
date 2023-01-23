#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.fdg.bianchi-test
  (:refer-clojure :exclude [+ - * /])
  (:require [clojure.test :refer [is deftest testing use-fixtures]]
            [emmy.env :as e :refer [+ -]]
            [emmy.simplify :refer [hermetic-simplify-fixture]]
            [emmy.value :as v]))

(use-fixtures :each hermetic-simplify-fixture)

(def simplify
  (comp v/freeze e/simplify))

(defn cyclic-sum [f]
  (fn [x y z]
    (+ (f x y z)
       (f y z x)
       (f z x y))))

(defn check-bianchi [coordsys f]
  (let [omega (e/literal-oneform-field 'omega-rect coordsys)
        X (e/literal-vector-field 'X-rect coordsys)
        Y (e/literal-vector-field 'Y-rect coordsys)
        Z (e/literal-vector-field 'Z-rect coordsys)
        V (e/literal-vector-field 'V-rect coordsys)]
    (f omega X Y Z V)))

(defn torsion-symmetric
  "FDG, section 8.4, page 129: A system with a symmetric connection is
  torsion-free. The returned expression should simplify to zero."
  [coordsys]
  (let [C (e/symmetrize-Cartan
           (e/literal-Cartan 'C coordsys))
        del (e/covariant-derivative C)]
    (check-bianchi
     coordsys
     (fn [omega X Y _ _]
       (((e/torsion del) omega X Y)
        (e/typical-point coordsys))))))

(defn Bianchi1-symmetric
  "FDG, equation 8.32, page 130 - first Bianchi identity with a
  symmetric (torsion-free) connection."
  [coordsys]
  (let [C (e/symmetrize-Cartan
           (e/literal-Cartan 'C coordsys))
        del (e/covariant-derivative C)
        R (e/Riemann del)]
    (check-bianchi
     coordsys
     (fn [omega X Y Z _]
       (((cyclic-sum
          (fn [x y z]
            (R omega x y z)))
         X Y Z)
        (e/typical-point coordsys))))))

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
       (((cyclic-sum
          (fn [x y z]
            (- (omega ((R x y) z))
               (+ (omega (T (T x y) z))
                  (((del x) TT) omega y z)))))
         X Y Z)
        (e/typical-point coordsys))))))

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
       (((cyclic-sum
          (fn [x y z]
            (((del x) R) omega V y z)))
         X Y Z)
        (e/typical-point coordsys))))))

(defn Bianchi2-general
  "[Bianchi's second
  identity](https://en.wikipedia.org/wiki/Torsion_tensor#Curvature_and_the_Bianchi_identities)
  with a general (not necessarily torsion-free) connection."
  [coordsys]
  (let [C   (e/literal-Cartan 'C coordsys)
        del (e/covariant-derivative C)
        R   (e/Riemann del)
        T   (e/torsion-vector del)]
    (check-bianchi
     coordsys
     (fn [omega X Y Z V]
       (((cyclic-sum
          (fn [x y z]
            (+ (R omega V (T x y) z)
               (((del x) R) omega V y z))))
         X Y Z)
        (e/typical-point coordsys))))))

(deftest bianchi-identities
  ;; Feel free to run these with different coordinate systems! The chart below
  ;; contains timing information for R2-rect, R3-rect and R4-rect.
  ;;
  ;;
  ;; With a (torsion-free) symmetric connection:
  ;;
  ;; +----------+--------------+--------------+
  ;; |          | Bianchi 1    | Bianchi 2    |
  ;; +----------+--------------+--------------+
  ;; |R2        | 52ms, 120ms  | 900ms, 1.4s  |
  ;; +----------+--------------+--------------+
  ;; |R3        | 230ms, 700ms | 42s, 75s     |
  ;; +----------+--------------+--------------+
  ;; |R4        | 1.2s, 2.7s   | 12m, 32.7m   |
  ;; +----------+--------------+--------------+
  ;;
  ;; With a general connection (with torsion):
  ;;
  ;; +----------+--------------+--------------+
  ;; |          | Bianchi 1    | Bianchi 2    |
  ;; +----------+--------------+--------------+
  ;; |R2        | 100ms, 350ms | 1.38s, 2.5s  |
  ;; +----------+--------------+--------------+
  ;; |R3        | 700ms, 1.9s  | 9s, 22s      |
  ;; +----------+--------------+--------------+
  ;; |R4        | 3.7s, 9s     | ~2m,  4m     |
  ;; +----------+--------------+--------------+

  (testing "A system with a symmetric connection is torsion-free."
    (is (= 0 (simplify
              (torsion-symmetric e/R2-rect)))))

  (testing "Bianchi identities with symmetric (torsion-free) connection"
    (testing "First bianchi identity"
      (is (= 0 (simplify
                (Bianchi1-symmetric e/R2-rect)))))

    (testing "Second bianchi identity"
      (is (= 0 (simplify
                (Bianchi2-symmetric e/R2-rect))))))

  (testing "Bianchi identities with general connection"
    (testing "First (general) bianchi identity"
      (is (= 0 (simplify
                (Bianchi1-general e/R2-rect)))))

    (testing "Second (general) bianchi identity"
      (is (= 0 (simplify
                (Bianchi2-general e/R2-rect)))))))
