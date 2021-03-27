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

(ns sicmutils.calculus.bianchi-test
  (:refer-clojure :exclude [+ - * /])
  (:require [clojure.test :refer [is deftest testing use-fixtures]]
            [sicmutils.calculus.connection :as conn]
            [sicmutils.calculus.covariant :as cov]
            [sicmutils.calculus.curvature :as curv]
            [sicmutils.calculus.manifold :as m :refer [R2-rect R3-rect R4-rect]]
            [sicmutils.calculus.form-field :as ff]
            [sicmutils.calculus.vector-field :as vf]
            [sicmutils.generic :as g :refer [+ - * /]]
            [sicmutils.simplify :refer [hermetic-simplify-fixture]]
            [sicmutils.value :as v]))

(use-fixtures :each hermetic-simplify-fixture)

;; TODO make Bianchi1-symmetric, etc

(def simplify
  (comp v/freeze g/simplify))

(defn cyclic-sum [f]
  (fn [x y z]
    (+ (f x y z)
       (f y z x)
       (f z x y))))

(defn check-bianchi [coordsys f]
  (let [omega (ff/literal-oneform-field 'omega-rect coordsys)
        X (vf/literal-vector-field 'X-rect coordsys)
        Y (vf/literal-vector-field 'Y-rect coordsys)
        Z (vf/literal-vector-field 'Z-rect coordsys)
        V (vf/literal-vector-field 'V-rect coordsys)]
    (f omega X Y Z V)))

(defn Bianchi1-general
  "[Bianchi's first
  identity](https://en.wikipedia.org/wiki/Torsion_tensor#Curvature_and_the_Bianchi_identities)."
  [nabla]
  (fn [omega X Y Z]
    (let [R  (curv/Riemann-curvature nabla)
          T  (curv/torsion-vector nabla)
          TT (curv/torsion nabla)]
      ((cyclic-sum
        (fn [x y z]
          (- (omega ((R x y) z))
             (+ (omega (T (T x y) z))
                (((nabla x) TT) omega y z)))))
       X Y Z))))

(defn Bianchi2-general
  "[Bianchi's second
  identity](https://en.wikipedia.org/wiki/Torsion_tensor#Curvature_and_the_Bianchi_identities)."
  [nabla]
  (fn [omega X Y Z V]
    (let [R (curv/Riemann-curvature nabla)
          RT (curv/Riemann nabla)
          T (curv/torsion-vector nabla)]
      ((cyclic-sum
        (fn [x y z]
          (+ (omega ((R (T x y) z) V))
             (((nabla x) RT) omega V y z))))
       X Y Z))))

(defn literal-first-Bianchi [nabla coordsys]
  (let [omega (ff/literal-oneform-field 'omega-rect coordsys)
        X (vf/literal-vector-field 'X-rect coordsys)
        Y (vf/literal-vector-field 'Y-rect coordsys)
        Z (vf/literal-vector-field 'Z-rect coordsys)]
    (simplify
     (((Bianchi1-general nabla) omega X Y Z)
      (m/typical-point coordsys)))))

(defn literal-second-Bianchi [nabla coordsys]
  (let [omega (ff/literal-oneform-field 'omega-rect coordsys)
        X (vf/literal-vector-field 'X-rect coordsys)
        Y (vf/literal-vector-field 'Y-rect coordsys)
        Z (vf/literal-vector-field 'Z-rect coordsys)
        V(vf/literal-vector-field 'V-rect coordsys)]
    (simplify
     (((Bianchi2-general nabla) omega X Y Z V)
      (m/typical-point coordsys)))))

;; With a (torsion-free) symmetric connection:
;;
;; +----------+----------+-----------+
;; |          |Bianchi 1 | Bianchi 2 |
;; +----------+----------+-----------+
;; |R2        |800ms     | 13s       |
;; +----------+----------+-----------+
;; |R3        |10s       |1955s (32m)|
;; +----------+----------+-----------+
;; |R4        |107s      |???        |
;; +----------+----------+-----------+
;;
;; With a general connection (with torsion):
;;
;; +----------+----------+-----------+
;; |          |Bianchi 1 | Bianchi 2 |
;; +----------+----------+-----------+
;; |R2        |2.2s      |18s        |
;; +----------+----------+-----------+
;; |R3        |27s       |1521s (25m)|
;; +----------+----------+-----------+
;; |R4        |235s      |???        |
;; +----------+----------+-----------+

(deftest bianchi-identities
  ;; TODO break this out and test the R1 case.
  (testing "Bianchi identities with symmetric (torsion-free) connection"
    (let [coordsys R2-rect
          nabla (cov/covariant-derivative
                 (cov/symmetrize-Cartan
                  (conn/literal-Cartan 'C coordsys)))
          R (curv/Riemann nabla)]

      (testing "torsion check."
        (check-bianchi
         coordsys
         (fn [omega X Y _ _]
           (is (= 0 (simplify
                     (((curv/torsion nabla) omega X Y)
                      (m/typical-point coordsys))))
               "Why the hell is this failing?"))))

      (testing "First bianchi identity"
        (check-bianchi
         coordsys
         (fn [omega X Y Z _]
           (is (= 0 (simplify
                     (((cyclic-sum
                        (fn [x y z]
                          (R omega x y z)))
                       X Y Z)
                      (m/typical-point coordsys))))
               "eq 8.32, first identity."))))

      #_
      (testing "Second bianchi identity"
        (check-bianchi
         coordsys
         (fn [omega X Y Z V]
           (is (= 0 (simplify
                     (((cyclic-sum
                        (fn [x y z]
                          (((nabla x) R) omega V y z)))
                       X Y Z)
                      (m/typical-point coordsys))))
               "eq 8.33, second identity."))))))

  (testing "Bianchi identities with general (not-torsion-free) connection"
    (let [coordsys R2-rect
          nabla (cov/covariant-derivative
                 (conn/literal-Cartan 'C coordsys))
          R (curv/Riemann nabla)
          T (curv/torsion-vector nabla)
          TT (fn [omega x y]
               (omega (T x y)))]
      (check-bianchi
       coordsys
       (fn [omega X Y Z _]
         #_(is (= 0 (simplify
                     (((cyclic-sum
                        (fn [x y z]
                          (- (R omega z x y)
                             (+ (omega (T (T x y) z))
                                (((nabla x) TT) omega y z)))))
                       X Y Z)
                      (m/typical-point coordsys))))
               "first bianchi identity, page 131.")

         #_
         (is (= 0 (simplify
                   (((cyclic-sum
                      (fn [x y z]
                        (+ (R omega V (T x y) z)
                           (((nabla x) R) omega V y z))))
                     X Y Z)
                    (m/typical-point coordsys))))
             "second bianchi identity, page 131. "))))))

;; TODO tighten up!

(deftest gjs-style-bianchi-tests
  #_(testing "Bianchi identities from GJS"
      (let [coordsys R2-rect
            C (cov/symmetrize-Cartan
               (conn/literal-Cartan 'C coordsys))

            omega (ff/literal-oneform-field 'omega coordsys)
            X     (vf/literal-vector-field 'X coordsys)
            Y     (vf/literal-vector-field 'Y coordsys)
            Z     (vf/literal-vector-field 'Z coordsys)
            V     (vf/literal-vector-field 'V coordsys)
            del   (cov/covariant-derivative C)
            R     (curv/Riemann del)]

        (is (= 0 (simplify
                  ((+ (R omega X Y Z)
                      (R omega Z X Y)
                      (R omega Y Z X))
                   (m/typical-point coordsys))))
            "works in 10 seconds.")

        (is  (= 0 (simplify
                   ((+ (((del V) R) omega X Y Z)
                       (((del Z) R) omega X V Y)
                       (((del Y) R) omega X Z V))
                    (m/typical-point coordsys))))
             "second example."))

      ;; now, with torsion:
      ;;
      ;; Bianchi identities:
      ;; According to Wikipedia:Torsion tensor (Bertschinger)
      ;; Apparently stolen from
      ;;  the Encyclopedic Dictionary of Mathematics #80J, MIT Press.
      (let [coordsys R3-rect
            C (conn/literal-Cartan 'C coordsys)
            omega (ff/literal-oneform-field 'omega coordsys)
            X (vf/literal-vector-field 'X coordsys)
            Y (vf/literal-vector-field 'Y coordsys)
            Z (vf/literal-vector-field 'Z coordsys)
            V (vf/literal-vector-field 'V coordsys)
            del (cov/covariant-derivative C)]
        #_
        (is (= 0 (simplify
                  (((Bianchi1-general del) omega X Y Z)
                   (m/typical-point coordsys)))))

        #_
        (is (= 0 (simplify
                  (((Bianchi2-general del) omega V X Y Z)
                   (m/typical-point coordsys))))))))
