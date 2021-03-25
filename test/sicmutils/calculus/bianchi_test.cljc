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
            [sicmutils.calculus.manifold :as m :refer [R3-rect R4-rect]]
            [sicmutils.calculus.form-field :as ff]
            [sicmutils.calculus.vector-field :as vf]
            [sicmutils.generic :as g :refer [+ - * /]]
            [sicmutils.simplify :refer [hermetic-simplify-fixture]]
            [sicmutils.value :as v]))

(use-fixtures :each hermetic-simplify-fixture)

(def simplify
  (comp v/freeze g/simplify))

(defn cyclic-sum [f]
  (fn [x y z]
    (+ (f x y z)
       (f y z x)
       (f z x y))))

(defn Bianchi1 [nabla]
  (fn [w x y z]
    (let [R (curv/Riemann-curvature nabla)
          T (curv/torsion-vector nabla)
          TT (curv/torsion nabla)]
      ((- (cyclic-sum
           (fn [x y z]
             (w ((R x y) z))))
          (cyclic-sum
           (fn [x y z]
             (+ (w (T (T x y) z))
                (((nabla x) TT) w y z)))))
       x y z))))

(defn Bianchi2 [nabla]
  (fn [w x y z v]
    (let [R (curv/Riemann-curvature nabla)
          RT (curv/Riemann nabla)
          T (curv/torsion-vector nabla)]
      ((cyclic-sum
        (fn [x y z]
          (+ (w ((R (T x y) z) v))
             (((nabla x) RT) w v y z))))
       x y z))))

(deftest bianchi-identities
  (testing "bianchi identities from book!"
    (let [omega (ff/literal-oneform-field 'omega-rect R4-rect)
          X (vf/literal-vector-field 'X-rect R4-rect)
          Y (vf/literal-vector-field 'Y-rect R4-rect)
          Z (vf/literal-vector-field 'Z-rect R4-rect)
          V (vf/literal-vector-field 'V-rect R4-rect)]

      (let [nabla (cov/covariant-derivative
                   (cov/Christoffel->Cartan
                    (cov/symmetrize-Christoffel
                     (conn/literal-Christoffel-2 'C R4-rect))))
            R  (curv/Riemann nabla)]
        (is (= 0 (simplify
                  (((curv/torsion nabla) omega X Y)
                   (m/typical-point R4-rect))))
            "fast! This is from page 129.")

        #_
        (is (= 0 (simplify
                  (((cyclic-sum
                     (fn [x y z]
                       (R omega x y z)))
                    X Y Z)
                   (m/typical-point R4-rect))))
            "woohoo, takes 107 seconds... eq 8.32, first identity.")

        #_
        (is (= 0 (simplify
                  (((cyclic-sum
                     (fn [x y z]
                       (((nabla x) R) omega V y z)))
                    X Y Z)
                   (m/typical-point R4-rect))))
            " eq 8.33, second identity."))

      ;; now, write them differently with torsion...
      (let [nabla (cov/covariant-derivative
                   (cov/Christoffel->Cartan
                    (conn/literal-Christoffel-2 'C R4-rect)))
            R  (curv/Riemann nabla)
            T  (curv/torsion-vector nabla)
            TT (fn [omega x y]
                 (omega (T x y)))]
        #_
        (is (= 0 (simplify
                  (((cyclic-sum
                     (fn [x y z]
                       (- (R omega x y z)
                          (+ (omega (T (T x y) z))
                             (((nabla x) TT) omega y z)))))
                    X Y Z)
                   (m/typical-point R4-rect))))
            "224 seconds, first bianchi identity, page 131. FAIL!")


        #_
        (is (= 0 (simplify
                  (((cyclic-sum
                     (fn [x y z]
                       (+ (((nabla x) R) omega V y z)
                          (R omega V (T x y) z))))
                    X Y Z)
                   (m/typical-point R4-rect))))
            "second bianchi identity, page 131. FAIL!"))))

  (testing "Bianchi identities from GJS"
    (comment
      "from GJS examples: weirdly it fails when I do this...."
      #_(cov/symmetrize-Cartan
         (conn/literal-Cartan 'C R3-rect)))

    (let [C (cov/Christoffel->Cartan
             (cov/symmetrize-Christoffel
              (conn/literal-Christoffel-2 'C R3-rect)))

          omega (ff/literal-oneform-field 'omega R3-rect)
          X     (vf/literal-vector-field 'X R3-rect)
          Y     (vf/literal-vector-field 'Y R3-rect)
          Z     (vf/literal-vector-field 'Z R3-rect)
          V     (vf/literal-vector-field 'V R3-rect)
          del   (cov/covariant-derivative C)
          R     (curv/Riemann del)]
      #_
      (is (= 0 (simplify
                ((+ (R omega X Y Z)
                    (R omega Z X Y)
                    (R omega Y Z X))
                 (m/typical-point R3-rect))))
          "works in 10 seconds.")

      #_
      (is (= 0 (simplify
                ((+ (((del V) R) omega X Y Z)
                    (((del Z) R) omega X V Y)
                    (((del Y) R) omega X Z V))
                 (m/typical-point R3-rect))))
          "second example.")

      ;; Bianchi identities:
      ;; According to Wikipedia:Torsion tensor (Bertschinger)
      ;; Apparently stolen from
      ;;  the Encyclopedic Dictionary of Mathematics #80J, MIT Press.
      (let [C (conn/literal-Cartan 'C R4-rect)
            omega (ff/literal-oneform-field 'omega R4-rect)
            X (vf/literal-vector-field 'X R4-rect)
            Y (vf/literal-vector-field 'Y R4-rect)
            Z (vf/literal-vector-field 'Z R4-rect)
            V (vf/literal-vector-field 'V R4-rect)
            del (cov/covariant-derivative C)]
        #_
        (is (= 0 (simplify
                  (((Bianchi1 del) omega X Y Z)
                   (m/typical-point R4-rect))))))



      (let [C (conn/literal-Cartan 'C R3-rect)
            omega (ff/literal-oneform-field 'omega R3-rect)
            X (vf/literal-vector-field 'X R3-rect)
            Y (vf/literal-vector-field 'Y R3-rect)
            Z (vf/literal-vector-field 'Z R3-rect)
            V (vf/literal-vector-field 'V R3-rect)
            del (cov/covariant-derivative C)]
        #_
        (is (= 0 (simplify
                  (((Bianchi2 del) omega V X Y Z)
                   (m/typical-point R3-rect)))))))))
