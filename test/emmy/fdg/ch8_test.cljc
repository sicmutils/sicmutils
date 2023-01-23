#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.fdg.ch8-test
  (:refer-clojure :exclude [+ - * / zero? ref partial])
  (:require [clojure.test :refer [is deftest testing use-fixtures]]
            [emmy.calculus.curvature-test :refer [S2-Christoffel]]
            [emmy.env :as e :refer [+ - * / sin zero?
                                         D partial
                                         up
                                         point chart
                                         R2-rect R2-polar
                                         define-coordinates]]
            [emmy.operator :as o]
            [emmy.simplify :refer [hermetic-simplify-fixture]]
            [emmy.value :as v]))

(use-fixtures :each hermetic-simplify-fixture)

(def simplify
  (comp v/freeze e/simplify))

(define-coordinates [theta phi] e/S2-spherical)

(def S2-basis (e/coordinate-system->basis S2-spherical))
(def S2C (S2-Christoffel S2-basis theta))
(def sphere-Cartan (e/Christoffel->Cartan S2C))

(deftest section-8-1
  (is (= 1 (simplify
            (((e/Riemann (e/covariant-derivative sphere-Cartan))
              dphi d:dtheta d:dphi d:dtheta)
             ((point S2-spherical) (up 'theta0 'phi0)))))
      "result from page 116."))

;; ## Verification in two dimensions

(def Chi-inverse (point R2-rect))
(def Chi (chart R2-rect))

(def general-Cartan-2
  (e/Christoffel->Cartan
   (e/literal-Christoffel-2 'Gamma R2-rect)))

(defn make-state [sigma u] [sigma u])
(defn Sigma [state] (nth state 0))
(defn U-select [state] (nth state 1))

(defn Du [v]
  (fn [state]
    (let [CF (e/Cartan->forms general-Cartan-2)]
      (* -1
         ((CF v) (Chi-inverse (Sigma state)))
         (U-select state)))))

(defn Dsigma [v]
  (fn [state]
    ((v Chi) (Chi-inverse (Sigma state)))))

(defn g [v]
  (fn [state]
    (make-state ((Dsigma v) state) ((Du v) state))))

(defn L [v]
  (letfn [(l [h]
            (fn [state]
              (* ((D h) state) ((g v) state))))]
    (o/make-operator l)))

(defn curvature-from-transport [Cartan]
  (fn [w v]
    (fn [u]
      (fn [f]
        (let [CF (e/Cartan->forms Cartan)
              basis (e/Cartan->basis Cartan)
              fi (e/basis->oneform-basis basis)
              ei (e/basis->vector-basis basis)]
          (* (ei f)
             (+ (* (- (- (w (CF v)) (v (CF w)))
                      (CF (e/commutator w v)))
                   (fi u))
                (- (* (CF w) (* (CF v) (fi u)))
                   (* (CF v) (* (CF w) (fi u)))))))))))

(deftest page-120-onward
  (let [U (e/literal-vector-field 'U-rect R2-rect)
        W (e/literal-vector-field 'W-rect R2-rect)
        V (e/literal-vector-field 'V-rect R2-rect)
        sigma (up 'sigma0 'sigma1)
        nabla (e/covariant-derivative general-Cartan-2)
        m (Chi-inverse sigma)
        s (make-state sigma ((U Chi) m))]
    (is (= '(up 0 0)
           (simplify
            (- (((- (e/commutator (L V) (L W))
                    (L (e/commutator V W)))
                 U-select)
                s)
               (((((e/Riemann-curvature nabla) W V) U) Chi) m))))
        "The lowest-order change resulting from explicit parallel transport of a
         vector around an infinitesimal loop is what is computed by the Riemann
         curvature."))

  (testing "page 121"
    (letfn [(run-test [coordsys Cartan]
              (let [m (e/typical-point coordsys)
                    u (e/literal-vector-field 'u-coord coordsys)
                    w (e/literal-vector-field 'w-coord coordsys)
                    v (e/literal-vector-field 'v-coord coordsys)
                    f (e/literal-manifold-function 'f-coord coordsys)
                    nabla (e/covariant-derivative Cartan)]
                (simplify
                 (- (((((curvature-from-transport Cartan) w v) u) f) m)
                    (((((e/Riemann-curvature nabla) w v) u) f) m)))))]
      (is (zero?
           (run-test R2-rect general-Cartan-2)))

      (is (zero?
           (run-test R2-polar general-Cartan-2)))))

  (testing "page 124"
    (doseq [x [d:dtheta d:dphi]
            y [d:dtheta d:dphi]]
      (is (zero?
           (simplify
            ((((e/torsion-vector (e/covariant-derivative sphere-Cartan))
               x y)
              (e/literal-manifold-function 'f S2-spherical))
             ((point S2-spherical) (up 'theta0 'phi0))))))))

  (testing "Longitude lines on a sphere, p127"
    (let [T d:dtheta
          U d:dphi
          omega (e/literal-oneform-field 'omega S2-spherical)
          f (e/literal-manifold-function 'f S2-spherical)
          m ((point S2-spherical) (up 'theta0 'phi0))
          Cartan (e/Christoffel->Cartan S2C)
          nabla (e/covariant-derivative Cartan)]
      (is (zero?
           (simplify
            ((omega (((e/covariant-derivative Cartan) T) T)) m)))
          "every longitude line is a geodesic.")

      (is (zero?
           (simplify
            (((e/commutator U T) f) m)))
          "Now let U be d:dphi, then U commutes with T")

      (let [X (e/literal-vector-field 'X-sphere S2-spherical)
            Y (e/literal-vector-field 'Y-sphere S2-spherical)]
        (is (zero?
             (simplify
              ((((e/torsion-vector nabla) X Y) f) m)))
            "The torsion for the usual connection for the sphere is zero"))

      (is (zero?
           (simplify
            ((+ (omega ((nabla T) ((nabla T) U)))
                ((e/Riemann nabla) omega T U T))
             m)))
          "compute the geodesic deviation using `Riemann`")

      (testing "p128"
        (is (= '(/ (cos theta0) (sin theta0))
               (simplify
                ((dphi ((nabla T) U)) m))))

        (is (= -1 (simplify
                   ((dphi ((nabla T) ((nabla T) U))) m))))

        (letfn [(delta [R]
                  (fn [_ theta Delta-phi]
                    (* R (sin theta) Delta-phi)))]
          (is (= '(* Delta-phi R (cos theta0))
                 (v/freeze
                  (((partial 1) (delta 'R)) 'phi0 'theta0 'Delta-phi))))

          (let [phi-hat (* (/ 1 (sin theta)) d:dphi)]
            (is (= '(/ (* Delta-phi R (cos theta0)) (sin theta0))
                   (simplify
                    ((dphi (* (((partial 1) (delta 'R))
                               'phi0 'theta0 'Delta-phi)
                              phi-hat))
                     m))))

            (is (= '(* -1 Delta-phi R (sin theta0))
                   (simplify
                    (((partial 1) ((partial 1) (delta 'R)))
                     'phi0 'theta0 'Delta-phi)))
                "magnitude of the acceleration")

            (is (= '(* -1 Delta-phi R)
                   (simplify
                    ((dphi (* (((partial 1) ((partial 1) (delta 'R)))
                               'phi0 'theta0 'Delta-phi)
                              phi-hat))
                     m)))
                "Measuring acceleration with dφ")))))))

;; NOTE that The Bianchi identities live on their own, in
;; `emmy.fdg.bianchi-test`.
