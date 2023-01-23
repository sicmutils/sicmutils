#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.calculus.manifold-test
  (:refer-clojure :exclude [* - / +])
  (:require [clojure.string :as cs]
            [clojure.test :refer [is deftest testing use-fixtures]]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]]
            [same :refer [ish?]]
            [emmy.abstract.number :refer [literal-number]]
            [emmy.calculus.manifold :as m]
            [emmy.function :as f :refer [compose]]
            [emmy.generators :as sg]
            [emmy.generic :as g :refer [cos sin * - +]]
            [emmy.simplify :refer [hermetic-simplify-fixture]]
            [emmy.structure :refer [up]]
            [emmy.util :as u]
            [emmy.value :as v]))

(use-fixtures :each hermetic-simplify-fixture)

(def s-freeze
  (comp v/freeze g/simplify))

(defn rt
  "Round trip the supplied coordinates through the supplied `coordinate-system`."
  [coordinate-system coords]
  (->> coords
       (m/coords->point coordinate-system)
       (m/point->coords coordinate-system)))

(defn cacheless-rt
  "Round trip the supplied coordinates through the `coordinate-system` without
  touching any internal cache."
  [coordinate-system coords]
  (let [p (-> (m/coords->point coordinate-system coords)
              (assoc :coordinate-representations (atom {})))]
    (m/point->coords coordinate-system p)))

(defn roundtrips?
  "Checks that `coords` round-trips using the cache and skipping the cache."
  [coordinate-system coords]
  (is (ish? coords (cacheless-rt coordinate-system coords))
      "coordinates round-trip without the cache.")

  (is (ish? coords (rt coordinate-system coords))
      "coordinates round-trip potentially using the cache."))

(defn rep-roundtrips? [coord-system point rep]
  (is (= rep (s-freeze
              (m/manifold-point-representation point)))
      "check the point representation.")

  (is (= rep (s-freeze
              (m/manifold-point-representation
               ((compose (m/point coord-system)
                         (m/chart coord-system))
                point))))
      "rep stays the same after round-tripping back through the coordinate
      system."))

(defn check-manifold
  "Collection of tests to run on the supplied `manifold`."
  [manifold]
  (is (m/manifold? manifold))
  (is (not (m/manifold-family? manifold)))
  (is (m/manifold-family? (:family manifold))))

(defn check-manifold-family
  "Collection of tests to run on some manifold family `fam`."
  [fam]
  (is (m/manifold-family? fam)))

(defn check-coord-system [coord-system coords]
  (is (m/coordinate-system? coord-system)
      "The input is in fact a coordinate system.")

  (is (not (or (m/manifold? coord-system)
               (m/manifold-family? coord-system)))
      "A coordsys is not a manifold...")

  (is (m/manifold? (m/manifold coord-system))
      "But `m/manifold` returns a valid, attached manifold.")

  (is (m/manifold? (m/manifold
                    (m/manifold coord-system)))
      "manifold returns a manifold when given a manifold.")

  (is (m/check-coordinates coord-system coords)
      "Coordinates pass.")

  (is (m/check-coordinates coord-system (m/typical-coords coord-system))
      "coords returned by `typical-coords` passes.")

  (let [point (m/coords->point coord-system coords)]
    (is (m/manifold-point? point)
        "`coords->point` returns a valid point.")

    (is (m/check-point coord-system point)
        "point returned by `m/coords->point` is always valid.")

    (is (m/check-point coord-system (m/typical-point coord-system))
        "Point returned by `typical-point` is always valid.")

    (let [proto   (m/coordinate-prototype coord-system)
          proto   (if (sequential? proto) proto [proto])
          typical (m/typical-coords coord-system)
          typical (if (sequential? typical) typical [typical])]
      (is (every? true?
                  (map (fn [proto-sym coord-sym]
                         (and (cs/starts-with?
                               (str coord-sym)
                               (str proto-sym))
                              (not= proto-sym coord-sym)))
                       proto
                       typical))
          "each coordinate in `typical-coords` starts with the prototype, but none
        are equal to the prototype symbol."))

    (is (= coords
           (m/get-coordinates
            point coord-system
            #(u/exception "Cache not installed!")))
        "the throw continuation is meant to assert that the thunk is not
        called when retrieving the coordinates from the system with which the
        manifold-point was created.")))

(deftest Rn-manifold-tests
  (check-manifold-family m/Rn)

  (doseq [manifold [m/R1 m/R2 m/R3 m/R4 m/spacetime]]
    (check-manifold manifold))

  (checking "R1-rect" 100 [x sg/real]
            ;; Both a bare number and a single-entry structure are valid for R1.
            (check-coord-system m/R1-rect x)
            (roundtrips? m/R1-rect x)
            (check-coord-system m/R1-rect (up x))
            (roundtrips? m/R1-rect (up x)))

  (checking "the-real-line alias for R1-rect" 100 [x sg/real]
            (check-coord-system m/the-real-line x)
            (roundtrips? m/the-real-line x)
            (check-coord-system m/the-real-line (up x))
            (roundtrips? m/the-real-line (up x)))

  (checking "R2-rect" 100 [coords (sg/up1 sg/real 2)]
            (check-coord-system m/R2-rect coords)
            (roundtrips? m/R2-rect coords))

  (testing "R2-rect unit"
    (testing "check-coordinates"
      (is (not (m/check-coordinates m/R2-rect (up 1))))
      (is (not (m/check-coordinates m/R2-rect (up 1 2 3))))
      (is (not (m/check-coordinates m/R2-rect 99)))))

  (checking "R2-polar" 100 [coords (sg/up1 sg/real 2)]
            (let [c0 (nth coords 0)]
              (if (neg? c0)
                (is (not (m/check-coordinates m/R2-polar coords)))
                (check-coord-system m/R2-polar coords))

              (when (pos? c0)
                (roundtrips? m/R2-rect coords))))

  (testing "R2-polar unit"
    (m/coordinate-system? m/R2-polar)

    (testing "check-coordinates"
      (is (not (m/check-coordinates m/R2-polar (up 1))))
      (is (not (m/check-coordinates m/R2-polar (up 1 2 3))))
      (is (not (m/check-coordinates m/R2-polar 99)))))

  (checking "R2-rect->polar" 100 [x (gen/fmap #(g/modulo % 1000) sg/real)
                                  y (gen/fmap #(g/modulo % 1000) sg/real)]
            (when-not (and (v/zero? x) (v/zero? y))
              (let [r     (g/abs (up x y))
                    theta (g/atan y x)]
                (is (= (up r theta)
                       (->> (up x y)
                            (m/coords->point m/R2-rect)
                            (m/point->coords m/R2-polar)))
                    "Rectangular to polar works."))))

  (testing "R2-polar->rect unit"
    (is (= (up (* 'ρ (cos 'θ))
               (* 'ρ (sin 'θ)))
           (->> (up 'ρ 'θ)
                (m/coords->point m/R2-polar)
                (m/point->coords m/R2-rect)))))

  (checking "R3-rect" 100 [coords (sg/structure1 sg/real 3)]
            (check-coord-system m/R3-rect coords)
            (roundtrips? m/R3-rect coords))

  (checking "R4-rect" 100 [coords (sg/structure1 sg/real 4)]
            (check-coord-system m/R4-rect coords)
            (roundtrips? m/R4-rect coords))

  (checking "spacetime-rect" 100 [coords (sg/structure1 sg/real 4)]
            (check-coord-system m/spacetime-rect coords)
            (roundtrips? m/spacetime-rect coords)))

(defn run-S2-tests
  [prefix S2-spherical S2-tilted S2-Riemann S2-gnomonic S2-stereographic]
  (testing prefix
    (testing "S2-spherical"
      (let [point (m/coords->point S2-spherical (up 'theta 'phi))]
        (is (= (up 0 0 0)
               (g/simplify
                (g/- (up (g/* (g/cos 'phi) (g/sin 'theta))
                         (g/* (g/sin 'theta) (g/sin 'phi))
                         (g/cos 'theta))
                     (m/manifold-point-representation point)))))

        (testing "sample points roundtrip through cache, non-cache"
          (roundtrips? S2-spherical (up 1 0))
          (roundtrips? S2-spherical (up 'theta 'phi)))

        (is (= (up 0 1) (rt S2-spherical (up 0 1)))
            "Even though this point is singular, the cache takes care of getting
            the right result.")

        (is (= (up 0 0) (cacheless-rt S2-spherical (up 0 1)))
            "Even though this point is singular, the cache takes care of getting
            the right result.")))

    (testing "S2-tilted"
      (let [point (m/coords->point S2-tilted (up 'theta 'phi))]
        (is (= (up 0 0 0)
               (g/simplify
                (g/- (up (g/* (g/cos 'phi) (g/sin 'theta))
                         (g/* -1 (g/cos 'theta))
                         (g/* (g/sin 'phi) (g/sin 'theta)))
                     (m/manifold-point-representation point)))))

        (testing "sample points roundtrip through cache, non-cache"
          (roundtrips? S2-tilted (up 1 0))
          (roundtrips? S2-tilted (up 'theta 'phi)))

        (is (= (up 0 1) (rt S2-tilted (up 0 1)))
            "Even though this point is singular, the cache takes care of getting
            the right result.")

        (is (= (up 0 0) (cacheless-rt S2-tilted (up 0 1)))
            "Even though this point is singular, the cache takes care of getting
            the right result.")))

    (testing "Riemann"
      (roundtrips? S2-Riemann (up 'x 'y))

      (let [point (m/coords->point S2-Riemann (up 'x 'y))
            rep   '(up (/ (* 2 x)
                          (+ (expt x 2) (expt y 2) 1))
                       (/ (* 2 y)
                          (+ (expt x 2) (expt y 2) 1))
                       (/ (+ (expt x 2) (expt y 2) -1)
                          (+ (expt x 2) (expt y 2) 1)))]
        (rep-roundtrips? S2-Riemann point rep))

      (is (= '(up (cos theta) (sin theta) 0)
             (s-freeze
              (m/manifold-point-representation
               ((m/point S2-Riemann)
                (up (cos 'theta) (sin 'theta))))))
          "The equator is invariant, so points map to themselves with an extra 0
          coordinate for the height."))

    (testing "gnomonic"
      (roundtrips? S2-gnomonic (up 'x 'y))

      (let [point (m/coords->point S2-gnomonic (up 'x 'y))
            rep   '(up (/ x (sqrt (+ (expt x 2) (expt y 2) 1)))
                       (/ y (sqrt (+ (expt x 2) (expt y 2) 1)))
                       (/ 1 (sqrt (+ (expt x 2) (expt y 2) 1))))]
        (rep-roundtrips? S2-gnomonic point rep))

      (is (= '(up (/ (cos theta) (sqrt 2))
                  (/ (sin theta) (sqrt 2))
                  (/ 1 (sqrt 2)))
             (s-freeze
              (m/manifold-point-representation
               ((m/point m/S2-gnomonic)
                (up (g/cos 'theta) (g/sin 'theta)))))))

      ;; The unit circle on the plane represents the intersection of S2 and z
      ;; = (/ 1 (sqrt 2))

      ;; Straight lines in the gnomonic coordinates are geodesics. We compute a
      ;; straight line, then transform it back to stereographic coordinates.
      (let [q ((m/point S2-stereographic) (up -1.5 1.5))
            p ((m/point S2-stereographic) (up 1.5 0))]
        (is (= '(up
                 (/ (+ (* 3.257142857142857 t) -0.8571428571428571)
                    (+ (sqrt (+ (* 11.343673469387754 (expt t 2))
                                (* -7.053061224489795 t)
                                2.4693877551020407))
                       -1))
                 (/ (+ (* -0.8571428571428571 t) 0.8571428571428571)
                    (+ (sqrt (+ (* 11.343673469387754 (expt t 2))
                                (* -7.053061224489795 t)
                                2.4693877551020407))
                       -1)))
               (s-freeze
                ((m/chart S2-stereographic)
                 ((m/point S2-gnomonic)
                  (+ (* 't ((m/chart S2-gnomonic) p))
                     (* (- 1 't) ((m/chart S2-gnomonic) q))))))))))))

(deftest S1-tests
  (check-manifold m/S1)

  (testing "S1"
    (roundtrips? m/S1-circular (literal-number 'theta))

    (let [point (m/coords->point m/S1-circular 'theta)]
      (is (= (up (g/cos 'theta)
                 (g/sin 'theta))
             (m/manifold-point-representation point))))

    (is (= '(atan (cos theta) (* -1 (sin theta)))
           (s-freeze
            ((compose (m/chart m/S1-circular)
                      (m/point m/S1-tilted))
             'theta)))
        "S1-tilted->circular"))

  (testing "S1-slope"
    (roundtrips? m/S1-slope (literal-number 's))

    (let [point ((m/point m/S1-slope) 's)
          rep   '(up (/ (* 2 s)
                        (+ (expt s 2) 1))
                     (/ (+ (expt s 2) -1)
                        (+ (expt s 2) 1)))]
      (rep-roundtrips? m/S1-slope point rep))))

(deftest S2-tests
  (check-manifold-family m/S2-type)
  (check-manifold m/S2)

  (run-S2-tests "S2"
                m/S2-spherical m/S2-tilted m/S2-Riemann
                m/S2-gnomonic m/S2-stereographic)

  (check-manifold-family m/Sn)
  (check-manifold m/S2p)

  (run-S2-tests "S2p"
                m/S2p-spherical m/S2p-tilted m/S2p-Riemann
                m/S2p-gnomonic m/S2p-stereographic)

  (testing "The simplifier can't show that these are the same, yet, but they
  are! S2 and S2p behave differently so we have two separate tests."
    (testing "S2p-tilted->spherical and back"
      (is (= '(up (atan (sqrt (+ (* (expt (sin theta) 2)
                                    (expt (cos phi) 2))
                                 (expt (cos theta) 2)))
                        (* (sin theta) (sin phi)))
                  (atan (* -1 (cos theta))
                        (* (sin theta) (cos phi))))
             (s-freeze
              ((compose (m/chart m/S2p-spherical)
                        (m/point m/S2p-tilted))
               (up 'theta 'phi)))))

      (is (= '(up (atan (sqrt (+ (* (expt (sin theta) 2)
                                    (expt (cos phi) 2))
                                 (expt (cos theta) 2)))
                        (* -1 (sin theta) (sin phi)))
                  (atan (cos theta)
                        (* (sin theta) (cos phi))))
             (s-freeze
              ((compose (m/chart m/S2p-tilted)
                        (m/point m/S2p-spherical))
               (up 'theta 'phi))))))

    (testing "S2-tilted->spherical and back"
      (is (= '(up (acos (* (sin theta)
                           (sin phi)))
                  (atan (* -1 (cos theta))
                        (* (sin theta) (cos phi))))
             (s-freeze
              ((compose (m/chart m/S2-spherical)
                        (m/point m/S2-tilted))
               (up 'theta 'phi)))))

      (is (= '(up (acos (* -1 (sin theta)
                           (sin phi)))
                  (atan (cos theta)
                        (* (sin theta) (cos phi))))
             (s-freeze
              ((compose (m/chart m/S2-tilted)
                        (m/point m/S2-spherical))
               (up 'theta 'phi))))))))

(deftest S3-tests
  (check-manifold m/S3)

  ;; NOTE: Should be warned singular!
  (roundtrips? m/S3-spherical (up 0 0 0))
  (roundtrips? m/S3-spherical (up 'a 'b 'c))
  (roundtrips? m/S3-tilted (up 'a 'b 'c))

  (testing "S3-{spherical,tilted}"
    (is (= '(up (atan
                 (sqrt
                  (+ (* (expt (sin b) 2) (expt (sin c) 2) (expt (cos a) 2))
                     (* (expt (sin c) 2) (expt (cos b) 2))
                     (expt (cos c) 2)))
                 (* (sin a) (sin b) (sin c)))
                (atan
                 (sqrt
                  (+ (* (expt (sin a) 2) (expt (cos c) 2) (expt (sin b) 2))
                     (expt (cos a) 2)))
                 (* (sin a) (cos b)))
                (atan
                 (* -1 (cos a)) (* (sin a) (cos c) (sin b))))
           (s-freeze
            ((compose (m/chart m/S3-spherical)
                      (m/point m/S3-tilted))
             (up 'a 'b 'c))))))

  (testing "S3-{gnomonic,stereographic}"
    (roundtrips? m/S3-gnomonic (up 'x 'y 'z))
    (roundtrips? m/S3-stereographic (up 'x 'y 'z))

    ;; S3 is one-to-one with the quaternions.
    ;; We interpret the first three components of the embedding space as the
    ;; i,j,k imaginary party and the 4th component as the real part.
    ;; The gnomonic projection removes the double-cover of quaternions to rotations.
    ;; The solid unit-sphere of the stereographic projection from the south pole likewise.
    (is (= '(up (/ (* 2 x) (+ (expt x 2) (expt y 2) (expt z 2) -1))
                (/ (* 2 y) (+ (expt x 2) (expt y 2) (expt z 2) -1))
                (/ (* 2 z) (+ (expt x 2) (expt y 2) (expt z 2) -1)))
           (s-freeze
            ((m/chart m/S3-gnomonic)
             ((m/point m/S3-stereographic)
              (up 'x 'y 'z))))))

    (is  (= '(up (/ x (+ (sqrt (+ (expt x 2) (expt y 2) (expt z 2) 1)) -1))
                 (/ y (+ (sqrt (+ (expt x 2) (expt y 2) (expt z 2) 1)) -1))
                 (/ z (+ (sqrt (+ (expt x 2) (expt y 2) (expt z 2) 1)) -1)))
            (s-freeze
             ((m/chart m/S3-stereographic)
              ((m/point m/S3-gnomonic)
               (up 'x 'y 'z))))))

    (is (= '(/ (sqrt (+ (expt x 2) (expt y 2) (expt z 2)))
               (sqrt (+ (expt x 2) (expt y 2) (expt z 2)
                        (* -2 (sqrt (+ (expt x 2) (expt y 2) (expt z 2) 1)))
                        2)))
           (s-freeze
            (g/abs
             ((m/chart m/S3-stereographic)
              ((m/point m/S3-gnomonic)
               (up 'x 'y 'z)))))))))

(deftest SO3-tests
  (check-manifold-family m/SO3-type)
  (check-manifold m/SO3)

  (testing "SO(3)"
    (roundtrips? m/alternate-angles (up 'theta 'phi 'psi))
    (roundtrips? m/Euler-angles (up 'theta 'phi 'psi))

    (is (= '(up theta phi psi)
           (s-freeze
            ((f/compose (m/chart m/Euler-angles)
                        (m/point m/alternate-angles)
                        (m/chart m/alternate-angles)
                        (m/point m/Euler-angles))
             (up 'theta 'phi 'psi)))))

    (is (= '(up (asin (* (sin theta) (cos psi)))
                (atan (+ (* (sin phi) (cos theta) (cos psi)) (* (cos phi) (sin psi)))
                      (+ (* (cos theta) (cos phi) (cos psi)) (* -1 (sin phi) (sin psi))))
                (atan (* -1 (sin theta) (sin psi)) (cos theta)))
           (s-freeze
            ((f/compose (m/chart m/alternate-angles)
                        (m/point m/Euler-angles))
             (up 'theta 'phi 'psi)))))))
