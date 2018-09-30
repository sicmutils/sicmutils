;
; Copyright © 2017 Colin Smith.
; This work is based on the Scmutils system of MIT/GNU Scheme:
; Copyright © 2002 Massachusetts Institute of Technology
;
; This is free software;  you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3 of the License, or (at
; your option) any later version.
;
; This software is distributed in the hope that it will be useful, but
; WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
; General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this code; if not, see <http://www.gnu.org/licenses/>.
;

(ns sicmutils.calculus.manifold
  (:require [sicmutils
             [function :as f]
             [generic :as g]
             [expression :as x]
             [simplify :refer [simplify-numerical-expression]]
             [matrix :as matrix]
             [structure :as s]]
            [sicmutils.mechanics.rotation :refer [rotate-x-matrix rotate-y-matrix rotate-z-matrix]]))

(defn make-manifold-family
  [name-format & {:keys [over] :or {over 'Real}}]
  {:over over
   :name-format name-format
   :patch {}})

(defn make-manifold
  "Specialize a manifold-family into a particular manifold by specifying
  its dimension."
  [manifold-family n & [embedding-dimension]]
  {:pre [(integer? n)
         (> n 0)]}
  {:manifold-family     manifold-family
   :name                (format (manifold-family :name-format) n)
   :dimension           n
   :embedding-dimension (or embedding-dimension n)})

(defn make-patch [name]
  "Constructor for patches."
  {:name name
   :coordinate-system {}})

(defn attach-patch
  "Produces a new manifold with the supplied patch attached."
  [manifold-family patch-name]
  (update manifold-family :patch assoc patch-name (make-patch patch-name)))

(defn attach-coordinate-system
  "Produces a new manifold family with the given coordinate system
  constructor attached and indexed by the patch and coordinate system
  names."
  [manifold-family coordinate-system-name patch-name coordinate-system-ctor]
  (update-in manifold-family [:patch patch-name :coordinate-system]
             assoc coordinate-system-name coordinate-system-ctor))

(defn ^:private default-coordinate-prototype
  [manifold]
  (let [k (:dimension manifold)]
    (s/generate k ::s/up #(symbol (str "x" %)))))

(defn coordinate-system-at
  "Looks up the named coordinate system in the named patch of the given
  manifold; this locates a constructor, which is then applied to manifold
  to produce the result: an object implementing ICoordinateSystem."
  [coordinate-system-name patch-name manifold]
  ((get-in manifold [:manifold-family
                     :patch patch-name
                     :coordinate-system coordinate-system-name])
   manifold
   (default-coordinate-prototype manifold)))

(defn diffop-name
  [form]
  (or (:name form)
      (and (= (:type form) ::x/numerical-expression)
           (x/expression-of form))
      '...))

(defprotocol ICoordinateSystem
  (check-coordinates [this coords])
  (coords->point [this coords])
  (check-point [this point])
  (point->coords [this point])
  (coordinate-prototype [this])
  (with-coordinate-prototype [this coordinate-prototype])
  (manifold [this]))

(defn ^:private make-manifold-point
  "Make a point in an abstract manifold, specified by a concrete point
  in some coordinate system, and the concrete coordinates in Euclidean
  space. The map of coordinate representaions can be lazily extended to
  yet other coordinate systems."
  [spec manifold coordinate-system coordinate-rep]
  {:type ::manifold-point
   :spec spec
   :manifold manifold
   :coordinate-representation (atom {coordinate-system coordinate-rep})})

(defn ^:private manifold-point-representation
  [manifold-point]
  (manifold-point :spec))

(defn get-coordinates
  "Get the representation of manifold-point in coordinate-system. The
  point contains a cache of the coordinate system->representation
  mapping.  If an entry for the given coordinate system is not found,
  thunk is called to produce the representation, which is then
  installed in the cache."
  [manifold-point coordinate-system thunk]
  (let [reps (manifold-point :coordinate-representation)]
    (if-let [rep (@reps coordinate-system)]
     rep
     (let [rep (s/mapr simplify-numerical-expression (thunk))]
       (swap! reps assoc coordinate-system rep)
       rep))))

(defn point->manifold
  "Return the manifold upon which this point was defined."
  [point]
  (point :manifold))

(defn ^:private my-manifold-point?
  "True if this point was created under the aegis of manifold"
  [point manifold]
  (= (point->manifold point) manifold))

(defn ^:private frame?
  "True if this coordinate system is actually a frame. FIXME: frames aren't
  implemented yet."
  ;; Note: when we get around to doing so, it probably makes sense to have
  ;; frames implement ICoordinateSystem in their own way, rather than the
  ;; hacky polymorphism used in scmutils
  [coordinate-system]
  false                                                     ; FIXME
  )

(defn chart
  [coordinate-system]
  #(point->coords coordinate-system %))

(defn point
  [coordinate-system]
  #(coords->point coordinate-system %))

(defn literal-manifold-function
  [name coordinate-system]
  (let [n (:dimension (manifold coordinate-system))
        domain (apply s/up (repeat n 0))
        range 0]
    (vary-meta
      (f/compose (f/literal-function name domain range)
                 #(point->coords coordinate-system %))
      assoc
      :name name
      :coordinate-system coordinate-system
      :type ::manifold-function)))

(defn ^:private ->Rectangular
  [manifold coordinate-prototype]
  (reify
    ICoordinateSystem
    (check-coordinates [this coords]
      (= (s/dimension coords) (manifold :dimension)))
    (coords->point [this coords]
      (assert (check-coordinates this coords))
      (make-manifold-point coords manifold this coords))
    (check-point [this point]
      (my-manifold-point? point manifold))
    (point->coords [this point]
      (assert (check-point this point))
      ;; might be an opportunity for a courtesy construtor here
      (get-coordinates point this
                       (fn []
                         (let [prep (manifold-point-representation point)]
                           (assert (= (s/dimension prep) (manifold :embedding-dimension)))
                           prep))))
    (coordinate-prototype [this] coordinate-prototype)
    (with-coordinate-prototype [this prototype] (->Rectangular manifold prototype))
    (manifold [this] manifold)))

(defn ^:private ->PolarCylindrical
  [manifold coordinate-prototype]
  (reify ICoordinateSystem
    (check-coordinates [this coords]
      (and (s/up? coords)
           (= (s/dimension coords) (manifold :dimension))
           (> (s/dimension coords) 1)
           (or (not (number? coords))
               (>= (nth coords 0) 0))))
    (coords->point [this coords]
      (assert (check-coordinates this coords))
      (let [[r theta] coords]
        (make-manifold-point
         (s/generate (count coords) ::s/up
                     (fn [^long i]
                       (case i
                         0 (g/* r (g/cos theta))
                         1 (g/* r (g/sin theta))
                         (nth coords i))))
         manifold
         this
         coords)))
    (check-point [this point]
      (my-manifold-point? point manifold))
    (point->coords [this point]
      (assert (check-point this point))
      (get-coordinates point this
                       (fn []
                         (let [prep (manifold-point-representation point)]
                           (when-not (and (s/up? prep)
                                          (= (s/dimension prep) (manifold :embedding-dimension)))
                             (throw (IllegalArgumentException. "PolarCylindrical bad point")))
                           (let [[x y] prep
                                 rsq (g/+ (g/square x) (g/square y))]
                             (when (g/zero? rsq)
                               (throw (IllegalStateException. "PolarCylindrical singular")))
                             (s/generate (count prep) ::s/up
                                         (fn [^long i]
                                           (case i
                                             0 (g/sqrt rsq)
                                             1 (g/atan y x)
                                             (nth prep i)))))))))
    (coordinate-prototype [this] coordinate-prototype)
    (with-coordinate-prototype [this prototype] (->PolarCylindrical manifold prototype))
    (manifold [this] manifold)))

(defn ^:private ->S2-coordinates
  "Colatitude-longitude coordinates for the surface of the sphere
  S(2). The orientation map (on vectors) can be used to reposition the
  polar coordinate singularities."
  [orientation]
  (let [inverse-orientation (g/invert orientation)]
    (fn ctor [manifold coordinate-prototype]
      (reify ICoordinateSystem
        (check-coordinates [this coords]
          (and (s/up? coords)
               (= (s/dimension coords) 2)
               (or (not (number? coords))
                   (>= (nth coords 0) 0))))
        (coords->point [this coords]
          (assert (check-coordinates this coords))
          (let [[colatitude longitude] coords]
            (make-manifold-point
             (g/* orientation
                  (s/up (g/* (g/sin colatitude) (g/cos longitude))
                        (g/* (g/sin colatitude) (g/sin longitude))
                        (g/cos colatitude)))
             manifold
             this
             coords)))
        (check-point [this point]
          (my-manifold-point? point manifold))
        (point->coords [this point]
          (assert (check-point this point))
          (get-coordinates point this
                           (fn []
                             (let [prep (g/* inverse-orientation (manifold-point-representation point))]
                               (when-not (and (s/up? prep)
                                              (= (s/dimension prep) (manifold :embedding-dimension)))
                                 (throw (IllegalArgumentException. "S2-coordinates bad point")))
                               (let [[x y z] prep]
                                 (s/up (g/acos z) (g/atan y x)))))))
        (coordinate-prototype [this] coordinate-prototype)
        (with-coordinate-prototype [this prototype] (ctor manifold prototype))
        (manifold [this] manifold)))))

(defn ^:private ->SphericalCylindrical
  [manifold coordinate-prototype]
  (reify
    ICoordinateSystem
    (check-coordinates [this coords]
      (and (s/up? coords)
           (= (s/dimension coords) (manifold :dimension))
           (or (not (number? coords))
               (>= (nth coords 0) 0))))
    (coords->point [this coords]
      (assert (check-coordinates this coords))
      (let [[r theta phi] coords]
        (make-manifold-point
         (s/generate (s/dimension coords) ::s/up
                     (fn [^long i]
                       (case i
                         0 (g/* r (g/sin theta) (g/cos phi))
                         1 (g/* r (g/sin theta) (g/sin phi))
                         2 (g/* r (g/cos theta))
                         (nth coords i))))
         manifold
         this
         coords)))
    (check-point [this point]
      (my-manifold-point? point manifold))
    (point->coords [this point]
      (assert (check-point this point))
      (get-coordinates point this
                       (fn []
                         (let [prep (manifold-point-representation point)]
                           (when-not (and (s/up? prep)
                                          (= (s/dimension prep) (manifold :embedding-dimension)))
                             (throw (IllegalArgumentException. "SphericalCylindrical bad point")))
                           (let [[x y z] prep
                                 r (g/sqrt (g/+ (g/square x) (g/square y) (g/square z)))]
                             (when (g/zero? r)
                               (throw (IllegalStateException. "SphericalCylindrical singular")))
                             (s/generate (s/dimension prep) ::s/up
                                         (fn [^long i]
                                           (case i
                                             0 r
                                             1 (g/acos (g/divide z r))
                                             2 (g/atan y x)
                                             (nth prep i)))))))))
    (coordinate-prototype [this] coordinate-prototype)
    (with-coordinate-prototype [this prototype] (->SphericalCylindrical manifold prototype))
    (manifold [this] manifold)))

(defn ^:private ->Stereographic
  "Stereographic projection from the final coordinate. The default pole is (0 0 ... 1),
  but this can be moved by the orthogonal (n+1) by (n+1) matrix
  returned by orientation function."
  [orientation-function]
  (fn ctor [manifold coordinate-prototype]
    (let [n (manifold :dimension)
          orientation-matrix (orientation-function (+ n 1))
          orientation-inverse-matrix (g/invert orientation-matrix)]
      (reify ICoordinateSystem
        (check-coordinates [this coords]
          (or (and (= n 1) (= (s/dimension coords) 1))
              (and (s/up? coords) (= (s/dimension coords) n))))
        (coords->point [this coords]
          (assert (check-coordinates this coords))
          (let [coords (if (= n 1) (s/up coords) coords)
                delta (g/square coords)
                xn (g/divide (g/- delta 1) (g/+ 1 delta))
                pt (s/generate (+ n 1) ::s/up #(if (= % n) xn
                                                           (g/divide (g/* 2 (nth coords %))
                                                                     (g/+ 1 delta))))]
            (make-manifold-point (g/* orientation-matrix pt) manifold this coords)))
        (check-point [this point]
          (my-manifold-point? point manifold))
        (point->coords [this point]
          (assert (check-point this point))
          (get-coordinates point this
                           (fn []
                             (let [pt (g/* orientation-inverse-matrix (manifold-point-representation point))]
                               (when (and (number? (nth pt n)) (= (nth pt n) 1))
                                 (throw (IllegalStateException. "S^n stereographic singular")))
                               (let [coords (s/generate n ::s/up #(g/divide (nth pt %) (g/- 1 (nth pt n))))]
                                 (if (= n 1) (first coords) coords))))))
        (coordinate-prototype [this] coordinate-prototype)
        (with-coordinate-prototype [this prototype] (ctor manifold prototype))
        (manifold [this] manifold)))))

(defn ^:private ->Euler-chart
  "Euler angles for SO(3)."
  [manifold coordinate-prototype]
  (let [n (manifold :dimension)]
    (reify ICoordinateSystem
      (check-coordinates [this coords]
        (and (s/up? coords)
             (= (s/dimension coords) n)
             (or (not (number? (nth coords 0)))
                 (not (zero? (nth coords 0))))))
      (coords->point [this coords]
        (assert (check-coordinates this coords))
        (let [[theta phi psi] coords
              ;; NB: scmutils uses rotate-?-tuple instead of matrix;
              ;; therefore we must transpose indices in get-coordinates
              Mx-theta (rotate-x-matrix theta)
              Mz-phi (rotate-z-matrix phi)
              Mz-psi (rotate-z-matrix psi)
              M (g/* Mz-phi Mx-theta Mz-psi)]
          (make-manifold-point M manifold this coords)))
      (check-point [this point]
        (my-manifold-point? point manifold))
      (point->coords [this point]
        (assert (check-point this point))
        (get-coordinates point this
                         (fn []
                           (let [M (manifold-point-representation point)
                                 theta (g/acos (matrix/get-in M [2 2]))
                                 phi (g/atan (matrix/get-in M [0 2])
                                             (g/negate (matrix/get-in M [1 2])))
                                 psi (g/atan (matrix/get-in M [2 0])
                                             (matrix/get-in M [2 1]))]
                             (s/up theta phi psi)))))
      (coordinate-prototype [this] coordinate-prototype)
      (with-coordinate-prototype [this prototype] (->Euler-chart manifold prototype))
      (manifold [this] manifold))))

(defn ^:private ->Alternate-chart
  "Alternate angles for SO(3)."
  [manifold coordinate-prototype]
  (let [n (manifold :dimension)]
    (reify ICoordinateSystem
      (check-coordinates [this coords]
        (and (s/up? coords)
             (= (s/dimension coords) n)
             (or (not (number? (nth coords 0)))
                 (and (< (/ Math/PI -2) (nth coords 0) (/ Math/PI 2) )))))
      (coords->point [this coords]
        (assert (check-coordinates this coords))
        (let [[theta phi psi] coords
              ;; NB: scmutils uses rotate-?-tuple instead of matrix;
              ;; therefore we must transpose indices in get-coordinates
              Mx-theta (rotate-x-matrix theta)
              Mz-phi (rotate-z-matrix phi)
              My-psi (rotate-y-matrix psi)
              pt (g/* Mz-phi Mx-theta My-psi)]
          (make-manifold-point pt manifold this coords)))
      (check-point [this point]
        (my-manifold-point? point manifold))
      (point->coords [this point]
        (assert (check-point this point))
        (get-coordinates point this
                         (fn []
                           (let [M (manifold-point-representation point)
                                 theta (g/asin (matrix/get-in M [2 1]))
                                 phi (g/atan (g/negate (matrix/get-in M [0 1]))
                                             (matrix/get-in M [1 1]))
                                 psi (g/atan (g/negate (matrix/get-in M [2 0]))
                                             (matrix/get-in M [2 2]))]
                             (s/up theta phi psi)))))
      (coordinate-prototype [this] coordinate-prototype)
      (with-coordinate-prototype [this prototype] (->Alternate-chart manifold prototype))
      (manifold [this] manifold))))

(def Rn (-> "R(%d)"
            make-manifold-family
            (attach-patch :origin)
            (attach-coordinate-system :rectangular :origin ->Rectangular)
            (attach-coordinate-system :polar-cylindrical :origin ->PolarCylindrical)))


(def R1 (make-manifold Rn 1))
(def R2 (make-manifold Rn 2))
(def R3 (make-manifold Rn 3))
(def R1-rect (coordinate-system-at :rectangular :origin R1))
(def R2-rect (coordinate-system-at :rectangular :origin R2))
(def R3-rect (coordinate-system-at :rectangular :origin R3))
(def R3-cyl (coordinate-system-at :polar-cylindrical :origin R3))
(def R2-polar (coordinate-system-at :polar-cylindrical :origin R2))

(def Sn (-> "S(%d)"
            make-manifold-family
            (attach-patch :north-pole)
            (attach-coordinate-system :spherical :north-pole ->SphericalCylindrical)
            (attach-coordinate-system :stereographic :north-pole (->Stereographic matrix/I))))

(def S2-type (-> "S2"
                 make-manifold-family
                 (attach-patch :north-pole)
                 (attach-patch :south-pole)
                 (attach-coordinate-system :spherical :north-pole
                                           (->S2-coordinates (s/down (s/up 1 0 0)
                                                                     (s/up 0 1 0)
                                                                     (s/up 0 0 1))))
                 (attach-coordinate-system :spherical :south-pole
                                           (->S2-coordinates (s/down (s/up 1 0 0)
                                                                     (s/up 0 1 0)
                                                                     (s/up 0 0 -1))))
                 (attach-coordinate-system :stereographic :north-pole (->Stereographic matrix/I))))

(def S2 (make-manifold S2-type 2 3))
(def S2-spherical (coordinate-system-at :spherical :north-pole S2))
(def S2-stereographic (coordinate-system-at :stereographic :north-pole S2))
(def S2-Riemann S2-stereographic)
;; double-check these: what goes in S2-type, and what goes in Sn(2)?


(def SO3-type (-> "SO3"
                  make-manifold-family
                  (attach-patch :Euler-patch)
                  (attach-patch :alternate-patch)
                  (attach-coordinate-system :Euler :Euler-patch ->Euler-chart)
                  (attach-coordinate-system :alternate :alternate-patch ->Alternate-chart)))

(def SO3 (make-manifold SO3-type 3))
(def Euler-angles (coordinate-system-at :Euler :Euler-patch SO3))
(def alternate-angles (coordinate-system-at :alternate :alternate-patch SO3))
