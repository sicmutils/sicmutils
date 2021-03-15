;;
;; Copyright © 2017 Colin Smith.
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

(ns sicmutils.calculus.manifold
  (:require #?(:cljs [goog.string :refer [format]])
            [sicmutils.abstract.function :as af]
            [sicmutils.abstract.number :as an]
            [sicmutils.expression :as x]
            [sicmutils.function :as f]
            [sicmutils.generic :as g]
            [sicmutils.matrix :as matrix]
            [sicmutils.operator :as o]
            [sicmutils.simplify :refer [simplify-numerical-expression]]
            [sicmutils.structure :as s]
            [sicmutils.util :as u]
            [sicmutils.value :as v]
            [sicmutils.mechanics.rotation :refer [rotate-x-matrix rotate-y-matrix rotate-z-matrix]]))

;; Manifolds

;; NOTE: this is a chunk of `specify-manifold`, `type` there is `over` here.
;;
;; NOTE: GJS calls this a `manifold-type` in scmutils.

(defn make-manifold-family
  [name-format & {:keys [over] :or {over 'Real}}]
  {:over over
   :name-format name-format
   :patches {}})

(defn make-manifold
  "Specialize a manifold-family into a particular manifold by specifying
  its dimension."
  ([family n]
   (make-manifold family n n))
  ([family n embedding-dimension]
   {:pre [(integer? n)
          (> n 0)
          (>= embedding-dimension n)]}
   {:manifold-family     family
    :name                (format (:name-format family) n)
    :dimension           n
    :embedding-dimension embedding-dimension}))

(defn patch
  "Returns the patch named by `patch-name` within the supplied `manifold` if
  registered. Throws otherwise.

  TODO note that this NOW sticks a reference to the manifold. onto the patch
  when you grab it."
  [patch-name manifold]
  (if-let [gen (get-in manifold [:manifold-family
                                 :patches patch-name])]
    (assoc gen :manifold manifold)
    (throw
     (ex-info "Unknown patch."
              {:patch-name patch-name
               :manifold manifold}))))

;; TODO :dimension, :embedding-dimension use keyword lookup. Weird?
;;
;; TODO distinguished-point, with-distinguished-point

(defn patch-names
  "Returns a set of patch names registered in the supplied manifold."
  [manifold]
  (u/keyset
   (get-in manifold [:manifold-family :patches])))

;; maybe missing... `manifold` identity function?

;; ## Patch Attachment!

(defn make-patch
  "Makes a bare batch, with no manifold on it!"
  [name]
  {:name name
   :coordinate-systems {}})

;; now we get patches back into manifolds...

(defn attach-patch
  "Produces a new manifold with the supplied patch attached."
  [manifold-family patch-name]
  (update manifold-family
          :patches
          assoc patch-name (make-patch patch-name)))

;; NOTE patch gets its manifold. coordinate system gets its patch.

;; ## Coordinate Systems
;;
;; Coordinate systems are added to coordinate patches.
;; A coordinate system is an invertible map from the space to R^n.

(defn coordinate-system
  "If the system exists, boom, return it. Else, errors."
  [system-name patch]
  (or (get-in patch [:coordinate-systems system-name])
      (throw
       (ex-info "Unknown coordinate system."
                {:coordinate-system-name system-name
                 :patch patch}))))

(defn coordinate-system-at
  "Looks up the named coordinate system in the named patch of the given
  `manifold`; this locates a constructor, which is then applied to manifold to
  return an object implementing [[ICoordinateSystem]]."
  [coordinate-system-name patch-name manifold]
  (let [ctor (coordinate-system coordinate-system-name
                                (patch patch-name manifold))]
    (ctor manifold)))

(defn coordinate-system-names
  "Returns a set of coordinate system names registered in the supplied patch."
  [patch]
  (u/keyset
   (:coordinate-systems patch)))

(defn attach-coordinate-system
  "Produces a new manifold family with the given coordinate system
  constructor attached and indexed by the patch and coordinate system
  names."
  [manifold-family coordinate-system-name patch-name coordinate-system-ctor]
  (let [ks [:patches patch-name :coordinate-systems]
        k  coordinate-system-name
        v  coordinate-system-ctor]
    (update-in manifold-family ks assoc k v)))

(defn- default-coordinate-prototype
  "similar to `literal-up` without the variance superscript in the generated
  symbols."
  [manifold]
  (let [k (:dimension manifold)]
    (s/generate k ::s/up (fn [i]
                           (symbol (str "x" i))))))

;; then joint accessors.

(defn diffop-name [form]
  (cond (o/operator? form)        (o/name form)
        (an/literal-number? form) (x/expression-of form)
        :else '...))

(defprotocol ICoordinateSystem
  (check-coordinates [this coords])
  (coords->point [this coords])
  (point->coords [this point])
  (check-point [this point])
  ;; missing check-coords, typical-coords, access-chains, dual-chains
  (coordinate-prototype [this])
  (with-coordinate-prototype [this coordinate-prototype])

  ;; TODO maybe this should be `patch` instead... and then THAT can give us a
  ;; manifold?
  (manifold [this]))

;; ## Manifold Points!

(defn- make-manifold-point
  "Make a point in an abstract manifold, specified by a concrete point
  in some coordinate system, and the concrete coordinates in Euclidean
  space.

  The map of coordinate representaions can be lazily extended to yet other
  coordinate systems."
  ([spec manifold]
   {:type ::manifold-point
    :spec spec
    :manifold manifold
    :coordinate-representation (atom {})})
  ([spec manifold coordinate-system coordinate-rep]
   (let [point (make-manifold-point spec manifold)
         reps  (:coordinate-representation point)]
     (swap! reps assoc coordinate-system coordinate-rep)
     point)))

(defn manifold-point?
  "Returns true if `p` is a manifold point, false otherwise."
  [p]
  (= (v/kind p) ::manifold-point))

(defn- manifold-point-representation
  [manifold-point]
  (:spec manifold-point))

(defn point->manifold
  "Return the manifold upon which this point was defined."
  [point]
  (:manifold point))

(defn transfer-point
  "TODO docs, this is new."
  [embedded embedding]
  (fn [point]
    (assert (= (manifold embedded) (point->manifold point)))
    (assert (= (:embedding-dimension (manifold embedded))
	             (:embedding-dimension (manifold embedding))))
    (make-manifold-point
	   (manifold-point-representation point)
	   (manifold embedding))))

;; NOTE missing coordinate-reps, set-coordinate-reps!... but we only use them
;; internally here, so no stress.

(defn get-coordinates
  "Returns the representation of `manifold-point` in `coordinate-system`.

  The point contains a cache of the coordinate system -> representation mapping.
  If an entry for the given `coordinate-system` is not found, `thunk` is called
  to produce the representation, which is then installed in the cache."
  [manifold-point coordinate-system thunk]
  (let [reps (:coordinate-representation manifold-point)]
    (if-let [rep (@reps coordinate-system)]
      rep
      ;; TODO I think this can just be "simplify" now.
      (let [rep (s/mapr simplify-numerical-expression (thunk))]
        (swap! reps assoc coordinate-system rep)
        rep))))

(defn- my-manifold-point?
  "Returns true if `point` was created under the aegis of `manifold`, false
  otherwise."
  [point manifold]
  (and (manifold-point? point)
       (= (point->manifold point) manifold)))

(defn- frame?
  "True if this coordinate system is actually a frame.

  FIXME: frames aren't implemented yet."
  ;; Note: when we get around to doing so, it probably makes sense to have
  ;; frames implement ICoordinateSystem in their own way, rather than the hacky
  ;; polymorphism used in scmutils
  [coordinate-system]
  false)

;; TODO get these working with frames!

(defn chart [coordinate-system]
  #(point->coords coordinate-system %))

(defn point [coordinate-system]
  #(coords->point coordinate-system %))

(comment
  (define (typical-coords coordinate-system)
    (s:map/r generate-uninterned-symbol
	           (coordinate-system 'coordinate-prototype)))

  (define (typical-point coordinate-system)
    ((point coordinate-system)
     (typical-coords coordinate-system)))

  (define (corresponding-velocities coords)
    (s:map/r (lambda (x)
	                   (string->uninterned-symbol
	                    (string-append "v:"
			                               (symbol->string x))))
	           coords)))

(defn literal-manifold-function
  [name coordinate-system]
  (let [n (:dimension (manifold coordinate-system))
        domain (apply s/up (repeat n 0))
        range 0]
    (vary-meta
     (f/compose (af/literal-function name domain range)
                #(point->coords coordinate-system %))
     assoc
     :name name
     :coordinate-systems coordinate-system
     :type ::manifold-function)))

(defn- ->Rectangular
  ([manifold]
   (let [proto (default-coordinate-prototype manifold)]
     (->Rectangular manifold proto)))
  ([manifold coordinate-prototype]
   (reify
     ICoordinateSystem
     (check-coordinates [this coords]
       (= (s/dimension coords)
          (:dimension manifold)))

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
     (manifold [this] manifold))))

(defn- ->PolarCylindrical
  ([manifold]
   (let [proto (default-coordinate-prototype manifold)]
     (->PolarCylindrical manifold proto)))
  ([manifold coordinate-prototype]
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
                                           (= (s/dimension prep)
                                              (:embedding-dimension manifold)))
                              (u/illegal "PolarCylindrical bad point"))
                            (let [[x y] prep
                                  rsq (g/+ (g/square x) (g/square y))]
                              (when (v/zero? rsq)
                                (u/illegal-state "PolarCylindrical singular"))
                              (s/generate (count prep) ::s/up
                                          (fn [^long i]
                                            (case i
                                              0 (g/sqrt rsq)
                                              1 (g/atan y x)
                                              (nth prep i)))))))))
     (coordinate-prototype [this] coordinate-prototype)
     (with-coordinate-prototype [this prototype] (->PolarCylindrical manifold prototype))
     (manifold [this] manifold))))

(defn ^:private ->S2-coordinates
  "Colatitude-longitude coordinates for the surface of the sphere
  S(2). The orientation map (on vectors) can be used to reposition the
  polar coordinate singularities."
  [orientation]
  (let [inverse-orientation (g/invert orientation)]
    (fn ctor
      ([manifold]
       (let [proto (default-coordinate-prototype manifold)]
         (ctor manifold proto)))
      ([manifold coordinate-prototype]
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
                                               (= (s/dimension prep)
                                                  (:embedding-dimension manifold)))
                                  (u/illegal "S2-coordinates bad point"))
                                (let [[x y z] prep]
                                  (s/up (g/acos z) (g/atan y x)))))))
         (coordinate-prototype [this] coordinate-prototype)
         (with-coordinate-prototype [this prototype] (ctor manifold prototype))
         (manifold [this] manifold))))))

(defn ^:private ->SphericalCylindrical
  ([manifold]
   (let [proto (default-coordinate-prototype manifold)]
     (->SphericalCylindrical manifold proto)))
  ([manifold coordinate-prototype]
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
                                           (= (s/dimension prep)
                                              (:embedding-dimension manifold)))
                              (u/illegal "SphericalCylindrical bad point"))
                            (let [[x y z] prep
                                  r (g/sqrt (g/+ (g/square x) (g/square y) (g/square z)))]
                              (when (v/zero? r)
                                (u/illegal-state "SphericalCylindrical singular"))
                              (s/generate (s/dimension prep) ::s/up
                                          (fn [^long i]
                                            (case i
                                              0 r
                                              1 (g/acos (g/divide z r))
                                              2 (g/atan y x)
                                              (nth prep i)))))))))
     (coordinate-prototype [this] coordinate-prototype)
     (with-coordinate-prototype [this prototype] (->SphericalCylindrical manifold prototype))
     (manifold [this] manifold))))

(defn ^:private ->Stereographic
  "Stereographic projection from the final coordinate. The default pole is (0 0 ... 1),
  but this can be moved by the orthogonal (n+1) by (n+1) matrix
  returned by orientation function."
  [orientation-function]
  (fn ctor
    ([manifold]
     (let [proto (default-coordinate-prototype manifold)]
       (ctor manifold proto)))
    ([manifold coordinate-prototype]
     (let [n        (:dimension manifold)
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
                                  (u/illegal-state "S^n stereographic singular"))
                                (let [coords (s/generate n ::s/up #(g/divide (nth pt %) (g/- 1 (nth pt n))))]
                                  (if (= n 1) (first coords) coords))))))
         (coordinate-prototype [this] coordinate-prototype)
         (with-coordinate-prototype [this prototype] (ctor manifold prototype))
         (manifold [this] manifold))))))

(defn ^:private ->Euler-chart
  "Euler angles for SO(3)."
  ([manifold]
   (let [proto (default-coordinate-prototype manifold)]
     (->Euler-chart manifold proto)))
  ([manifold coordinate-prototype]
   (let [n (:dimension manifold)]
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
                                  theta (g/acos (get-in M [2 2]))
                                  phi (g/atan (get-in M [0 2])
                                              (g/negate (get-in M [1 2])))
                                  psi (g/atan (get-in M [2 0])
                                              (get-in M [2 1]))]
                              (s/up theta phi psi)))))
       (coordinate-prototype [this] coordinate-prototype)
       (with-coordinate-prototype [this prototype] (->Euler-chart manifold prototype))
       (manifold [this] manifold)))))

(defn- ->Alternate-chart
  "Alternate angles for SO(3)."
  ([manifold]
   (let [proto (default-coordinate-prototype manifold)]
     (->Alternate-chart manifold proto)))
  ([manifold coordinate-prototype]
   (let [n (:dimension manifold)]
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
                                  theta (g/asin (get-in M [2 1]))
                                  phi (g/atan (g/negate (get-in M [0 1]))
                                              (get-in M [1 1]))
                                  psi (g/atan (g/negate (get-in M [2 0]))
                                              (get-in M [2 2]))]
                              (s/up theta phi psi)))))
       (coordinate-prototype [this] coordinate-prototype)
       (with-coordinate-prototype [this prototype] (->Alternate-chart manifold prototype))
       (manifold [this] manifold)))))

;; ## Manifolds

(def Rn
  (-> "R(%d)"
      (make-manifold-family)
      (attach-patch :origin)
      (attach-coordinate-system :rectangular :origin ->Rectangular)
      (attach-coordinate-system :polar-cylindrical :origin ->PolarCylindrical)))

;; NOTE spherical-cylindrical missing, attached at origin. same?
;; NOTE spacetime-spherical missing

(def R1 (make-manifold Rn 1))
(def R1-rect (coordinate-system-at :rectangular :origin R1))
(def the-real-line R1-rect)

(def R2 (make-manifold Rn 2))
(def R2-rect (coordinate-system-at :rectangular :origin R2))
(def R2-polar (coordinate-system-at :polar-cylindrical :origin R2))

(def R3 (make-manifold Rn 3))
(def R3-rect (coordinate-system-at :rectangular :origin R3))
(def R3-cyl (coordinate-system-at :polar-cylindrical :origin R3))

(comment
  (def R3-spherical (coordinate-system-at :spherical-cylindrical :origin R3)))

(comment
  (def R4 (make-manifold Rn 4))
  (def R4-rect (coordinate-system-at :rectangular :origin R4))
  (def R4-cyl (coordinate-system-at :polar-cylindrical :origin R4)))

(comment
  (def spacetime (make-manifold Rn 4))
  (def spacetime-rect
    (coordinate-system-at :rectangular :origin spacetime))
  (def spacetime-sphere
    (coordinate-system-at :spacetime-spherical :origin spacetime)))

;; The surface of a sphere, specialized to two dimensions.
;;
;; TODO continue later, up to line 795

(def S2-type
  (-> "S2"
      make-manifold-family
      (attach-patch :north-pole)
      (attach-patch :south-pole)
      (attach-patch :tilted)
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
(def S2-tilted (coordinate-system-at :spherical :tilted S2))
(def S2-stereographic (coordinate-system-at :stereographic :north-pole S2))

(def S2-Riemann S2-stereographic)

;; TODO double-check these: what goes in S2-type, and what goes in Sn(2)?

(def Sn
  (-> "S(%d)"
      make-manifold-family
      (attach-patch :north-pole)
      (attach-coordinate-system :spherical :north-pole ->SphericalCylindrical)
      (attach-coordinate-system :stereographic :north-pole (->Stereographic matrix/I))))

(def SO3-type
  (-> "SO3"
      make-manifold-family
      (attach-patch :Euler-patch)
      (attach-patch :alternate-patch)
      (attach-coordinate-system :Euler :Euler-patch ->Euler-chart)
      (attach-coordinate-system :alternate :alternate-patch ->Alternate-chart)))

(def SO3
  (make-manifold SO3-type 3))

(def Euler-angles
  (coordinate-system-at :Euler :Euler-patch SO3))

(def alternate-angles
  (coordinate-system-at :alternate :alternate-patch SO3))
