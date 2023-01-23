#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.calculus.manifold
  "This namespace defines a functional API for:

  - differentiable manifolds (both manifold families like [[Rn]] and manifolds
    specialized to a concrete dimension)
  - manifold points
  - coordinate patches

  As well as a whole bunch of defined manifolds and coordinate systems for
  exploration and fun!"
  (:refer-clojure :exclude [uuid])
  (:require #?(:cljs [goog.string :refer [format]])
            [emmy.abstract.function :as af]
            [emmy.abstract.number :refer [simplify-numerical-expression]]
            [emmy.calculus.frame :as cf]
            [emmy.function :as f]
            [emmy.generic :as g]
            [emmy.matrix :as matrix]
            [emmy.mechanics.rotation
             :refer [rotate-x-matrix rotate-y-matrix rotate-z-matrix]]
            [emmy.structure :as s]
            [emmy.util :as u]
            [emmy.value :as v]
            [taoensso.timbre :as log]))

;; # Disclaimer (from @sritchie)
;;
;; I'm convinced that the scmutils code used to implement the ideas
;; in "Functional Differential Geometry" doesn't have the final say on the best
;; API for differential geometry. I'm going to leave notes throughout the
;; namespace suggesting ways that we might make it better; please take these as
;; challenges and erase the notes as you make improvements!
;;
;; Big TODO items:
;;
;; - `manifold` and `patch` should be protocols, so that manifolds, patches and
;;   coordinate systems can report their manifold, and patches and coordinate
;;   systems can report their patch. `point` can report its manifold too. Once
;;   this change is made, `transfer-point` should use the `manifold` protocol to
;;   simplify its implementation. (NOTE that `manifold` now works on coordsys
;;   and manifolds, but not yet on patches.)
;;
;; - `patch`, `manifold` and `point` should be defrecords, so that they can
;;   implement the protocols above in different ways. We can also implement
;;   `dimension` correctly.
;;
;; - `make-patch` should return a patch template; only when you specialize the
;;   manifold, or retrieve the patch FROM the specialized manifold with
;;   `get-patch` should you actually build the defrecord.
;;
;; - the original codebase assigns a UUID to each manifold specialized off a
;;   family. Is this a good idea?
;;
;; - coordinate systems now use a UUID in the protocol; this feels like a code
;;   smell. This exists so that coordinate prototypes can live in metadata, and
;;   can be changed without affecting equality of coordinate systems. BUT
;;   reconsider this design!
;;
;; - coordinate systems have many more functions like `access-chains`,
;;  `dual-chains`, `coordinate-basis` and friends. These are missing, and SHOULD
;;  go into `coordinate.cljc.` Basically caching functions that can do these
;;  transformations for a coordinate system should live in one spot.
;;
;; - it feels like `ICoordinateSystem` WANTS to live in `coordinate.cljc`... but
;;   maybe not. I have a sense that we're a bit tangled.
;;
;; - keeping caches inside the points feels wrong. Can we just cache the
;;   function itself?
;;
;; - There is a huge amount of repetition between the different coordinate
;;   system definitions! There is clearly a smaller protocol that would work,
;;   like `point->coords` and `coords->point`, plus maybe the validator.
;;   `my-manifold-point?` does all the work for point checking, and we can
;;   compose with a coordinate system defrecord to return `manifold`, `patch`
;;   and `prototype.`
;;
;;  If we do this it'll make it easier to memoize just these smaller functions,
;;  and keep an outer `point->coords` that can memoize the internal thing.
;;
;; - TODO: document all of the coordinate systems at the bottom of the page!
;;
;; - TODO: more tests of all of the new accessor functions. See Codecov for what
;; - to do here.
;;
;; Okay, on to the business.
;;
;;
;; ## Manifold Families
;;
;; Manifolds like R1, R2, S1, S2 etc are specialized versions of "manifold
;; templates" like Rn and Sn. We call these "manifold families", while scmutils
;; calls these "manifold types".
;;
;; NOTE: rather than taking a `name-format` string, `make-manifold-family`
;; should take a function of the single dimension argument.
;;
;; NOTE: the original scmutils codebase keeps a `:distinguished-points` list
;; inside the manifold family. This isn't used anywhere in the book or codebase.
;; Keep an eye out and either implement or delete this note.

(defn make-manifold-family
  "Generates a manifold family (a template for building manifolds) from the
  supplied `name-format`.

  Generated manifolds locally resemble Euclidean space (Rn) by default. You can
  optionally pass `'Complex` or `'Quaternion` to `over` to customize the field
  of the vector space that the manifold locally resembles at each point.

  NOTE: only `'Real` does anything as of 3.15.2021."
  ([name-format]
   (make-manifold-family name-format 'Real))
  ([name-format over]
   {:pre [(contains? #{'Real 'Complex 'Quaternion} over)]}
   {:over over
    :name-format name-format
    :patch-templates {}
    :type ::manifold-family}))

(defn manifold-family?
  "Returns `true` if `m` is a dictionary representing a manifold family, false
  otherwise."
  [m]
  (= (v/kind m) ::manifold-family))

(defn make-manifold
  "Returns a concrete manifold generated by specializing the supplied manifold
  `family` into a concrete manifold of dimension `n`. `n` must be a positive
  integer.

  Optionally takes an `embedding-dimension`; this must be >= the value of `n`.
  Use this in cases like an n-sphere embedded in a euclidean space of dimension
  n+1.

  A [manifold](https://en.wikipedia.org/wiki/Manifold) is a topological space
  that locally resembles Euclidean space near each point."
  ([family n]
   (make-manifold family n n))
  ([family n embedding-dimension]
   {:pre [(integer? n)
          (> n 0)
          (>= embedding-dimension n)]}
   {:family family
    :name (format (:name-format family) n)
    :dimension n
    :embedding-dimension embedding-dimension
    :type ::manifold}))

(defn manifold?
  "Returns `true` if `m` is a dictionary representing a manifold, false
  otherwise."
  [m]
  (= (v/kind m) ::manifold))

(defn manifold-type
  "The supplied manifold `m` locally resembles some vector space; this function
  returns the field over which that vector space was specified."
  [manifold]
  (get-in manifold [:family :over]))

(defn manifold
  "If `m` is a manifold, acts as identity. Else, if given some structure
  associated with a manifold (like a coordinate system), returns the associated
  manifold."
  [m]
  (if (manifold? m)
    m
    (::manifold (meta m))))

;; ## Coordinate Patches

(defn- make-patch
  "Returns a bare `patch` with no manifold attached."
  [name]
  {:name name
   :coordinate-systems {}})

(defn attach-patch
  "Takes a manifold `family` and attaches a patch template with the supplied
  `patch-name`. Returns a new manifold family.

  All manifolds generated from the returned family will have this coordinate
  patch attached."
  [family patch-name]
  (let [patch (make-patch patch-name)]
    (assoc-in family [:patch-templates patch-name] patch)))

(defn patch-names
  "Returns a set of patch names registered in the supplied manifold."
  [manifold]
  (u/keyset
   (get-in manifold [:family :patch-templates])))

(defn get-patch
  "Returns the patch named by `patch-name` within the supplied `manifold` if
  registered. Throws otherwise.

  NOTE that the returned patch will keep a reference to the supplied `manifold`
  under a `:manifold` key.

  A coordinate patch is a simply-connected open set around a point in the
  manifold. A manifold might have many patches. Coordinate systems are defined
  on patches; these allow the parameterization of any point on the patch in
  terms of a tuple of real numbers (the coordinates)."
  [manifold patch-name]
  (if-let [gen (get-in manifold [:family :patch-templates patch-name])]
    (assoc gen :manifold manifold)
    (throw
     (ex-info "Unknown patch."
              {:patch-name patch-name
               :manifold manifold}))))

;; ## Coordinate Systems
;;
;; Coordinate systems are added to coordinate patches. A coordinate system is an
;; invertible map from the space to R^n (or C^n or H^n, depending on the field
;; over which the manifold's defined!)

(defn attach-coordinate-system
  "Returns a new manifold family generated by attaching the supplied coordinate
  system constructor to `family`, indexed by the supplied patch and coordinate
  system names."
  [family coordinate-system-name patch-name coordinate-system-ctor]
  (let [ks [:patch-templates patch-name
            :coordinate-systems coordinate-system-name]
        v  coordinate-system-ctor]
    (assoc-in family ks v)))

(defn coordinate-system-names
  "Returns a set of names of all coordinate system constructors registered in the
  supplied patch."
  [patch]
  (u/keyset
   (:coordinate-systems patch)))

(defn- get-coordinate-system
  "If a coordinate system constructor registered at `system-name` exists in the
  supplied `patch`, return it. Else, error.

  NOTE for FDG-goers: This is called `coordinate-system` in scmutils."
  [patch system-name]
  (or (get-in patch [:coordinate-systems system-name])
      (throw
       (ex-info "Unknown coordinate system."
                {:coordinate-system-name system-name
                 :patch patch}))))

(defn coordinate-system-at
  "Returns an [[ICoordinateSystem]] instance specialized to the patch named
  `patch-name` on `manifold`."
  [manifold coordinate-system-name patch-name]
  (let [patch (get-patch manifold patch-name)
        ctor  (get-coordinate-system patch coordinate-system-name)]
    (ctor manifold)))

;; ## Manifold Points
;;
;; This section defines constructors and accessors for
;; non-coordinate-constrained points on some manifold.

(declare uuid)

(defn- make-manifold-point
  "Returns a point in `manifold` specified by its Euclidean coordinates `spec`.

  Mathematically, a point is defined in the manifold in a coordinate-free way.
  To compute with the point, you'll need to get it into a coordinate
  representation using `((chart coord-system) point)`.

  Optionally, you can pass a `coordinate-system` and a
  representation (`coordinate-rep`) of the point in that coordinate system. The
  returned point keeps a mutable cache of its coordinate representations, keyed
  by `:coordinate-representations`; passing these values will seed the cache."
  ([spec manifold]
   {:type ::manifold-point
    :spec spec
    :manifold manifold
    :coordinate-representations (atom {})})
  ([spec manifold coordinate-system coordinate-rep]
   (let [point (make-manifold-point spec manifold)
         reps  (:coordinate-representations point)]
     (swap! reps assoc (uuid coordinate-system) coordinate-rep)
     point)))

(defn manifold-point-representation
  "Returns the backing Euclidean space representation of the supplied manifold
  point."
  [point]
  (:spec point))

(defn point->manifold
  "Return the manifold upon which this `point` was defined."
  [point]
  (:manifold point))

(defn manifold-point?
  "Returns true if `p` is a manifold point, false otherwise."
  [p]
  (= (v/kind p) ::manifold-point))

(defn- my-manifold-point?
  "Returns true if `point` was created under the aegis of `manifold`, false
  otherwise."
  [point manifold]
  (and (manifold-point? point)
       (= (point->manifold point)
          manifold)))

(defn get-coordinates
  "Returns the representation of `manifold-point` in `coordinate-system`.

  If an entry for the given `coordinate-system` is not found, `thunk` is called
  to produce the representation. The representation is cached in the point."
  [manifold-point coordinate-system thunk]
  (let [reps (:coordinate-representations manifold-point)
        coordsys-id (uuid coordinate-system)]
    (or (@reps coordsys-id)
        (let [rep (s/mapr simplify-numerical-expression (thunk))]
          (swap! reps assoc coordsys-id rep)
          rep))))

;; ## Coordinate System Protocol

(defprotocol ICoordinateSystem
  (check-coordinates [this coords]
    "Returns true if the supplied coordinates `coords` can be converted into a
    point by this [[ICoordinateSystem]], false otherwise.")

  (check-point [this point]
    "Returns true if the supplied `point` can be converted into coordinates by
    this [[ICoordinateSystem]], false otherwise.")

  (coords->point [this coords]
    "Returns the manifold point on this [[ICoordinateSystem]]'s manifold
    corresponding to the supplied `coords`." )

  (point->coords [this point]
    "Returns a coordinate representation of the supplied manifold point `point`,
    as specified by this [[ICoordinateSystem]].")

  (uuid [this]
    "Returns a unique identifier for this instance of [[ICoordinateSystem]].

    (This is an internal implementation detail to allow us to attach coordinate
    prototypes and other items as metadata to an [[ICoordinateSystem]] without
    affecting equality.)"))

(defn coordinate-system?
  "Returns true if `x` implements [[ICoordinateSystem]], false otherwise."
  [x]
  (satisfies? ICoordinateSystem x))

(defn coordinate-prototype
  "Returns the symbolic coordinate prototype associated with `coordsys`. This is
   a structure of the correct dimension for this coordinate system, with all
   symbolic entries.

  Returns nil for non-valid inputs."
  [coordsys]
  (::coord-prototype (meta coordsys)))

(defn with-coordinate-prototype
  "Returns an identical `coordsys` with the new `coordinate-prototype` installed."
  [coordsys prototype]
  (let [current-proto (coordinate-prototype coordsys)]
    (if (= current-proto prototype)
      coordsys
      (vary-meta coordsys assoc ::coord-prototype prototype))))

(defn chart
  "Given an [[ICoordinateSystem]], returns a function from a point on the
  coordinate system's manifold to the coordinate representation specified by the
  supplied `coordinate-system`."
  [coordinate-system]
  (if (cf/frame? coordinate-system)
    (fn [event]
      (cf/event->coords coordinate-system event))
    (fn [point]
      (point->coords coordinate-system point))))

(defn point
  "Given an [[ICoordinateSystem]], returns a function from coordinates in
  `coordinate-system`'s repesentation to the matching point on the manifold
  associated with `coordinate-system`."
  [coordinate-system]
  (if (cf/frame? coordinate-system)
    (fn [coords]
      (cf/coords->event coordinate-system coords))
    (fn [coords]
      (coords->point coordinate-system coords))))

(defn typical-coords
  "Given an [[ICoordinateSystem]], returns a structure that matches
  the [[coordinate-prototype]] of `coordinate-system`, with all unique,
  gensym-ed entries.

  Use [[typical-coords]] if you require a unique symbolic coordinate
  representation compatible with `coordinate-system`.

  See [[typical-point]] for a coordinate-free version of this function."
  [coordinate-system]
  (s/mapr gensym (coordinate-prototype coordinate-system)))

(defn typical-point
  "Given an [[ICoordinateSystem]], returns a unique, symbolically-represented
  point on the manifold associated with `coordinate-system`.

  See [[typical-coords]] for a coordinate-based version of this function."
  [coordinate-system]
  (let [coords (typical-coords coordinate-system)]
    (coords->point coordinate-system coords)))

(defn transfer-point
  "Returns a function that takes a single manifold `point` embedded in the
  manifold `embedded` and transfers the point to the supplied `embedding`
  manifold.

  The embedding dimension must be the same for both manifolds.

  NOTE that `embedded` and `embedding` can be either manifolds, or instances
  of [[ICoordinateSystem]]. In the latter case `embedded` and `embedding` will
  bind to the manifold associated with the supplied [[ICoordinateSystem]]."
  [embedded embedding]
  (let [embedded-m  (manifold embedded)
        embedding-m (manifold embedding)]
    (assert (= (:embedding-dimension embedded-m)
               (:embedding-dimension embedding-m)))
    (fn [point]
      (assert (= embedded-m (point->manifold point)))
      (make-manifold-point
       (manifold-point-representation point)
       embedding-m))))

(defn corresponding-velocities
  "Takes a coordinate representation `coords` of a manifold point with all
  symbolic entries, and returns a structure of the same shape with `v:`
  prepended to all symbols.

  This structure is appropriate for representing the velocities associated with
  each coordinate."
  [coords]
  (s/mapr (fn [x]
            (symbol (str "v:" x)))
          coords))

(defn literal-manifold-function
  "Given a symbolic name `sym` and an [[ICoordinateSystem]], returns a literal
  function that maps coordinate-free manifold points to a scalar output.

  Also aliased as [[literal-manifold-function]]."
  [sym coordinate-system]
  (let [n (:dimension (manifold coordinate-system))
        domain (s/up* (repeat n 0))
        range  0]
    (vary-meta
     (f/compose (af/literal-function sym domain range)
                (chart coordinate-system))
     assoc
     :name name
     :coordinate-system coordinate-system
     :type ::manifold-function)))

(def ^{:doc "Alias for [[literal-manifold-function]], present for scmutils
codebase compatibility."}
  literal-scalar-field
  literal-manifold-function)

(defn zero-manifold-function
  "Manifold function that maps every input manifold `point` to the scalar value 0."
  [point]
  {:pre [(manifold-point? point)]}
  0)

(defn one-manifold-function
  "Manifold function that maps every input manifold `point` to the scalar value 1."
  [point]
  {:pre [(manifold-point? point)]}
  1)

(defn constant-manifold-function
  "Takes some constant `c` and returns a manifold function that maps every input
  manifold `point` to `c.`"
  [c]
  (fn [point]
    {:pre [(manifold-point? point)]}
    c))

;; ## Explicit Coordinate Systems
;;
;; This section defines many instances of [[ICoordinateSystem]].

(defn c:generate
  "Generates a coordinate structure of the supplied dimension `n`, and
  `orientation` using the supplied function `f` for entries. See the very
  similar [[emmy.structure/generate]] for more details.

  NOTE from GJS: this is a kludge introduced only to allow a coordinate of
  dimension 1 to automatically unwrap itself."
  [n orientation f]
  (if (= n 1)
    (f 0)
    (s/generate n orientation f)))

(defn- default-coordinate-prototype
  "Takes a `manifold` and returns a [[emmy.structure/up]] instance of the
  same dimension as `manifold`, with symbolic entries in each position. "
  [manifold]
  (let [k (:dimension manifold)]
    (c:generate
     k ::s/up (fn [i] (symbol (str "x" i))))))

(defn- ->Rectangular
  "Returns an [[ICoordinateSystem]] instance that converts between `manifold`
  points in Rn (where `n` is the dimension of `manifold`) to an explicit Rn
  structure.

  This is as close to an identity coordinate transformation as the system gets!"
  ([manifold]
   (let [proto (default-coordinate-prototype manifold)]
     (->Rectangular manifold proto)))
  ([manifold coordinate-prototype]
   (let [id (u/uuid)]
     (-> (reify ICoordinateSystem
           (check-coordinates [_ coords]
             (= (s/dimension coords)
                (:dimension manifold)))

           (check-point [_ point]
             (my-manifold-point? point manifold))

           (coords->point [this coords]
             (assert (check-coordinates this coords))
             (make-manifold-point coords manifold this coords))

           (point->coords [this point]
             (assert (check-point this point))
             (get-coordinates
              point this
              (fn []
                (let [rep (manifold-point-representation point)]
                  (assert (= (s/dimension rep)
                             (:embedding-dimension manifold)))
                  rep))))

           (uuid [_] id))
         (with-meta {::coord-prototype coordinate-prototype
                     ::manifold manifold})))))

(defn- ->PolarCylindrical
  "Returns an [[ICoordinateSystem]] instance that converts between `manifold`
  points in Rn (where `n` is the dimension of `manifold`) to [cylindrical
  coordinates](https://en.wikipedia.org/wiki/Cylindrical_coordinate_system).

  The first two Rn coordinates in the manifold point become `r` and `theta`, and
  all other points are untouched."
  ([manifold]
   (let [proto (default-coordinate-prototype manifold)]
     (->PolarCylindrical manifold proto)))
  ([manifold coordinate-prototype]
   (let [id (u/uuid)]
     (-> (reify ICoordinateSystem
           (check-coordinates [_ coords]
             (and (s/up? coords)
                  (= (s/dimension coords)
                     (:dimension manifold))
                  (> (s/dimension coords) 1)
                  (let [c0 (nth coords 0)]
                    (or (not (v/number? c0))
                        (>= c0 0)))))

           (check-point [_ point]
             (my-manifold-point? point manifold))

           (coords->point [this coords]
             (assert (check-coordinates this coords))
             (let [[r theta] coords]
               (-> coords
                   (assoc 0 (g/* r (g/cos theta)))
                   (assoc 1 (g/* r (g/sin theta)))
                   (make-manifold-point manifold this coords))))

           (point->coords [this point]
             (assert (check-point this point))
             (get-coordinates
              point this
              (fn []
                (let [rep (manifold-point-representation point)]
                  (when-not (and (s/up? rep)
                                 (= (s/dimension rep)
                                    (:embedding-dimension manifold)))
                    (u/illegal "PolarCylindrical bad point"))
                  (let [[x y] rep
                        rsq (g/+ (g/square x)
                                 (g/square y))]
                    (when (v/zero? rsq)
                      (u/illegal-state "PolarCylindrical singular"))
                    (-> rep
                        (assoc 0 (g/sqrt rsq))
                        (assoc 1 (g/atan y x))))))))

           (uuid [_] id))
         (with-meta {::coord-prototype coordinate-prototype
                     ::manifold manifold})))))

(defn- ->SphericalCylindrical
  "Returns an [[ICoordinateSystem]] instance that converts between `manifold`
  points in Rn (where `n` is the dimension of `manifold`) to
  generalized [spherical
  coordinates](https://en.wikipedia.org/wiki/Spherical_coordinate_system).

  The first three Rn coordinates in the manifold point become `r` and `theta`,
  `phi` (radius, colatitude and longitude) and all other points are untouched.
  This last bit allows us to use spherical coordinates for manifolds with higher
  than three dimensions."
  ([manifold]
   (let [proto (default-coordinate-prototype manifold)]
     (->SphericalCylindrical manifold proto)))
  ([manifold coordinate-prototype]
   (let [id (u/uuid)]
     (-> (reify ICoordinateSystem
           (check-coordinates [_ coords]
             (and (s/up? coords)
                  (= (g/dimension coords)
                     (:dimension manifold))
                  (or (not (v/number? coords))
                      (>= (nth coords 0) 0))))

           (check-point [_ point]
             (my-manifold-point? point manifold))

           (coords->point [this coords]
             (assert (check-coordinates this coords))
             (let [[r theta phi] coords]
               (-> coords
                   (assoc 0 (g/* r (g/sin theta) (g/cos phi)))
                   (assoc 1 (g/* r (g/sin theta) (g/sin phi)))
                   (assoc 2 (g/* r (g/cos theta)))
                   (make-manifold-point manifold this coords))))

           (point->coords [this point]
             (assert (check-point this point))
             (get-coordinates
              point this
              (fn []
                (let [rep (manifold-point-representation point)]
                  (when-not (and (s/up? rep)
                                 (= (g/dimension rep)
                                    (:embedding-dimension manifold)))
                    (u/illegal "SphericalCylindrical bad point"))
                  (let [[x y z] rep
                        r (g/sqrt
                           (g/+ (g/square x)
                                (g/square y)
                                (g/square z)))]
                    (when (v/zero? r)
                      (u/illegal-state "SphericalCylindrical singular"))
                    (-> rep
                        (assoc 0 r)
                        (assoc 1 (g/acos (g/divide z r)))
                        (assoc 2 (g/atan y x))))))))

           (uuid [_] id))
         (with-meta {::coord-prototype coordinate-prototype
                     ::manifold manifold})))))

(defn- ->SpacetimeSpherical
  "Returns an [[ICoordinateSystem]] instance that converts between `manifold`
  points in R4 to 'spacetime spherical coordinates'. The first coordinate is
  time, and the remaining three coordinates are [spherical spatial
  coordinates](https://en.wikipedia.org/wiki/Spherical_coordinate_system).

  The spatial coordinates are `r` and `theta`, `phi` (radius, colatitude and
  longitude)."
  ([manifold]
   (let [proto (default-coordinate-prototype manifold)]
     (->SpacetimeSpherical manifold proto)))
  ([manifold coordinate-prototype]
   (let [id (u/uuid)]
     (-> (reify ICoordinateSystem
           (check-coordinates [_ coords]
             (and (s/up? coords)
                  (= (g/dimension coords) 4)))

           (check-point [_ point]
             (my-manifold-point? point manifold))

           (coords->point [this coords]
             (assert (check-coordinates this coords))
             (let [[t r theta phi] coords]
               (make-manifold-point
                (s/up t
                      (g/* r (g/sin theta) (g/cos phi))
                      (g/* r (g/sin theta) (g/sin phi))
                      (g/* r (g/cos theta)))
                manifold this coords)))

           (point->coords [this point]
             (assert (check-point this point))
             (get-coordinates
              point this
              (fn []
                (let [rep (manifold-point-representation point)]
                  (if-not (check-coordinates this rep)
                    (throw
                     (ex-info "bad ->SpacetimeSpherical point: "
                              {:point point
                               :coordinate-system this}))
                    (let [[t x y z] rep
                          r (g/sqrt
                             (g/+ (g/square x)
                                  (g/square y)
                                  (g/square z)))]
                      (when (and (v/number? r)
                                 (v/zero? r))
                        (throw
                         (ex-info "->SpacetimeSpherical singular: "
                                  {:point point
                                   :coordinate-system this})))
                      (s/up t
                            r
                            (g/acos (g// z r))
                            (g/atan y x))))))))

           (uuid [_] id))
         (with-meta {::coord-prototype coordinate-prototype
                     ::manifold manifold})))))

(defn- ->S2-coordinates
  "Returns an [[ICoordinateSystem]] instance that converts between `manifold`
  points and colatitude-longitude coordinates for the surface of the sphere
  S(2).

  Also accepts a unitary `orientation` matrix (2-tensor, technically, a down of
  ups, dimension 3 each, since S(2) is embedded in 3-space) used to reposition
  the north pole of the spherical coordinate system.

  See [n-sphere: Spherical
  Coordinates](https://en.wikipedia.org/wiki/N-sphere#Spherical_coordinates) for
  notes about the generalized versions of these coordinates, for S(n)."
  ([orientation]
   (let [inverse-orientation (g/invert orientation)]
     (fn ctor
       ([manifold]
        (let [proto (default-coordinate-prototype manifold)]
          (ctor manifold proto)))
       ([manifold coordinate-prototype]
        (let [id (u/uuid)]
          (-> (reify ICoordinateSystem
                (check-coordinates [_ coords]
                  (and (s/up? coords)
                       (= (g/dimension coords) 2)
                       (or (not (v/number? coords))
                           (>= (nth coords 0) 0))))

                (check-point [_ point]
                  (my-manifold-point? point manifold))

                (coords->point [this coords]
                  (assert (check-coordinates this coords))
                  (let [[colatitude longitude] coords]
                    (make-manifold-point
                     (g/* orientation
                          (s/up (g/* (g/sin colatitude) (g/cos longitude))
                                (g/* (g/sin colatitude) (g/sin longitude))
                                (g/cos colatitude)))
                     manifold this coords)))

                (point->coords [this point]
                  (assert (check-point this point))
                  (get-coordinates
                   point this
                   (fn []
                     (let [rep (g/* inverse-orientation
                                    (manifold-point-representation point))]
                       (if (and (s/up? rep)
                                (= (g/dimension rep)
                                   (:embedding-dimension manifold)))
                         (let [[x y z] rep]
                           (s/up (g/acos z) (g/atan y x)))
                         (u/illegal "S2-coordinates bad point"))))))

                (uuid [_] id))
              (with-meta {::coord-prototype coordinate-prototype
                          ::manifold manifold}))))))))

(defn- ->Sn-coordinates
  "Returns an [[ICoordinateSystem]] instance that converts between `manifold`
  points and spherical on the
  unit [n-sphere](https://en.wikipedia.org/wiki/N-sphere#Spherical_coordinates),
  ie, S(n). The sphere is embedded in a space of dimension n+1.

  Also accepts an `orientation-function` from dimension `(+ n 1)` to a unitary
  `orientation` matrix (2-tensor, technically, a down of ups, dimension `n+1`
  each, since S(n) is embedded in n+1 dimensional-space). This 2-tensor is used
  to reposition the north pole of the spherical coordinate system.

  See [n-sphere: Spherical
  Coordinates](https://en.wikipedia.org/wiki/N-sphere#Spherical_coordinates) for
  notes about these coordinates."
  [orientation-function]
  (letfn [(rotate-left [l]
            (lazy-cat (rest l) [(first l)]))]
    (fn ctor
      ([manifold]
       (let [proto (default-coordinate-prototype manifold)]
         (ctor manifold proto)))
      ([manifold coordinate-prototype]
       (let [n (:dimension manifold)
             orientation-matrix (orientation-function (+ n 1))
             orientation-inverse-matrix (g/invert orientation-matrix)
             id (u/uuid)]
         (-> (reify ICoordinateSystem
               (check-coordinates [_ coords]
                 (let [dim (g/dimension coords)]
                   (or (and (= n 1)
                            (= dim 1))
                       (and (s/up? coords)
                            (= dim n)
                            ;; check that every coordinate but the final one is
                            ;; positive, if it's a number.
                            (every? (map-indexed
                                     (fn [i coord]
                                       (or (= (inc i) n)
                                           (not (v/number? coord))
                                           (not (g/negative? coord)))))
                                    coords)))))

               (check-point [_ point]
                 (my-manifold-point? point manifold))

               (coords->point [this coords]
                 (assert (check-coordinates this coords))
                 (if (= n 1)
                   (let [pt (s/up (g/cos coords)
                                  (g/sin coords))]
                     (make-manifold-point
                      (g/* orientation-matrix pt)
                      manifold this coords))
                   (let [sines (map g/sin coords)
                         cosines (map g/cos coords)
                         pt (s/up*
                             (rotate-left
                              (map (fn [i]
                                     (if (= i n)
                                       (apply g/* sines)
                                       (apply g/* (cons (nth cosines i)
                                                        (take i sines)))))
                                   (range (inc n)))))]

                     (make-manifold-point
                      (g/* orientation-matrix pt)
                      manifold this coords))))

               (point->coords [this point]
                 (assert (check-point this point))
                 (get-coordinates
                  point this
                  (fn []
                    (letfn [(safe-atan [y x]
                              (when (and (number? y) (number? x)
                                         (v/zero? y) (v/zero? x))
                                (log/warn "Sn-coordinates singular!"))
                              (g/atan y x))]
                      (let [pt (rotate-left
                                (reverse
                                 (g/* orientation-inverse-matrix
                                      (manifold-point-representation point))))]
                        (if (= n 1)
                          (safe-atan (nth pt 1) (nth pt 0))
                          (loop [r    (first pt)
                                 more (rest pt)
                                 ans  [(safe-atan (first pt) (second pt))]]
                            ;; There is almost certainly a more efficient way to do
                            ;; this. Study the transformation here, and see how many
                            ;; times we're taking square roots and then squaring again.
                            ;; https://en.wikipedia.org/wiki/N-sphere#Spherical_coordinates
                            (if-not (next more)
                              (s/up* ans)
                              (let [r' (g/sqrt (g/+ (g/square (first more))
                                                    (g/square r)))]
                                (recur r'
                                       (rest more)
                                       (cons (safe-atan r' (second more))
                                             ans)))))))))))

               (uuid [_] id))
             (with-meta {::coord-prototype coordinate-prototype
                         ::manifold manifold})))))))

(defn- ->Sn-stereographic
  "Returns an [[ICoordinateSystem]] instance that converts between `manifold`
  points and a [stereographic
  projection]((https://en.wikipedia.org/wiki/N-sphere#Stereographic_projection))
  of the
  unit [n-sphere](https://en.wikipedia.org/wiki/N-sphere#Stereographic_projection)
  S(n) from the final coordinate.

  Also accepts an `orientation-function` from dimension `(+ n 1)` to a unitary
  `orientation` matrix (2-tensor, technically, a down of ups, dimension `n+1`
  each, since S(n) is embedded in n+1 dimensional-space). This 2-tensor is used
  to reposition the north pole of the spherical coordinate system.

  Notes from scmutils:

  The `orientation-function` should return an orthogonal (n+1)-by-(n+1) matrix.
  It can be interpreted as moving the pole / plane of projection and possibly
  reflecting.

  The default pole is (0 0 ... 1).
  We fire a ray through m = (m_0 ... m_n)

  x(t) = p + t(m - p)
  x(0) = p, x(1) = m
  x_n(t) = 1-t(1+m_n), 0 = x_n(1/(1+m_n))"
  [orientation-function]
  (fn ctor
    ([manifold]
     (let [proto (default-coordinate-prototype manifold)]
       (ctor manifold proto)))
    ([manifold coordinate-prototype]
     (let [n (:dimension manifold)
           orientation-matrix (orientation-function (+ n 1))
           orientation-inverse-matrix (g/invert orientation-matrix)
           id (u/uuid)]
       (-> (reify ICoordinateSystem
             (check-coordinates [_ coords]
               (or (and (= n 1) (= (g/dimension coords) 1))
                   (and (s/up? coords) (= (g/dimension coords) n))))

             (check-point [_ point]
               (my-manifold-point? point manifold))

             (coords->point [this coords]
               (assert (check-coordinates this coords))
               (let [coords' (if (= n 1) (s/up coords) coords)
                     delta  (g/dot-product coords' coords')
                     xn (g/divide (g/- delta 1)
                                  (g/+ 1 delta))
                     pt (s/generate (+ n 1)
                                    ::s/up
                                    #(if (= % n) xn
                                         (g/divide (g/* 2 (nth coords' %))
                                                   (g/+ 1 delta))))]
                 (make-manifold-point
                  (g/* orientation-matrix pt)
                  manifold this coords)))

             (point->coords [this point]
               (assert (check-point this point))
               (get-coordinates
                point this
                (fn []
                  (let [pt (g/* orientation-inverse-matrix
                                (manifold-point-representation point))]
                    (when (and (v/number? (nth pt n))
                               (= (nth pt n) 1))
                      (u/illegal-state "S^n stereographic singular"))
                    (let [coords (s/generate
                                  n ::s/up
                                  #(g/divide (nth pt %)
                                             (g/- 1 (nth pt n))))]
                      (if (= n 1)
                        (first coords)
                        coords))))))

             (uuid [_] id))
           (with-meta {::coord-prototype coordinate-prototype
                       ::manifold manifold}))))))

(defn- ->Sn-gnomonic
  "Returns an [[ICoordinateSystem]] instance that converts between `manifold`
  points and a [Gnomonic
  Projection](https://en.wikipedia.org/wiki/Gnomonic_projection) of the [unit
  n-sphere](https://en.wikipedia.org/wiki/N-sphere).


   We map the nothern hemisphere to the plane by firing a ray from the origin.
   The coordinates are given by the intersection with the z = 1 plane.
   x(t) = t*m
   x_n(t) = t*m_n, 1 = x_n(1/m_n)

  `orientation-function` should should return an n+1-by-n+1 orthogonal matrix.
  It can be interpreted as moving the plane of projection, and point mapped to
  the origin, as well as possibly reflecting.

   Given the coordinates x we have

   <x,x> = (1-m_n^2)/m_n^2
   1 + <x,x> = (m_n^2 + 1 - m_n^2)/m_n^2
   m_n = sqrt(1/(1+<x,x>))

   where positive square root is sufficient for the northern hemisphere."
  [orientation-function]
  (fn ctor
    ([manifold]
     (let [proto (default-coordinate-prototype manifold)]
       (ctor manifold proto)))
    ([manifold coordinate-prototype]
     (let [n (:dimension manifold)
           orientation-matrix (orientation-function (+ n 1))
           orientation-inverse-matrix (g/invert orientation-matrix)
           id (u/uuid)]
       (-> (reify ICoordinateSystem
             (check-coordinates [_ coords]
               (or (and (= n 1)
                        (= (g/dimension coords) 1))
                   (and (s/up? coords)
                        (= (g/dimension coords) n))))

             (check-point [_ point]
               (my-manifold-point? point manifold))

             (coords->point [this coords]
               (assert (check-coordinates this coords))
               (let [coords (if (= n 1)
                              (s/up coords)
                              coords)
                     delta (g/dot-product coords coords)
                     d     (g/sqrt (g/+ 1 delta))
                     xn    (g// 1 d)
                     pt    (s/generate
                            (g/+ n 1) ::s/up
                            (fn [i]
                              (if (= i n)
                                xn
                                (g// (nth coords i) d))))]
                 (make-manifold-point
                  (g/* orientation-matrix pt)
                  manifold this coords)))

             (point->coords [this point]
               (assert (check-point this point))
               (get-coordinates
                point this
                (fn []
                  (let [pt (g/* orientation-inverse-matrix
                                (manifold-point-representation point))
                        final-coord (nth pt n)]
                    (when (and (v/number? final-coord)
                               (or (g/negative? final-coord)
                                   (v/zero? final-coord)))
                      (throw
                       (ex-info "Point not covered by S^n-gnomic coordinate patch."
                                {:point point
                                 :coordinate-system this})))
                    (let [coords (s/generate
                                  n ::s/up
                                  (fn [i]
                                    (g// (nth pt i) final-coord)))]
                      (if (= n 1)
                        (nth coords 0)
                        coords))))))

             (uuid [_] id))
           (with-meta {::coord-prototype coordinate-prototype
                       ::manifold manifold}))))))


(defn- ->Euler-chart
  "Returns an [[ICoordinateSystem]] instance that converts between `manifold`
  points and the [Euler angles](https://en.wikipedia.org/wiki/Euler_angles) for
  SO(3)."
  ([manifold]
   (let [proto (default-coordinate-prototype manifold)]
     (->Euler-chart manifold proto)))
  ([manifold coordinate-prototype]
   (let [n (:dimension manifold)
         id (u/uuid)]
     (-> (reify ICoordinateSystem
           (check-coordinates [_ coords]
             (and (s/up? coords)
                  (= (g/dimension coords) n)
                  (let [c0 (nth coords 0)]
                    (or (not (v/number? c0))
                        (not (v/zero? c0))))))

           (check-point [_ point]
             (my-manifold-point? point manifold))

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

           (point->coords [this point]
             (assert (check-point this point))
             (get-coordinates
              point this
              (fn []
                (let [M (manifold-point-representation point)
                      theta (g/acos (get-in M [2 2]))
                      phi (g/atan (get-in M [0 2])
                                  (g/negate (get-in M [1 2])))
                      psi (g/atan (get-in M [2 0])
                                  (get-in M [2 1]))]
                  (s/up theta phi psi)))))

           (uuid [_] id))
         (with-meta {::coord-prototype coordinate-prototype
                     ::manifold manifold})))))

(defn- ->Alternate-chart
  "Returns an [[ICoordinateSystem]] instance that converts between `manifold`
  points and the alternate angles for SO(3).

  NOTE: Please add docs about what these are!"
  ([manifold]
   (let [proto (default-coordinate-prototype manifold)]
     (->Alternate-chart manifold proto)))
  ([manifold coordinate-prototype]
   (let [n (:dimension manifold)
         id (u/uuid)]
     (-> (reify ICoordinateSystem
           (check-coordinates [_ coords]
             (and (s/up? coords)
                  (= (g/dimension coords) n)
                  (or (not (v/number? (nth coords 0)))
                      (< (/ Math/PI -2)
                         (nth coords 0)
                         (/ Math/PI 2) ))))

           (check-point [_ point]
             (my-manifold-point? point manifold))

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

           (point->coords [this point]
             (assert (check-point this point))
             (get-coordinates
              point this
              (fn []
                (let [M (manifold-point-representation point)
                      theta (g/asin (get-in M [2 1]))
                      phi (g/atan (g/negate (get-in M [0 1]))
                                  (get-in M [1 1]))
                      psi (g/atan (g/negate (get-in M [2 0]))
                                  (get-in M [2 2]))]
                  (s/up theta phi psi)))))

           (uuid [_] id))
         (with-meta {::coord-prototype coordinate-prototype
                     ::manifold manifold})))))

;; ## Manifold Definitions
;;
;; This section binds many common manifold families and coordinate systems for
;; use. Enjoy!

(def Rn
  (-> "R(%d)"
      (make-manifold-family)
      (attach-patch :origin)
      (attach-coordinate-system :rectangular :origin ->Rectangular)
      (attach-coordinate-system :polar-cylindrical :origin ->PolarCylindrical)
      (attach-coordinate-system :spherical-cylindrical :origin ->SphericalCylindrical)))

(def R1 (make-manifold Rn 1))
(def R1-rect (coordinate-system-at R1 :rectangular :origin))
(def the-real-line R1-rect)

(def R2 (make-manifold Rn 2))
(def R2-rect (coordinate-system-at R2 :rectangular :origin))
(def R2-polar (coordinate-system-at R2 :polar-cylindrical :origin))

(def R3 (make-manifold Rn 3))
(def R3-rect (coordinate-system-at R3 :rectangular :origin))
(def R3-cyl (coordinate-system-at R3 :polar-cylindrical :origin))
(def R3-spherical (coordinate-system-at R3 :spherical-cylindrical :origin))

(def R4 (make-manifold Rn 4))
(def R4-rect (coordinate-system-at R4 :rectangular :origin))
(def R4-cyl (coordinate-system-at R4 :polar-cylindrical :origin))

(def spacetime
  (-> Rn
      (attach-coordinate-system :spacetime-spherical :origin ->SpacetimeSpherical)
      (make-manifold 4)))

(def spacetime-rect
  (coordinate-system-at spacetime :rectangular :origin))

(def spacetime-sphere
  (coordinate-system-at spacetime :spacetime-spherical :origin))

;; The surface of a sphere, specialized to two dimensions. See [[S2p]] for the 2
;; dimensional specialization of S(n).
;;
;; The `:tilted` patch rotates the north pole 90 degrees toward the positive y
;; axis.

(def S2-type
  (-> "S2"
      (make-manifold-family)
      (attach-patch :north-pole)
      (attach-patch :south-pole)
      (attach-patch :tilted)
      (attach-coordinate-system :spherical :north-pole
                                (->S2-coordinates
                                 (s/down (s/up 1 0 0)
                                         (s/up 0 1 0)
                                         (s/up 0 0 1))))
      (attach-coordinate-system :spherical :tilted
                                (->S2-coordinates
                                 (s/down (s/up 1 0 0)
                                         (s/up 0 0 1)
                                         (s/up 0 -1 0))))
      (attach-coordinate-system :spherical :south-pole
                                (->S2-coordinates
                                 (s/down (s/up 1 0 0)
                                         (s/up 0 1 0)
                                         (s/up 0 0 -1))))
      (attach-coordinate-system :stereographic :north-pole
                                (->Sn-stereographic matrix/I))
      (attach-coordinate-system :stereographic :south-pole
                                (->Sn-stereographic
                                 (fn [n]
                                   ;; TODO: just go flip that coordinate in matrix/I
                                   (matrix/generate
                                    n n
                                    (fn [i j]
                                      (if (= i j)
                                        (if (= j n) -1 1)
                                        0))))))

      (attach-coordinate-system :gnomonic :north-pole
                                (->Sn-gnomonic matrix/I))

      (attach-coordinate-system :gnomonic :south-pole
                                (->Sn-gnomonic
                                 (fn [n]
                                   ;; TODO: just go flip that coordinate in matrix/I
                                   (matrix/generate
                                    n n
                                    (fn [i j]
                                      (if (= i j)
                                        (if (= j n) -1 1)
                                        0))))))))

(def S2 (make-manifold S2-type 2 3))
(def S2-spherical (coordinate-system-at S2 :spherical :north-pole))

(def ^{:doc "Similar to the [[S2-spherical]] coordinate system, with the north
  pole rotated 90 degrees and lying along the positive y axis."}
  S2-tilted
  (coordinate-system-at S2 :spherical :tilted))

(def S2-stereographic (coordinate-system-at S2 :stereographic :north-pole))
(def S2-Riemann S2-stereographic)
(def S2-gnomonic (coordinate-system-at S2 :gnomonic :north-pole))

(def Sn
  (-> (make-manifold-family "S(%d)")
      (attach-patch :north-pole)
      (attach-patch :south-pole)
      (attach-patch :tilted)
      (attach-coordinate-system :spherical :north-pole (->Sn-coordinates matrix/I))
      (attach-coordinate-system :spherical :south-pole
                                (->Sn-coordinates
                                 (fn [n]
                                   ;; TODO: just go flip that coordinate in matrix/I
                                   (matrix/generate
                                    n n
                                    (fn [i j]
                                      (if (= i j)
                                        (if (= j n) -1 1)
                                        0))))))

      (attach-coordinate-system :spherical :tilted
                                (->Sn-coordinates
                                 (fn [n]
                                   (s/generate
                                    n ::s/down
                                    (fn [col]
                                      (s/generate
                                       n ::s/up
                                       (fn [row]
                                         (cond (and (= row (- n 2)) (= col (- n 1))) -1
                                               (and (= row (- n 1)) (= col (- n 2))) 1
                                               (and (= row col) (< row (- n 2))) 1
                                               :else 0))))))))

      (attach-coordinate-system :gnomonic :north-pole (->Sn-gnomonic matrix/I))
      (attach-coordinate-system :gnomonic :south-pole
                                (->Sn-gnomonic
                                 (fn [n]
                                   ;; TODO: just go flip that coordinate in matrix/I
                                   (matrix/generate
                                    n n
                                    (fn [i j]
                                      (if (= i j)
                                        (if (= j n) -1 1)
                                        0))))))

      (attach-coordinate-system :stereographic :north-pole (->Sn-stereographic matrix/I))
      (attach-coordinate-system :stereographic :south-pole
                                (->Sn-stereographic
                                 (fn [n]
                                   ;; TODO: just go flip that coordinate in matrix/I
                                   (matrix/generate
                                    n n
                                    (fn [i j]
                                      (if (= i j)
                                        (if (= j n) -1 1)
                                        0))))))))

(def S1 (make-manifold Sn 1))
(def S1-circular (coordinate-system-at S1 :spherical :north-pole))
(def S1-tilted (coordinate-system-at S1 :spherical :tilted))
(def S1-slope (coordinate-system-at S1 :stereographic :north-pole))
(def S1-gnomonic (coordinate-system-at S1 :gnomonic :north-pole))

(def S2p (make-manifold Sn 2))
(def S2p-spherical (coordinate-system-at S2p :spherical :north-pole))
(def S2p-tilted (coordinate-system-at S2p :spherical :tilted))
(def S2p-stereographic (coordinate-system-at S2p :stereographic :north-pole))
(def S2p-Riemann S2p-stereographic)
(def S2p-gnomonic (coordinate-system-at S2p :gnomonic :north-pole))

(def S3 (make-manifold Sn 3))
(def S3-spherical (coordinate-system-at S3 :spherical :north-pole))
(def S3-tilted (coordinate-system-at S3 :spherical :tilted))
(def S3-gnomonic (coordinate-system-at S3 :gnomonic :north-pole))

;; TODO is south pole right??
(def S3-stereographic (coordinate-system-at S3 :stereographic :south-pole))

;; ## SO(3)
;;
;; Points are represented by 3x3 (down (up ...) ...)
;;
;; There is only one instance of an SOn manifold defined, SO3. As a consequence
;; the name is not SOn but rather SO3-type.

(def SO3-type
  (-> (make-manifold-family "SO3")
      (attach-patch :Euler-patch)
      (attach-patch :alternate-patch)
      (attach-coordinate-system :Euler :Euler-patch ->Euler-chart)
      (attach-coordinate-system :alternate :alternate-patch ->Alternate-chart)))

(def SO3
  (make-manifold SO3-type 3))

(def Euler-angles
  (coordinate-system-at SO3 :Euler :Euler-patch))

(def alternate-angles
  (coordinate-system-at SO3 :alternate :alternate-patch))
