(ns sicmutils.calculus.manifold
  (:require [clojure.string :as str]
            [sicmutils
             [value :as v]
             [generic :as g]
             [structure :as s]])
  )

(defn make-manifold-family
  [name-format & {:keys [over] :or {over 'Real}}]
  {:over over
   :name-format name-format
   :patch {}})

(defn make-manifold
  "Specialize a manifold-family into a particular manifold by specifying
  its dimension."
  [manifold-family n]
  {:pre [(integer? n)
         (> n 0)]}
  {:manifold-family     manifold-family
   :name                (format (manifold-family :name-format) n)
   :dimension           n
   :embedding-dimension n})

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

(defn coordinate-system-at
  "Looks up the named coordinate system in the named patch of the given
  manifold; this locates a constructor, which is then applied to manifold
  to produce the result: an instantiated coordinate system."
  [coordinate-system-name patch-name manifold]
  ((get-in manifold [:manifold-family
                     :patch patch-name
                     :coordinate-system coordinate-system-name]) manifold))

(defprotocol ICoordinateSystem
  (check-coordinates [this coords])
  (coords->point [this coords])
  (check-point [this point])
  (point->coords [this point])
  (manifold [this]))

(defn ^:private make-manifold-point
  "Make a point in an abstract manifold, specified by a concreate point
  in some coordinate system, and the concrete coordinates in Euclidean
  space. The map of coordinate representaions can be lazily extended to
  yet other coordinate systems."
  [spec manifold coordinate-system coordinate-rep]
  {:spec spec
   :manifold manifold
   :coordinate-representation (atom {coordinate-system coordinate-rep})})

(defn ^:private manifold-point-representation
  [manifold-point]
  (manifold-point :spec))

(defn get-coordinates
  "Get the representation of manifold-point in coordinate-system. The
  point contains a cache of the coordinate system->representation mapping.
  If an entry for the given coordinate system is not found, thunk is
  called to produce the representation, which is then installed in the
  cache."
  [manifold-point coordinate-system thunk]
  (let [reps (manifold-point :coordinate-representation)]
    (if-let [rep (@reps coordinate-system)]
     rep
     (let [rep (thunk)]
       (swap! reps assoc coordinate-system rep)
       rep))))

(defn ^:private point->manifold
  [point]
  (point :manifold))

(defn ^:private my-manifold-point?
  [point manifold]
  (= (point->manifold point) manifold))

(deftype Rectangular [manifold]
  ICoordinateSystem
  (check-coordinates [this coords]
    (= (count coords) (manifold :dimension)))
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
                         prep
                         )))

    )
  (manifold [this] manifold))

(deftype PolarCylindrical [manifold]
  ICoordinateSystem
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
        (s/->Structure ::s/up
                       (mapv (fn [i]
                               (case i
                                 0 (g/* r (g/cos theta))
                                 1 (g/* r (g/sin theta))
                                 (nth coords i))) (range (count coords))))
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
                           (throw (IllegalStateException. "PolarCylindrical singular")))
                         (let [[x y] prep
                               rsq (g/+ (g/square x) (g/square y))]
                           (when (v/nullity? rsq)
                             (throw (IllegalStateException. "PolarCylindrical singular")))
                           (s/->Structure ::s/up
                                          (mapv (fn [i]
                                                  (case i
                                                    0 (g/sqrt rsq)
                                                    1 (g/atan y x)
                                                    (nth prep i)))
                                                (range (count prep)))))))))
  (manifold [this] manifold))

(def Rn (-> "R(%d)"
            make-manifold-family
            (attach-patch :origin)
            (attach-coordinate-system :rectangular :origin ->Rectangular)
            (attach-coordinate-system :polar-cylindrical :origin ->PolarCylindrical)))

(def R2 (make-manifold Rn 2))
(def R2-rect (coordinate-system-at :rectangular :origin R2))
(def R2-polar (coordinate-system-at :polar-cylindrical :origin R2))


;(defn make-manifold
;  [manifold-family dimension]
;  {:family manifold-family
;   :name (format (:name-format manifold-family) dimension)
;   })


;(def R3 (make-manifold Rn 3))
;(attach-patch )

;(defn specify-manifold
;  [manifold-name & {:keys [over] :or {over 'Real}}]
;
;  {:type  ::manifold-specification
;   :over  over
;   :patch {}
;   :generator
;          (fn [dimension & {:keys [embedding-dimension]}]
;            (let [counter (atom 0)
;                  name (symbol (str (let [namestring (str manifold-name)]
;                                      (if (re-matches #".*[↑^]n" namestring)
;                                        (str/replace namestring #"n$" (str dimension))
;                                        namestring))
;                                    "-"
;                                    (swap! counter inc)))]
;              {:dimension           dimension
;               :embedding-dimension (or embedding-dimension dimension)
;               :name                name
;               :over over
;               }))
;   })


;; specify-manifold produces an object answering the SETUP api.
;;   new-patch(new-patch-name, patch-generator, patch-setup)
;;     -- mutates manifold by adding a patch
;;        this just means we add the argument triple to the patches list.
;;   patch-setup(patch-name) -- returns the setup entry of the named patch.
;;   generator() -- returns generator object.

;; generator takes (dimension & optionally embedding-dimension) to produce
;; a manifold object with that data baked in.

;; the manifold object answers the following API:
;;   name
;;   manifold (i.e., self?)
;;   type (local)
;;   dimension or manifold-dimension --> dimension (local)
;;   embedding-dimension --> ditto
;;   get-patch
;;   distinguished points
;;   add-distinguished-point! --> (does anyone call this? apparently not)
;;   patch-names (from patches list)
