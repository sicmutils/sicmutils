(ns sicmutils.calculus.manifold
  (:require [clojure.string :as str]
            [sicmutils
             [value :as v]
             [structure :as s]])
  )

;; (defn coordinate-system-at
;;   [coordinate-system-name patch-name manifold]
;;   (coordinate-system coordinate-system-name (patch patch-name manifold)))

;(defprotocol IManifoldSpecification
;  (new-patch [this name generator setup])
;  (patch-setup [this name])
;  (generator [this]))

(defn make-manifold-family
  [name-format & {:keys [over] :or {over 'Real}}]
  {:over over
   :name-format name-format
   :patch {}})

(defn make-manifold
  [manifold-family n]
  {:pre [(integer? n)
         (> n 0)]}
  {:manifold-family manifold-family
   :name (format (manifold-family :name-format) n)
   :dimension n
   :embedding-dimension n})

;; to each manifold family, one or more patches may be adjoined.
;; each has a name.
;; each has patch-generator and patch-setup functions.
;; the name and these two functions are stored with the manifold in
;; its patch map.

(defn make-patch [name]
  {:name name
   :coordinate-system {}})

(defn attach-patch
  [manifold-family patch-name]
  (update manifold-family :patch assoc patch-name (make-patch patch-name)))

;; attach-patch does more, though. It also creates a patch generator,
;; which is a function of the patch, which knows the manifold, and
;; which takes care of instantiating the coordinate system.

; coordinate systems are added to coordinate patches.
; a coordinate system is an invertible map from the space to R(n).

(defn attach-coordinate-system
  [manifold-family coordinate-system-name patch-name coordinate-system]
  (update-in manifold-family [:patch patch-name :coordinate-system]
             assoc coordinate-system-name coordinate-system))

;; where we left off: this currently evaluates to the manifold
;; constructor function, and not the manifold itself. When is
;; the manifold constructor applied? It can't be when the coordinate
;; system is attached, because those are attached to patches.

(defn coordinate-system-at
  [coordinate-system-name patch-name manifold]
  (get-in manifold [:manifold-family
                    :patch patch-name
                    :coordinate-system coordinate-system-name]))

(defprotocol ICoordinateSystem
  (check-coordinates [this coords])
  (coords->point [this coords])
  (check-point [this point])
  (point->coords [this point])
  (manifold [this]))

;; where we left off: we discover that in scmutils, a manifold-point
;; may be immutable in spirit, but lazily computes its representations
;; in various coordinate systems. One idea would be for the point to
;; hold an atom containing a mapping.

(defn ^:private make-manifold-point
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
    ))



; features of a coordinate system object:
;
; name
; patch (pointer to the patch in which it was defined...which would be a cycle :(
; ->point (transform-delivery 'coords->point)
; ->coords (transform-delivery 'point->coords)
; check-point
; check-coords
; typical-coords
; coordinate-prototype
; set-coordinate-protoype!
;   this has the effect of clearing the following internal state variables:
;    coordinate-{prototype,function-specs,basis-vector-field-specs,basis-1form-field-specs,basis}
;    access-chains (but what about dual-chains? bug?)
; access-chains
; dual-chains (initialized as the flip of access chains).


(def Rn (-> "R(%d)"
            make-manifold-family
            (attach-patch :origin)
            (attach-coordinate-system :rectangular :origin ->Rectangular)))
(def R2 (make-manifold Rn 2))
(def R2-rect (coordinate-system-at :rectangular :origin R2))

(println "R2 is" R2)
(println "R2-rect is " R2-rect)



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
