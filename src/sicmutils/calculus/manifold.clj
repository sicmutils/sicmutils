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
  to produce the result: an object implementing ICoordinateSystem."
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

(defn coordinate-functions
  [coordinate-system coordinate-prototype]
  (s/mapr (fn [coordinate-name access-chain]
            (comp (apply s/component access-chain)
                  #(point->coords coordinate-system %)))
          coordinate-prototype
          (s/structure->access-chains coordinate-prototype)))

(defn ^:private quotify-coordinate-prototype
  [p]
  (cond (and (sequential? p)
             ('#{up down} (first p))) `(~(first p) ~@(map quotify-coordinate-prototype (rest p)))
        (vector? p) (mapv quotify-coordinate-prototype p)
        (symbol? p) `'~p
        :else (throw (IllegalArgumentException. "Invalid coordinate prototype"))))

(defn ^:private symbols-from-prototype
  [p]
  (cond (and (sequential? p)
             ('#{up down} (first p))) (mapcat symbols-from-prototype (rest p))
        (vector? p) (mapcat symbols-from-prototype p)
        (symbol? p) `(~p)
        :else (throw (IllegalArgumentException. (str "Invalid coordinate prototype: " p)))))

(defmacro using-coordinates
  "Example:
    (using-coordinates (up x y) R2-rect
      body...)"
  [coordinate-prototype coordinate-system & body]
  (let [qcp (quotify-coordinate-prototype coordinate-prototype)
        coordinates (symbols-from-prototype coordinate-prototype)]
    `(let [prototype# ~qcp
           c-fns# (coordinate-functions ~coordinate-system prototype#)
           f# (fn ~(vec coordinates) ~@body)]
       (apply f# (flatten c-fns#)))))

(defmacro let-coordinates
  "Example:
    (let-coordinates [[x y] R2-rect
                      [r theta] R2-polar]
      body...)"
  [bindings & body]
  (when-not (even? (count bindings))
    (throw (IllegalArgumentException. "let-coordinates requires an even number of bindings")))
  (let [pairs (partition 2 bindings)
        prototypes (map first pairs)
        c-systems (mapv second pairs)
        coordinates (mapcat symbols-from-prototype prototypes)]
    `(let [prototypes# ~(mapv quotify-coordinate-prototype prototypes)
           c-systems# ~c-systems
           c-fns# (map coordinate-functions c-systems# prototypes#)
           f# (fn ~(vec coordinates) ~@body)]
       (apply f# (mapcat flatten c-fns#)))))

(defn ^:private make-manifold-point
  "Make a point in an abstract manifold, specified by a concrete point
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
        (s/generate (count coords) ::s/up
                    #(case %
                       0 (g/* r (g/cos theta))
                       1 (g/* r (g/sin theta))
                       (nth coords %)))
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
                           (s/generate (count prep) ::s/up
                                       #(case %
                                          0 (g/sqrt rsq)
                                          1 (g/atan y x)
                                          (nth prep %))))))))
  (manifold [this] manifold))

(def Rn (-> "R(%d)"
            make-manifold-family
            (attach-patch :origin)
            (attach-coordinate-system :rectangular :origin ->Rectangular)
            (attach-coordinate-system :polar-cylindrical :origin ->PolarCylindrical)))

(def R2 (make-manifold Rn 2))
(def R2-rect (coordinate-system-at :rectangular :origin R2))
(def R2-polar (coordinate-system-at :polar-cylindrical :origin R2))

