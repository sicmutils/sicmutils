
;;
;;
;;

(ns hestenic
  "the superabsorbent and lint-free world of geometric algebra (and on into
  geo'tric calculus)")

;;
;; the approach we'll use here is protocol-rich... or -heavy, depending
;; on your taste. but in general we wish to represent elements in GA
;; at a few natural levels: weighted basis blades ('Bladoid');
;; collections of a number of Bladoids of uniform grade ('Gradeling'),
;; sorted lexicographically on basis; collections of Gradelings of
;; heterogeneous grade ('MV', for multivector), sorted on grade; and
;; vector itself ('Vect'). in addition, for the moment we have a special
;; class to represent the zero element ('ZeroElement'), which is identical
;; for all grades.
;;
;; the protocol immediately following is the (presently malnourished but
;; still growing) set of canonical operations applicable to each of the
;; aforenamed elements. it might be argued that the final protocol in
;; this section ('IScamperUpAndDownLadder') could reasonably be merged
;; with the basic protocol ('IHestenic'), but from the current insomniac
;; perspective it seems cognitively useful to keep 'em separate.
;;
;; note that we extend many of these protocols too to java.lang.Number
;; to enjoy automatic interop of existing numeric types with the GA
;; system. thanks, lisp dialect!
;;

(defprotocol IHestenic
  (scl [this s])
  (sum [this otha])
  (neg [this])
  (dif [this otha])
  (prd [this otha])
  (inv [this])
  (quo [this otha])
  (dot [this otha])
  (wdg [this otha])
  (eq? [this otha])
  (grade-part [this n]))

(defprotocol IGradable
  (grade [this]))

(defprotocol IAsBladoid
  (asBladoid [this]))

(defprotocol IAsGradeling
  (asGradeling [this]))

(defprotocol IAsMV
  (asMV [this]))

(defprotocol IScamperUpAndDownLadder
  (spankOutNothingness [this])
  (upRungify [this])
  (downRungify [this]))


;;
;; the following composition should reduce any GA object to its 'minimal'
;; representation.
;;

(defn- accordion-down [hestish]
  (downRungify
   (spankOutNothingness
    hestish)))


;;
;; all things Bladoid, don't you know. well, not all. in any event, one of these
;; is a weighted (scaled) basis element of the GA. does not for the moment
;; presuppose any particular dimension. also does not afford configurable
;; metrics...
;;

(defprotocol IBladoid
  (coef [this])
  (basis [this])
  (square-sign [this])
  (same-basis? [this otha]))


(deftype Bladoid [f_coef f_basis]
  IBladoid
  (coef [_] f_coef)
  (basis [_] f_basis)
  (square-sign [_]
    (let [gr (count f_basis)
          flormp (/ (* gr (- gr 1)) 2)]
      (if (even? flormp) +1 -1)))
  (same-basis? [this otha]
    (= (basis this) (basis otha)))

  IGradable
  (grade [_] (count f_basis))

  Object
  (toString [_] (str f_coef ":[" (clojure.string/join f_basis) "]")))


(defn bladoid
  ([]
   (Bladoid. 1.0 []))
  ([c]
   (Bladoid. c []))
  ([c b]
   (Bladoid. c (vec (sort b)))))  ;; force sort to give back the same type.


(defmethod print-method Bladoid [^Bladoid doid ^java.io.Writer w]
  (.write w (str "#b(" (coef doid) " ["
                 (clojure.string/join " " (basis doid))
                 "])")))

(defn parse-bladoid [[wgt bss]]
  (prn wgt bss)
  (bladoid wgt bss))


(defn- collapse [doids]
  (map (partial reduce sum)
       (partition-by basis doids)))


(defn- confirm-grades!
  ([blds]  ;; ar/1: infer grade from first bloadoid
   (if (empty? blds)
     blds
     (confirm-grades! blds (grade (first blds)))))
  ([blds gra]  ;; ar/2: with grade specified
   (if (empty? blds)
     blds
     (loop [head (first blds)
            body (rest blds)]
       (if (not= gra (grade head))
         (throw (Exception. (str "incompatible grade: "
                                 head " but should be " gra)))
         (if (not (empty? body))
           (recur (first body) (rest body))))))
   blds))

(defn- basis-prod [bas1 bas2]
  (let [ess1 (set bas1)
        ess2 (set bas2)]
    (vec (sort
          (clojure.set/difference (clojure.set/union ess1 ess2)
                                  (clojure.set/intersection ess1 ess2))))))

(defn- scoot-count [val rseq]
  (loop [scoo 0
         arr rseq]
    (if (or (empty? arr)
            (<= val (first arr)))
      scoo
      (recur (inc scoo) (rest arr)))))

(defn- order-swap-count [basl basr]
  (loop [cnt 0
         lind (dec (count basl))]
    (if (< lind 0)
      cnt
      (let [lval (nth basl lind)
            sc (scoot-count lval basr)]
        (if (= sc 0)
          cnt
          (recur (+ cnt sc) (dec lind)))))))

(defn- order-swap-parity [basl basr]
  (if (even? (order-swap-count basl basr)) +1 -1))

(defn- sort-on-bases [bldds]
  (vec (sort-by basis bldds)))

(defn- sort-on-grades [elmnts]
  (vec (sort-by grade elmnts)))

(defn- same-grade? [this otha]
  (= (grade this) (grade otha)))

(defn- different-rung? [this otha]
  (not (instance? (class this) otha)))


;;
;; A Gradeling is a bevy of Bladoids of the same grade. It may or may not be
;; itself a blade (the 'not' part becomes a possibility for dimensions above
;; three, which offers bewildering disappointinments like e01 + e12).
;;

(defprotocol IGradeling
  (bladoids [this])
  (bladoid-with-basis [this b]))


(deftype Gradeling [f_grade f_bladoids]
  IGradeling
  (bladoids [_] f_bladoids)
  (bladoid-with-basis [this b]
    (first (filter (fn [el] (= (basis el) b))
                   f_bladoids)))

  IGradable
  (grade [_] f_grade)

  Object
  (toString [_] (str "g" f_grade "("
                     (clojure.string/join " " f_bladoids)
                     ")")))


;; singin' them old-timey singletunes:

(def the-empty-gradeling (Gradeling. -1 []))

(defn gradeling [grade-or-bladoids]
  (if (integer? grade-or-bladoids)
    (Gradeling. grade-or-bladoids [])
    (let [gra (if (empty? grade-or-bladoids)
                -1
                (grade (first grade-or-bladoids)))]
      (Gradeling. gra (collapse
                       (sort-on-bases
                        (confirm-grades! grade-or-bladoids gra)))))))

(defmethod print-method Gradeling [^Gradeling grdl ^java.io.Writer w]
  (.write w (str "#g" (comment (grade grdl)) "("
                 (clojure.string/join " " (bladoids grdl))
                 ")")))


;;
;; Multivector is, mirabile dictu, a multivector: an collection of zero or
;; more Gradelings: which is to say an arbitrary collection of Bladoids
;;

(defprotocol IMV
  (gradelings [this])
  (gradeling-of-grade [this gra])
  (grades [this]))

(deftype MV [f_gradelings]
  IMV
  (gradelings [_] f_gradelings)
  (gradeling-of-grade [this gra]
    (first (filter (fn [grdl] (= (grade grdl) gra))
                   f_gradelings)))
  (grades [_] (map grade f_gradelings))

  Object
  (toString [_] (str "MV["
                     (clojure.string/join " " f_gradelings)
                     "]")))

(defmethod print-method MV [^MV emvy ^java.io.Writer w]
  (.write w (str "#MV" "["
                 (clojure.string/join " " (gradelings emvy))
                 "]")))

(def the-empty-mv (MV. []))

(defn mv [gradels]
  (MV. (sort-on-grades gradels)))


;;
;; Vect is, of course, a vector (meaning -- yes! -- a real geometric,
;; mathematical vector, for Zoroaster's sake; not an 'array', per
;; lexical cretinism).
;;
;; In a strict sense this is a redundant type-entity, since one of these
;; could always be represented as a Gradeling of grade one. But because
;; vectors have a vaunted position in GA -- they are rightly the generators
;; of the whole algebra, y' see-- it's very much worth representing them
;; explicitly.
;;

(defprotocol IVect
  (coefs [this]))

(deftype Vect [f_coefs]
  IVect
  (coefs [_] f_coefs)

  IGradable
  (grade [_] 1)

  Object
  (toString [_] (str "v(" (clojure.string/join " " f_coefs) ")")))


(defn vect [& cs] (Vect. (vec cs)))

(defmethod print-method Vect [^Vect veee ^java.io.Writer w]
  (.write w (str "#v" "("
                 (clojure.string/join " " (coefs veee))
                 ")")))

(defn parse-vect [coefficients-aplenty]
  (apply vect coefficients-aplenty))


;;
;; herewith the special representation for that certain lack of anything.
;;
;; a philosophical quandary whether we're better off with or without
;; this one; the interop with built-in numbers, including of course
;; 0 (& 0.0 et al.), is so pleasing and frictionless that we could
;; ditch this creature.
;;

(deftype ZeroElement []
  IGradable
  (grade [_] 0)

  IScamperUpAndDownLadder
  (spankOutNothingness [this]
    this)
  (upRungify [_]
    (Bladoid. 0 []))
  (downRungify [_] 0)

  IHestenic
  (scl [this s] this)
  (sum [this otha] otha)
  (neg [this] this)
  (dif [this otha] (neg otha))
  (prd [this otha] this)
  (inv [this] (/ 1 0))
  (quo [this otha]
    (let [invvy (inv otha)]
      this))
  (dot [this otha] this)
  (wdg [this otha] this)
  (eq? [this otha]
    (or (instance? (class this) otha)
        (and (number? otha) (zero? otha))))
  (grade-part [this n] this))


;; the incomparable nullful stylings of sarah and the singletones:

(def the-zero-element (ZeroElement.))


;;
;; a shadowy cabal of utility-interior mathy bits.
;;

(defn- gradeling-absorb-bladoid [l-grdl r-doid]
  (let [gra (grade l-grdl)]
    (if (= gra -1)
      (Gradeling. (grade r-doid) (vector r-doid))
      (if (not= gra (grade r-doid))
        (throw (Exception. (str "can't absorb grade" (grade r-doid)
                                " blade into grade " gra
                                "gradeling...")))
        (if (bladoid-with-basis l-grdl (basis r-doid))
          (Gradeling. gra (vec
                           (map (fn [l-doid]
                                  (if (same-basis? l-doid r-doid)
                                    (Bladoid. (+ (coef l-doid) (coef r-doid))
                                              (basis r-doid))
                                    l-doid))
                                (bladoids l-grdl))))
          (Gradeling. gra (sort-on-bases
                           (conj (bladoids l-grdl) r-doid))))))))

(defn- gradeling-absorb-gradeling [l-grdl r-grdl]
  (reduce gradeling-absorb-bladoid
          l-grdl (bladoids r-grdl)))

(defn- mv-absorb-gradeling [l-emvy r-grdl]
  (if (gradeling-of-grade l-emvy (grade r-grdl))
    (MV. (map (fn [l-grdl]
                (if (same-grade? l-grdl r-grdl)
                  (gradeling-absorb-gradeling l-grdl r-grdl)
                  l-grdl))
              (gradelings l-emvy)))
    (MV. (sort-on-grades (conj (gradelings l-emvy) r-grdl)))))

(defn- mv-absorb-bladoid [l-emvy r-doid]
  (mv-absorb-gradeling l-emvy (asGradeling r-doid)))

(defn- mv-absorb-mv [l-emvy r-emvy]
  (reduce mv-absorb-gradeling
          l-emvy (gradelings r-emvy)))

(defn- bladoid-mult-bladoid [l-doid r-doid]
  (let [basl (basis l-doid)
        basr (basis r-doid)]
    (Bladoid. (* (coef l-doid) (coef r-doid)
                 (order-swap-parity basl basr))
              (basis-prod basl basr))))

(defn- bladoid-mult-gradeling [l-doid r-grdl]
  (reduce (fn [acc-emvy r-doid]
            (mv-absorb-bladoid acc-emvy (prd l-doid r-doid)))
          the-empty-mv
          (bladoids r-grdl)))

(defn- gradeling-mult-gradeling [l-grdl r-grdl]
  (reduce (fn [acc-emvy doid] (mv-absorb-mv
                                acc-emvy
                                (bladoid-mult-gradeling doid r-grdl)))
          the-empty-mv
          (bladoids l-grdl)))

(defn- mv-mult-gradeling [l-emvy r-grdl]
  (reduce (fn [acc-mv l-grdl]
            (mv-absorb-mv
             acc-mv
             (gradeling-mult-gradeling l-grdl r-grdl)))
          the-empty-mv
          (gradelings l-emvy)))

(defn- mv-map-biexploded-gradelings-and-sum [funq l-emvy r-emvy]
  (reduce mv-absorb-mv
          the-empty-mv
          (for [l-grdl (gradelings l-emvy)
                r-grdl (gradelings r-emvy)]
            (asMV (funq l-grdl r-grdl)))))


;;
;; rung conversions: one, yea, unto another.
;;

(extend-type Bladoid
  IAsBladoid
  (asBladoid [this]
    this)

  IAsGradeling
  (asGradeling [this]
    (Gradeling. (grade this) (vector this)))

  IAsMV
  (asMV [this]
    (asMV (asGradeling this))))


(extend-type Gradeling
  IAsGradeling
  (asGradeling [this]
    this)

  IAsMV
  (asMV [this]
    (MV. [this])))


(extend-type MV
  IAsMV
  (asMV [this]
    this))


(extend-type Vect
  IAsGradeling
  (asGradeling [this]
    (Gradeling. 1 (vec (filter (fn [bl] (not= 0 (coef bl)))
                               (map-indexed
                                (fn [ind cf] (bladoid cf (vector ind)))
                                (coefs this))))))

  IAsMV
  (asMV [this]
    (asMV (asGradeling this))))


(extend-type ZeroElement
  IAsBladoid
  (asBladoid [_]
    (Bladoid. 0 []))

  IAsGradeling
  (asGradeling [this]
    (asGradeling (asBladoid this)))

  IAsMV
  (asMV [this]
    (asMV (asBladoid this))))


;;
;; simplification (and complexification) of species
;;

(extend-type Bladoid
  IScamperUpAndDownLadder
  (spankOutNothingness [this]
    (if (zero? (coef this))
      0
      this))
  (upRungify [this]
    (asGradeling this))
  (downRungify [this]
    (if (empty? (basis this))
      (coef this)
      this)))

(extend-type Gradeling
  IScamperUpAndDownLadder
  (spankOutNothingness [this]
    (let [spnkd (filter
                 (fn [bl] (not (zero? (coef bl))))
                 (bladoids this))]
      (if (empty? spnkd)
        0
        (Gradeling. (grade this) spnkd))))
  (upRungify [this]
    (asMV this))
  (downRungify [this]
    (let [bls (bladoids this)]
      (if (= 1 (count bls))
        (downRungify (first bls))
        this))))

(extend-type MV
  IScamperUpAndDownLadder
  (spankOutNothingness [this]
    (let [nullplop (map
                    spankOutNothingness
                    (gradelings this))
          spnkd (filter
                 (fn [grdl] (not (number? grdl)))
                 nullplop)]
      (if (empty? spnkd)
        0
        (MV. spnkd))))
  (upRungify [this]
    this)
  (downRungify [this]
    (let [grdls (gradelings this)]
      (if (= 1 (count grdls))
        (downRungify (first grdls))
        this))))

(extend-type Vect
  IScamperUpAndDownLadder
  (spankOutNothingness [this]
    (if (every? zero? (coefs this))
      0
      this))
  (upRungify [this]
    (asGradeling this))
  (downRungify [this]
    this))


;;
;; now for the good stuff.
;;

(extend-type Bladoid
  IHestenic
  (scl [this s]
    (Bladoid. (* (coef this) s) (basis this)))
  (sum [this otha]
    (if (different-rung? this otha)
      (sum (asMV this) (asMV otha))
      (accordion-down
       (if (same-basis? this otha)
         (Bladoid. (+ (coef this) (coef otha)) (basis this))
         (if (same-grade? this otha)
           (Gradeling. (grade this)
                       (sort-on-bases (vector this otha)))
           (MV. (sort-on-grades (vector (asGradeling this)
                                        (asGradeling otha)))))))))
  (neg [this] (Bladoid. (- (coef this)) (basis this)))
  (dif [this otha] (sum this (neg otha)))
  (prd [this otha]
    (if (different-rung? this otha)
      (if (number? otha)
        (scl this otha)
        (prd (asMV this) (asMV otha)))
      (accordion-down
       (bladoid-mult-bladoid this otha))))
  (inv [this]
    (let [bas (basis this)]
      (Bladoid. (* (/ 1 (coef this)) (square-sign this)) (basis this))))
  (quo [this otha] (prd this (inv otha)))
  (dot [this otha]
    (if (different-rung? this otha)
      (dot (asMV this) (asMV otha))
      (grade-part (prd this otha)
                  (Math/abs (- (grade this) (grade otha))))))
  (wdg [this otha]
    (if (different-rung? this otha)
      (wdg (asMV this) (asMV otha))
      (grade-part (prd this otha)
                  (+ (grade this) (grade otha)))))
  (eq? [this otha]
    (if (different-rung? this otha)
      (eq? (asMV this) otha)
      (and (= (coef this) (coef otha))
           (= (basis this) (basis otha)))))
  (grade-part [this n]
    (if (= (grade this) n)
      this
      the-zero-element)))


(extend-type Gradeling
  IHestenic
  (scl [this s]
    (Gradeling. (grade this) (map (fn [bld] (scl bld s))
                                  (bladoids this))))
  (sum [this otha]
    (if (different-rung? this otha)
      (sum (asMV this) (asMV otha))
      (accordion-down
       (if (same-grade? this otha)
         (gradeling-absorb-gradeling this otha)
         (MV. (sort-on-grades (vector this otha)))))))
  (neg [this] (Gradeling. (grade this)
                          (map (fn [bl] (neg bl)) (bladoids this))))
  (dif [this otha] (sum this (neg otha)))
  (prd [this otha]
    (if (different-rung? this otha)
      (if (number? otha)
        (scl this otha)
        (prd (asMV this) (asMV otha)))
      (accordion-down
       (gradeling-mult-gradeling this otha))))
  (inv [this] nil)
  (quo [this otha] (prd this (inv otha)))
  (dot [this otha]
    (if (different-rung? this otha)
      (dot (asMV this) (asMV otha))
      (grade-part (prd this otha)
                  (Math/abs (- (grade this) (grade otha))))))
  (wdg [this otha]
    (if (different-rung? this otha)
      (wdg (asMV this) (asMV otha))
      (grade-part (prd this otha)
                  (+ (grade this) (grade otha)))))
  (eq? [this otha]
    (if (different-rung? this otha)
      (eq? (asMV this) otha)
      (and (= (grade this) (grade otha))
           (every? true?
                   (map eq? (bladoids this) (bladoids otha))))))
  (grade-part [this n]
    (if (= (grade this) n)
      this
      the-zero-element)))



(defn- mv-bimap-via-gradelings [funq mvl mvr]
  (accordion-down
   (mv-map-biexploded-gradelings-and-sum
    funq mvl mvr)))

(extend-type MV
  IHestenic
  (scl [this s]
    (MV. (map (fn [grdl] (scl grdl s))
              (gradelings this))))
  (sum [this otha]
    (accordion-down
     (mv-absorb-mv this (asMV otha))))
  (neg [this]
    (MV. (map (fn [grdl] (neg grdl)) (gradelings this))))
  (dif [this otha]
    (sum this (neg otha)))
  (prd [this otha]
    (if (number? otha)
      (scl this otha)
      (mv-bimap-via-gradelings prd this (asMV otha))))
  (inv [this])
  (quo [this otha]
    (prd this (asMV (inv otha))))
  (dot [this otha]
    (mv-bimap-via-gradelings dot this (asMV otha)))
  (wdg [this otha]
    (mv-bimap-via-gradelings wdg this (asMV otha)))
  (eq? [this otha]
    (every? true?
            (map eq? (gradelings this) (gradelings (asMV otha)))))
  (grade-part [this n]
    (let [grdl (filter (fn [g] (= (grade g) n))
                       (gradelings this))]
      (if (empty? grdl)
        the-zero-element
        (first grdl)))))


(extend-type Vect
  IHestenic
  (scl [this s]
    (Vect. (vec (map (fn [co] (* co s))
                     (coefs this)))))
  (sum [this otha]
    (if (different-rung? this otha)
      (sum (asMV this) (asMV otha))
      (Vect. (vec (map (fn [hoo hah] (+ hoo hah))
                       (coefs this) (coefs otha))))))
  (neg [this]
    (Vect. (vec (map (fn [co] (- co))
                     (coefs this)))))
  (dif [this otha]
    (sum this (neg otha)))
  (prd [this otha]
    (prd (asMV this) (asMV otha)))
  (inv [this]
    (let [sq (dot this this)]
      (scl this (/ 1.0 sq))))
  (quo [this otha]
    (prd this (inv otha)))
  (dot [this otha]
    (reduce +
            (map *
                 (coefs this) (coefs otha))))
  (wdg [this otha]
    (if (different-rung? this otha)
      (wdg (asMV this) (asMV otha))
      (grade-part (prd this otha) 2)))
  (eq? [this otha]
    (if (different-rung? this otha)
      (eq? (asMV this) otha)
      (let [thc (coefs this)
            otc (coefs otha)]
        (and (= (count thc) (count otc))
             (every? true? (map == thc otc))))))
  (grade-part [this n]
    (if (= n 1)
      this
      the-zero-element)))


;;
;; infect clojure/java's inbuilt numeric system with hestenicity
;;

(extend-type java.lang.Number
  IAsBladoid
  (asBladoid [this]
    (Bladoid. this []))

  IAsGradeling
  (asGradeling [this]
    (asGradeling (asBladoid this)))

  IAsMV
  (asMV [this]
    (asMV (asBladoid this)))

  IScamperUpAndDownLadder
  (spankOutNothingness [this]
    this)
  (upRungify [this]
    (Bladoid. this []))
  (downRungify [this]
    this)

  IHestenic
  (scl [this s]
    (* this s))
  (sum [this otha]
    (if (number? otha)
      (+ this otha)
      (sum (asBladoid this) otha)))
  (neg [this]
         (- this))
  (dif [this otha]
    (if (number? otha)
      (- this otha)
      (dif (asBladoid this) otha)))
  (prd [this otha]
    (if (number? otha)
      (* this otha)
      (scl otha this)))
  (inv [this]
    (/ 1.0 this))
  (quo [this otha]
    (prd this (inv otha)))
  (dot [this otha]
    the-zero-element)
  (wdg [this otha]
    (scl otha this))
  (eq? [this otha]
    (if-not (number? otha)
      (eq? (asMV this) otha)
      (== this otha)))
  (grade-part [this n]
    (if (= n 0)
      this
      the-zero-element)))


;;
;; you know -- for kids.
;;

(def e0 (bladoid 1.0 [0]))
(def e1 (bladoid 1.0 [1]))
(def e2 (bladoid 1.0 [2]))
(def e01 (bladoid 1.0 [0 1]))
(def e02 (bladoid 1.0 [0 2]))
(def e12 (bladoid 1.0 [1 2]))
(def e012 (bladoid 1.0 [0 1 2]))
