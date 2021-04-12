
;;
;;
;;

(ns sicmutils.hestenic
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


(deftype Bladoid [cf bss]
  IBladoid
  (coef [_] cf)
  (basis [_] bss)
  (square-sign [_]
    (let [gr (count bss)
          flormp (/ (* gr (- gr 1)) 2)]
      (if (even? flormp) +1 -1)))
  (same-basis? [this otha]
    (= (basis this) (basis otha)))

  IGradable
  (grade [_] (count bss))

  Object
  (toString [_] (str cf ":" bss)))


(defn bladoid
  ([]
   (Bladoid. 1.0 []))
  ([c]
   (Bladoid. c []))
  ([c b]
   (Bladoid. c (vec (sort b)))))  ;; force sort to give back the same type.


(defn- collapse [blds]
  ;; assumes sortedness; sordidness optional
  (loop [head (first blds)
         neck (second blds)
         trunk (rest blds)]
    (cond (nil? head)
          '()
          (nil? neck)
          (list head)
          (same-basis? head neck)
          (recur (sum head neck) (second trunk) (rest trunk))
          :else
          (conj (collapse trunk) head))))

(defn- confirm-grades
  ([blds gra]
   (if (empty? blds)
     blds
     (loop [head (first blds)
            body (rest blds)]
       (if (not= gra (grade head))
         (throw (Exception. (str "incompatible grade: "
                                 head " but should be " gra)))
         (if (not (empty? body))
           (recur (first body) (rest body))))))
   blds)
  ([blds]
   (if (empty? blds)
     blds
     (confirm-grades blds (grade (first blds))))))

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
  (sort (fn [bla blb] (compare (basis bla) (basis blb))) bldds))

(defn- sort-on-grades [elmnts]
  (sort-by grade elmnts))

(defn- same-grade? [this otha]
  (= (grade this) (grade otha)))

(defn- same-rung? [this otha]
  (instance? (class this) otha))

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


(deftype Gradeling [gr bldds]
  IGradeling
  (bladoids [_] bldds)
  (bladoid-with-basis [this b]
    (first (filter (fn [el] (= (basis el) b))
                   (bladoids this))))

  IGradable
  (grade [_] gr)

  Object
  (toString [_] (str "g" gr "{"
                     (reduce (fn [sofar bl]
                               (str sofar (if (empty? sofar) "" " ") bl))
                             "" bldds)
                     "}")))


(defn gradeling [grade-or-bladoids]
  (if
      (integer? grade-or-bladoids)
    (Gradeling. grade-or-bladoids ())
    (let [gra (if (empty? grade-or-bladoids)
                -1
                (grade (first grade-or-bladoids)))]
      (Gradeling. gra (collapse
                       (sort
                        (confirm-grades grade-or-bladoids gra)))))))

(defn- gradeling-absorb-bladoid [grdl bl]
  (let [gra (grade grdl)]
    (if (< gra 0)
      (Gradeling. (grade bl) (list bl))
      (if (not= gra (grade bl))
        (throw (Exception. (str "can't absorb grade" (grade bl)
                                " blade into grade " gra
                                "gradeling...")))
        (if (bladoid-with-basis grdl (basis bl))
          (Gradeling. gra (map (fn [blel]
                                 (if (same-basis? bl blel)
                                   ;; (sum bl blel)
                                   ;; the foregoing can down-rung to zero, which
                                   ;; upsets the surrounding/transient machinery
                                   (Bladoid. (+ (coef blel) (coef bl))
                                             (basis bl))
                                   blel))
                               (bladoids grdl)))
          (Gradeling. gra (sort-on-bases (conj (bladoids grdl) bl))))))))

(defn- gradeling-absorb-gradeling [gra grb]
  (reduce gradeling-absorb-bladoid
          gra (bladoids grb)))


;;
;; Multivector is, mirabile dictu, a multivector: an collection of zero or
;; more Gradelings: which is to say an arbitrary collection of Bladoids
;;

(defprotocol IMV
  (gradelings [this])
  (gradeling-of-grade [this gra])
  (grades [this]))

(deftype MV [grdlgs]
  IMV
  (gradelings [_] grdlgs)
  (gradeling-of-grade [this gra]
    (first (filter (fn [grdl] (= (grade grdl) gra))
                   (gradelings this))))
  (grades [_] (map grade grdlgs))

  Object
  (toString [_] (str "MV["
                     (reduce (fn [sofar gr]
                               (str sofar (if (empty? sofar) "" " ") gr))
                             "" grdlgs)
                     "]")))

(defn mv [gradels]
  (MV. (sort-on-grades gradels)))


;;
;; Vect is, of course, a vector (meaning -- yes! -- a real geometric,
;; mathematical vector, for gods' sake; not an 'array', per lexical cretinism).
;; In a strict sense this is a redundant type-entity, since one of these
;; could always be represented as a Gradeling of grade one. But because
;; vectors have a vaunted position in GA -- they are rightly the generators
;; of the whole algebra, y' see-- it's worth representing them explicitly.
;;

(defprotocol IVect
  (coefs [this]))

(deftype Vect [cfs]
  IVect
  (coefs [_] cfs)

  IGradable
  (grade [_] 1)

  Object
  (toString [_] (str "v" cfs)))

(defn vect [& cs] (Vect. cs))



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
  (grade-part [this n] this))

(defn zero-element []
  (ZeroElement.))


;;
;; a shadowy cabal of utility-interior mathy bits.
;;

(defn- bladoid-mult-bladoid [bll blr]
  (let [basl (basis bll)
        basr (basis blr)]
    (Bladoid. (* (coef bll) (coef blr) (order-swap-parity basl basr))
              (basis-prod basl basr))))

(defn- mv-absorb-gradeling [emvy gr]
  (if (gradeling-of-grade emvy (grade gr))
    (MV. (map (fn [grel]
                (if (same-grade? grel gr)
                  (gradeling-absorb-gradeling grel gr)
                  grel))
              (gradelings emvy)))
    (MV. (sort-on-grades (conj (gradelings emvy) gr)))))

(defn- mv-absorb-bladoid [emvy bl]
  (mv-absorb-gradeling emvy (asGradeling bl)))

(defn- mv-absorb-mv [mva mvb]
  (reduce mv-absorb-gradeling
          mva (gradelings mvb)))

(defn- bladoid-mult-gradeling [bl gr]
  (reduce (fn [outmv bl_rt] (mv-absorb-bladoid outmv (prd bl bl_rt)))
          (MV. '())
          (bladoids gr)))

(defn- gradeling-mult-gradeling [grl grr]
  (reduce (fn [emvy bldlg] (mv-absorb-mv
                            emvy
                            (bladoid-mult-gradeling bldlg grr)))
          (MV. '())
          (bladoids grl)))

(defn- mv-mult-gradeling [emvy rt_grdl]
  (reduce (fn [out_mv lt_grdl]
            (mv-absorb-mv
             out_mv
             (gradeling-mult-gradeling lt_grdl rt_grdl)))
          (MV. '())
          (gradelings emvy)))

(defn- mv-map-biexploded-gradelings-and-sum [funq mvl mvr]
  (reduce mv-absorb-mv
          (MV. ())
          (for [grl (gradelings mvl)
                grr (gradelings mvr)]
            (asMV (funq grl grr)))))

(defn- do-unto-gradelings [funq mvl mvr]
  (accordion-down
   (mv-map-biexploded-gradelings-and-sum
    funq mvl mvr)))


;;
;; rung conversions: one, yea, unto another.
;;

(extend-type Bladoid
  IAsBladoid
  (asBladoid [this]
    this)

  IAsGradeling
  (asGradeling [this]
    (Gradeling. (grade this) (list this)))

  IAsMV
  (asMV [this]
    (asMV (asGradeling this))))


(extend-type Gradeling
  IAsGradeling
  (asGradeling [this]
    this)

  IAsMV
  (asMV [this]
    (MV. (list this))))


(extend-type MV
  IAsMV
  (asMV [this]
    this))


(extend-type Vect
  IAsGradeling
  (asGradeling [this]
    (Gradeling. 1 (filter (fn [bl] (not= 0 (coef bl)))
                          (map-indexed
                           (fn [ind cf] (bladoid cf (vector ind)))
                           (coefs this)))))

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
                       (sort-on-bases (list this otha)))
           (MV. (sort-on-grades (list (asGradeling this)
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
  (grade-part [this n]
    (if (= (grade this) n)
      this
      (zero-element))))


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
         (MV. (sort-on-grades (list this otha)))))))
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
  (grade-part [this n]
    (if (= (grade this) n)
      this
      (zero-element))))


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
      (do-unto-gradelings prd this (asMV otha))))
  (inv [this])
  (quo [this otha]
    (prd this (asMV (inv otha))))
  (dot [this otha]
    (do-unto-gradelings dot this (asMV otha)))
  (wdg [this otha]
    (do-unto-gradelings wdg this (asMV otha)))
  (grade-part [this n]
    (let [grdl (filter (fn [g] (= (grade g) n))
                       (gradelings this))]
      (if (empty? grdl)
        (zero-element)
        (first grdl)))))


(extend-type Vect
  IHestenic
  (scl [this s]
    (Vect. (map (fn [co] (* co s))
                (coefs this))))
  (sum [this otha]
    (if (different-rung? this otha)
      (sum (asMV this) (asMV otha))
      (Vect. (map (fn [hoo hah] (+ hoo hah))
                  (coefs this) (coefs otha)))))
  (neg [this]
    (Vect. (map (fn [co] (- co))
                (coefs this))))
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
            0 (map *
                   (coefs this) (coefs otha))))
  (wdg [this otha]
    (if (different-rung? this otha)
      (wdg (asMV this) (asMV otha))
      (grade-part (prd this otha) 2)))
  (grade-part [this n]
    (if (= n 1)
      this
      (zero-element))))


;;
;;
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
    (zero-element))
  (wdg [this otha]
    (scl otha this))
  (grade-part [this n]
    (if (= n 0)
      this
      (zero-element))))
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
