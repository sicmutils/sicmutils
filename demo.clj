(def q
  ; See p. 17
  (up (literal-function 'x)
      (literal-function 'y)
      (literal-function 'z)))
(def literal-q (literal-function 'q))
(defn test-path
  "See p. 20"
  [t]
  (up (+ (* 4 t) 7)
      (+ (* 3 t) 5)
      (+ (* 2 t) 1)))
(defn make-η
  "See p. 21"
  [ν t1 t2]
  (fn [t]
    (* (- t t1) (- t t2) (ν t))))
(defn varied-free-particle-action
  "See p. 21"
  [mass q ν t1 t2]
  (fn [ε]
    (let [η (make-η ν t1 t2)]
      (Lagrangian-action (L-free-particle mass)
                         (+ q (* ε η)) t1 t2))))
(Lagrangian-action (L-free-particle 3.0) test-path 0.0 10.0)
((varied-free-particle-action 3.0 test-path (up sin cos square) 0.0 10.0) 0.001)
;(minimize (varied-free-particle-action 3.0 test-path (up sin cos square) 0.0 10.0) -2 1)
(defn δ
  "The variation operator (p. 28)."
  [η]
  (fn [f]
    ;; Define g(ε) as in Eq. 1.22; then δ_η f[q] = Dg(0)
    (fn [q]
      (let [g (fn [ε]
                (f (+ q (* ε η))))]
        
          ((D g) 0)))))

(defn F
  "A generic path function."
  [q]
  (fn [t]
    ((literal-function 'f) (q t))))

(defn G
  "Another generic path function."
  [q]
  (fn [t]
    ((literal-function 'g) (q t))))
(defn φ
  "A path transformation function"
  [f]
  (fn [q]
    (fn [t]
         ((literal-function 'φ) ((f q) t)))))
;; Exercise 1.7
(def δ_η (δ (literal-function 'eta)))
(((δ_η   identity) literal-q) 't)
(((δ_η          F) literal-q) 't)
(((δ_η          G) literal-q) 't)
(((δ_η   (* 'c G)) literal-q) 't) ; scalar multiplication of variation
(((δ_η    (* F G)) literal-q) 't) ; product rule for variation
(((δ_η    (+ F G)) literal-q) 't) ; sum rule for variation
(((δ_η      (φ F)) literal-q) 't) ; composition rule for variation
;; p. 34
(((Lagrange-equations (L-free-particle 'm)) (literal-function 'q)) 't)
;; p. 35
(((Lagrange-equations (L-free-particle 'm)) test-path) 't)
;; p.36
(defn proposed-solution [t]
  (* 'a (cos (+ (* 'omega t) 'phi))))
(((Lagrange-equations (L-harmonic 'm 'k)) proposed-solution) 't)
;; p. 40
(((Lagrange-equations (L-uniform-acceleration 'm 'g)) (up (literal-function 'x) (literal-function 'y))) 't)
;; p. 41
(((Lagrange-equations (L-central-rectangular 'm (literal-function 'U)))
  (up (literal-function 'x)
      (literal-function 'y)))
  't)
;; p. 43
(((Lagrange-equations (L-central-polar 'm (literal-function 'U)))
  (up (literal-function 'r)
      (literal-function 'phi)))
  't)
;; Coordinate transformation (p. 47)
(prn "coord transform")
(velocity ((F->C p->r)
           (->local 't (up 'r 'phi) (up 'rdot 'phidot))))
(defn L-alternate-central-polar
  [m U]
  (comp (L-central-rectangular m U) (F->C p->r)))
((L-alternate-central-polar 'm (literal-function 'U))
  (->local 't (up 'r 'φ) (up 'rdot 'φdot)))
(defn T-pend
  [m l g ys]
  (fn [local]
    (let [[t theta thetadot] local
          vys (D ys)]
      (* 1/2 m
         (+ (square (* l thetadot))
            (square (vys t))
            (* 2 l (vys t) thetadot (sin theta)))))))
(defn V-pend
  [m l g ys]
  (fn [local]
    (let [[t theta _] local]
      (* m g (- (ys t) (* l (cos theta)))))))

;(def L-pend (- T-pend V-pend))
; the following is a hack until we can subtract functions of arity > 1:
(defn L-pend [m l g ys]
  (let [T (T-pend m l g ys)
        V (V-pend m l g ys)]
    #(- (T %) (V %))))

(((Lagrange-equations (L-pend 'm 'l 'g (literal-function 'y_s)))
   (literal-function 'theta))
  't)

77
((T-pend 'm 'l 'g (literal-function 'y_s)) ((Γ (literal-function 'theta)) 't)  )
55
((V-pend 'm 'l 'g (literal-function 'y_s)) ((Γ (literal-function 'theta)) 't)  )
66
(- ((T-pend 'm 'l 'g (literal-function 'y_s)) ((Γ (literal-function 'theta)) 't)  )
   ((V-pend 'm 'l 'g (literal-function 'y_s)) ((Γ (literal-function 'theta)) 't)  ))
