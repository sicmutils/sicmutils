(ns math.rules
  (:require [pattern.rule :refer [ruleset rule-simplifier]]))

(def ^:private => (constantly true))

(defn- more-than-two? [x] (and (number? x) (> x 2)))
(defn- at-least-two? [x] (and (number? x) (>= x 2)))
(defn- even-integer? [x] (and (number? x) (even? x)))

(def sin-sq->cos-sq
  (rule-simplifier
   (ruleset
    (expt (sin (:? x)) (:? n at-least-two?))
    => (* (expt (sin (:? x)) (:? #(- (% 'n) 2)))
          (- 1 (expt (cos (:? x)) 2))))))

(def ^:private split-high-degree-cosines
  (ruleset
   (* (:?? f1) (expt (cos (:? x)) (:? n more-than-two?)) (:?? f2))
   => (* (expt (cos (:? x)) 2)
         (expt (cos (:? x)) (:? #(- (% 'n) 2)))
         (:?? f1)
         (:?? f2))

   (+ (:?? a1) (expt (cos (:? x)) (:? n more-than-two?)) (:?? a2))
   => (+ (* (expt (cos (:? x)) 2)
            (expt (cos (:? x)) (:? #(- (% 'n) 2))))
         (:?? a1)
         (:?? a2))))

(def ^:private split-high-degree-sines
  (ruleset
   (* (:?? f1) (expt (sin (:? x)) (:? n more-than-two?)) (:?? f2))
   => (* (expt (sin (:? x)) 2)
         (expt (sin (:? x)) (:? #(- (% 'n) 2)))
         (:?? f1)
         (:?? f2))

   (+ (:?? a1) (expt (sin (:? x)) (:? n more-than-two?)) (:?? a2))
   => (+ (* (expt (sin (:? x)) 2)
            (expt (sin (:? x)) (:? #(- (% 'n) 2))))
         (:?? a1)
         (:?? a2))))

(def simplify-square-roots
  (rule-simplifier
   (ruleset
    (expt (sqrt (:? x)) (:? n even-integer?))
    => (expt (:? x) (:? #(/ (% 'n) 2)))

    (sqrt (expt (:? x) (:? n even-integer?)))
    => (expt (:? x) (:? #(/ (% 'n) 2)))

    ;; others to follow
    )))

(def divide-numbers-through
  (ruleset
   (* 1 (:? factor))
   => (:? factor)

   (* 1 (:?? factors))
   => (* (:?? factors))

   (/ (:? n number?) (:? d number?))
   => (:? #(/ (% 'n) (% 'd)))

   (/ (+ (:?? terms)) (:? d number?))
   => (+ (:?? #(map (fn [n] `(~'/ ~n ~(% 'd))) (% 'terms))))))

(def ^:private flush-obvious-ones
  (ruleset
   (+ (:?? a1) (expt (sin (:? x)) 2) (:?? a2) (expt (cos (:? x)) 2) (:?? a3))
   => (+ 1 (:?? a1) (:?? a2) (:?? a3)))
  ;; are sines always before cosines after we poly simplify?
  ;; they are in scmutils, so we should be alert for this.
  ;; in scmutils, there are a couple of others that involve rcf:simplify,
  ;; which we dont' have, and we don't know if pcf:simplify is an
  ;; acceptable substitute here; and we don't have a method for
  ;; pasting the value of a predicate into a rule, so this is far from
  ;; complete.
  )

(def sincos-flush-ones (rule-simplifier flush-obvious-ones
                                        split-high-degree-sines
                                        split-high-degree-cosines))
