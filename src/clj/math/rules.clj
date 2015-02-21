(ns math.rules
  (:require [pattern.rule :refer [ruleset]]))

(def ^:private => (constantly true))

(def ^:private split-high-degree-cosines
  (let [more-than-two? #(> % 2)]
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
            (:?? a2)))))

(def ^:private split-high-degree-sines
  (let [more-than-two? #(> % 2)]
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
            (:?? a2)))))

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

(def sincos-flush-ones (comp split-high-degree-cosines
                             split-high-degree-sines
                             flush-obvious-ones))
