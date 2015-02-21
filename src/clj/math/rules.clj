(ns math.rules
  (:require [pattern.rule :refer [ruleset]]))

(def ^:private => (constantly true))

(def flush-obvious-ones
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
