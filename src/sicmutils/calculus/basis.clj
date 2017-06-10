(ns sicmutils.calculus.basis)
(defn make-basis
  "Make a basis object out of a vector and dual basis. The dimensions must agree."
  [vector-basis dual-basis]
  (let [d (count (flatten vector-basis))]
    (assert (= (count (flatten dual-basis)) d))
    {:type ::basis
     :dimension d
     :vector-basis vector-basis
     :oneform-basis dual-basis}))

(defn basis->oneform-basis
  "Extract the dual basis from the given basis object."
  [b]
  {:pre [(= (:type b) ::basis)]}
  (:oneform-basis b))

(defn basis->vector-basis
  "Extract the vector basis from the given basis object."
  [b]
  {:pre [(= (:type b) ::basis)]}
  (:vector-basis b))
