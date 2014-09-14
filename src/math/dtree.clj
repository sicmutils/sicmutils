(ns math.dtree
  (:gen-class))

(def empty-dtree {:step {} :stop false})

(defn dtree-insert
  [{:keys [step stop] :as dtree} op [predicate & predicates]]
  (prn "step" step)
  (prn "stop" stop)
  (prn "predicate" predicate)
  (prn "predicates" predicates)

  ;; if there is another predicate...
  (if predicate
    ;; next dtree is either the one that governs this predicate at
    ;; this stage, or a new empty one.
    (let [next-dtree (or (step predicate) empty-dtree)]
      ;; augment the binding at this level
      (assoc dtree :step
             (assoc step predicate (dtree-insert next-dtree op predicates))))
    ;; no more predicates? augment the current stop function.
    (do
      (if stop (prn "overwriting a binding!!"))
      (assoc dtree :stop op)))
  )
  

(prn (dtree-insert empty-dtree :op [:p1 :p2]))
(prn (dtree-insert empty-dtree :op [:p1]))
(prn (dtree-insert empty-dtree :op []))

(prn (dtree-insert (dtree-insert empty-dtree :op1 [:p1 :p2]) :op2 [:p1 :p3]))
(prn (dtree-insert (dtree-insert (dtree-insert empty-dtree :op1 [:p1 :p2]) :op2 [:p1 :p3])
                   :op3 [:p2 :p1]))
(prn (dtree-insert (dtree-insert (dtree-insert empty-dtree :op1 [:p1 :p2]) :op2 [:p2 :p1])
                   :op3 [:p2 :p1 :p3]))

(prn)
