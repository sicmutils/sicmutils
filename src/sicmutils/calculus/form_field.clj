(ns sicmutils.calculus.form-field
  (:require [sicmutils
             [operator :as o]
             [structure :as s]
             [generic :as g]
             [function :as f]
             [value :as v]
             [expression :as x]]
            [sicmutils.calculus.basis :as b]
            [sicmutils.calculus.vector-field :as vf]
            [sicmutils.calculus.manifold :as m]))

(derive ::form-field ::o/operator)

(defn form-field?
  [f]
  (and (o/operator? f)
       (-> f :context :subtype (= ::form-field))))

(defn oneform-field?
  [f]
  (and (form-field? f)
       (-> f :context :rank (= 1))))

(defn procedure->oneform-field
  [fp name]
  (o/make-operator fp name
                   :subtype ::form-field
                   :rank 1
                   :arguments [::vf/vector-field]))

(declare wedge)
(defn procedure->nform-field
  [proc n name]
  (if (= n 0)
    (proc)
    (o/make-operator proc name
                     :subtype ::form-field
                     :arity [:exactly n]
                     :rank n
                     :arguments (repeat n ::vf/vector-field))))

(defn coordinate-name->ff-name
  "From the name of a coordinate, produce the name of the coordinate basis
  one-form field (as a symbol)"
  [n]
  (symbol (str \d n)))

(defn oneform-field-procedure
  [components coordinate-system]
  (fn [f]
    (s/mapr (fn [f]
              (assert (vf/vector-field? f))
              (f/compose (g/* components
                              (vf/vector-field->components f coordinate-system))
                         (m/chart coordinate-system)))
            f)))

(defn components->oneform-field
  [components coordinate-system & [name]]
  (let [name (or name `(~'oneform-field ~(v/freeze components)))]
    (procedure->oneform-field (oneform-field-procedure components coordinate-system) name)))

(defn oneform-field->components
  [form coordinate-system]
  {:pre [(form-field? form)]}
  (let [X (vf/coordinate-basis-vector-fields coordinate-system)]
    (f/compose (form X) #(m/point coordinate-system))))

;;; To get the elements of a coordinate basis for the 1-form fields

(defn coordinate-basis-oneform-field-procedure
  [coordinate-system & i]
  (fn [vf]
    (let [internal (fn [vf]
                     (assert (vf/vector-field? vf))
                     (vf (f/compose (apply s/component i) (m/chart coordinate-system))))]
      (s/mapr internal vf))))

(defn coordinate-basis-oneform-field
  [coordinate-system name & i]
  (procedure->oneform-field
   (apply coordinate-basis-oneform-field-procedure coordinate-system i)
   name))

(defn coordinate-basis-oneform-fields
  [coordinate-system]
  (let [prototype (s/mapr coordinate-name->ff-name (m/coordinate-prototype coordinate-system))]
    (s/mapr #(apply coordinate-basis-oneform-field coordinate-system %1 %2)
            prototype
            (s/structure->access-chains prototype))))

(defn function->oneform-field
  [f]
  {:pre [(fn? f)]}
  (procedure->oneform-field
    (fn [v] (s/mapr (fn [v]
                      (assert (vf/vector-field? v))
                      (fn [m] ((v f) m)))
                    v))
    `(~'d ~(m/diffop-name f))))

(defn literal-oneform-field
  [name coordinate-system]
  (let [n (:dimension (m/manifold coordinate-system))
        domain (apply s/up (repeat n 0))
        range 0
        components (s/generate n ::s/down #(f/literal-function
                                            (symbol (str name \_ %))
                                            domain
                                            range))]
    (components->oneform-field components coordinate-system name)))

(defn get-rank
  [f]
  (cond (o/operator? f) (or (:rank (:context f))
                            (throw (IllegalArgumentException. (str "operator, but not a differential form: " f))))
        (fn? f) 0
        :else (throw (IllegalArgumentException. "not a differential form"))))

(defn exterior-derivative-procedure
  [kform]
  (let [k (get-rank kform)]
    (if (= k 0)
      (function->oneform-field kform)
      (let [without #(concat (take %1 %2) (drop (inc %1) %2))
            k+1form (fn [& vectors]
                      (assert (= (count vectors) (inc k)))
                      (fn [point]
                        (let [n ((m/point->manifold point) :dimension)]
                          (if (< k n)
                            (reduce g/+ (for [i (range 0 (inc k))]
                                          (let [rest (without i vectors)]
                                            (g/+ (g/* (if (even? i) 1 -1)
                                                      (((nth vectors i) (apply kform rest))
                                                       point))
                                                 (reduce g/+ (for [j (range (inc i) (inc k))]
                                                               (g/* (if (even? (+ i j)) 1 -1)
                                                                    ((apply kform
                                                                            (cons
                                                                             (o/commutator (nth vectors i)
                                                                                           (nth vectors j))
                                                                             (without (dec j) rest)))
                                                                     point))))))))
                            0))))]
        (procedure->nform-field k+1form (inc k) `(~'d ~(m/diffop-name kform)))))))

(def d (o/make-operator exterior-derivative-procedure 'd))

(defn permutation-sequence
  "This is an unusual way to go about this in a functional language,
  but it's fun. Produces an iterable sequence developing the
  permutations of the input sequence of objects (which are considered
  distinct) in church-bell-changes order, that is, each permutation
  differs from the previous by a transposition of adjacent
  elements (Algorithm P from §7.2.1.2 of Knuth). This has the
  side-effect of arranging for the parity of the generated
  permutations to alternate; the first permutation yielded is the
  identity permutation (which of course is even). Inside, there is a
  great deal of mutable state, but this cannot be observed by the
  user."
  [as]
  (let [n (count as)
        a (object-array as)
        c (int-array n (repeat 0)) ;; P1. [Initialize.]
        o (int-array n (repeat 1))
        return #(into [] %)
        first (atom true)
        the-next (atom (return a))
        has-next (atom true)
        ;; step implements one-through of algorithm P up to step P2,
        ;; at which point we return false if we have terminated, true
        ;; if a has been set to a new permutation. Knuth's code is
        ;; one-based; this is zero-based.
        step (fn [j s]
               (let [q (+ (aget c j) (aget o j))] ;; P4. [Ready to change?]
                 (cond (< q 0)
                       (do ;; P7. [Switch direction.]
                         (aset o j (- (aget o j)))
                         (recur (dec j) s))

                       (= q (inc j))
                       (if (zero? j)
                         false ;; All permutations have been delivered.
                         (do (aset o j (- (aget o j))) ;; P6. [Increase s.]
                             (recur (dec j) (inc s)))) ;; P7. [Switch direction.]

                       :else ;; P5. [Change.]
                       (let [i1 (+ s (- j (aget c j)))
                             i2 (+ s (- j q))
                             t (aget a i1)
                             ]
                         (aset a i1 (aget a i2))
                         (aset a i2 t)
                         (aset c j q)
                         true ;; More permutations are forthcoming.
                         ))))]
    (iterator-seq
     (reify java.util.Iterator
       (hasNext [this] @has-next)
       (next [this]  ;; P2. [Visit.]
         (let [prev @the-next]
           (reset! has-next (step (dec n) 0))
           (reset! the-next (return a))
           prev))))))

(defn ^:private factorial
  [n]
  (reduce *' (range 2 (inc n))))

(defn ^:private wedge2
  [form1 form2]
  (let [n1 (get-rank form1)
        n2 (get-rank form2)]
    (if (or (zero? n1) (zero? n2))
      (g/* form1 form2)
      (let [n (+ n1 n2)
            w (fn [& args]
                (assert (= (count args) n) "Wrong number of args to wedge product")
                (g/* (/ 1 (factorial n1) (factorial n2))
                     (reduce g/+ (map (fn [permutation parity]
                                        (let [a1 (take n1 permutation)
                                              a2 (drop n1 permutation)]
                                          (g/* parity (apply form1 a1) (apply form2 a2))))
                                      (permutation-sequence args)
                                      (cycle [1 -1])))))]
        (procedure->nform-field w n `(~'wedge ~(m/diffop-name form1) ~(m/diffop-name form2)))))))

(defn wedge
  [& fs]
  (reduce wedge2 fs))
