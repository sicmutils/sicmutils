(ns sicmutils.calculus.map
  (:require [sicmutils
             [operator :as o]
             [structure :as s]
             [generic :as g]
             [function :as f]
             [value :as v]
             [expression :as x]]
            [sicmutils.calculus.coordinate :as c]
            [sicmutils.calculus.vector-field :as vf]
            [sicmutils.calculus.form-field :as ff]
            [sicmutils.calculus.manifold :as m]))

(defn vector-field->vector-field-over-map
  "FDG p.72"
  [mu:N->M]
  (fn [v-on-M]
    (vf/procedure->vector-field
     (fn [f-on-M]
       (f/compose (v-on-M f-on-M) mu:N->M))
     `((~'vector-field->vector-field-over-map ~(ff/diffop-name mu:N->M)) ~(ff/diffop-name v-on-M)))))

(defn differential
  "FDG p.72"
  [mu:N->M]
  (fn [v-on-N]
    (let [v-on-M (fn [g-on-M] (v-on-N (f/compose g-on-M mu:N->M)))]
      (assert (vf/vector-field? v-on-N))
      (vf/procedure->vector-field v-on-M
                                  `((~'d ~(ff/diffop-name mu:N->M)) ~(ff/diffop-name v-on-N))))))

(defn literal-manifold-map
  [name source target]
  (let [n (:dimension (m/manifold source))
        m (:dimension (m/manifold target))
        domain (if (= n 1) [0] (apply s/up (repeat n 0)))]
    (f/compose (m/point target)
               (s/generate m ::s/up #(f/literal-function (symbol (str name "↑" %)) domain 0))
               (m/chart source))))

(defn form-field->form-field-over-map
  [mu:N->M]
  (fn [w-on-M]
    (let [make-fake-vector-field (fn [V-over-mu n]
                                   (let [u (fn [f]
                                             (fn [m]
                                               ((V-over-mu f) n)))]
                                     (vf/procedure->vector-field u)))]
      (ff/procedure->nform-field
       (fn [& vectors-over-map]
         (fn [n]
           ((apply w-on-M
                   (map (fn [V-over-mu] (make-fake-vector-field V-over-mu n))
                        vectors-over-map))
            (mu:N->M n))))
       (ff/get-rank w-on-M)
       `((~'form-field->form-field-over-map ~(ff/diffop-name mu:N->M))
         ~(ff/diffop-name w-on-M))))))

(defn basis->basis-over-map
  [mu:N->M basis-on-M]
  (let [vector-basis-on-M (c/basis->vector-basis basis-on-M)
        dual-basis-on-M (c/basis->oneform-basis basis-on-M)]
    (c/make-basis (s/mapr (vector-field->vector-field-over-map mu:N->M) vector-basis-on-M)
                  (s/mapr (form-field->form-field-over-map mu:N->M) dual-basis-on-M))))
