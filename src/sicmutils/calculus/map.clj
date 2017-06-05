(ns sicmutils.calculus.map
  (:require [sicmutils
             [operator :as o]
             [structure :as s]
             [generic :as g]
             [function :as f]
             [value :as v]
             [expression :as x]]
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
