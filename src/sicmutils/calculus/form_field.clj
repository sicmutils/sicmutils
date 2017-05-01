(ns sicmutils.calculus.form-field
  (:require [sicmutils.operator :as o]
            [sicmutils.structure :as s]
            [sicmutils.calculus.vector-field :as vf]
            [sicmutils.generic :as g]
            [sicmutils.function :as f]
            [sicmutils.calculus.manifold :as m]))


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
  ;; TODO: constrain argument type to vector field
  (o/make-operator fp name :subtype ::form-field :rank 1))

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
  [components coordinate-system name]
  (procedure->oneform-field (oneform-field-procedure components coordinate-system) name))

;(defn oneform-field->components
;  [form coordinate-system]
;  (assert (form-field? form))
;  (let [X (csvb)]))
