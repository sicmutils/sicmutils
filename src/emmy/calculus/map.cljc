#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.calculus.map
  "Various definitions and utilities for working with maps between manifolds."
  (:require [emmy.abstract.function :as af]
            [emmy.calculus.basis :as b]
            [emmy.calculus.form-field :as ff]
            [emmy.calculus.manifold :as m]
            [emmy.calculus.vector-field :as vf]
            [emmy.function :as f]
            [emmy.structure :as s]
            [emmy.util :as u]
            [emmy.value :as v]))

;; ## Maps between Manifolds
;;
;; If we have a function on a manifold M and a map from manifold N to manifold M
;; we can define a function on N:

(defn pullback-function
  [mu:N->M]
  (fn [f-on-M]
    (f/compose f-on-M mu:N->M)))

;; If we have an inverse map mu^-1:M->N, we can push a function
;; on N forward through the map:

(defn pushforward-function [mu-inverse:M->N]
  (fn [f-on-N]
    (f/compose f-on-N mu-inverse:M->N)))

;; The map between manifolds induces various ways to transport vectors from one
;; manifold to another. The simplest of these is the differential.

;; The differential of a function mu:N->M from N to M takes a vector field on
;; the source manifold N to a vector field-like operator on the target manifold
;; M. This results in a vector field over the map mu:N->M. The result takes
;; directional derivatives of functions defined on M, at points of M that are
;; targets of points of N.

(defn differential-of-map
  "Defined on FDG p.72."
  [mu:N->M]
  (fn [v-on-N]
    {:pre [(vf/vector-field? v-on-N)]}
    (let [v-on-M (fn [g-on-M]
                   (v-on-N
                    (f/compose g-on-M mu:N->M)))
          name `((~'d ~(v/freeze mu:N->M))
                 ~(v/freeze v-on-N))]
      (vf/procedure->vector-field v-on-M name))))

(def ^{:doc "Alias for [[differential-of-map]]."}
  differential
  differential-of-map)

;; For a long time we were confused between the concepts of differential and
;; pushforward. The resolution seems to be that the differential takes the
;; manifold position in the source manifold and the pushforward takes the
;; manifold position in the target manifold of the map. So the pushforward needs
;; an inverse map to define it and so the pushforward is not a very useful idea.

(defn pushforward-vector
  [mu:N->M mu-inverse:M->N]
  (fn [v-on-N]
    (let [op (fn [f]
               (f/compose (v-on-N (f/compose f mu:N->M))
                          mu-inverse:M->N))
          name `((~'pushforward ~(v/freeze mu:N->M))
                 ~(v/freeze v-on-N))]
      (vf/procedure->vector-field op name))))

(defn literal-manifold-map
  [name source target]
  (let [n (:dimension (m/manifold source))
        m (:dimension (m/manifold target))
        domain (if (= n 1) 0 (s/up* (repeat n 0)))
        range  (s/up* (repeat m 0))]
    (f/compose (m/point target)
               (af/literal-function name domain range)
               (m/chart source))))

;; Another way to obtain a vector field over a map is to start with a vector
;; field on the target manifold. Given a vector field v-on-M and a map mu:N->M,
;; we obtain a vector field over the map. This is a thing like a vector field on
;; M restricted to the targets of mu:N->M and evaluated on points of N.

(defn vector-field->vector-field-over-map
  "Defined on FDG p.72."
  [mu:N->M]
  (fn [v-on-M]
    (let [op (fn [f-on-M]
               (f/compose (v-on-M f-on-M)
                          mu:N->M))
          name `((~'vector-field->vector-field-over-map ~(v/freeze mu:N->M))
                 ~(v/freeze v-on-M))]
      (vf/procedure->vector-field op name))))

;; A form field can also be transported across a map.  Given a form
;; field on M and a map mu:N->M, we obtain a thing like a form field
;; on M that measures vectors over the map mu:N->M and is evaluated
;; on points of N.

(defn form-field->form-field-over-map
  [mu:N->M]
  (fn [w-on-M]
    (letfn [(make-fake-vector-field [V-over-mu n]
              (vf/procedure->vector-field
               (fn [f]
                 (fn [_]
                   ((V-over-mu f) n)))
               `(~'make-fake-vector-field
                 ~(v/freeze V-over-mu))))
            (op [& vectors-over-map]
              (assert (= (count vectors-over-map)
                         (ff/get-rank w-on-M)))
              (fn [n]
                ((apply w-on-M
                        (map (fn [V-over-mu]
                               (make-fake-vector-field V-over-mu n))
                             vectors-over-map))
                 (mu:N->M n))))]
      (let [rank (ff/get-rank w-on-M)
            name `((~'form-field->form-field-over-map ~(v/freeze mu:N->M))
                   ~(v/freeze w-on-M))]
        (ff/procedure->nform-field op rank name)))))

(defn basis->basis-over-map
  [mu:N->M basis-on-M]
  (let [vector-basis-on-M (b/basis->vector-basis basis-on-M)
        dual-basis-on-M   (b/basis->oneform-basis basis-on-M)]
    (b/make-basis
     (s/mapr (vector-field->vector-field-over-map mu:N->M)
             vector-basis-on-M)
     (s/mapr (form-field->form-field-over-map mu:N->M)
             dual-basis-on-M))))

(defn pullback-form
  "Returns a function which will pull a form back across a map (without needing
  its inverse)"
  [mu:N->M]
  (fn [omega-on-M]
    (let [k (ff/get-rank omega-on-M)]
      (if (zero? k)
        ((pullback-function mu:N->M) omega-on-M)
        (let [op (fn [& vectors-on-N]
                   (apply ((form-field->form-field-over-map mu:N->M) omega-on-M)
                          (map (differential mu:N->M)
                               vectors-on-N)))
              name `((~'pullback ~(v/freeze mu:N->M))
                     ~(v/freeze omega-on-M))]
          (ff/procedure->nform-field op k name))))))

(defn pullback-vector-field
  [mu:N->M mu-inverse:M->N]
  (pushforward-vector mu-inverse:M->N mu:N->M))

(defn pullback
  ([mu:N->M] (pullback mu:N->M nil))
  ([mu:N->M mu-inverse:M->N]
   (fn [thing]
     (if (vf/vector-field? thing)
       (if (nil? mu-inverse:M->N)
         (u/illegal "Pullback of a vector requires inverse map")
         ((pullback-vector-field mu:N->M mu-inverse:M->N) thing))
       ((pullback-form mu:N->M) thing)))))
