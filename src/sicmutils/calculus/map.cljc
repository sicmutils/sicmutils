;
; Copyright © 2017 Colin Smith.
; This work is based on the Scmutils system of MIT/GNU Scheme:
; Copyright © 2002 Massachusetts Institute of Technology
;
; This is free software;  you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3 of the License, or (at
; your option) any later version.
;
; This software is distributed in the hope that it will be useful, but
; WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
; General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this code; if not, see <http://www.gnu.org/licenses/>.
;

(ns sicmutils.calculus.map
  (:require [sicmutils.abstract.function :as af]
            [sicmutils.calculus.basis :as b]
            [sicmutils.calculus.form-field :as ff]
            [sicmutils.calculus.vector-field :as vf]
            [sicmutils.calculus.manifold :as m]
            [sicmutils.function :as f]
            [sicmutils.structure :as s]))

(defn vector-field->vector-field-over-map
  "FDG p.72"
  [mu:N->M]
  (fn [v-on-M]
    (vf/procedure->vector-field
     (fn [f-on-M]
       (f/compose (v-on-M f-on-M) mu:N->M))
     `((~'vector-field->vector-field-over-map ~(m/diffop-name mu:N->M)) ~(m/diffop-name v-on-M)))))

(defn differential
  "FDG p.72"
  [mu:N->M]
  (fn [v-on-N]
    (let [v-on-M (fn [g-on-M] (v-on-N (f/compose g-on-M mu:N->M)))]
      (assert (vf/vector-field? v-on-N))
      (vf/procedure->vector-field v-on-M
                                  `((~'d ~(m/diffop-name mu:N->M)) ~(m/diffop-name v-on-N))))))

(defn literal-manifold-map
  [name source target]
  (let [n (:dimension (m/manifold source))
        m (:dimension (m/manifold target))
        domain (if (= n 1) [0] (apply s/up (repeat n 0)))]
    (f/compose (m/point target)
               (s/generate m ::s/up #(af/literal-function
                                      (symbol (str name "↑" %)) domain 0))
               (m/chart source))))

(defn form-field->form-field-over-map
  [mu:N->M]
  (fn [w-on-M]
    (let [make-fake-vector-field (fn [V-over-mu n]
                                   (let [u (fn [f]
                                             (fn [_]
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
       `((~'form-field->form-field-over-map ~(m/diffop-name mu:N->M))
         ~(m/diffop-name w-on-M))))))

(defn basis->basis-over-map
  [mu:N->M basis-on-M]
  (let [vector-basis-on-M (b/basis->vector-basis basis-on-M)
        dual-basis-on-M (b/basis->oneform-basis basis-on-M)]
    (b/make-basis (s/mapr (vector-field->vector-field-over-map mu:N->M) vector-basis-on-M)
                  (s/mapr (form-field->form-field-over-map mu:N->M) dual-basis-on-M))))

(defn pullback-function
  [mu:N->M]
  (fn [f-on-M]
    (f/compose f-on-M mu:N->M)))

(defn pushforward-vector
  [mu:N->M mu-inverse:M->N]
  (fn [v-on-N]
    (vf/procedure->vector-field
     #(f/compose (v-on-N (f/compose % mu:N->M)) mu-inverse:M->N)
     `((~'pushforward ~(m/diffop-name mu:N->M)) ~(m/diffop-name v-on-N)))))

(defn pullback-vector-field
  [mu:N->M mu-inverse:M->N]
  (pushforward-vector mu-inverse:M->N mu:N->M))

(defn pullback-form
  "Returns a function which will pull a form back across a map (without needing its inverse)"
  [mu:N->M]
  (fn [omega-on-M]
    (let [k (ff/get-rank omega-on-M)]
      (if (zero? k)
        ((pullback-function mu:N->M) omega-on-M)
        (ff/procedure->nform-field
         (fn [& vectors-on-N]
           (apply ((form-field->form-field-over-map mu:N->M) omega-on-M)
                  (map (differential mu:N->M) vectors-on-N)))
         k
         `((~'pullback ~(m/diffop-name mu:N->M)) ~(m/diffop-name omega-on-M)))))))

(defn pullback
  ([mu:N->M mu-inverse:M->N]
   (fn [thing]
     (if (vf/vector-field? thing)
      (do
        (assert mu-inverse:M->N "Pullback of a vector requires inverse map")
        ((pullback-vector-field mu:N->M mu-inverse:M->N) thing))
      ((pullback-form mu:N->M) thing))))
  ([mu:N->M]
   (pullback mu:N->M nil)))
