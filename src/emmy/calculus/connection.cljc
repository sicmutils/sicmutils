#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.calculus.connection
  (:require [emmy.calculus.basis :as b]
            [emmy.calculus.covariant :as cov]
            [emmy.calculus.manifold :as m]
            [emmy.calculus.metric :as metric]
            [emmy.function :as f]
            [emmy.generic :as g]
            [emmy.operator :as o]
            [emmy.structure :as s]
            [emmy.util :as u]))

;; A metric induces a torsion-free connection.

(defn make-Christoffel-1
  "Returns a data structure representing [Christoffel symbols of the first
  kind](https://en.wikipedia.org/wiki/Christoffel_symbols#Christoffel_symbols_of_the_first_kind)."
  [symbols basis]
  {:type ::Christoffel-1
   :symbols symbols
   :basis basis})

(defn metric->Christoffel-1 [metric basis]
  {:pre [(b/coordinate-basis? basis)]}
  (let [vector-basis (b/basis->vector-basis basis)
        half (g// 1 2)]
    (make-Christoffel-1
     (s/mapr
      (fn [e_k]
        (s/mapr
         (fn [e_j]
           (s/mapr
            (fn [e_i]
              (g/* half
                   (g/- (g/+ (e_k (metric e_i e_j))
                             (e_j (metric e_i e_k)))
                        (e_i (metric e_j e_k)))))
            vector-basis))
         vector-basis))
      vector-basis)
     basis)))

(defn metric->Christoffel-2 [metric basis]
  {:pre [(b/coordinate-basis? basis)]}
  (let [metric (f/memoize
                (comp f/memoize metric))
        gi (metric/invert metric basis)
        vector-basis (b/basis->vector-basis basis)
        oneform-basis (b/basis->oneform-basis basis)
        half (g// 1 2)]
    (cov/make-Christoffel
     (s/mapr
      (fn [e_k]
        (s/mapr
         (fn [e_j]
           (s/mapr
            (fn [w_i]
              (b/contract
               (fn [e_m w_m]
                 (g/* (gi w_i w_m)
                      (g/* half
                           (g/- (g/+ (e_k (metric e_m e_j))
                                     (e_j (metric e_m e_k)))
                                (e_m (metric e_j e_k))))))
               basis))
            oneform-basis))
         vector-basis))
      vector-basis)
     basis)))

(defn- literal-Christoffel-names
  [name [s0 s1 s2 :as scripts] n]
  {:pre [(= s0 s1)]}
  (letfn [(tex [s]
            (or (s/orientation->separator s)
                (u/illegal (str "Bad scripts: " scripts))))
          (Gijk [i j k]
            (symbol
             (str name (tex s0) i j (tex s2) k)))]
    (s/generate
     n s0
     (fn [i]
       (s/generate
        n s1
        (fn [j]
          (s/generate
           n s2
           (fn [k]
             (Gijk i j k)))))))))

(defn literal-Christoffel-1 [name coordsys]
  (let [n (:dimension (m/manifold coordsys))]
    (make-Christoffel-1
     (s/mapr (fn [name]
               (m/literal-manifold-function name coordsys))
             (literal-Christoffel-names
              name [::s/down ::s/down ::s/down] n))
     (b/coordinate-system->basis coordsys))))

(defn literal-Christoffel-2 [name coordsys]
  (let [n (:dimension (m/manifold coordsys))]
    (cov/make-Christoffel
     (s/mapr (fn [name]
               (m/literal-manifold-function name coordsys))
             (literal-Christoffel-names
              name [::s/down ::s/down ::s/up] n))
     (b/coordinate-system->basis coordsys))))

(defn literal-Cartan [name coordsys]
  (cov/Christoffel->Cartan
   (literal-Christoffel-2 name coordsys)))

;; Connections for non-coordinate basis -- MTW p.210

;; c_ijk = g_kl c_ij^l = g_kl e^l([e_i, e_j])

(defn structure-constant [e_i e_j e_k basis metric]
  (b/contract
   (fn [e_l w_l]
     (g/* (metric e_k e_l)
          (w_l (o/commutator e_i e_j))))
   basis))

(defn metric->connection-1 [metric basis]
  (let [vector-basis (b/basis->vector-basis basis)
        half (g// 1 2)]
    (cov/make-Christoffel
     (s/mapr
      (fn [e_k]
        (s/mapr
         (fn [e_j]
           (s/mapr
            (fn [e_i]
              (g/* half
                   (g/+ (g/- (g/+ (e_k (metric e_i e_j))
                                  (e_j (metric e_i e_k)))
                             (e_i (metric e_j e_k)))
                        (g/- (g/+ (structure-constant
                                   e_i e_j e_k basis metric)
                                  (structure-constant
                                   e_i e_k e_j basis metric))
                             (structure-constant
                              e_j e_k e_i basis metric)))))
            vector-basis))
         vector-basis))
      vector-basis)
     basis)))

(defn metric->connection-2 [metric basis]
  (let [vector-basis (b/basis->vector-basis basis)
        oneform-basis (b/basis->oneform-basis basis)
        inverse-metric (metric/invert metric basis)
        half (g// 1 2)]
    (cov/make-Christoffel
     (s/mapr
      (fn [e_k]
        (s/mapr
         (fn [e_j]
           (s/mapr
            (fn [w_i]
              (b/contract
               (fn [e_m w_m]
                 (g/* (inverse-metric w_i w_m)
                      (g/*
                       half
                       (g/+ (g/- (g/+ (e_k (metric e_m e_j))
                                      (e_j (metric e_m e_k)))
                                 (e_m (metric e_j e_k)))
                            (g/- (g/+ (structure-constant
                                       e_m e_j e_k basis metric)
                                      (structure-constant
                                       e_m e_k e_j basis metric))
                                 (structure-constant
                                  e_j e_k e_m basis metric))))))
               basis))
            oneform-basis))
         vector-basis))
      vector-basis)
     basis)))
