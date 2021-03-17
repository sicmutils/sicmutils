;;
;; Copyright © 2021 Sam Ritchie.
;; This work is based on the Scmutils system of MIT/GNU Scheme:
;; Copyright © 2002 Massachusetts Institute of Technology
;;
;; This is free software;  you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or (at
;; your option) any later version.
;;
;; This software is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this code; if not, see <http://www.gnu.org/licenses/>.
;;

(ns sicmutils.calculus.connection
  (:require [sicmutils.calculus.basis :as b]
            [sicmutils.calculus.covariant :as cov]
            [sicmutils.calculus.coordinate :as cc]
            [sicmutils.calculus.form-field :as ff]
            [sicmutils.calculus.manifold :as m]
            [sicmutils.calculus.vector-field :as vf]
            [sicmutils.generic :as g]
            [sicmutils.operator :as o]
            [sicmutils.structure :as s]
            [sicmutils.util :as u]
            [sicmutils.value :as v]))

;; A metric induces a torsion-free connection

;; We reserve *Christoffel* and Christoffel? for Christoffel type 2

(defn make-Christoffel-1 [symbols basis]
  (list '*Christoffel-1* symbols basis))

(declare coordinate-basis?)

(defn metric->Christoffel-1 [metric basis]
  {:pre [(coordinate-basis? basis)]}
  (let [vector-basis (b/basis->vector-basis basis)]
    (make-Christoffel-1
     (s/mapr
      (fn [e_k]
		    (s/mapr
         (fn [e_j]
			     (s/mapr
            (fn [e_i]
				      (* (/ 1 2)
                 (- (+ (e_k (metric e_i e_j))
						           (e_j (metric e_i e_k)))
						        (e_i (metric e_j e_k)))))
				    vector-basis))
			   vector-basis))
	    vector-basis)
     basis)))

(comment
  (def two-sphere R2-rect)
  (install-coordinates two-sphere (up 'theta 'phi))

  ;; probably has to use with-coordinates...
  (defn g-sphere [R]
    (fn [u v]
      (* (square R)
         (+ (* (dtheta u) (dtheta v))
	          (* (compose (square sin) theta)
	             (dphi u)
	             (dphi v))))))

  (pec
   ((Christoffel->symbols
     (metric->Christoffel-1 (g-sphere 'R)
			                      (coordinate-system->basis two-sphere)))
    ((two-sphere '->point) (s/up 'theta0 'phi0))))
  ;; Result:
  (down
   (down
    (down 0 0) (down 0 (* (* (cos theta0) (sin theta0)) (expt R 2))))
   (down
    (down 0 (* (* (cos theta0) (sin theta0)) (expt R 2)))
    (down (* (* -1 (cos theta0) (sin theta0)) (expt R 2)) 0))))

(declare metric:invert)

(defn metric->Christoffel-2 [metric basis]
  #_{:pre [(coordinate-basis? basis)]}
  (let [gi (metric:invert metric basis)
        vector-basis  (b/basis->vector-basis basis)
        oneform-basis (b/basis->oneform-basis basis)]
    (cov/make-Christoffel
     (s/mapr
      (fn [e_k]
		    (s/mapr
         (fn [e_j]
			     (s/mapr
            (fn [w_i]
					    (b/contract
					     (fn [e_m w_m]
					       (* (gi w_i w_m)
					          (* (/ 1 2)
						           (- (+ (e_k (metric e_m e_j))
						                 (e_j (metric e_m e_k)))
						              (e_m (metric e_j e_k))))))
					     basis))
				    oneform-basis))
			   vector-basis))
		  vector-basis)
     basis)))


(comment
  (pec ((Christoffel->symbols
         (metric->Christoffel-2 (g-sphere 'R)
			                          (coordinate-system->basis two-sphere)))
        ((two-sphere '->point) (up 'theta0 'phi0))))
  ;; Result:
  (down
   (down (up 0 0) (up 0 (/ (cos theta0) (sin theta0))))
   (down (up 0 (/ (cos theta0) (sin theta0)))
         (up (* -1 (sin theta0) (cos theta0)) 0))))

(declare literal-Christoffel-names
         coordinate-system-dimension)

(comment
  (defn literal-Christoffel-names [name scripts n]
    (define (Gijk i j k)
      (define (tex s)
        (cond ((eq? s 'up) "^")
	            ((eq? s 'down) "_")
	            (else (error "Bad scripts"))))
      (string->symbol
       (string-append (symbol->string name)
		                  (tex (car scripts))
		                  (number->string i)
		                  (number->string j)
		                  (tex (caddr scripts))
		                  (number->string k))))
    (assert (eq? (car scripts) (cadr scripts)))
    (s:generate n (car scripts)
                (fn (i)
                  (s:generate n (cadr scripts)
	                            (fn (j)
	                              (s:generate n (caddr scripts)
	                                          (fn (k)
	                                            (Gijk i j k)))))))))

(defn literal-Christoffel-1 [name coordsys]
  (let [n (coordinate-system-dimension coordsys)]
    (make-Christoffel-1
     (s/mapr (fn [name]
		           (m/literal-manifold-function name coordsys))
	           (literal-Christoffel-names name '(down down down) n))
     (b/coordinate-system->basis coordsys))))

(defn literal-Christoffel-2 [name coordsys]
  (let [n (coordinate-system-dimension coordsys)]
    (cov/make-Christoffel
     (s/mapr (fn [name]
		           (m/literal-manifold-function name coordsys))
	           (literal-Christoffel-names name '(down down up) n))
     (b/coordinate-system->basis coordsys))))

;; Connections for non-coordinate basis -- MTW p.210

;; c_ijk = g_kl c_ij^l = g_kl e^l([e_i, e_j])

(defn structure-constant [e_i e_j e_k basis metric]
  (b/contract
   (fn [e_l w_l]
     (g/* (metric e_k e_l)
	        (w_l (o/commutator e_i e_j))))
   basis))

(defn metric->connection-1 [metric basis]
  (let [vector-basis  (b/basis->vector-basis basis)
	      oneform-basis (b/basis->oneform-basis basis)]
    (cov/make-Christoffel
     (s/mapr
      (fn [e_k]
	      (s/mapr
	       (fn [e_j]
	         (s/mapr
	          (fn [e_i]
	            (* (/ 1 2)
                 (+ (- (+ (e_k (metric e_i e_j))
			                    (e_j (metric e_i e_k)))
			                 (e_i (metric e_j e_k)))
			              (- (+ (structure-constant e_i e_j e_k basis metric)
			                    (structure-constant e_i e_k e_j basis metric))
			                 (structure-constant e_j e_k e_i basis metric)))))
	          vector-basis))
	       vector-basis))
      vector-basis)
     basis)))

(defn metric->connection-2 [metric basis]
  (let [vector-basis   (b/basis->vector-basis basis)
	      oneform-basis  (b/basis->oneform-basis basis)
	      inverse-metric (metric:invert metric basis)]
    (cov/make-Christoffel
     (s/mapr
      (fn [e_k]
	      (s/mapr
	       (fn [e_j]
	         (s/mapr
	          (fn [w_i]
	            (b/contract
	             (fn [e_m w_m]
		             (* (inverse-metric w_i w_m)
		                (* (/ 1 2)
                       (+ (- (+ (e_k (metric e_m e_j))
				                        (e_j (metric e_m e_k)))
				                     (e_m (metric e_j e_k)))
			                    (- (+ (structure-constant e_m e_j e_k basis metric)
				                        (structure-constant e_m e_k e_j basis metric))
				                     (structure-constant e_j e_k e_m basis metric))))))
	             basis))
	          oneform-basis))
	       vector-basis))
      vector-basis)
     basis)))
