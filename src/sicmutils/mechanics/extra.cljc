;;
;; Copyright © 2017 Colin Smith.
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

(ns sicmutils.mechanics.extra
  (:refer-clojure :exclude [+ - * / partial])
  (:require [sicmutils.calculus.derivative :refer [D partial]]
            [sicmutils.function :as f]
            [sicmutils.generic :as g :refer [+ - *]]
            [sicmutils.matrix :as matrix]
            [sicmutils.mechanics.hamilton :as h]
            [sicmutils.mechanics.lagrange :as l]
            [sicmutils.structure :as s :refer [up]]))

;; It is sometimes convenient to be able to split a state.

(defn ->parts
  "cont = (lambda (t q p-or-qdot) ...)"
  [state cont]
  (apply cont state))

(defn with-state [cont]
  (fn [state]
    (->parts state cont)))

(defn with-dynamic-state [cont]
  (fn [[t q pv]]
    #_(cont t q (if (vector? pv)
			            (matrix/num-rows pv)
			            pv))))

(defn flatten-state [state]
  #_(list->vector
     (apply append
	          (map (fn [x]
		               (cond (vector? x) (vector->list x)
		                     (matrix/column? x) (vector->list (column->vector x))
		                     (matrix/row? x) (vector->list (row->vector x))
		                     :else (list x)))
	               (vector->list state)))))

(def local->istate flatten-state)
(def H-state->istate flatten-state)

(defn ->istate [& args]
  (flatten-state
   (s/up* args)))

(defn flat-state->t [[t]] t)

;; Assumes k state vector components: [t q q' ... q^(k)]
;;  Needs to know n (degrees of freedom).

(defn unflatten-L-state
  ([flat-state]
   (let [n (-> (count flat-state)
               (dec)
               (quot 2))]
     (unflatten-L-state flat-state n)))
  ([flat-state n]
   (let [kn+1 (count flat-state)
	       kn   (dec kn+1)
	       k    (quot kn n)]
     (assert (zero? (rem kn n)))
     (if (= n 1)
	     flat-state
       (s/generate (inc k) ::s/up
                   (fn [i]
		                 (if (zero? i)
			                 (get flat-state 0)
			                 (matrix/up->column-matrix
			                  (subvec (vec flat-state)
				                        (inc (* (dec i) n))
				                        (inc (* i n)))))))))))

(def istate->local unflatten-L-state)
(def istate->t l/time)

;;; Assumes that only t,q,p are present.

(defn unflatten-H-state [flat-state]
  (let [two-n+1 (count flat-state)
	      two-n (dec two-n+1)
	      n (quot two-n 2)
        ;; TODO add a structural subvec
        statev (vec flat-state)]
    (assert (odd? two-n+1))
    (if (= n 1)
	    flat-state
	    (h/->H-state
	     (get flat-state 0)
	     (matrix/column* (subvec statev 1 (inc n)))
	     (matrix/row* (subvec statev (inc n) two-n+1))))))

(def istate->H-state unflatten-H-state)

;;; An alternative way to obtain Lagrange's equations arises from
;;;  expanding the derivative of the momentum by the chain rule to
;;;  get the Lagrange operator.  Lagrange's equations are then
;;;  obtained by calling the Lagrange operator on the objects
;;;  q, qdot, qddot, all functions of time.

;;; ******* This stuff only works for L(t,q,qd)

(defn on-jet [q qdot]
  (fn [lfun]
    (f/compose lfun
	             (fn [t]
	               (up t (q t) (qdot t))))))

(defn Lagrange-operator [Lagrangian]
  (let [P ((partial 2) Lagrangian)
	      F ((partial 1) Lagrangian)
        Pq ((partial 1) P)
        Pqdot ((partial 2) P)
        Pt ((partial 0) P)]
    (fn [q qdot qddot]
	    (let [lift (on-jet q qdot)]
	      (+ (* (lift Pqdot) qddot)
	         (* (lift Pq) qdot)
	         (lift Pt)
	         (- (lift F)))))))

(defn Lagrange-equations-from-operator [Lagrangian]
  (let [lop (Lagrange-operator Lagrangian)]
    (fn [q]
      (let [qdot (D q)
            qddot (D qdot)]
	      (lop q qdot qddot)))))

;; #|
;; (show-expression
;;  (((Lagrange-equations-from-operator
;;     (L-sliding-pend 'm_1 'm_2 'b 'g))
;;    (coordinate-tuple (literal-function 'x)
;; 		                 (literal-function 'theta)))
;;   't))
;; (down
;;  (+ (* -1 b m_2 (sin (theta t)) (expt ((D theta) t) 2))
;;     (* b m_2 (((expt D 2) theta) t) (cos (theta t)))
;;     (* m_1 (((expt D 2) x) t))
;;     (* m_2 (((expt D 2) x) t)))
;;  (+ (* (expt b 2) m_2 (((expt D 2) theta) t))
;;     (* b g m_2 (sin (theta t)))
;;     (* b m_2 (((expt D 2) x) t) (cos (theta t)))))
;; |#

(defn tz->tqp [t z]
  (let [two-n (count z)
	      n     (quot two-n 2)]
    (assert (even? two-n))
    #_(if (= n 1)
	      (->H-state t (get z 0) (get z 1))
	      (->H-state t
		               (vector->column-matrix
                    (subvec z 0 n))
		               (vector->row-matrix
                    (subvec z n two-n))))))

(defn z->tqp [z]
  (tz->tqp :unknown-time z))

(defn tqp->z [[_ q p]]
  #_
  (if (and (column? q) (row? p))
	  (vector->up
	   (vector-append (column->vector q)
			              (row->vector p)))
	  (up q p)))

(defn tqp->tz [[t q p]]
  #_(if (and (column? q) (row? p))
	    (up t
	        (vector->up
	         (vector-append (column->vector q)
			                    (row->vector p))))
	    (up t (up q p))))
