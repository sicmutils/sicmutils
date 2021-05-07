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

(ns sicmutils.quaternion
  (:require [sicmutils.function :as f]
            [sicmutils.generic :as g]
            [sicmutils.matrix :as m]
            [sicmutils.value :as v]))

(deftype Quaternion [r i j k]
  ;; TODO implement `get`, `assoc` by index
  )

(defn make
  "Same as `make`, and `real&3vector->quaternion`."
  ([r [i j k]]
   (->Quaternion r i j k))
  ([r i j k]
   (->Quaternion r i j k)))

(defn quaternion? [q]
  (instance? Quaternion q))

(defn ->vector [^Quaternion q]
  [(.-r q) (.-i q) (.-j q) (.-k q)])

(defn real-part [^Quaternion q]
  (.-r q))

(defn three-vector [^Quaternion q]
  [(.-i q) (.-j q) (.-k q)])

(defn q:+ [^Quaternion q1 ^Quaternion q2]
  (->Quaternion
   (g/+ (.-r q1) (.-r q2))
   (g/+ (.-i q1) (.-i q2))
   (g/+ (.-j q1) (.-j q2))
   (g/+ (.-k q1) (.-k q2))))

(defn q:- [^Quaternion q1 ^Quaternion q2]
  (->Quaternion
   (g/- (.-r q1) (.-r q2))
   (g/- (.-i q1) (.-i q2))
   (g/- (.-j q1) (.-j q2))
   (g/- (.-k q1) (.-k q2))))

(defn q:* [^Quaternion q1 ^Quaternion q2]
  (let [r1 (.-r q1) i1 (.-i q1) j1 (.-j q1) k1 (.-k q1)
	      r2 (.-r q2) i2 (.-i q2) j2 (.-j q2) k2 (.-k q2)]
    (->Quaternion
     (g/- (g/* r1 r2) (g/+ (g/* i1 i2) (g/* j1 j2) (g/* k1 k2)))
	   (g/+ (g/* r1 i2) (g/* i1 r2) (g/* j1 k2) (g/* -1 k1 j2))
	   (g/+ (g/* r1 j2) (g/* -1 i1 k2) (g/* j1 r2) (g/* k1 i2))
	   (g/+ (g/* r1 k2) (g/* i1 j2) (g/* -1 j1 i2) (g/* k1 r2)))))

(defn q:conjugate [^Quaternion q]
  (->Quaternion
   (.-r q)
	 (g/negate (.-i q))
	 (g/negate (.-j q))
	 (g/negate (.-k q))))

(defn q:negate [^Quaternion q]
  (->Quaternion
   (g/negate (.-r q))
   (g/negate (.-i q))
   (g/negate (.-j q))
   (g/negate (.-k q))))

(defn scalar*q [s ^Quaternion q]
  (->Quaternion
   (g/* s (.-r q))
   (g/* s (.-i q))
   (g/* s (.-j q))
   (g/* s (.-k q))))

(defn q*scalar [^Quaternion q s]
  (->Quaternion
   (g/* (.-r q) s)
   (g/* (.-i q) s)
   (g/* (.-j q) s)
   (g/* (.-k q) s)))

(defn q-div-scalar [^Quaternion q s]
  (->Quaternion
   (g// (.-r q) s)
   (g// (.-i q) s)
   (g// (.-j q) s)
   (g// (.-k q) s)))

(defn invert [^Quaternion q]
  (q-div-scalar (q:conjugate q)
		            (g/+ (g/square (.-r q))
			               (g/square (.-i q))
			               (g/square (.-j q))
			               (g/square (.-k q)))))

(defn q:div [q1 q2]
  (q:* q1 (invert q2)))

(defn magnitude [^Quaternion q]
  (g/sqrt
   (g/+ (g/square (.-r q))
	      (g/square (.-i q))
	      (g/square (.-j q))
	      (g/square (.-k q)))))

(defn make-unit [q]
  (q-div-scalar q (magnitude q)))

;; TODO vector dot product, just do it directly.

(defn unit? [q]
  (let [v (->vector q)]
    (v/one? (g/dot-product v v))))

(defn exp [q]
  (let [a (real-part q)
        v (three-vector q)]
    (let [vv (g/abs v)]
      (g/* (g/exp a)
	         (make (g/cos vv)
				         (g/* (g/sin vv)
					            (g// v vv)))))))

(defn log [q]
  (let [a  (real-part q)
        v  (three-vector q)
	      qq (g/abs (->vector q))
        vv (g/abs v)]
    (make (g/log qq)
		      (g/* (g/acos (g// a qq))
				       (g// v vv)))))

(let [zero (->Quaternion 0 0 0 0)]
  (defn zero-like [_] zero))

(defn q:zero? [^Quaternion q]
  (and (v/zero? (.-r q))
       (v/zero? (.-i q))
       (v/zero? (.-j q))
       (v/zero? (.-k q))))

(defn q:= [^Quaternion q1 ^Quaternion q2]
  (and (v/= (.-r q1) (.-r q2))
       (v/= (.-i q1) (.-i q2))
       (v/= (.-j q1) (.-j q2))
       (v/= (.-k q1) (.-k q2))))

(defn exact? [^Quaternion q]
  (and (v/exact? (.-r q))
       (v/exact? (.-i q))
       (v/exact? (.-j q))
       (v/exact? (.-k q))))

(defn q:apply [^Quaternion q args]
  (->Quaternion
   (apply (.-r q) args)
   (apply (.-i q) args)
   (apply (.-j q) args)
   (apply (.-k q) args)))

(defn arity [q]
  (let [v (->vector q)
        n 4]
    (loop [i 1
           a (f/arity (nth v 0))]
	    (if (= i n)
	      a
	      (if-let [b (f/joint-arity a (f/arity (nth v i)))]
          (recur (inc i) b)
          false)))))

(defn partial-derivative [q selectors]
  (let [v (->vector q)]
    (->Quaternion
     (g/partial-derivative (.-r q) selectors)
     (g/partial-derivative (.-i q) selectors)
     (g/partial-derivative (.-j q) selectors)
     (g/partial-derivative (.-k q) selectors))))

;; ## Quaternions as 4x4 matrices

(def q:1
  (m/by-rows
   [1 0 0 0]
   [0 1 0 0]
   [0 0 1 0]
   [0 0 0 1]))

(def q:i
  (m/by-rows
   [0 1 0 0]
   [-1 0 0 0]
   [0 0 0 -1]
   [0 0 1 0]))

(def q:j
  (m/by-rows
   [0 0 1 0]
   [0 0 0 1]
   [-1 0 0 0]
   [0 -1 0 0]))

(def q:k
  (m/by-rows
   [0 0 0 1]
   [0 0 -1 0]
   [0 1 0 0]
   [-1 0 0 0]))

(comment
  (define s:1 (Mmn->A↑m_n q:1))
  (define s:i (Mmn->A↑m_n q:i))
  (define s:j (Mmn->A↑m_n q:j))
  (define s:k (Mmn->A↑m_n q:k))

  (defn ->4x4 [^Quaternion q]
    (let [r (.-r q)
	        x (.-i q)
	        y (.-j q)
	        z (.-k q)]
      (matrix+matrix
       (matrix+matrix (scalar*matrix r q:1)
				              (scalar*matrix x q:i))
		   (matrix+matrix (scalar*matrix y q:j)
				              (scalar*matrix z q:k)))))

  (defn q:4x4-> [four-matrix]
    (->Quaternion (m:nth-row four-matrix 0)))

  ;; ## Quaternions and 3D rotations

  ;; Given a axis (a unit 3-vector) and an angle

  (define (angle-axis->quaternion theta axis)
    ;; (assert (v:unit? axis))
    ;; This assertion is really:
    (let ((v (g/simplify (v:dot-product axis axis))))
      (assume! `(= ,v 1) 'angle-axis->quaternion))
    (real&3vector->quaternion (g/cos (g// theta 2))
			                        (g/* (g/sin (g// theta 2))
				                           axis)))

  (define q:angle-axis-> angle-axis->quaternion)

  ;; Problem: this is singular if the vector part is zero.

  (define (quaternion->angle-axis q & {:keys [continue]})
    (assert (quaternion? q))
    (let ((continue
           (if (default-object? continue) list continue)))
      (let* ((v (q:3vector q))
             (theta (g/* 2 (g/atan (euclidean-norm v)
                                   (q:real-part q))))
             (axis (vector/scalar v (euclidean-norm v))))
        (continue theta axis))))

  (define q:->angle-axis quaternion->angle-axis)


  (is (= '(theta
           (up x y (sqrt (+ 1 (* -1 (expt x 2))
                            (* -1 (expt y 2))))))
         (quaternion->angle-axis
          (angle-axis->quaternion
           'theta
           (up 'x 'y (sqrt (- 1 (square 'x) (square 'y))))))))

  ;; To rotate a 3-vector by the angle prescribed by a unit quaternion.

  (define (q:rotate q)
    (assert (quaternion? q))
    ;;(assert (q:unit? q))
    ;; This assertion is really:
    (let* ((vv (quaternion->vector q))
           (v (g/simplify (v:dot-product vv vv))))
      (assume! `(= ,v 1) 'q:rotate))
    (let ((q* (q:conjugate q)))
      (define (the-rotation three-vector)
        (quaternion->3vector
         (quaternion*quaternion q
	                              (quaternion*quaternion
	                               (real&3vector->quaternion 0 three-vector)
	                               q*))))
      the-rotation))


  ;; ## Relation to rotation matrices
  ;;
  ;; Expanded Matt Mason method.

  (define (rotation-matrix->quaternion M)
    ;; (assert (orthogonal-matrix? M))
    ;; returns a unit quaternion
    (let ((r11 (matrix-ref M 0 0)) (r12 (matrix-ref M 0 1)) (r13 (matrix-ref M 0 2))
	        (r21 (matrix-ref M 1 0)) (r22 (matrix-ref M 1 1)) (r23 (matrix-ref M 1 2))
	        (r31 (matrix-ref M 2 0)) (r32 (matrix-ref M 2 1)) (r33 (matrix-ref M 2 2)))
      (let ((q0↑2 (g/* 1/4 (g/+ 1 r11 r22 r33)))
	          (q1↑2 (g/* 1/4 (g/+ 1 r11 (g/- r22) (g/- r33))))
	          (q2↑2 (g/* 1/4 (g/+ 1 (g/- r11) r22 (g/- r33))))
	          (q3↑2 (g/* 1/4 (g/+ 1 (g/- r11) (g/- r22) r33)))

	          (q0q1 (g/* 1/4 (g/- r32 r23)))
	          (q0q2 (g/* 1/4 (g/- r13 r31)))
	          (q0q3 (g/* 1/4 (g/- r21 r12)))
	          (q1q2 (g/* 1/4 (g/+ r12 r21)))
	          (q1q3 (g/* 1/4 (g/+ r13 r31)))
	          (q2q3 (g/* 1/4 (g/+ r23 r32))))
        (let ((q0↑2s (careful-simplify q0↑2))
	            (q1↑2s (careful-simplify q1↑2))
	            (q2↑2s (careful-simplify q2↑2))
	            (q3↑2s (careful-simplify q3↑2)))
	        (cond ((and (number? q0↑2s) (number? q1↑2s)
		                  (number? q2↑2s) (number? q3↑2s))
	               (cond ((>= q0↑2s (max q1↑2s q2↑2s q3↑2s))
		                    (let ((q0 (sqrt q0↑2s)))
			                    (let ((q1 (g// q0q1 q0))
			                          (q2 (g// q0q2 q0))
			                          (q3 (g// q0q3 q0)))
			                      (quaternion q0 q1 q2 q3))))
		                   ((>= q1↑2s (max q0↑2s q2↑2s q3↑2s))
		                    (let ((q1 (sqrt q1↑2s)))
			                    (let ((q0 (g// q0q1 q1))
			                          (q2 (g// q1q2 q1))
			                          (q3 (g// q1q3 q1)))
			                      (quaternion q0 q1 q2 q3))))
		                   ((>= q2↑2s (max q0↑2s q1↑2s q3↑2s))
		                    (let ((q2 (sqrt q2↑2s)))
			                    (let ((q0 (g// q0q2 q2))
			                          (q1 (g// q1q2 q2))
			                          (q3 (g// q2q3 q2)))
			                      (quaternion q0 q1 q2 q3))))
		                   (else
		                    (let ((q3 (sqrt q3↑2s)))
			                    (let ((q0 (g// q0q3 q3))
			                          (q1 (g// q1q3 q3))
			                          (q2 (g// q2q3 q3)))
			                      (quaternion q0 q1 q2 q3))))))
	              ((not (and (number? q0↑2s) (zero? q0↑2s)))
	               (let ((q0 (g/sqrt q0↑2)))
		               (let ((q1 (g// q0q1 q0))
		                     (q2 (g// q0q2 q0))
		                     (q3 (g// q0q3 q0)))
		                 (quaternion q0 q1 q2 q3))))
	              ((not (and (number? q1↑2s) (zero? q1↑2s)))
	               (let ((q1 (g/sqrt q1↑2)))
		               (let ((q0 0)
		                     (q2 (g// q1q2 q1))
		                     (q3 (g// q1q3 q1)))
		                 (quaternion q0 q1 q2 q3))))
	              ((not (and (number? q2↑2s) (zero? q2↑2s)))
	               (let ((q2 (g/sqrt q2↑2)))
		               (let ((q0 0)
		                     (q1 0)
		                     (q3 (g// q2q3 q2)))
		                 (quaternion q0 q1 q2 q3))))
	              (else
	               (quaternion 0 0 0 0)))))))

  (define q:rotation-matrix-> rotation-matrix->quaternion)


  (is (= (quaternion 0. 0. 1.734723475976807e-18 0.)
         (let ((M (* (rotate-z-matrix 0.1)
	                   (rotate-x-matrix 0.2)
	                   (rotate-z-matrix 0.3))))
           (- (rotation-matrix->quaternion-mason M)
              (rotation-matrix->quaternion M)))))

  (define (quaternion->rotation-matrix q)
    (assert (quaternion? q))
    ;;(assert (q:unit? q))
    ;; This assertion is really:
    (let* ((vv (quaternion->vector q))
           (v (g/simplify (v:dot-product vv vv))))
      (assume! `(= ,v 1) 'quaternion->rotation-matrix))
    (let ((q0 (.-r q)) (q1 (.-i q))
          (q2 (.-j q)) (q3 (.-k q)))
      (let ((m↑2 (g/+ (g/expt q0 2) (g/expt q1 2)
                      (g/expt q2 2) (g/expt q3 2))))
        (m/by-rows
         (list (g// (g/+ (g/expt q0 2)
                         (g/expt q1 2)
                         (g/* -1 (g/expt q2 2))
                         (g/* -1 (g/expt q3 2)))
                    m↑2)
               (g// (g/* 2 (g/- (g/* q1 q2) (g/* q0 q3)))
                    m↑2)
               (g// (g/* 2 (g/+ (g/* q1 q3) (g/* q0 q2)))
                    m↑2))
         (list (g// (g/* 2 (g/+ (g/* q1 q2) (g/* q0 q3)))
                    m↑2)
               (g// (g/+ (g/expt q0 2)
                         (g/* -1 (g/expt q1 2))
                         (g/expt q2 2)
                         (g/* -1 (g/expt q3 2)))
                    m↑2)
               (g// (g/* 2 (g/- (g/* q2 q3) (g/* q0 q1)))
                    m↑2))
         (list (g// (g/* 2 (g/- (g/* q1 q3) (g/* q0 q2)))
                    m↑2)
               (g// (g/* 2 (g/+ (g/* q2 q3) (g/* q0 q1)))
                    m↑2)
               (g// (g/+ (g/expt q0 2)
                         (g/* -1 (g/expt q1 2))
                         (g/* -1 (g/expt q2 2))
                         (g/expt q3 2))
                    m↑2))))))

  (define q:->rotation-matrix quaternion->rotation-matrix)


  (is (= (up 0 (up 0 0 0))
         (let ((theta 'theta) (v (up 'x 'y 'z)))
           (let ((axis (v:make-unit v)))
             (let ((result
	                  ((compose quaternion->angle-axis
		                          rotation-matrix->quaternion
		                          quaternion->rotation-matrix
		                          angle-axis->quaternion)
	                   theta axis)))
               (up (- (car result) theta)
	                 (- (cadr result) axis)))))))

  ;; But look at (show-notes) to see the assumptions.
  ;;
  ;; Indeed:

  (is (= '(up 2.
              (up -.5345224838248488
                  -1.0690449676496976
                  -1.6035674514745464))
         (let ((theta -1) (v (up 1 2 3)))
           (let ((axis (v:make-unit v)))
             (let ((result
	                  ((compose quaternion->angle-axis
		                          rotation-matrix->quaternion
		                          quaternion->rotation-matrix
		                          angle-axis->quaternion)
	                   theta axis)))
               (up (- (car result) theta)
	                 (- (cadr result) axis)))))))

  (assign-operation 'type             q:type            quaternion?)
  (assign-operation 'type-predicate   q:type-predicate  quaternion?)

  (assign-operation 'arity            q:arity           quaternion?)

  (assign-operation 'inexact?         q:inexact?        quaternion?)

  (assign-operation 'zero-like        q:zero-like       quaternion?)

  (assign-operation 'zero?            q:zero?           quaternion?)

  (assign-operation 'negate           q:negate          quaternion?)




  (assign-operation 'magnitude        q:magnitude       quaternion?)


  (assign-operation 'conjugate        q:conjugate       quaternion?)
  (assign-operation 'invert 	    q:invert 	      quaternion?)

  (assign-operation 'real-part        q:real-part       quaternion?)


  (assign-operation 'exp              q:exp             quaternion?)
  (assign-operation 'log              q:log             quaternion?)



  (assign-operation '=     q:=                       quaternion? quaternion?)

  (assign-operation '+     quaternion+quaternion     quaternion? quaternion?)
  (assign-operation '-     quaternion-quaternion     quaternion? quaternion?)

  (assign-operation '*     quaternion*quaternion     quaternion? quaternion?)

  (assign-operation '*     scalar*quaternion         scalar?     quaternion?)
  (assign-operation '*     quaternion*scalar         quaternion? scalar?)

  (assign-operation '/     q-div-scalar         quaternion? scalar?)
  (assign-operation '/     quaternion/quaternion     quaternion? quaternion?)

  (assign-operation 'apply            q:apply        quaternion? any?)

  (assign-operation 'partial-derivative q:partial-derivative quaternion? any?)


  (assign-operation 'solve-linear-right     q-div-scalar         quaternion? scalar?)
  (assign-operation 'solve-linear-right     quaternion/quaternion     quaternion? quaternion?)

  (assign-operation 'solve-linear-left  (lambda (x y) (q-div-scalar y x))         scalar? quaternion?)
  (assign-operation 'solve-linear-left  (lambda (x y) (quaternion/quaternion y x))     quaternion? quaternion?)

  (assign-operation 'solve-linear  (lambda (x y) (q-div-scalar y x))         scalar? quaternion?)
  (assign-operation 'solve-linear  (lambda (x y) (quaternion/quaternion y x))     quaternion? quaternion?)
  )
