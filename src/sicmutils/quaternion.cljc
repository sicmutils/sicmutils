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
            [sicmutils.util.logic :as ul]
            [sicmutils.matrix :as m]
            [sicmutils.value :as v]))

(deftype Quaternion [r i j k]
  ;; TODO implement `get`, `assoc` by index
  ;;
  ;; - kind ::quaternion
  ;; - IFn, call q:apply
  ;; - zero?, zero-like... identity too?
  ;; = does what you'd expect
  ;; - print-method and friends
  ;;
  ;; arity
  )

(comment
  ;; protocols
  (defmethod g/apply q:apply        quaternion? any?)
  (defmethod g/arity            q:arity           quaternion?)
  (defmethod g/inexact?         q:inexact?        quaternion?)
  (defmethod g/zero-like        q:zero-like       quaternion?)
  (defmethod g/zero?            q:zero?           quaternion?)
  (defmethod g/=     q:=                       quaternion? quaternion?))

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

(defn q:*
  ([q] q)
  ([^Quaternion q1 ^Quaternion q2]
   (let [r1 (.-r q1) i1 (.-i q1) j1 (.-j q1) k1 (.-k q1)
         r2 (.-r q2) i2 (.-i q2) j2 (.-j q2) k2 (.-k q2)]
     (->Quaternion
      (g/- (g/* r1 r2) (g/+ (g/* i1 i2) (g/* j1 j2) (g/* k1 k2)))
      (g/+ (g/* r1 i2) (g/* i1 r2) (g/* j1 k2) (g/* -1 k1 j2))
      (g/+ (g/* r1 j2) (g/* -1 i1 k2) (g/* j1 r2) (g/* k1 i2))
      (g/+ (g/* r1 k2) (g/* i1 j2) (g/* -1 j1 i2) (g/* k1 r2)))))
  ([q1 q2 & more]
   (reduce q:* (q:* q1 q2) more)))

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

(def s:1 (m/->structure q:1))
(def s:i (m/->structure q:i))
(def s:j (m/->structure q:j))
(def s:k (m/->structure q:k))

(defn ->4x4 [^Quaternion q]
  (g/+ (g/* (.-r q) q:1)
       (g/* (.-i q) q:i)
       (g/* (.-j q) q:j)
       (g/* (.-k q) q:k)))

(defn q:4x4-> [four-matrix]
  (apply ->Quaternion
         (nth four-matrix 0)))

;; ## Quaternions and 3D rotations

;; Given a axis (a unit 3-vector) and an angle

(defn angle-axis-> [theta axis]
  (let [v (g/simplify (g/dot-product axis axis))]
    (ul/assume! (list '= v 1) 'angle-axis->quaternion))
  (make (g/cos (g// theta 2))
        (g/* (g/sin (g// theta 2))
             axis)))

;; Problem: this is singular if the vector part is zero.

(defn ->angle-axis
  ([q] (->angle-axis q vector))
  ([q continue]
   {:pre [(quaternion? q)]}
   (let [v     (three-vector q)
         theta (g/* 2 (g/atan (g/abs v)
                              (real-part q)))
         axis  (g// v (g/abs v))]
     (continue theta axis))))

(comment
  (is (= '(theta
           (up x y (sqrt (+ 1 (* -1 (expt x 2))
                            (* -1 (expt y 2))))))
         (g/simplify
          (->angle-axis
           (angle-axis->
            'theta
            ['x 'y (g/sqrt
                    (g/- 1 (g/square 'x) (g/square 'y)))]))))))

;; To rotate a 3-vector by the angle prescribed by a unit quaternion.

(defn rotate [q]
  {:pre [(quaternion? q)]}
  ;;(assert (q:unit? q))
  ;; This assertion is really:
  (let [vv (->vector q)
        v  (g/simplify (g/dot-product vv vv))]
    (ul/assume! (list '= v 1) 'rotate))
  (let [q* (q:conjugate q)]
    (fn the-rotation [three-v]
      (three-vector
       (q:* q (make 0 three-v) q*)))))

;; ## Relation to rotation matrices
;;
;; Expanded Matt Mason method.

(comment
  (def ^:dynamic *factoring* false)

  ;; Hamiltonians look better if we divide them out.
  (defn ham:simplify [hexp]
    (cond (and (quotient? hexp) *divide-out-terms*)
          (if (sum? (symb:numerator hexp))
            (let [d (symb:denominator hexp)]
              (a-reduce symb:+
                        (map (fn [n]
                               (g/simplify (symb:div n d)))
                             (operands
                              (symb:numerator hexp)))))
            hexp)

          (compound-data-constructor? hexp)
          (cons (operator hexp) (map ham:simplify (operands hexp)))

          :else hexp))

  (define clean-differentials
    ;; TODO clean a CLEANED differential... aren't these all done??
    (rule-simplifier
     (ruleset
      (make-differential-quantity
       [??lterms
        (make-differential-term (? dx) 0)
        ??rterms])
      =>
      (make-differential-quantity [??lterms ??rterms])

      (make-differential-quantity
       [(make-differential-term '() ?x)]) => ?x

      (make-differential-quantity []) => 0)))

  (define (flush-literal-function-constructors expr)
    (if (pair? expr)
      (if (eq? (car expr) 'literal-function)
        (if (and (pair? (cadr expr)) (eq? (caadr expr) 'quote))
          (flush-literal-function-constructors (cadadr expr))
          (cadr expr))
        (cons (flush-literal-function-constructors (car expr))
              (flush-literal-function-constructors (cdr expr))))
      expr))

  (defn simplify [exp]
    ((access clean-differentials rule-environment)
     (flush-derivative
      (flush-literal-function-constructors
       (ham:simplify
        ((if *factoring* poly:factor (fn [expr] expr))
         (g:simplify exp)))))))

  ;; Is this enough? move to simplify.
  (define (careful-simplify e)
    (simplify e)))

(def careful-simplify g/simplify)

(defn rotation-matrix->quaternion-mason [M]
  (let [r11 (get-in M [0 0]) r12 (get-in M [0 1]) r13 (get-in M [0 2])
        r21 (get-in M [1 0]) r22 (get-in M [1 1]) r23 (get-in M [1 2])
        r31 (get-in M [2 0]) r32 (get-in M [2 1]) r33 (get-in M [2 2])
        quarter (g// 1 4)

        q0-2 (g/* 1/4 (g/+ 1 r11 r22 r33))

        q0q1 (g/* 1/4 (g/- r32 r23))
        q0q2 (g/* 1/4 (g/- r13 r31))
        q0q3 (g/* 1/4 (g/- r21 r12))
        q1q2 (g/* 1/4 (g/+ r12 r21))
        q1q3 (g/* 1/4 (g/+ r13 r31))
        q2q3 (g/* 1/4 (g/+ r23 r32))]
    ;; If numerical, choose largest of squares.
    ;; If symbolic, choose nonzero square.
    (let [q0 (g/sqrt q0-2)
          q1 (g// q0q1 q0)
          q2 (g// q0q2 q0)
          q3 (g// q0q3 q0)]
      (make q0 q1 q2 q3))))

(defn rotation-matrix->
  "TODO change >= etc to using compare... OR just go ahead and add those to v/
  finally!"
  [M]
  ;; (assert (orthogonal-matrix? M))
  ;; returns a unit quaternion
  (let [r11 (get-in M [0 0]) r12 (get-in M [0 1]) r13 (get-in M [0 2])
        r21 (get-in M [1 0]) r22 (get-in M [1 1]) r23 (get-in M [1 2])
        r31 (get-in M [2 0]) r32 (get-in M [2 1]) r33 (get-in M [2 2])
        quarter (g// 1 4)

        q0-2 (g/* quarter (g/+ 1 r11 r22 r33))
        q1-2 (g/* quarter (g/+ 1 r11 (g/- r22) (g/- r33)))
        q2-2 (g/* quarter (g/+ 1 (g/- r11) r22 (g/- r33)))
        q3-2 (g/* quarter (g/+ 1 (g/- r11) (g/- r22) r33))

        q0q1 (g/* quarter (g/- r32 r23))
        q0q2 (g/* quarter (g/- r13 r31))
        q0q3 (g/* quarter (g/- r21 r12))
        q1q2 (g/* quarter (g/+ r12 r21))
        q1q3 (g/* quarter (g/+ r13 r31))
        q2q3 (g/* quarter (g/+ r23 r32))

        q0-2s (careful-simplify q0-2)
        q1-2s (careful-simplify q1-2)
        q2-2s (careful-simplify q2-2)
        q3-2s (careful-simplify q3-2)]
    (cond (and (v/number? q0-2s) (v/number? q1-2s)
               (v/number? q2-2s) (v/number? q3-2s))
          (cond (>= q0-2s (max q1-2s q2-2s q3-2s))
                (let [q0 (g/sqrt q0-2s)
                      q1 (g// q0q1 q0)
                      q2 (g// q0q2 q0)
                      q3 (g// q0q3 q0)]
                  (make q0 q1 q2 q3))

                (>= q1-2s (max q0-2s q2-2s q3-2s))
                (let [q1 (g/sqrt q1-2s)
                      q0 (g// q0q1 q1)
                      q2 (g// q1q2 q1)
                      q3 (g// q1q3 q1)]
                  (make q0 q1 q2 q3))

                (>= q2-2s (max q0-2s q1-2s q3-2s))
                (let [q2 (g/sqrt q2-2s)
                      q0 (g// q0q2 q2)
                      q1 (g// q1q2 q2)
                      q3 (g// q2q3 q2)]
                  (make q0 q1 q2 q3))

                :else
                (let [q3 (g/sqrt q3-2s)
                      q0 (g// q0q3 q3)
                      q1 (g// q1q3 q3)
                      q2 (g// q2q3 q3)]
                  (make q0 q1 q2 q3)))

          (not (v/numeric-zero? q0-2s))
          (let [q0 (g/sqrt q0-2)
                q1 (g// q0q1 q0)
                q2 (g// q0q2 q0)
                q3 (g// q0q3 q0)]
            (make q0 q1 q2 q3))

          (not (v/numeric-zero? q1-2s))
          (let [q1 (g/sqrt q1-2)
                q0 0
                q2 (g// q1q2 q1)
                q3 (g// q1q3 q1)]
            (make q0 q1 q2 q3))

          (not (v/numeric-zero? q2-2s))
          (let [q2 (g/sqrt q2-2)
                q0 0
                q1 0
                q3 (g// q2q3 q2)]
            (make q0 q1 q2 q3))

          :else (make 0 0 0 0))))

(comment
  (require '[sicmutils.mechanics.rotation :as mr])
  (is (= (quaternion 0. 0. 1.734723475976807e-18 0.)
         (let [M (g/* (mr/rotate-z-matrix 0.1)
                      (mr/rotate-x-matrix 0.2)
                      (mr/rotate-z-matrix 0.3))]
           (g/- (rotation-matrix->quaternion-mason M)
                (rotation-matrix->quaternion M))))))

(defn ->rotation-matrix [q]
  {:pre [(quaternion? q)]}
  ;;(assert (q:unit? q))
  ;; This assertion is really:
  (let [vv (->vector q)
        v  (g/simplify (g/dot-product vv vv))]
    (ul/assume! (list '= v 1) 'quaternion->rotation-matrix))
  (let [q0 (.-r q) q1 (.-i q)
        q2 (.-j q) q3 (.-k q)
        m-2 (g/+ (g/square q0) (g/square q1)
                 (g/square q2) (g/square q3))]
    (m/by-rows [(g// (g/+ (g/expt q0 2)
                          (g/expt q1 2)
                          (g/* -1 (g/expt q2 2))
                          (g/* -1 (g/expt q3 2)))
                     m-2)
                (g// (g/* 2 (g/- (g/* q1 q2) (g/* q0 q3)))
                     m-2)
                (g// (g/* 2 (g/+ (g/* q1 q3) (g/* q0 q2)))
                     m-2)]
               [(g// (g/* 2 (g/+ (g/* q1 q2) (g/* q0 q3)))
                     m-2)
                (g// (g/+ (g/expt q0 2)
                          (g/* -1 (g/expt q1 2))
                          (g/expt q2 2)
                          (g/* -1 (g/expt q3 2)))
                     m-2)
                (g// (g/* 2 (g/- (g/* q2 q3) (g/* q0 q1)))
                     m-2)]
               [(g// (g/* 2 (g/- (g/* q1 q3) (g/* q0 q2)))
                     m-2)
                (g// (g/* 2 (g/+ (g/* q2 q3) (g/* q0 q1)))
                     m-2)
                (g// (g/+ (g/expt q0 2)
                          (g/* -1 (g/expt q1 2))
                          (g/* -1 (g/expt q2 2))
                          (g/expt q3 2))
                     m-2)])))

(comment
  (is (= (s/up 0 (s/up 0 0 0))
         (let [theta 'theta
               v (s/up 'x 'y 'z)
               axis (make-unit v)
               result
               ((comp quaternion->angle-axis
                      rotation-matrix->quaternion
                      quaternion->rotation-matrix
                      angle-axis->quaternion)
                theta axis)]
           (s/up (g/- (first result) theta)
                 (g/- (second result) axis)))))

  ;; But look at (show-notes) to see the assumptions.
  ;;
  ;; Indeed:

  (is (= '(up 2.0
              (up -0.5345224838248488
                  -1.0690449676496976
                  -1.6035674514745464))
         (let [theta -1
               v (up 1 2 3)
               axis (make-unit v)
               result
               ((comp quaternion->angle-axis
                      rotation-matrix->quaternion
                      quaternion->rotation-matrix
                      angle-axis->quaternion)
                theta axis)]
           (s/up (g/- (first result) theta)
                 (g/- (second result) axis))))))

;; Generic Method Installation

(defmethod g/add [::quaternion ::quaternion] [a b] (q:+ a b))

(defmethod g/negate [::quaternion] [q] (q:negate q))
(defmethod g/sub [::quaternion ::quaternion] [a b] (q:- a b))

(defmethod g/mul [::quaternion ::quaternion] [a b] (q:* a b))
(defmethod g/mul [::v/scalar ::quaternion] [s q] (scalar*q s q))
(defmethod g/mul [::quaternion ::v/scalar] [q s] (q*scalar q s))

(defmethod g/invert [::quaternion] [q] (invert q))
(defmethod g/div [::quaternion ::v/scalar] [q s] (q-div-scalar q s))
(defmethod g/div [::quaternion ::quaternion] [a b] (q:div a b))

(defmethod g/magnitude [::quaternion] [q] (magnitude q))
(defmethod g/conjugate [::quaternion] [q] (q:conjugate q))
(defmethod g/real-part [::quaternion] [q] (real-part q))
(defmethod g/exp [::quaternion] [q] (exp q))
(defmethod g/log [::quaternion] [q] (log q))

(defmethod g/partial-derivative [::quaternion v/seqtype] [q selectors]
  (partial-derivative q selectors))

(defmethod g/solve-linear-right [::quaternion ::scalar] [q s] (q-div-scalar q s))
(defmethod g/solve-linear-right [::quaternion ::quaternion] [a b] (q:div a b))

(defmethod g/solve-linear [::v/scalar ::quaternion] [s q] (q-div-scalar q s))
(defmethod g/solve-linear [::quaternion ::quaternion] [a b] (q:div b a))
