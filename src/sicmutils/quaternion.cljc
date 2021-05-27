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
  (:require [sicmutils.differential :as d]
            [sicmutils.function :as f]
            [sicmutils.generic :as g]
            [sicmutils.util.logic :as ul]
            [sicmutils.matrix :as m]
            [sicmutils.util :as u]
            [sicmutils.value :as v])
  #?(:clj
     (:import (clojure.lang AFn Associative Counted IObj IFn Sequential))))

(declare arity q:= exact? q:apply q:zero? zero-like)

(deftype Quaternion [r i j k m]
  v/Value
  (zero? [this] (q:zero? this))
  (one? [_] false

    ;; TODO add one or identity from https://github.com/infusion/Quaternion.js/

    )
  (identity? [_] false)
  (zero-like [this] (zero-like this))
  (one-like [o] (u/unsupported (str "one-like: " o)))
  (identity-like [o] (u/unsupported (str "identity-like: " o)))
  (exact? [this] (exact? this))
  (freeze [_] (list 'quaternion r i j k))
  (kind [_] ::quaternion)

  f/IArity
  (arity [this] (arity this))

  d/IPerturbed
  (perturbed? [_]
    (or (d/perturbed? r)
        (d/perturbed? i)
        (d/perturbed? j)
        (d/perturbed? k)))

  (replace-tag [_ old new]
    (Quaternion.
     (d/replace-tag r old new)
     (d/replace-tag i old new)
     (d/replace-tag j old new)
     (d/replace-tag k old new)
     m))

  (extract-tangent [_ tag]
    (Quaternion.
     (d/extract-tangent r tag)
     (d/extract-tangent i tag)
     (d/extract-tangent j tag)
     (d/extract-tangent k tag)
     m))

  #?@(:clj
      [Object
       (toString [_] (str "#sicm/quaternion [" r " " i  " " j " " k "]"))
       (equals [self q] (q:= self q))

       IObj
       (meta [_] m)
       (withMeta [_ m] (Quaternion. r i j k m))

       Sequential

       Associative
       (assoc [_ k v]
              (case k
                0 (Quaternion. v i j k m)
                1 (Quaternion. r v j k m)
                2 (Quaternion. r i v k m)
                3 (Quaternion. r i j v m)
                (IndexOutOfBoundsException.)))

       (containsKey [_ k] (boolean (#{0 1 2 3} k)))
       (entryAt [this k]
                (get this k nil))

       ;; TODO: count does NOT get called, it seems!!
       (count [_] 4)
       (seq [_] (list r i j k))
       (valAt [this k]
              (get this k nil))
       (valAt [_ k default]
              (case k
                0 r
                1 i
                2 j
                3 k
                default))
       (empty [this] (Quaternion. 0 0 0 0 m))
       (equiv [this that] (.equals this that))

       IFn
       (invoke [this]
               (q:apply this []))
       (invoke [this a]
               (q:apply this [a]))
       (invoke [this a b]
               (q:apply this [a b]))
       (invoke [this a b c]
               (q:apply this [a b c]))
       (invoke [this a b c d]
               (q:apply this [a b c d]))
       (invoke [this a b c d e]
               (q:apply this [a b c d e]))
       (invoke [this a b c d e f]
               (q:apply this [a b c d e f]))
       (invoke [this a b c d e f g]
               (q:apply this [a b c d e f g]))
       (invoke [this a b c d e f g h]
               (q:apply this [a b c d e f g h]))
       (invoke [this a b c d e f g h i]
               (q:apply this [a b c d e f g h i]))
       (invoke [this a b c d e f g h i j]
               (q:apply this [a b c d e f g h i j]))
       (invoke [this a b c d e f g h i j k]
               (q:apply this [a b c d e f g h i j k]))
       (invoke [this a b c d e f g h i j k l]
               (q:apply this [a b c d e f g h i j k l]))
       (invoke [this a b c d e f g h i j k l m]
               (q:apply this [a b c d e f g h i j k l m]))
       (invoke [this a b c d e f g h i j k l m n]
               (q:apply this [a b c d e f g h i j k l m n]))
       (invoke [this a b c d e f g h i j k l m n o]
               (q:apply this [a b c d e f g h i j k l m n o]))
       (invoke [this a b c d e f g h i j k l m n o p]
               (q:apply this [a b c d e f g h i j k l m n o p]))
       (invoke [this a b c d e f g h i j k l m n o p q]
               (q:apply this [a b c d e f g h i j k l m n o p q]))
       (invoke [this a b c d e f g h i j k l m n o p q r]
               (q:apply this [a b c d e f g h i j k l m n o p q r]))
       (invoke [this a b c d e f g h i j k l m n o p q r s]
               (q:apply this [a b c d e f g h i j k l m n o p q r s]))
       (invoke [this a b c d e f g h i j k l m n o p q r s t]
               (q:apply this [a b c d e f g h i j k l m n o p q r s t]))
       (invoke [this a b c d e f g h i j k l m n o p q r s t rest]
               (q:apply this (into [a b c d e f g h i j k l m n o p q r s t] rest)))
       (applyTo [s xs] (AFn/applyToHelper s xs))]

      :cljs
      [Object
       (toString [_]
                 (str "#sicm/quaternion [" r " " i  " " j " " k "]"))

       IMeta
       (-meta [_] m)

       IWithMeta
       (-with-meta [_ m] (Quaternion. r i j k m))

       IPrintWithWriter
       (-pr-writer [x writer _]
                   (write-all writer (.toString x)))

       ;; ICollection
       ;; (-conj [_ item] (. orientation (-conj v item) m))

       IEmptyableCollection
       (-empty [_] (Quaternion. 0 0 0 0 m))

       ISequential

       IEquiv
       (-equiv [this that] (q:= this that))

       ISeqable
       (-seq [_] (list r i j k))

       ICounted
       (-count [_] 4)

       ;; IIndexed
       ;; (-nth [_ n] (-nth v n))
       ;; (-nth [_ n not-found] (-nth v n not-found))

       ;; ILookup
       ;; (-lookup [_ k] (-lookup v k))
       ;; (-lookup [_ k not-found] (-lookup v k not-found))

       ;; IAssociative
       ;; (-assoc [_ k entry] (Structure. orientation (-assoc v k entry) m))
       ;; (-contains-key? [_ k] (-contains-key? v k))

       ;; IFind
       ;; (-find [_ n] (-find v n))

       ;; IReduce
       ;; (-reduce [_ f] (-reduce v f))
       ;; (-reduce [_ f start] (-reduce v f start))

       IFn
       (-invoke [this]
                (q:apply this []))
       (-invoke [this a]
                (q:apply this [a]))
       (-invoke [this a b]
                (q:apply this [a b]))
       (-invoke [this a b c]
                (q:apply this [a b c]))
       (-invoke [this a b c d]
                (q:apply this [a b c d]))
       (-invoke [this a b c d e]
                (q:apply this [a b c d e]))
       (-invoke [this a b c d e f]
                (q:apply this [a b c d e f]))
       (-invoke [this a b c d e f g]
                (q:apply this [a b c d e f g]))
       (-invoke [this a b c d e f g h]
                (q:apply this [a b c d e f g h]))
       (-invoke [this a b c d e f g h i]
                (q:apply this [a b c d e f g h i]))
       (-invoke [this a b c d e f g h i j]
                (q:apply this [a b c d e f g h i j]))
       (-invoke [this a b c d e f g h i j k]
                (q:apply this [a b c d e f g h i j k]))
       (-invoke [this a b c d e f g h i j k l]
                (q:apply this [a b c d e f g h i j k l]))
       (-invoke [this a b c d e f g h i j k l m]
                (q:apply this [a b c d e f g h i j k l m]))
       (-invoke [this a b c d e f g h i j k l m n]
                (q:apply this [a b c d e f g h i j k l m n]))
       (-invoke [this a b c d e f g h i j k l m n o]
                (q:apply this [a b c d e f g h i j k l m n o]))
       (-invoke [this a b c d e f g h i j k l m n o p]
                (q:apply this [a b c d e f g h i j k l m n o p]))
       (-invoke [this a b c d e f g h i j k l m n o p q]
                (q:apply this [a b c d e f g h i j k l m n o p q]))
       (-invoke [this a b c d e f g h i j k l m n o p q r]
                (q:apply this [a b c d e f g h i j k l m n o p q r]))
       (-invoke [this a b c d e f g h i j k l m n o p q r s]
                (q:apply this [a b c d e f g h i j k l m n o p q r s]))
       (-invoke [this a b c d e f g h i j k l m n o p q r s t]
                (q:apply this [a b c d e f g h i j k l m n o p q r s t]))
       (-invoke [this a b c d e f g h i j k l m n o p q r s t rest]
                (q:apply this (into [a b c d e f g h i j k l m n o p q r s t] rest)))]))

(do (ns-unmap 'sicmutils.quaternion '->Quaternion)
    (defn ->Quaternion
      "Positional factory function for [[Quaternion]].

  The final argument `m` defaults to nil if not supplied."
      ([r i j k]
       (Quaternion. r i j k nil))
      ([r i j k m]
       (Quaternion. r i j k m))))

#?(:clj
   (defmethod print-method Quaternion [^Quaternion q ^java.io.Writer w]
     (.write w (.toString q))))

(defn quaternion? [q]
  (instance? Quaternion q))

(defn get-r
  "Get the r component of a quaternion."
  [^Quaternion q]
  (.-r q))

(defn real-part [^Quaternion q]
  (.-r q))

(defn get-i
  "Get the i component of a quaternion."
  [^Quaternion q]
  (.-i q))

(defn get-j
  "Get the j component of a quaternion."
  [^Quaternion q]
  (.-j q))

(defn get-k
  "Get the k component of a quaternion."
  [^Quaternion q]
  (.-k q))

(defn ->vector [^Quaternion q]
  [(.-r q) (.-i q) (.-j q) (.-k q)])

(defn three-vector [^Quaternion q]
  [(.-i q) (.-j q) (.-k q)])

;; Constructors
;;
;; TODO check for duplicates!

(defn make
  "Same as `make`, and `real&3vector->quaternion`... plus one more."
  ([v]
   (if (instance? Quaternion v)
     v
     (apply ->Quaternion v)))
  ([r [i j k]]
   (->Quaternion r i j k))
  ([r i j k]
   (->Quaternion r i j k)))

(def ^{:doc "The identity quaternion."}
  q:identity
  (make 1 0 0 0))

(defn from-angle-normal-axis
  "Create a quaternion from an angle in radians and a normalized axis vector."
  [angle [x y z]]
  (let [half-angle (g// angle 2)
        half-sine (g/sin half-angle)]
    (->Quaternion (g/cos half-angle)
                  (g/* half-sine x)
                  (g/* half-sine y)
                  (g/* half-sine z))))

(defn from-angle-axis
  "Create a quaternion from an angle in radians and an arbitrary axis vector."
  [angle axis]
  (let [vv (g/abs axis)]
    (from-angle-normal-axis angle (g// axis vv))))

(defn pitch
  "Create a quaternion representing a pitch rotation by an angle in radians."
  [angle]
  (from-angle-normal-axis angle [1 0 0]))

(defn yaw
  "Create a quaternion representing a yaw rotation by an angle in radians."
  [angle]
  (from-angle-normal-axis angle [0 1 0]))

(defn roll
  "Create a quaternion representing a roll rotation by an angle in radians."
  [angle]
  (from-angle-normal-axis angle [0 0 1]))

;; Algebra

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

(defn magnitude
  "The norm of the quaternion."
  [^Quaternion q]
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

(defn q:= [^Quaternion q1 q2]
  (or (identical? q1 q2)
      (and (instance? Quaternion q2)
           (let [q2 ^Quaternion q2]
             (and (v/= (.-r q1) (.-r q2))
                  (v/= (.-i q1) (.-i q2))
                  (v/= (.-j q1) (.-j q2))
                  (v/= (.-k q1) (.-k q2)))))
      (and (counted? q2)
           (= (count q2) 4)
           (= (.-r q1) (q2 0))
           (= (.-i q1) (q2 1))
           (= (.-j q1) (q2 2))
           (= (.-k q1) (q2 3)))))

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
        (if-let [b (f/combine-arities a (f/arity (nth v i)))]
          (recur (inc i) b)
          false)))))

(defn partial-derivative [^Quaternion q selectors]
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
  (make (nth four-matrix 0)))

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

;; To rotate a 3-vector by the angle prescribed by a unit quaternion.

(comment
  ;; TODO see if it's more efficient to do this, AND if so move the next one to
  ;; the tests.

  (defn rotate
    "Rotate a vector with a quaternion."
    [^Quaternion q ^Vector3D v]
    (let [qx (.getX q), qy (.getY q), qz (.getZ q), qw (.getW q)
          vx (.getX v), vy (.getY v), vz (.getZ v)]
      (Vector3D.
       (+ (* qw qw vx)     (* 2 qy qw vz) (* -2 qz qw vy)  (* qx qx vx)
          (* 2 qy qx vy)   (* 2 qz qx vz) (- (* qz qz vx)) (- (* qy qy vx)))
       (+ (* 2 qx qy vx)   (* qy qy vy)   (* 2 qz qy vz)   (* 2 qw qz vx)
          (- (* qz qz vy)) (* qw qw vy)   (* -2 qx qw vz)  (- (* qx qx vy)))
       (+ (* 2 qx qz vx)   (* 2 qy qz vy) (* qz qz vz)     (* -2 qw qy vx)
          (- (* qy qy vz)) (* 2 qw qx vy) (- (* qx qx vz)) (* qw qw vz)))))
  )
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

        q0-2 (g/* quarter (g/+ 1 r11 r22 r33))

        q0q1 (g/* quarter (g/- r32 r23))
        q0q2 (g/* quarter (g/- r13 r31))
        q0q3 (g/* quarter (g/- r21 r12))
        q1q2 (g/* quarter (g/+ r12 r21))
        q1q3 (g/* quarter (g/+ r13 r31))
        q2q3 (g/* quarter (g/+ r23 r32))]
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

(defn ->rotation-matrix [^Quaternion q]
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

;; Generic Method Installation

(defmethod v/= [::quaternion ::quaternion] [a b] (q:= a b))

;; TODO test... maybe we just need vectors, not sequences?
(defmethod v/= [v/seqtype ::quaternion] [a b] (q:= a b))
(defmethod v/= [::quaternion v/seqtype] [a b] (q:= a b))

(defmethod g/simplify [::quaternion] [^Quaternion q]
  (->Quaternion
   (g/simplify (.-r q))
   (g/simplify (.-i q))
   (g/simplify (.-j q))
   (g/simplify (.-k q))
   (meta q)))

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


;; TODO see remaining interfaces here, and PICK the functions below: https://github.com/weavejester/euclidean/blob/master/src/euclidean/math/quaternion.clj

(comment
  (defn axes
    "Return the three axes of the quaternion."
    [^Quaternion q]
    (let [n  (norm q),  s  (if (> n 0) (/ 2.0 n) 0.0)
          x  (.getX q), y  (.getY q), z  (.getZ q), w  (.getW q)
          xs (* x s),   ys (* y s),   zs (* z s),   ws (* w s)
          xx (* x xs),  xy (* x ys),  xz (* x zs),  xw (* x ws)
          yy (* y ys),  yz (* y zs),  yw (* y ws)
          zz (* z zs),  zw (* z ws)]
      [(Vector3D. (- 1.0 (+ yy zz)) (+ xy zw) (- xz yw))
       (Vector3D. (- xy zw) (- 1.0 (+ xx zz)) (+ yz xw))
       (Vector3D. (+ xz yw) (- yz xw) (- 1.0 (+ xx yy)))]))

  (defn from-axes
    "Create a quaternion from three axis vectors."
    [^Vector3D x-axis ^Vector3D y-axis ^Vector3D z-axis]
    (let [m00 (.getX x-axis), m01 (.getX y-axis), m02 (.getX z-axis)
          m10 (.getY x-axis), m11 (.getY y-axis), m12 (.getY z-axis)
          m20 (.getZ x-axis), m21 (.getZ y-axis), m22 (.getZ z-axis)
          trace (+ m00 m11 m22)]
      (cond
        (>= trace 0)
        (let [s (Math/sqrt (inc trace))
              r (/ 0.5 s)]
          (Quaternion. (* r (- m21 m12))
                       (* r (- m02 m20))
                       (* r (- m10 m01))
                       (* 0.5 s)))
        (and (> m00 m11) (> m00 m22))
        (let [s (Math/sqrt (- (inc m00) m11 m22))
              r (/ 0.5 s)]
          (Quaternion. (* 0.5 s)
                       (* r (+ m10 m01))
                       (* r (+ m02 m20))
                       (* r (- m21 m12))))
        (> m11 m22)
        (let [s (Math/sqrt (- (inc m11) m00 m22))
              r (/ 0.5 s)]
          (Quaternion. (* r (+ m10 m01))
                       (* 0.5 s)
                       (* r (+ m21 m12))
                       (* r (- m02 m20))))
        :else
        (let [s (Math/sqrt (- (inc m22) m00 m11))
              r (/ 0.5 s)]
          (Quaternion. (* r (+ m02 m20))
                       (* r (+ m21 m12))
                       (* 0.5 s)
                       (* r (- m10 m01)))))))

  (defn look-at
    "Create a quaternion that is directed at a point specified by a vector."
    [direction up]
    (let [z-axis (v/normalize direction)
          x-axis (v/normalize (v/cross up direction))
          y-axis (v/normalize (v/cross direction x-axis))]
      (from-axes x-axis y-axis z-axis))))


(comment
  ;; TODO look at https://github.com/infusion/Quaternion.js/ and see if there
  ;; are more functions that we want!
  )
