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

(ns sicmutils.modint
  (:require [sicmutils.euclid :as e]
            [sicmutils.generic :as g]
            [sicmutils.util :as u]
            [sicmutils.value :as v]))

;; TODO copy this approach. `residue` == i in this case.

(comment
  (define (mod:= x y)
    (assert (and (modint? x) (modint? y))
            "Not modular integers -- =" (list x y))
    (let ((modulus (mod:modulus x)))
      (assert (int:= modulus (mod:modulus y))
              "Not same modulus -- =" (list x y))
      (int:= (modulo (mod:residue x) modulus)
             (modulo (mod:residue y) modulus)))))

(deftype ModInt [i m]
  v/Value
  (nullity? [_] (v/nullity? i))
  (unity? [_] (v/unity? i))
  (zero-like [_] (ModInt. (v/zero-like i) m))
  (one-like [_] (ModInt. (v/one-like i) m))
  (exact? [_] true)
  (numerical? [_] true)
  (kind [_] ::modint)

  ;; TODO add a string representation, etc, and a type tag so we can define
  ;; these with a pair.
  #?@(:clj
      [Object
       (equals [_ b])]

      :cljs
      [IEquiv
       (-equiv [_ b])])

  )

(defn make [i m]
  (ModInt. (g/modulo i m) m))

(defn ^:private modular-binop [op]
  (fn [^ModInt a ^ModInt b]
    (if-not (= (.-m a) (.-m b))
      (u/arithmetic-ex "unequal moduli")
      (make (op (.-i a) (.-i b)) (.-m a)))))

(defn ^:private modular-inv [^ModInt m]
  (let [modulus (:m m)
        [g a _] (e/extended-gcd (:i m) modulus)]
    (if (< g 2) (make a modulus)
        (u/arithmetic-ex (str m " is not invertible mod " modulus)))))

(def ^:private add (modular-binop g/add))
(def ^:private sub (modular-binop g/sub))
(def ^:private mul (modular-binop g/mul))
(def ^:private remainder (modular-binop g/remainder))
(def ^:private modulo (modular-binop g/modulo))

(defmethod g/add [::modint ::modint] [a b] (add a b))
(defmethod g/mul [::modint ::modint] [a b] (mul a b))
(defmethod g/sub [::modint ::modint] [a b] (sub a b))
(defmethod g/negate [::modint] [a] (make (g/negate (:i a)) (:m a)))
(defmethod g/invert [::modint] [a] (modular-inv a))
(defmethod g/magnitude [::modint] [{:keys [i m] :as a}] (g/modulo i m))
(defmethod g/abs [::modint] [{:keys [i m] :as a}] (if (g/negative? i)
                                                    (make i m)
                                                    a))
(defmethod g/quotient [::modint ::modint] [a b] (mul a (modular-inv b)))
(defmethod g/remainder [::modint ::modint] [a b] (remainder a b))
(defmethod g/modulo [::modint ::modint] [a b] (modulo a b))
(defmethod g/exact-divide [::modint ::modint] [a b] (mul a (modular-inv b)))
(defmethod g/negative? [::modint] [a] (g/negative? (:i a)))

;; TODO add exponent
(comment
  (define (modint:expt base exponent p)
    (define (square x)
      (modint:* x x p))
    (let lp ((exponent exponent))
         (cond ((int:= exponent 0) 1)
               ((even? exponent)
                (square (lp (quotient exponent 2))))
               (else
                (modint:* base (lp (int:- exponent 1)) p))))))


;; Methods that allow interaction with other integral types. The first block is
;; perhaps slightly more efficient:
(doseq [op [g/add g/mul g/sub g/expt]]
  (defmethod op [::v/integral ::modint] [a b] (make (op a (:i b)) (:m b)))
  (defmethod op [::modint ::v/integral] [a b] (make (op (:i a) b) (:m a))))

;; The second block promotes any integral type to a ModInt before operating.
(doseq [op [g/quotient g/remainder g/exact-divide]]
  (defmethod op [::v/integral ::modint] [a b] (op (make a (:m b)) b))
  (defmethod op [::modint ::v/integral] [a b] (op a (make b (:m a)))))

(comment
  ;; TODO get this test going, whatever it is...
  ;; (define (test p)
  ;;   (let jlp ((j (- p)))
  ;;        (cond ((int:= j p) 'ok)
  ;;              (else
  ;;               (let ilp ((i (- p)))
  ;;                    ;;(write-line `(trying ,i ,j))
  ;;                    (cond ((int:= i p) (jlp (int:+ j 1)))
  ;;                          ((int:= (modulo i p) 0) (ilp (int:+ i 1)))
  ;;                          (else
  ;;                           (let ((jp (mod:make j p))
  ;;                                 (ip (mod:make i p)))
  ;;                             (let ((b (mod:/ jp ip)))
  ;;                               (if (mod:= (mod:* b ip) jp)
  ;;                                 (ilp (int:+ i 1))
  ;;                                 (begin (write-line `(problem dividing ,j ,i))
  ;;                                        (write-line `((/ ,jp ,ip) =  ,(mod:/ jp ip)))
  ;;                                        (write-line `((* ,b ,ip) = ,(mod:* b ip))))))))))))))

  ;; (test 47)
  )
