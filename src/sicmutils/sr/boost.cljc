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

(ns sicmutils.sr.bost
  (:refer-clojure :exclude [+ - * /])
  (:require [sicmutils.generic :as g :refer [+ - * /]]))


;; ## Special Relativity -- Boosts

(comment
  (define (make-4tuple ct space)
    (up ct (ref space 0) (ref space 1) (ref space 2)))

  (define (4tuple->ct v)
    (ref v 0))

  (define (4tuple->space v)
    (up (ref v 1) (ref v 2) (ref v 3)))


  (define (proper-time-interval 4tuple)
    (sqrt (- (square (4tuple->ct 4tuple))
             (square (4tuple->space 4tuple)))))

  (define (proper-space-interval 4tuple)
    (sqrt (- (square (4tuple->space 4tuple))
             (square (4tuple->ct 4tuple)))))


  (define ((general-boost beta) xi-p)
    (let ((gamma (expt (- 1 (square beta)) -1/2)))
      (let ((factor (/ (- gamma 1) (square beta))))
        (let ((xi-p-time (4tuple->ct xi-p))
              (xi-p-space (4tuple->space xi-p)))
          (let ((beta-dot-xi-p (dot-product beta xi-p-space)))
            (make-4tuple
             (* gamma (+ xi-p-time beta-dot-xi-p))
             (+ (* gamma beta xi-p-time)
                xi-p-space
                (* factor beta beta-dot-xi-p))))))))
  )

;; It is inconvenient that the general boost as just defined does not
;; work if $\bfbeta$ is zero.  An alternate way to specify a boost is
;; through the magnitude of $v/c$ and a direction:
;;
;; this one works for zero v/c ...
;; direction is a unit 3-vector, v/c is the speed, a number.

(comment
  (define ((general-boost2 direction v/c) 4tuple-prime)
    (let ((delta-ct-prime (4tuple->ct 4tuple-prime))
          (delta-x-prime (4tuple->space 4tuple-prime)))
      (let ((betasq (square v/c)))
        (let ((bx (dot-product direction delta-x-prime))
              (gamma (/ 1 (sqrt (- 1 betasq)))))
          (let ((alpha (- gamma 1)))
            (let ((delta-ct
                   (* gamma (+ delta-ct-prime (* bx v/c))))
                  (delta-x
                   (+ (* gamma v/c direction delta-ct-prime)
                      delta-x-prime
                      (* alpha direction bx))))
              (make-4tuple delta-ct delta-x))))))))


(comment
  ;;----------------------------------------------------------------
  ;; extended rotations

  ;; Boosts are linear functions of incremental vectors.
  ;; To be parallel we take rotations to functions as well
  ;; rather than as multipliers.

  (define ((extended-rotation R) xi-p)
    (make-4tuple
     (4tuple->ct xi-p)
     (R (4tuple->space xi-p))))
  )
