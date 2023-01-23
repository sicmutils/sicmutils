#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.sr.boost
  (:refer-clojure :exclude [+ - * /])
  (:require [emmy.generic :as g :refer [+ - * /]]
            [emmy.structure :refer [up]]))

;; ## Special Relativity -- Boosts

(defn make-four-tuple [ct [x y z]]
  (up ct x y z))

(defn four-tuple->ct [[ct]] ct)

(defn four-tuple->space [[_ x y z]]
  (up x y z))

(defn proper-time-interval [four-tuple]
  (g/sqrt
   (- (g/square (four-tuple->ct four-tuple))
      (g/square (four-tuple->space four-tuple)))))

(defn proper-space-interval [four-tuple]
  (g/sqrt
   (- (g/square (four-tuple->space four-tuple))
      (g/square (four-tuple->ct four-tuple)))))

(defn general-boost [beta]
  (fn [xi-p]
    (let [gamma (/ 1 (g/sqrt (- 1 (g/square beta))))
          factor (/ (+ -1 gamma)
                    (g/square beta))
          xi-p-time  (four-tuple->ct xi-p)
          xi-p-space (four-tuple->space xi-p)
          beta-dot-xi-p (g/dot-product beta xi-p-space)]
      (make-four-tuple
       (* gamma (+ xi-p-time beta-dot-xi-p))
       (+ (* gamma beta xi-p-time)
          xi-p-space
          (* factor beta beta-dot-xi-p))))))

;; It is inconvenient that the general boost as just defined does not work if
;; $\bfbeta$ is zero. An alternate way to specify a boost is through the
;; magnitude of $v/c$ and a direction:
;;
;; this one works for zero v:c ... direction is a unit 3-vector, v:c is the
;; speed, a number.

(defn general-boost2
  "Takes a unit 3-vector `direction` (representing a direction) and a velocity
  `v:c` normalized by `C`."
  [direction v:c]
  (fn [four-tuple-prime]
    (let [delta-ct-prime (four-tuple->ct four-tuple-prime)
          delta-x-prime (four-tuple->space four-tuple-prime)
          betasq (g/square v:c)
          bx (g/dot-product direction delta-x-prime)
          gamma (/ 1 (g/sqrt (- 1 betasq)))
          alpha (- gamma 1)
          delta-ct (* gamma (+ delta-ct-prime (* bx v:c)))
          delta-x (+ (* gamma v:c direction delta-ct-prime)
                     delta-x-prime
                     (* alpha direction bx))]
      (make-four-tuple delta-ct delta-x))))

;; ## extended rotations

;; Boosts are linear functions of incremental vectors. To be parallel we take
;; rotations to functions as well, rather than as multipliers.

(defn extended-rotation [R]
  (fn [xi-p]
    (make-four-tuple
     (four-tuple->ct xi-p)
     (R (four-tuple->space xi-p)))))
