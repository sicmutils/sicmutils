;;
;; Copyright © 2022 Sam Ritchie.
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

(ns sicmutils.numerical.roots.bisect
  (:require [sicmutils.util :as u]
            [sicmutils.util.stream :as us]
            [taoensso.timbre :as log]))

;; ## Root finding by successive bisection

;; ### Simple bisection search

;; In IEEE 754 binary floating point I think this will always converge to full
;; precision, but I have not proved it. GJS

(defn bisect-1 [f x0 x1]
  (let [fx0 (f x0)
        fx1 (f x1)]
    (when (> (* fx0 fx1) 0.0)
      (throw
       (ex-info "root not bounded"
                {:x0 x0 :x1 x1 :fx0 fx0 :fx1 fx1})))
    (loop [x0 x0 fx0 fx0 x1 x1 fx1 fx1]
      (cond (zero? fx0) x0
	          (zero? fx1) x1
	          :else
	          (let [xm (/ (+ x0 x1) 2.0)]
	            (cond (= x0 xm) x0
		                (= x1 xm) x1
		                :else
		                (let [fxm (f xm)]
			                (if (< (* fx1 fxm) 0.0)
			                  (recur xm fxm x1 fx1)
			                  (recur x0 fx0 xm fxm)))))))))

(defn bisect-2
  "Simple bisection search terminating when x0 close to x1."
  [f x0 x1 eps]
  (let [close? (us/close-enuf? eps)]
    (loop [x0 x0 fx0 (f x0) x1 x1 fx1 (f x1)]
      (cond (zero? fx0) x0
	          (zero? fx1) x1
	          :else
            (if (> (* fx1 fx0) 0.0)
		          (u/illegal "root not bounded")
		          (let [xm (/ (+ x0 x1) 2.0)]
		            (if (close? x0 x1)
		              xm
		              (let [fxm (f xm)]
			              (if (< (* fx1 fxm) 0.0)
			                (recur xm fxm x1 fx1)
			                (recur x0 fx0 xm fxm))))))))))


;;; Bisection with interpolation

(defn bisect-fp [f x0 x1 eps]
  (let [close? (us/close-enuf? eps)]
    (loop [x0 x0 fx0 (f x0) x1 x1 fx1 (f x1)]
      (cond (zero? fx0) x0
	          (zero? fx1) x1
            :else
            (if (> (* fx1 fx0) 0.0)
		          (u/illegal "root not bounded")
		          (let [xm (/ (- (* fx1 x0)
                             (* fx0 x1))
                          (- fx1 fx0))]
		            (if (close? x0 x1)
		              xm
		              (let [fxm (f xm)]
			              (if (< (* fx1 fxm) 0.0)
			                (recur xm fxm x1 fx1)
			                (recur x0 fx0 xm fxm))))))))))

;; for example

;; (define (kepler ecc m)
;;   (bisect-fp
;;    (lambda (e)
;;            (write-line e)
;;            (- e (* ecc (sin e)) m))
;;    0.0
;;    2pi
;;    1e-15))

;; (kepler .99 .01)
;; 6.283185307179586
;; 0.
;; .01
;; .01988423649613729
;; 2.9653394755776365e-2
;; 3.9307245802801455e-2
;; ;;; Total of 536 lines here -- ugh!
;; .3422703164917746
;; .3422703164917747
;; .34227031649177475
;; .3422703164917748
;; .34227031649177486
;; .342270316491775
;;                                         ;Value: .342270316491775

;;; Mixed strategy
;;;   for iterations up to *bisect-break* uses midpoint
;;;   for iterations after *bisect-break* uses linear interpolation

(def ^:dynamic *bisect-break* 60)

(def ^:dynamic *bisect-wallp* false)

(def ^:dynamic *bisect-error?* false)

(defn bisect
  ([f x0 x1 eps]
   (bisect f x0 x1 eps *bisect-break*))
  ([f x0 x1 eps n-break]
   (let [done? (us/close-enuf? eps)]
     (loop [x0 x0
            fx0 (f x0)
            x1 x1
            fx1 (f x1)
            iter 0]
       (when *bisect-wallp* (log/info [x0 x1]))
       (cond (zero? fx0) x0
	           (zero? fx1) x1

             (> (* fx1 fx0) 0.0)
             (if *bisect-error?*
               (u/illegal "root not bounded")
               false)
		         :else
             (let [xm (if (< iter n-break)
				                (/ (+ x0 x1) 2.0)
				                (/ (- (* fx1 x0) (* fx0 x1))
                           (- fx1 fx0)))]
		           (if (done? x0 x1)
			           xm
			           (let [fxm (f xm)]
			             (if (< (* fx1 fxm) 0.0)
			               (recur xm fxm x1 fx1 (inc iter))
			               (recur x0 fx0 xm fxm (inc iter)))))))))))

;; If we don't know anything, it is usually a good idea to break the interval
;; into dx-sized pieces and look for roots in each interval.

(defn find-a-root [f x0 x1 dx eps continue failure]
  (letfn [(find [x0 x1]
            (if (> (Math/abs (- x0 x1)) dx)
	            (let [f1 (f x1)
                    f0 (f x0)]
	              (if (< (* f0 f1) 0)
	                (continue (bisect f x0 x1 eps))
	                (let [xm (/ (+ x0 x1) 2)]
		                (find x0 xm)
		                (find xm x1))))
	            failure))]
    (find x0 x1)))

;; Collect the roots found.

(defn search-for-roots [f x0 x1 eps small]
  (letfn [(find-roots [x0 x1]
            (let [f1 (f x1) f0 (f x0)]
              (if (< (Math/abs (- x1 x0)) small)
	              (if (< (* f0 f1) 0)
	                [(bisect f x0 x1 eps)]
	                [])
	              (let [xm (/ (+ x0 x1) 2)]
	                (into (find-roots x0 xm)
		                    (find-roots xm x1))))))]
    (find-roots x0 x1)))
