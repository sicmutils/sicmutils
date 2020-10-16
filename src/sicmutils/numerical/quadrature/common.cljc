;;
;; Copyright © 2020 Sam Ritchie.
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

(ns sicmutils.numerical.quadrature.common
  (:require [sicmutils.util.stream :as us]
            [taoensso.timbre :as log]))

(def ^:dynamic *roundoff-cutoff* 1e-14)

;; Intervals... these no longer wrap the values themselves, but live alongside.

(def open        [::open ::open])
(def closed      [::closed ::closed])
(def open-closed [::open ::closed])
(def closed-open [::closed ::open])

(defn closed? [x] (= x closed))
(def open? (complement closed?))

(defn close-l [[_ r]] [::closed r])
(defn close-r [[l _]] [l ::closed])
(defn open-l [[_ r]] [::open r])
(defn open-r [[l _]] [l ::open])
(defn flip [[l r]] [r l])

(defn update-interval [opts f]
  (update-in opts [:interval] f))

(def infinities #{:-infinity :+infinity})
(def infinite? (comp boolean infinities))

(defn- narrow-slice?
  "Returns true if we're in the middle of a strip whose width is smaller than the
  roundoff error between the two endpoints, false otherwise. (TODO figure out if
  this is a true statement!)

  |b - a| / |a| + |b| <= `cutoff`"
  [a b cutoff]
  (let [diff   (Math/abs (- b a))
        sum    (+ (Math/abs a)
                  (Math/abs b))]
    (<= diff (* cutoff sum))))

(defn make-integrator
  "`area-fn` is called in cases where the `a` and `b` are too narrow to
  generate any intervals.

  `seq-fn` generates a sequence of estimates."
  [area-fn seq-fn]
  (fn call
    ([f a b] (call f a b {}))
    ([f a b {:keys [roundoff-cutoff]
             :or {roundoff-cutoff *roundoff-cutoff*}
             :as opts}]
     (if (narrow-slice? a b roundoff-cutoff)
       (do (log/info "Integrating narrow slice: " a b)
           {:converged true
            :terms-checked 1
            :result (area-fn f a b)})
       :else (-> (seq-fn f a b opts)
                 (us/seq-limit opts))))))


(comment
  ;; TODO move `:accelerate?` into the SEQUENCE functions... so that
  ;; `make-integrator` can call those sequence fns.

  (def integrate-closed-finite
    (make-integrator
     trap/single-trapezoid
     (fn [f a b _]
       (bs/closed-sequence f a b))))

  (def integrate-open-finite
    (make-integrator
     mid/single-midpoint
     (fn [f a b _]
       (bs/open-sequence f a b)))))
