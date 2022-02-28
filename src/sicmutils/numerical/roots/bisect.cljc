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
  (:require [sicmutils.util.stream :as us]))

;; ## Root finding by successive bisection

(def ^{:dynamic true
       :doc "If `true` the functions in this namespace will throw an error when
  encountering bounds of the same sign (in which no root is guaranteed.) If
  `false`, functions will return false.

  `false` by default."}
  *bisect-error?* false)

(def ^{:dynamic true
       :doc "If bound to some function `f`, [[bisect]] will pass each vector `[x0,
  x1]` it encounters during search to `f`."}
  *bisect-tap* false)

(def ^{:dynamic true
       :doc "Controls the default behavior of [[bisect]]'s search.
  See [[bisect]]'s docstring for more info."}
  *bisect-break* 60)

;; ### Simple bisection search
;;
;; Note from GJS: "In IEEE 754 binary floating point I think this will always
;; converge to full precision, but I have not proved it."

(defn bisect-1
  "Given a smooth function `f` and (inclusive) lower and upper bounds `x0` and
  `x1` on the domain, attempts to find a root of `f`, ie, a value `x` for
  which `(f x)` is equal to 0.

  [[bisect-1]] works by successively bisecting the range until it's not possible
  to distinguish neighboring values.

  Errors if the bounds `x0` and `x1` do not contain a root."
  [f x0 x1]
  (let [fx0 (f x0)
        fx1 (f x1)]
    (if (> (* fx0 fx1) 0.0)
      (if *bisect-error?*
        (throw
         (ex-info "root not bounded"
                  {:x0 x0 :x1 x1 :fx0 fx0 :fx1 fx1}))
        false)
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
                          (recur x0 fx0 xm fxm))))))))))

(defn bisect-2
  "Given a smooth function `f` and (inclusive) lower and upper bounds `x0` and
  `x1` on the domain, attempts to find a root of `f`, ie, a value `x` for
  which `(f x)` is equal to 0.

  Like [[bisect-1]], [[bisect-2]] successively bisects the domain of `f` in its
  search for a root. Unlike [[bisect-1]], [[bisect-2]] terminates (and returns
  the midpoint) when the down search narrows down to a bound less than `eps`
  wide.

  Errors if the bounds `x0` and `x1` do not contain a root."
  [f x0 x1 eps]
  (let [close? (us/close-enuf? eps)]
    (loop [x0 x0 fx0 (f x0) x1 x1 fx1 (f x1)]
      (cond (zero? fx0) x0
            (zero? fx1) x1
            :else
            (if (> (* fx1 fx0) 0.0)
              (if *bisect-error?*
                (throw
                 (ex-info "root not bounded"
                          {:x0 x0 :x1 x1 :fx0 fx0 :fx1 fx1}))
                false)
              (let [xm (/ (+ x0 x1) 2.0)]
                (if (close? x0 x1)
                  xm
                  (let [fxm (f xm)]
                    (if (< (* fx1 fxm) 0.0)
                      (recur xm fxm x1 fx1)
                      (recur x0 fx0 xm fxm))))))))))

;; ### Bisection with interpolation

(defn bisect-fp
  "Given a smooth function `f` and (inclusive) lower and upper bounds `x0` and
  `x1` on the domain, attempts to find a root of `f`, ie, a value `x` for
  which `(f x)` is equal to 0.

  [[bisect-fp]] chooses the split point for its searches by interpolating
  between `(x0, f(x0))` and `(x1, f(x1))` during each cut.

  [[bisect-fp]] terminates (and returns the midpoint) when the search narrows
  down to a bound less than `eps` wide.

  Errors if the bounds `x0` and `x1` do not contain a root."
  [f x0 x1 eps]
  (let [close? (us/close-enuf? eps)]
    (loop [x0 x0 fx0 (f x0) x1 x1 fx1 (f x1)]
      (cond (zero? fx0) x0
            (zero? fx1) x1
            :else
            (if (> (* fx1 fx0) 0.0)
              (if *bisect-error?*
                (throw
                 (ex-info "root not bounded"
                          {:x0 x0 :x1 x1 :fx0 fx0 :fx1 fx1}))
                false)
              (let [xm (/ (- (* fx1 x0)
                             (* fx0 x1))
                          (- fx1 fx0))]
                (if (close? x0 x1)
                  xm
                  (let [fxm (f xm)]
                    (if (< (* fx1 fxm) 0.0)
                      (recur xm fxm x1 fx1)
                      (recur x0 fx0 xm fxm))))))))))

;; ### Mixed strategy

(defn bisect
  "Given a smooth function `f` and (inclusive) lower and upper bounds `x0` and
  `x1` on the domain, attempts to find a root of `f`, ie, a value `x` for
  which `(f x)` is equal to 0.

  [[bisect]] uses a strategy mixed between that of [[bisect-2]]
  and [[bisect-fp]]:

  - for iterations up to `n-break` uses midpoint
  - for iterations after `n-break` uses linear interpolation

  `n-break` defaults to the dynamically bindable `*bisect-break*` (which
  defaults to 60). Bind `*bisect-break*` to modify the behavior of [[bisect]]
  when it's used inside a nested routine.

  If `*bisect-tap*` is bound to a function, it will be called with the vector
  `[x0 x1]` for each range investigated.

    If `*bisect-error*` is bound to true, [[bisect]] will throw if the bounds
  `x0` and `x1` do not contain a root. If bound to `false`, returns `false` if
  no root is found."
  ([f x0 x1 eps]
   (bisect f x0 x1 eps *bisect-break*))
  ([f x0 x1 eps n-break]
   (let [done? (us/close-enuf? eps)]
     (loop [x0 x0
            fx0 (f x0)
            x1 x1
            fx1 (f x1)
            iter 0]
       (when *bisect-tap*
         (*bisect-tap* [x0 x1]))

       (cond (zero? fx0) x0
             (zero? fx1) x1

             (> (* fx1 fx0) 0.0)
             (if *bisect-error?*
               (throw
                (ex-info "root not bounded"
                         {:x0 x0 :x1 x1 :fx0 fx0 :fx1 fx1}))
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

(defn search-for-roots
  "Given a smooth function `f` and (inclusive) lower and upper bounds `x0` and
  `x1` on the domain, attempts to find all roots of `f`, ie, a vector of values
  `x_n` such that each `(f x_n)` is equal to 0.

  [[search-for-roots]] first attempts to cut the (inclusive) range `[x0, x1]`
  into pieces at most `dx` wide; then [[bisect]] is used to search each segment
  for a root."
  [f x0 x1 eps dx]
  (letfn [(find-roots [x0 x1]
            (let [f1 (f x1) f0 (f x0)]
              (if (< (Math/abs (- x1 x0)) dx)
                (if (< (* f0 f1) 0.0)
                  [(bisect f x0 x1 eps)]
                  [])
                (let [xm (/ (+ x0 x1) 2.0)]
                  (into (find-roots x0 xm)
                        (find-roots xm x1))))))]
    (find-roots x0 x1)))
