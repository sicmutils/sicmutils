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

(ns sicmutils.numerical.quadrature.infinite
  (:require [clojure.core.match :refer [match]]
            [sicmutils.numerical.quadrature.common :as qc]
            [sicmutils.numerical.quadrature.substitute :as qs]))

;; Simpler version of `evaluate-improper-integral`, from Press, et al.
;; http://phys.uri.edu/nigh/NumRec/bookfpdf/f4-4.pdf
;;
(defn- fill-defaults [opts]
  (merge {:breakpoint 1
          :interval qc/open}
         opts))

(defn evaluate-infinite-integral
  "Handles all integrals with an infinite endpoint. I think there are a lot of
  fuckups going on here. You really want to be able to control WHAT method is
  getting used inside for the pieces... well, to a point. but right now we
  hardcode a LOT in here. This is not great design.

  DONE In the Press book, he shows how you can basically just replace... the
  endpoints, etc, in the midpoint routine and get everything working out of the
  box. That is definitely a WAY clearer way to do things - handle the variable
  change with a wrapper to the entire midpoint function, the entire open-open
  function itself, not doing it so weird and ad hoc here.

  :breakpoint Where should you choose the breakpoint? At a sufficiently large
  positive value so that the function funk is at least beginning to approach its
  asymptotic decrease to zero value at infinity. The polynomial extrapolation
  implicit in the second call to qromo deals with a polynomial in 1/x, not in
  x."
  ([integrator f a b]
   (evaluate-infinite-integral integrator f a b {}))
  ([integrator f a b opts]
   {:pre [(or (qc/infinite? a)
              (qc/infinite? b))]}
   (let [{:keys [interval breakpoint] :as opts} (fill-defaults opts)
         call (fn [integrate l r interval]
                (integrate f l r (assoc opts :interval interval)))
         integrate     (partial call integrator)
         inf-integrate (partial call (qs/infinitize integrate))
         r-break  (Math/abs breakpoint)
         l-break  (- r-break)]
     (match [[a b]]
            [(:or [:-infinity :-infinity]
                  [:+infinity :+infinity])]
            0.0

            [(:or [_ :-infinity] [:+infinity _])]
            (- (evaluate-infinite-integral f b a opts))

            ;; Break the region up into three pieces: a central closed core and
            ;; two open endpoints where we create a change of variables, letting
            ;; the boundary go to infinity. We use an OPEN interval on the
            ;; infinite side.
            [(:or [:-infinity :+infinity]
                  [:+infinity :-infinity])]
            (let [-inf->l (inf-integrate a l-break qc/open-closed)
                  l->r    (integrate     l-break r-break qc/closed)
                  r->+inf (inf-integrate r-break b qc/closed-open)]
              (+ -inf->l l->r r->+inf))

            [[:-infinity _]]
            (if (<= b l-break)
              (inf-integrate a b interval)
              (let [-inf->l (inf-integrate a l-break qc/open-closed)
                    l->b    (integrate     l-break b (qc/close-l interval))]
                (+ -inf->l l->b)))

            [[_ :+infinity]]
            (if (>= a r-break)
              (inf-integrate a b interval)
              (let [a->r    (integrate     a r-break (qc/close-r interval))
                    r->+inf (inf-integrate r-break b qc/closed-open)]
                (+ a->r r->+inf)))))))
