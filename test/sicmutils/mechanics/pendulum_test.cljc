#_"SPDX-License-Identifier: GPL-3.0"

(ns sicmutils.mechanics.pendulum-test
  (:refer-clojure :exclude [+ - * / partial])
  (:require [clojure.test :refer [is deftest #_testing use-fixtures]]
            [same :refer [ish?] :include-macros true]
            [sicmutils.generic :as g :refer [+ - * / sin]]
            [sicmutils.mechanics.hamilton :as h]
            [sicmutils.mechanics.lagrange :as l]
            [sicmutils.mechanics.pendulum :as p]
            [sicmutils.numerical.derivative :refer [D-numeric]]
            [sicmutils.series :as series]
            [sicmutils.simplify :refer [hermetic-simplify-fixture]]
            [sicmutils.special.elliptic :as ell]
            [sicmutils.value :as v]))

(use-fixtures :each hermetic-simplify-fixture)

(def simplify
  (comp v/freeze g/simplify))


;; ## series solutions

;; don't use this without thinking...

(defn sum-series [term eps]
  (loop [n 1 sum 0.0 lastf 1.0]
    (let [f (term n)]
      (if (and (< (g/abs f) eps)
               (< (g/abs lastf) eps))
        sum
        (recur (inc n) (+ sum f) f)))))

;; purpose of checking last two is to prevent some premature terminations
;; because a term is "accidently" zero

(defn pendulum-oscillating-solution-series [alpha beta E omega eps]
  (fn [t]
    (let [k (g/sqrt (/ (+ E beta) (* 2.0 beta)))
          omega-0 (g/sqrt (/ beta alpha))
          Kp (ell/elliptic-k (g/sqrt (- 1.0 (g/square k))))
          term (fn [n]
                 (let [omega-n (* omega (- (* 2.0 n) 1.))]
                   (/ (sin (* omega-n t))
                      (* omega-n (g/cosh (/ (* omega-n Kp) omega-0))))))]
      (* 4.0 omega (sum-series term eps)))))

(defn pendulum-circulating-solution-series [alpha beta E omega eps]
  (fn [t]
    (let [k (g/sqrt (/ (* 2.0 beta) (+ E beta)))
          omega-R (g/sqrt (g/abs (/ (+ E beta) (* 2.0 alpha))))
          Kp (ell/elliptic-k (g/sqrt (- 1. (g/square k))))
          term (fn [n]
                 (let [omega-n (* omega n)]
                   (/ (sin (* omega-n t))
                      (* omega-n (g/cosh (/ (* omega-n Kp) omega-R))))))]
      (+ (* omega t)
         (* 2.0 omega (sum-series (term t) eps))))))

(defn pendulum-solution-series [alpha beta]
  (fn [state]
    (fn [t]
      (let [E ((p/Hpendulum alpha beta) state)
            omega (p/pendulum-frequency alpha beta E)
            beta (g/abs beta)]
        (if (< E beta)
          (let [k (g/sqrt (/ (+ E beta) (* 2 beta)))
                omega-0 (g/sqrt (g/abs (/ beta alpha)))
                Kp (ell/elliptic-k (g/sqrt (- 1 (g/square k))))
                term (fn [n]
                       (let [omega-n (* omega (- (* 2 n) 1))]
                         (/ (sin (* omega-n t))
                            (* omega-n (g/cosh (/ (* omega-n Kp) omega-0))))))]
            (* 4 omega (series/generate #(term (inc %)))))
          (let [k (g/sqrt (/ (* 2 beta) (+ E beta)))
                omega-R (g/sqrt (g/abs (/ (+ E beta) (* 2 alpha))))
                Kp (ell/elliptic-k (g/sqrt (- 1 (g/square k))))
                term (fn [t]
                       (fn [n]
                         (let [omega-n (* omega n)]
                           (/ (sin (* omega-n t))
                              (* omega-n (g/cosh (/ (* omega-n Kp) omega-R)))))))]
            (+ (* omega t)
               (* 2 omega (series/generate #((term t) (inc %)))))))))))

(deftest cake2-test
  (is (= '((* 1.8349993630564587 (sin (* 2.504396273593202 t)))
           (* 0.038213003445971006 (sin (* 7.5131888207796065 t)))
           (* 0.0013531286425114077 (sin (* 12.52198136796601 t)))
           (* 5.702944261999191E-5 (sin (* 17.530773915152416 t)))
           (* 2.6172332237417442E-6 (sin (* 22.53956646233882 t)))
           (* 1.2635138738869158E-7 (sin (* 27.548359009525225 t)))
           (* 6.308369363000467E-9 (sin (* 32.55715155671163 t)))
           (* 3.225945107424546E-10 (sin (* 37.56594410389803 t)))
           (* 1.6795273362666193E-11 (sin (* 42.574736651084436 t)))
           (* 8.866866369088412E-13 (sin (* 47.58352919827084 t))))
         (->> (((pendulum-solution-series 1.0 9.8)
                (h/->H-state 0.0 0.0 4.9006733894348145)) 't)
              (take 10)
              (simplify)))))

;; Check that the canonical transformation is area-preserving.

(defn der-qq [f [t q p]]
  ((D-numeric
    #(l/state->q
      (f (h/->H-state t % p)))
    {:tolerance 1.e-8
     :initial-h 0.01})
   q))

(defn der-qp [f [t q p]]
  ((D-numeric
    #(l/state->q
      (f (h/->H-state t q %)))
    {:tolerance 1.e-8
     :initial-h 0.01})
   p))

(defn der-pq [f [t q p]]
  ((D-numeric
    #(h/state->p
      (f (h/->H-state t % p)))
    {:tolerance 1.e-8
     :initial-h 0.01})
   q))

(defn der-pp [f [t q p]]
  ((D-numeric
    #(h/state->p
      (f (h/->H-state t q %)))
    {:tolerance 1.e-8
     :initial-h 0.01})
   p))

(deftest cake-test
  (let [f (p/pendulum-circulating-aa-state-to-state 2.0 9.8)
        g (p/pendulum-circulating-state-to-aa-state 2.0 9.8)]
    (let [state (h/->H-state 1.0 1.0 15.0)
          aa-state (g state)]
      (ish? (= 1.0000000000003484
               (- (* (der-qq f aa-state) (der-pp f aa-state))
                  (* (der-pq f aa-state) (der-qp f aa-state)))))

      (ish? (= 0.9999999999986688
               (- (* (der-qq g state) (der-pp g state))
                  (* (der-pq g state) (der-qp g state))))))

    (let [state    (h/->H-state 1.0 1.0 1.0)
          aa-state (g state)]
      (ish? (= 1.000000000000406
               (- (* (der-qq f aa-state) (der-pp f aa-state))
                  (* (der-pq f aa-state) (der-qp f aa-state)))))
      (ish? (= 1.000000000000521
               (- (* (der-qq g state) (der-pp g state))
                  (* (der-pq g state) (der-qp g state))))))))
