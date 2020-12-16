;;
;; Copyright © 2017 Colin Smith.
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

(ns sicmutils.numerical.elliptic
  (:require [sicmutils.util :as u]
            [sicmutils.value :as v]))

(defn carlson-rf
  "From W.H. Press, Numerical Recipes in C++, 2ed. NR::rf from section 6.11

  Here's the reference for what's going on here:
  http://phys.uri.edu/nigh/NumRec/bookfpdf/f6-11.pdf

  Comment from Press, page 257:

  'Computes Carlson’s elliptic integral of the first kind, RF (x, y, z). x, y,
  and z must be nonnegative, and at most one can be zero. TINY must be at least
  5 times the machine underflow limit, BIG at most one fifth the machine
  overflow limit.'

  A value of 0.08 for the error tolerance parameter is adequate for single
  precision (7 significant digits). Since the error scales as 6 n, we see that
  0.0025 will yield double precision (16 significant digits) and require at most
  two or three more iterations.'

  This is called `Carlson-elliptic-1` in scmutils."
  [x y z]
  (let [errtol 0.0025
        tiny 1.5e-38
        big 3.0e37
        third (/ 1.0 3.0)
        c1 (/ 1 24.0)
        c2 0.1
        c3 (/ 3.0 44.0)
        c4 (/ 1.0 14.0)]
    (when (or (< (min x y z) 0)
              (< (min (+ x y) (+ x z) (+ y z)) tiny)
              (> (max x y z) big))
      (u/illegal "Carlson R_F"))
    (loop [xt x
           yt y
           zt z]
      (let [sqrtx (Math/sqrt xt)
            sqrty (Math/sqrt yt)
            sqrtz (Math/sqrt zt)
            alamb (+ (* sqrtx (+ sqrty sqrtz))
                     (* sqrty sqrtz))
            xt' (* 0.25 (+ xt alamb))
            yt' (* 0.25 (+ yt alamb))
            zt' (* 0.25 (+ zt alamb))
            ave (* third (+ xt' yt' zt'))
            delx (/ (- ave xt') ave)
            dely (/ (- ave yt') ave)
            delz (/ (- ave zt') ave)]
        (if (> (max (Math/abs delx)
                    (Math/abs dely)
                    (Math/abs delz))
               errtol)
          (recur xt' yt' zt')
          (let [e2 (- (* delx dely) (* delz delz))
                e3 (* delx dely delz)]
            (/ (+ 1.0
                  (* (- (* c1 e2)
                        c2
                        (* c3 e3))
                     e2)
                  (* c4 e3))
               (Math/sqrt ave))))))))

(defn carlson-rf-simple
  "Port of `Carlson-elliptic-1-simple` from `scmutils`."
  [x y z]
  (let [eps v/sqrt-machine-epsilon]
    (loop [xt x
           yt y
           zt z]
      (let [av (/ (+ xt yt zt) 3.0)]
        (if (< (max (Math/abs (/ (- xt av) av))
                    (Math/abs (/ (- yt av) av))
                    (Math/abs (/ (- zt av) av)))
               eps)
          (/ 1.0 (Math/sqrt av))
          (let [lamb (+ (Math/sqrt (* x y))
                        (Math/sqrt (* x z))
                        (Math/sqrt (* y z)))]
            (recur (/ (+ x lamb) 4.0)
                   (/ (+ y lamb) 4.0)
                   (/ (+ z lamb) 4.0))))))))

(defn carlson-rd
  "Comment from Press, section 6.11, page 257:

  'Computes Carlson’s elliptic integral of the second kind, RD(x, y, z). x and y must be
  nonnegative, and at most one can be zero. z must be positive. TINY must be at least twice
  the negative 2/3 power of the machine overflow limit. BIG must be at most 0.1 × ERRTOL
  times the negative 2/3 power of the machine underflow limit.'

  This is called `Carlson-elliptic-2` in scmutils. This code is a direct port of
  the scmutils version, so the error tolerances are different, we don't have a
  tiny/big, etc."
  [x y z]
  (let [eps 0.0015
        tiny 1.0e-25
        big  4.5e21
        C1   (/ 3.0 14.0)
        C2   (/ 1.0 6.0)
        C3   (/ 9.0 22.0)
        C4   (/ 3.0 26.0)
        C5   (* 0.25 C3)
        C6   (* 1.5 C4)]
    (when (or (< (min x y) 0)
              (< (min (+ x y) z) tiny)
              (> (max x y z) big))
      (u/illegal "Carlson R_D"))
    (loop [x x
           y y
           z z
           sum 0.0
           fac 1.0]
      (let [sqrtx (Math/sqrt x)
            sqrty (Math/sqrt y)
            sqrtz (Math/sqrt z)
            alamb (+ (* sqrtx (+ sqrty sqrtz))
                     (* sqrty sqrtz))
            sump  (+ sum (/ fac (* sqrtz (+ z alamb))))
            facp  (* 0.25 fac)
            xp    (* 0.25 (+ x alamb))
            yp    (* 0.25 (+ y alamb))
            zp    (* 0.25 (+ z alamb))
            ave   (* 0.2 (+ xp yp (* 3.0 zp)))
            delx  (/ (- ave xp) ave)
            dely  (/ (- ave yp) ave)
            delz  (/ (- ave zp) ave)]
        (if (> (max (Math/abs delx)
                    (Math/abs dely)
                    (Math/abs delz))
               eps)
          (recur xp yp zp sump facp)
          (let [ea (* delx dely)
                eb (* delz delz)
                ec (- ea eb)
                ed (- ea (* 6.0 eb))
                ee (+ ed ec ec)]
            (+ (* 3.0 sump)
               (/ (* facp
                     (+ 1.0
                        (* ed (- (* C5 ed) (* C6 delz ee) C1))
                        (* delz (+ (* C2 ee)
                                   (* delz (- (* C3 ec)
                                              (* delz C4 ea)))))))
                  (* ave (Math/sqrt ave))))))))))

(defn carlson-rc
  "Computes Carlson’s degenerate elliptic integral, $R_C(x, y)$. `x` must be
  nonnegative and `y` must be nonzero. If `y < 0`, the Cauchy principal value is
  returned.

  Internal details:

  - `tiny` must be at least 5 times the machine underflow limit
  - `big` at most one fifth the machine maximum overflow limit."
  [x y]
  (let [errtol 0.0012
        tiny   1.69e-38
        sqrtny 1.3e-19
        big    3.0e37
        tnbg   (* tiny big)
        comp1  (/ 2.236 sqrtny)
        comp2  (/ (* tnbg tnbg) 25)
        third (/ 1 3.0)
        C1 0.3
        C2 (/ 1.0 7.0)
        C3 0.375
        C4 (/ 9.0 22.0)]
    (when (or (< x 0)
              (= y 0)
              (< (+ x (Math/abs y)) tiny)
              (> (+ x (Math/abs y)) big)
              (and (< y (- comp1))
                   (> x 0)
                   (< x comp2)))
      (u/illegal "Carlson R_C"))
    (let [[xt yt w] (if (> y 0)
                      [x y 1]
                      (let [xt (- x y)
                            yt (- y)
                            w  (/ (Math/sqrt x)
                                  (Math/sqrt xt))]
                        [xt yt w]))]
      (loop [xt xt
             yt yt]
        (let [sqrtx (Math/sqrt xt)
              sqrty (Math/sqrt yt)
              alamb (+ (* 2 sqrtx sqrty)
                       yt)
              xp    (* 0.25 (+ xt alamb))
              yp    (* 0.25 (+ yt alamb))
              ave   (* third (+ xp yp yp))
              s     (/ (- yp ave) ave)]
          (if (> (Math/abs s) errtol)
            (recur xp yp)
            (* w (/ (+ 1.0 (* s s (+ C1 (* s (+ C2 (* s (+ C3 (* s C4))))))))
                    (Math/sqrt ave)))))))))

(defn carlson-rj
  "Computes Carlson’s elliptic integral of the third kind, RJ (x, y, z, p).

  `x`, `y`, and `z` must be nonnegative, and at most one can be zero. `p` must
  be nonzero.

  If p < 0, the Cauchy principal value is returned. TINY must be at least twice
  the cube root of the machine underflow limit, BIG at most one fifth the cube
  root of the machine overflow limit."
  [x y z p]
  (let [errtol 0.0015
        tiny 2.5e-13
        big 9.0e11
        C1 (/ 3.0 14.0)
        C2 (/ 1.0 3.0)
        C3 (/ 3.0 22.0)
        C4 (/ 3.0 26.0)
        C5 (* 0.75 C3)
        C6 (* 1.5 C4)
        C7 (* 0.5 C2)
        C8 (+ C3 C3)]
    (when (or (< (min x y z) 0)
              (< (min (+ x y)
                      (+ x z)
                      (+ y z)
                      (Math/abs p)) tiny)
              (> (max x y z (Math/abs p)) big))
      (u/illegal "Carlson R_J"))
    (let [[xt yt zt pt a b rcx]
          (if (> p 0)
            [x y z p]
            (let [xt  (min x y z)
                  zt  (max x y z)
                  yt  (- (+ x y z)
                         xt zt)
                  a   (/ 1.0 (- yt p))
                  b   (* a (- zt yt) (- yt xt))
                  pt  (+ yt b)
                  rho (/ (* xt zt) yt)
                  tau (/ (* p pt) yt)
                  rcx (carlson-rc rho tau)]
              [xt yt zt pt a b rcx]))]
      (loop [xt xt
             yt yt
             zt zt
             pt pt
             sum 0.0
             fac 1.0]
        (let [sqrtx (Math/sqrt xt)
              sqrty (Math/sqrt yt)
              sqrtz (Math/sqrt zt)
              alamb (+ (* sqrtx (+ sqrty sqrtz))
                       (* sqrty sqrtz))
              alpha (-> (+ (* pt (+ sqrtx sqrty sqrtz))
                           (* sqrtx sqrty sqrtz))
                        (Math/pow 2))
              beta  (* pt (Math/pow (+ pt alamb) 2))
              sump  (+ sum (* fac (carlson-rc alpha beta)))
              facp  (* 0.25 fac)
              xp    (* 0.25 (+ xt alamb))
              yp    (* 0.25 (+ yt alamb))
              zp    (* 0.25 (+ zt alamb))
              pp    (* 0.25 (+ pt alamb))
              ave   (* 0.2 (+ xp yp zp pp pp))
              delx  (/ (- ave xp) ave)
              dely  (/ (- ave yp) ave)
              delz  (/ (- ave zp) ave)
              delp  (/ (- ave pp) ave)]
          (if (> (max (Math/abs delx)
                      (Math/abs dely)
                      (Math/abs delz)
                      (Math/abs delp))
                 errtol)
            (recur xp yp zp pp sump facp)
            (let [ea (+ (* delx (+ dely delz))
                        (* dely delz))
                  eb (* delx dely delz)
                  ec (* delp delp)
                  ed (- ea (* 3.0 ec))
                  ee (+ eb (* 2.0 delp (- ea ec)))
                  rj (+ (* 3.0 sump)
                        (/ (* facp
                              (+ 1.0
                                 (* ed (- (* C5 ed)
                                          (* C6 ee)
                                          C1))
                                 (* eb (+ C7 (* delp (- (* delp C4) C8))))
                                 (* delp ea (- C2 (* delp C3)))
                                 (- (* C2 delp ec))))
                           (* ave (Math/sqrt ave))))]
              (if (<= p 0)
                (* a (+ (* b rj)
                        (* 3.0 (- rcx (carlson-rf xp yp zp)))))
                rj))))))))

(defn elliptic-f
  "Legendre elliptic integral of the first kind F(φ, k).
   See W.H. Press, Numerical Recipes in C++, 2ed. eq. 6.11.19

  Page 260 here: http://phys.uri.edu/nigh/NumRec/bookfpdf/f6-11.pdf"
  [phi k]
  (let [s  (Math/sin phi)
        sk (* s k)]
    (* s (carlson-rf (Math/pow (Math/cos phi) 2)
                     (* (- 1 sk)
                        (+ 1 sk))
                     1))))

(defn complete-elliptic-integral-K
  "Complete elliptic integral of the first kind - see Press, 6.11.18."
  [k]
  (elliptic-f (/ Math/PI 2) k))

(defn elliptic-e
  "Legendre elliptic integral of the second kind E(φ, k).
   See W.H. Press, Numerical Recipes in C++, 2ed. eq. 6.11.20

  Page 260 here: http://phys.uri.edu/nigh/NumRec/bookfpdf/f6-11.pdf"
  [phi k]
  (let [s  (Math/sin phi)
        c  (Math/cos phi)
        cc (* c c)
        sk (* s k)
        q  (* (- 1 sk)
              (+ 1 sk))]
    (* s (- (carlson-rf cc q 1.0)
            (* (* sk sk)
               (/ (carlson-rd cc q 1.0) 3.0))))))

(defn complete-elliptic-integral-E
  "Complete elliptic integral of the second kind - see Press, 6.11.20."
  [k]
  (elliptic-integral-E (/ Math/PI 2) k))

(defn elliptic-pi
  "Legendre elliptic integral of the third kind Π(φ, k).
   See W.H. Press, Numerical Recipes in C++, 2ed. eq. 6.11.21

  Page 260 here: http://phys.uri.edu/nigh/NumRec/bookfpdf/f6-11.pdf"
  [phi n k]
  (let [s   (Math/sin phi)
        c   (Math/cos phi)
        nss (* n s s)
        cc  (* c c)
        sk  (* s k)
        q   (* (- 1 sk)
               (+ 1 sk))]
    (* s (- (carlson-rf cc q 1.0)
            (* nss (/ (carlson-rj cc q 1.0 (+ 1.0 nss)) 3.0))))))

;; Note from `scmutils` to accompany the following ports: "older definition of
;; the complete elliptic integrals, probably from A&Stegun"

(defn elliptic-integrals
  "Computes the first and second complete elliptic integrals at once, and passes
  them to the supplied continuation as args `K` and `E`."
  [k continue]
  (if (= k 1)
    (continue ##Inf 1.0)
    (loop [a        1.0
           b        (Math/sqrt (- 1.0 (* k k)))
           c        k
           d        0.0
           powers-2 1.0]
      (if (< (Math/abs c) v/machine-epsilon)
        (let [first-elliptic-integral (/ (/ Math/PI 2) a)]
          (continue first-elliptic-integral
                    (* first-elliptic-integral
                       (- 1.0 (/ d 2.0)))))
        (recur (/ (+ a b) 2.0)
               (Math/sqrt (* a b))
               (/ (- a b) 2.0)
               (+ d (* (* c c) powers-2))
               (* powers-2 2.0))))))

(defn first-elliptic-integral
  "Complete elliptic integral of the first kind - see Press, 6.11.18."
  [k]
  (elliptic-integrals k (fn [K _] K)))


(defn second-elliptic-integral
  "Complete elliptic integral of the second kind - see Press, 6.11.18."
  [k]
  (elliptic-integrals k (fn [_ E] E)))

(defn first-elliptic-integral-and-deriv
  "Calls the supplied continuation `cont` with:

  - the elliptic integral of the first kind, `K`
  - the derivative `dK/dk`"
  [k cont]
  (if (= k 0.0)
    [(/ Math/PI 2) 0.0]
    (let [cont (fn [Kk Ek]
                 (cont Kk
                       (/ (- (/ Ek (- 1 (* k k))) Kk)
                          k)))]
      (elliptic-integrals k cont))))

(defn- emc-u-d
  "Internal helper to set constants for `Jacobi-elliptic-functions.`
  "[emc u d]
  (let [bo (< emc 0.0)]
    (if bo
      (let [d (- 1. emc)
            emc (- (/ emc d))
            d (Math/sqrt d)
            u (* u d)]
        [bo emc u d])
      [bo emc u d])))

(defn jacobi-elliptic-functions
  "Direct Clojure translation (via the Scheme translation in scmutils) of W.H.
  Press, Numerical Recipes, subroutine `sncndn`.

  Calls the supplied continuation `cont` with `sn`, `cn` and `dn` as defined
  below.

  Comments from Press, page 261:

  The Jacobian elliptic function sn is defined as follows: instead of
  considering the elliptic integral

  $$u(y, k) \\equiv u=F(\\phi, k)$$

  Consider the _inverse_ function:

  $$y = \\sin \\phi = \\mathrm{sn}(u, k)$$

  Equivalently,

  $$u=\\int_{0}^{\\mathrm{sn}} \\frac{d y}{\\sqrt{\\left(1-y^{2}\\right)\\left(1-k^{2} y^{2}\\right)}}$$

  When $k = 0$, $sn$ is just $\\sin$. The functions $cn$ and $dn$ are defined by
  the relations

  $$\\mathrm{sn}^{2}+\\mathrm{cn}^{2}=1, \\quad k^{2} \\mathrm{sn}^{2}+\\mathrm{dn}^{2}=1$$

  The function calls the continuation with all three functions $sn$, $cn$, and
  $dn$ since computing all three is no harder than computing any one of them."
  [u k cont]
  (let [eps v/sqrt-machine-epsilon
        emc (- 1. (* k k))]
    (if (= emc 0.0)
      (let [cn (/ 1.0 (Math/cosh u))]
        (cont (Math/tanh u) cn cn))
      (let [[bo emc u d] (emc-u-d emc u 1.0)]
        (loop [a   1.0
               emc emc
               i   1
               em  []
               en  []]
          (let [emc (Math/sqrt emc)
                c   (* 0.5 (+ a emc))]
            (if (and (> (Math/abs (- a emc))
                        (* eps a))
                     (< i 13))
              (recur c (* a emc)
                     (+ i 1)
                     (cons a em)
                     (cons emc en))
              ;; label 1
              (let [u (* c u)
                    sn (Math/sin u)
                    cn (Math/cos u)
                    [a sn cn dn] (if-not (= sn 0.0)
                                   (loop [em em
                                          en en
                                          a  (/ cn sn)
                                          c  (* a c)
                                          dn 1.0]
                                     (if (and (not (empty? em))
                                              (not (empty? en)))
                                       (let [b (first em)
                                             [a c dn] (let [a  (* c a)
                                                            c  (* dn c)
                                                            dn (/ (+ (first en) a) (+ a b))
                                                            a  (/ c b)]
                                                        [a c dn])]
                                         (recur (rest em) (rest en) a c dn))
                                       (let [a' (/ 1.0 (Math/sqrt (+ 1. (* c c))))
                                             [sn cn] (let [sn (if (< sn 0.0) (- a') a')
                                                           cn (* c sn)]
                                                       [sn cn])]
                                         [a sn cn dn]))))]

                (if bo
                  (cont (/ sn d) a cn)
                  (cont sn cn dn))))))))))
