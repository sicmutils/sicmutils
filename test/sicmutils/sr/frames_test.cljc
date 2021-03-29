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

(ns sicmutils.sr.frames-test
  (:refer-clojure :exclude [+ - * /])
  (:require [clojure.test :refer [is deftest testing use-fixtures]]
            ;; [sicmutils.calculus.basis :as b]
            ;; [sicmutils.calculus.coordinate :refer [let-coordinates]
            ;;  #?@(:cljs [:include-macros true])]
            ;; [sicmutils.calculus.hodge-star :as hs]
            ;; [sicmutils.calculus.manifold :as m]
            ;; [sicmutils.calculus.vector-field :as vf]
            [sicmutils.generic :as g :refer [+ - * /]]
            [sicmutils.simplify :refer [hermetic-simplify-fixture]]
            [sicmutils.structure :as s :refer [up down]]
            [sicmutils.value :as v]))

(use-fixtures :each hermetic-simplify-fixture)

(def simplify
  (comp v/freeze g/simplify))


(comment
  #|
;;; Galilean test

  (define (this->ancestor x) x)
  (define (ancestor->this x) x)

  (define (coordinates->event ancestor-frame this-frame
                              boost-direction v/c origin)
    (assert (eq? (frame-owner origin) ancestor-frame))
    (define (c->e coords)
      (assert (SR-coordinates? coords))
      ((point ancestor-frame)
       (make-SR-coordinates ancestor-frame
                            (+ (this->ancestor coords)
                               origin))))
    c->e)


  (define (event->coordinates ancestor-frame this-frame
                              boost-direction v/c origin)
    (assert (eq? (frame-owner origin) ancestor-frame))
    (define (e->c event)
      (assert (event? event))
      (make-SR-coordinates this-frame
                           (ancestor->this
                            (- ((chart ancestor-frame) event)
                               origin))))
    e->c)
  |#
  )


(comment
  #|
  (symbolic-constants #f)
  (set! *divide-out-terms* #f)

;;; Velocity addition formula

  (define A
    (make-SR-frame 'A the-ether
                   (up 1 0 0)
                   (/ 'va :c)
                   (make-SR-coordinates the-ether
                                        #(0 0 0 0))))

  (define B
    (make-SR-frame 'B A
                   (up 1 0 0)
                   (/ 'vb :c)
                   (make-SR-coordinates A
                                        #(0 0 0 0))))

  (let ((foo ((chart the-ether)
              ((point B)
               (make-SR-coordinates B
                                    (up (* :c 'tau) 0 0 0))))))
    (/ (ref foo 1) (/ (ref foo 0) :c)))
  #|
  (/ (+ (* (expt :c 2) va)
        (* (expt :c 2) vb))
     (+ (expt :c 2) (* va vb)))
;;; Hand simplified to:
  (/ (+ va vb)
     (+ 1 (* (/ va :c) (/ vb :c))))

  )


(comment
  #|
;;; Simple test of reversibility

  (define A
    (make-SR-frame 'A the-ether (up 1 0 0) 'va/c
                   (make-SR-coordinates the-ether #(cta xa ya za))))


  ((chart A)
   ((point A)
    (make-SR-coordinates A #(ct x y z))))
  #|
  (up ct x y z)
  |#

;;; The ether coordinates of the origin of A relative to "the ether"
;;; is

  (define origin-A
    (coordinate-origin A))

  (frame-name (frame-owner origin-A))
  #| the-ether |#

  (define B
    (make-SR-frame 'B A (up 1 0 0) 'vba/c
                   (make-SR-coordinates A #(ctba xba yba zba))))

  ((chart B)
   ((point B)
    (make-SR-coordinates B
                         #(ct x y z))))
  #|
  (up ct x y z)
  |#
  |#
  
  #|
;;; Poincare formula


  (define A
    (make-SR-frame 'A the-ether (up 1 0 0) 'va/c
                   (make-SR-coordinates the-ether #(cta xa ya za))))

  (define B
    (make-SR-frame 'B A (up 1 0 0) 'vba/c
                   (make-SR-coordinates A #(ctba xba yba zba))))



;;; The ether coordinates of the origin of B relative to "the ether"
;;; is

  (define origin-B
    ((chart the-ether)
     ((point A)
      (coordinate-origin B))))

  origin-B
  #|
  (up
   (/ (+ (* cta (sqrt (+ 1 (* -1 (expt va/c 2))))) (* va/c xba) ctba)
      (sqrt (+ 1 (* -1 (expt va/c 2)))))
   (/ (+ (* ctba va/c) (* xa (sqrt (+ 1 (* -1 (expt va/c 2))))) xba)
      (sqrt (+ 1 (* -1 (expt va/c 2)))))
   (+ ya yba)
   (+ za zba))
  |#

  (define C
    (make-SR-frame 'C the-ether
                   (up 1 0 0)
                   (add-v/cs 'va/c 'vba/c)
                   origin-B))


;;; A typical event.

  (define foo
    ((point the-ether)
     (make-SR-coordinates the-ether
                          (up 'ct 'x 'y 'z))))
  |#

  )
