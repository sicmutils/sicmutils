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

(ns sicmutils.sr.frames
  (:refer-clojure :exclude [+ - * /])
  (:require [sicmutils.generic :as g :refer [+ - * /]]))

(comment
  ;;;;              Special-relativity frames.

;;; A frame is defined by a Poincare transformation from a given
;;; background 4-space frame (the "ancestor-frame").  The
;;; transformation is specified by a boost magnitude and a unit-vector
;;; boost direction, relative to the ancestor frame, and the position
;;; of the origin of the frame being defined in the ancestor frame.

;;; The events are absolute, in that it is always possible to compare
;;; them to determine if two are the same.  They will be represented
;;; with coordinates relative to some arbitrary absolute frame,
;;; "the-ether".

;;; To keep us from going nuts, an SR frame has a name, which it uses
;;; to label coordinates in its frame.

;;; ...
;;; Implementation of the coordinates uses a put/get table.

  (define (make-SR-coordinates frame 4tuple)
    (assert (vector? 4tuple))
    (assert (fix:= (vector-length 4tuple) 4))
    (eq-put! 4tuple 'SR-coordinates #t)
    (claim! 4tuple frame)
    4tuple)

  (define (SR-coordinates? coords)
    (eq-get coords 'SR-coordinates))

  (define (SR-name coords)
    ((frame-owner coords) 'name))
  
;;; SR frames

  (define (coordinates->event ancestor-frame this-frame
                              boost-direction v/c origin)
    (assert (eq? (frame-owner origin) ancestor-frame))
    (define (c->e coords)
      (assert (SR-coordinates? coords))
      ((point ancestor-frame)
       (make-SR-coordinates ancestor-frame
                            (+ ((general-boost2 boost-direction v/c)
                                coords)
                               origin))))
    c->e)


  (define (event->coordinates ancestor-frame this-frame
                              boost-direction v/c origin)
    (assert (eq? (frame-owner origin) ancestor-frame))
    (define (e->c event)
      (assert (event? event))
      (make-SR-coordinates this-frame
                           ((general-boost2 (- boost-direction) v/c)
                            (- ((chart ancestor-frame) event)
                               origin))))
    e->c)
  )

(comment

  (define (boost-direction frame)
    (list-ref (frame-params frame) 0))

  (define (v/c frame)
    (list-ref (frame-params frame) 1))

  (define (coordinate-origin frame)
    (list-ref (frame-params frame) 2))


  (define make-SR-frame
    (frame-maker coordinates->event event->coordinates))

  ;; The background frame
  (define ((base-frame-point ancestor-frame this-frame) coords)
    (assert (SR-coordinates? coords))
    (assert (eq? this-frame (frame-owner coords)))
    (make-event coords)
    coords)

  (define ((base-frame-chart ancestor-frame this-frame) event)
    (assert (event? event))
    (make-SR-coordinates this-frame event))

  (define the-ether
    ((frame-maker base-frame-point base-frame-chart)
     'the-ether 'the-ether))

  )

(comment
  (define (add-v/cs v1/c v2/c)
    (/ (+ v1/c v2/c)
       (+ 1 (* v1/c v2/c))))

  (define (add-velocities v1 v2)
    (/ (+ v1 v2)
       (+ 1 (* (/ v1 :c) (/ v2 :c)))))
  )
