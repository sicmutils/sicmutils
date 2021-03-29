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

(ns sicmutils.calculus.frame)

(defn frame?
  "True if this coordinate system is actually a frame.

  FIXME: frames aren't implemented yet."
  ;; Note: when we get around to doing so, it probably makes sense to have
  ;; frames implement ICoordinateSystem in their own way, rather than the hacky
  ;; polymorphism used in scmutils
  [coordinate-system]
  false)

;; TODO get more functions in manifold.cljc working with frames!

(declare event->coords coords->event)

(comment
  ;; from frame_maker.scm
  ;;
  ;; ## System code for making frames
  ;;
  ;; Every frame has a name, and a frame that it is built on (which may be #f).
  ;; Every frame owns coordinates that it may coerce to an absolute event or that
  ;; it may export as its representation of an absolute event.

  (define ((frame-maker c->e e->c) name ancestor-frame . params)
    (define (coordinates->event coords)
      (assert (eq? (frame-owner coords) this-frame))
      (let ((event
	           ((apply c->e ancestor-frame this-frame params) coords)))
        (assert (event? event))
        event))

    (define (event->coordinates event)
      (assert (event? event))
      (let ((coords
	           ((apply e->c ancestor-frame this-frame params) event)))
        (assert (eq? (frame-owner coords) this-frame))
        coords))

    (define (this-frame m)
      (case m
        ((coords->event) coordinates->event)
        ((event->coords) event->coordinates)
        ((name) name)
        ((ancestor-frame) ancestor-frame)
        ((params) params)
        ((manifold) #f)			;Kludge.  See frame? in manifold.scm
        (else (error "Unknown message: " name m))))
    this-frame)

  (define (event->coords frame) (frame 'event->coords))
  (define (coords->event frame) (frame 'coords->event))
  (define (ancestor-frame frame) (frame 'ancestor-frame))

  (define (make-event e)
    (eq-put! e 'event #t)
    e)

  (define (event? e)
    (eq-get e 'event))

  (define (frame-owner coords)
    (eq-get coords 'owner))

  (define (claim! coords owner)
    (let ((other (frame-owner coords)))
      (if other
	      (if (not (eq? other owner))
	        (error "Someone else owns these coords" coords owner))
	      (eq-put! coords 'owner owner))
      coords))

  (define (frame-params frame) (frame 'params))
  (define (frame-name frame) (frame 'name)))
