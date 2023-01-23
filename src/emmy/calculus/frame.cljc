#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.calculus.frame
  (:require [emmy.util :as u]))

;; ## Reference Frames, from frame_maker.scm.
;;
;; Every frame has a name, and a frame that it is built on (which may be false).
;; Every frame owns coordinates that it may coerce to an absolute event or that
;; it may export as its representation of an absolute event.

(defprotocol IFrame
  (coords->event [this coords]
    "Accepts a coordinate representation `coords` of some `event` and returns a
 coordinate-free representation of the event.

 `coords` must be owned this this reference frame; [[coords->event]] will throw
 if not.")

  (event->coords [this event]
    "Accepts a reference frame and an `event`, and returns this reference
    frame's coordinate representation of the supplied `event`.")

  (ancestor-frame [_]
    "Returns the ancestor [[IFrame]] instance of this frame, or nil if there is
    no ancestor.")

  (frame-name [_]
    "Returns the symbolic name of the suppplied frame.")

  (params [_]
    "Returns the parameters registered with the supplied frame."))

(defn frame?
  "Returns true if `x` implements [[IFrame]], false otherwise."
  [x]
  (satisfies? IFrame x))

(defn make-event
  "Marks the input event `e` as an event via its metadata. The return value will
  return `true` when passed to [[event?]]."
  [e]
  (vary-meta e assoc ::event? true))

(defn event?
  "Returns true if `e` is an event, false otherwise.

  Make new events with [[make-event]]."
  [e]
  (::event? (meta e) false))

(defn frame-owner
  "Returns the owning [[IFrame]] instance of the supplied coordinates `coords`,
  nil if there's no owner otherwise."
  [coords]
  (::owner (meta coords)))

(defn claim
  "Marks (via metadata) the supplied set of `coords` as being owned by `owner`. If
  `coords` already has an owner (that is not equal to `owner`), throws."
  [coords owner]
  (if-let [other (frame-owner coords)]
    (if (= other owner)
      coords
      (u/illegal (str "Someone else owns these coords: " coords owner)))
    (vary-meta coords assoc ::owner owner)))

(defn frame-maker
  "Takes:

  - `c->e`, a function mapping coordinates to events
  - `e->c`, a function mapping events to coordinates

  and returns a function that takes:

  - a symbolic name
  - an ancestor frame
  - a dictionary of params

  and returns instance of [[IFrame]].

  Both `c->e` and `e->c` must accept three arguments:

  - `ancestor-frame`
  - the [[IFrame]] instance
  - a map of parameters supplied to the returned function (possibly empty!)."
  [c->e e->c]
  (fn call
    ([name]
     (call name nil {}))
    ([name ancestor-frame]
     (call name ancestor-frame {}))
    ([name ancestor-frame params]
     (reify IFrame
       (ancestor-frame [_] ancestor-frame)
       (frame-name [_] name)
       (params [_] params)

       (coords->event [this coords]
         (assert (= (frame-owner coords) this))
         (let [event ((c->e ancestor-frame this params) coords)]
           (assert (event? event))
           event))

       (event->coords [this event]
         (assert (event? event))
         (let [coords ((e->c ancestor-frame this params) event)]
           (assert (= (frame-owner coords) this))
           coords))))))
