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

(ns sicmutils.calculus.frame
  (:require [sicmutils.util :as u]
            [sicmutils.value :as v]))

;; ## Reference Frames, from frame_maker.scm.
;;
;; Every frame has a name, and a frame that it is built on (which may be false).
;; Every frame owns coordinates that it may coerce to an absolute event or that
;; it may export as its representation of an absolute event.

(defprotocol IFrame
  (coords->event [this coords])
  (event->coords [this event])
  (ancestor-frame [_])
  (frame-name [_])
  (params [_]))

(defn frame?
  "Returns true if `x` implements [[IFrame]], false otherwise."
  [x]
  (satisfies? IFrame x))

(defn make-event [e]
  (vary-meta e assoc ::event? true))

(defn event? [e]
  (::event? (meta e) false))

(defn frame-owner [coords]
  (::owner (meta coords)))

(defn claim [coords owner]
  (if-let [other (frame-owner coords)]
    (if (= other owner)
      coords
      (u/illegal (str "Someone else owns these coords: " coords owner)))
    (vary-meta coords assoc ::owner owner)))

(defn frame-maker
  "c->e takes ancestor-frame, this, and a dict of params."
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
