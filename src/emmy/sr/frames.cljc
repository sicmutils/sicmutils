#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.sr.frames
  (:refer-clojure :exclude [+ - * /])
  (:require [emmy.calculus.frame :as cf]
            [emmy.calculus.manifold :as m]
            [emmy.generic :as g :refer [+ - * /]]
            [emmy.sr.boost :as b]
            [emmy.structure :as s]))

;; ## Special-relativity frames
;;
;; A frame is defined by a Poincare transformation from a given background
;; 4-space frame (the "ancestor-frame"). The transformation is specified by a
;; boost magnitude and a unit-vector boost direction, relative to the ancestor
;; frame, and the position of the origin of the frame being defined in the
;; ancestor frame.

;; The events are absolute, in that it is always possible to compare them to
;; determine if two are the same. They will be represented with coordinates
;; relative to some arbitrary absolute frame,
;; "the-ether".
;;
;; To keep us from going nuts, an SR frame has a name, which it uses to label
;; coordinates in its frame.

(defn make-SR-coordinates [frame four-tuple]
  {:pre [(s/up? four-tuple)
         (= (count four-tuple) 4)]}
  (-> four-tuple
      (vary-meta assoc ::SR-coordinates? true)
      (cf/claim frame)))

(defn SR-coordinates? [coords]
  (::SR-coordinates? (meta coords) false))

(defn SR-name [coords]
  (cf/frame-name
   (cf/frame-owner coords)))

;; ### SR frames

(defn- coordinates->event
  [ancestor-frame _ {:keys [boost-direction vc origin]}]
  {:pre [(= (cf/frame-owner origin) ancestor-frame)]}
  (fn c->e [coords]
    {:pre [(SR-coordinates? coords)]}
    ((m/point ancestor-frame)
     (make-SR-coordinates ancestor-frame
                          (+ ((b/general-boost2 boost-direction vc)
                              coords)
                             origin)))))

(defn- event->coordinates
  [ancestor-frame this-frame
   {:keys [boost-direction vc origin]}]
  {:pre [(= (cf/frame-owner origin) ancestor-frame)]}
  (fn e->c [event]
    {:pre [(cf/event? event)]}
    (let [coords ((b/general-boost2 (- boost-direction) vc)
                  (- ((m/chart ancestor-frame) event)
                     origin))]
      (make-SR-coordinates this-frame coords))))


(let [make (cf/frame-maker coordinates->event event->coordinates)]
  (defn make-SR-frame [name ancestor-frame boost-direction v-over-c origin]
    (make name ancestor-frame
          {:boost-direction boost-direction
           :vc v-over-c
           :origin origin})))

;; ### The background frame

(defn base-frame-point [_ this-frame _]
  #_{:clj-kondo/ignore [:redundant-fn-wrapper]}
  (fn [coords]
    {:pre [(SR-coordinates? coords)
           (= this-frame (cf/frame-owner coords))]}
    (cf/make-event coords)))

(defn base-frame-chart [_ this-frame _]
  (fn [event]
    {:pre [(cf/event? event)]}
    (make-SR-coordinates this-frame event)))

(def base-frame-maker
  (cf/frame-maker base-frame-point base-frame-chart))

(def the-ether
  (base-frame-maker 'the-ether 'the-ether))

(defn boost-direction [frame]
  (:boost-direction (cf/params frame)))

(defn v:c [frame]
  (:vc (cf/params frame)))

(defn coordinate-origin [frame]
  (:origin (cf/params frame)))

(defn add-v:cs [v1:c v2:c]
  (/ (+ v1:c v2:c)
     (+ 1 (* v1:c v2:c))))

(defn add-velocities
  "velocities must be in meters/second, since we don't yet have units support."
  [v1 v2]
  (/ (+ v1 v2)
     (+ 1 (* (/ v1 'C)
             (/ v2 'C)))))
