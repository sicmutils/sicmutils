#_"SPDX-License-Identifier: GPL-3.0"

(ns emmy.fdg.ch11-test
  (:refer-clojure :exclude [+ - * / zero? partial])
  (:require [clojure.test :refer [is deftest testing use-fixtures]]
            [emmy.env :as e :refer [+ - * /
                                    compose
                                    zero?
                                    up
                                    rotate-x rotate-y rotate-z
                                    point chart]]
            [emmy.simplify :refer [hermetic-simplify-fixture]]
            [emmy.value :as v]))

(use-fixtures :each hermetic-simplify-fixture)

(def simplify
  (comp v/freeze e/simplify))

(deftest ch11-tests
  (testing "Implementation, p175"
    (is (zero?
         (simplify
          (- (e/proper-space-interval
              ((e/general-boost (up 'vx 'vy 'vz))
               (e/make-four-tuple 'ct (up 'x 'y 'z))))
             (e/proper-space-interval
              (e/make-four-tuple 'ct (up 'x 'y 'z))))))
        "We can check that the interval is invariant, p176"))

  (comment
    ;; NOTE this does not complete!! Test in Scheme on my laptop, see how long
    ;; this takes. Is this another case of screwy non-caching in the coordinate
    ;; conversion?
    (testing "rotations, p177"
      (let [beta (up 'bx 'by 'bz)
            xi (e/make-four-tuple 'ct (up 'x 'y 'z))
            R (compose
               (rotate-x 'theta)
               (rotate-y 'phi)
               (rotate-z 'psi))
            R-inverse (compose
                       (rotate-z (- 'psi))
                       (rotate-y (- 'phi))
                       (rotate-x (- 'theta)))]
        (is (= '(up 0 0 0 0)
               (simplify
                (- ((e/general-boost beta) xi)
                   ((e/compose (e/extended-rotation R-inverse)
                               (e/general-boost (R beta))
                               (e/extended-rotation R))
                    xi))))))))

  (testing "special relativity frames, p179"
    (testing "Velocity addition formula"
      (let [home (e/base-frame-maker 'home 'home)
            A (e/make-SR-frame 'A
                               home
                               (up 1 0 0)
                               'vb:a
                               (e/make-SR-coordinates home (up 0 0 0 0)))
            B (e/make-SR-frame 'B
                               A
                               (up 1 0 0)
                               'vb:c
                               (e/make-SR-coordinates A (up 0 0 0 0)))
            foo ((chart home)
                 ((point B)
                  (e/make-SR-coordinates B (up 'ct 0 0 0))))]
        (is (= '(/ (+ vb:a vb:c)
                   (+ (* vb:a vb:c) 1))
               (simplify
                (/ (get foo 1)
                   (get foo 0)))))))

    (testing "twin paradox"
      (let [home (e/base-frame-maker 'home 'home)
            start-event ((point home)
                         (e/make-SR-coordinates home (up 0 0 0 0)))

            outgoing
            (e/make-SR-frame 'outgoing ; for debugging
                             home ; base frame
                             (up 1 0 0) ; x direction
                             (/ 24 25) ; velocity as fraction of c
                             ((chart home) start-event))

            traveller-at-turning-point-event
            ((point home)
             (e/make-SR-coordinates home
                                    (up (* 'c 25) (* 25 (/ 24 25) 'c) 0 0)))]
        (is (= '(up (* 7 c) 0 0 0)
               (simplify
                (- ((chart outgoing) traveller-at-turning-point-event)
                   ((chart outgoing) start-event))))
            "If we examine the displacement of the traveller in his own frame we
             see that the traveller has aged 7 years and he has not moved from
             his spatial origin.")

        (is (= '(up (* 25 c) (* 24 c) 0 0)
               (simplify
                (- ((chart home) traveller-at-turning-point-event)
                   ((chart home) start-event))))
            "But in the frame of the homebody we see that the time has advanced
             by 25 years.")

        (is (= '(* 7 c)
               (simplify
                (e/proper-time-interval
                 (- ((chart home) traveller-at-turning-point-event)
                    ((chart home) start-event))))
               (simplify
                (e/proper-time-interval
                 (- ((chart outgoing) traveller-at-turning-point-event)
                    ((chart outgoing) start-event)))))
            "The proper time interval is 7 years, as seen in any frame, because
             it measures the aging of the traveller:")

        ;; When the traveller is at the turning point, the event of the homebody
        ;; is:
        (let [halfway-at-home-event
              ((point home)
               (e/make-SR-coordinates home (up (* 'c 25) 0 0 0)))]
          (is (= '(* 25 c)
                 (simplify
                  (e/proper-time-interval
                   (- ((chart home) halfway-at-home-event)
                      ((chart home) start-event))))
                 (simplify
                  (e/proper-time-interval
                   (- ((chart outgoing) halfway-at-home-event)
                      ((chart outgoing) start-event)))))
              "And the homebody has aged this much, as seen from either
              frame."))

        ;; As seen by the traveller, home is moving in the −xˆ direction at
        ;; 24/25 of the velocity of light. At the turning point (7 years by his
        ;; time) home is at:
        (let [home-at-outgoing-turning-point-event
              ((point outgoing)
               (e/make-SR-coordinates
                outgoing
                (up (* 7 'c) (* 7 (/ -24 25) 'c) 0 0)))]

          ;; Since home is speeding away from the traveller, the twin at
          ;; home has aged less than the traveller. This may seem weird, but
          ;; it is OK because this event is different from the halfway event in
          ;; the home frame.
          (is (= '(* (/ 49 25) c)
                 (simplify
                  (e/proper-time-interval
                   (- ((chart home) home-at-outgoing-turning-point-event)
                      ((chart home) start-event))))
                 (simplify
                  (e/proper-time-interval
                   (- ((chart outgoing) home-at-outgoing-turning-point-event)
                      ((chart outgoing) start-event)))))))

        ;; The traveller turns around abruptly at this point (painful!) and
        ;; begins the return trip. The incoming trip is the reverse of the
        ;; outgoing trip, with origin at the turning-point event:
        (let [incoming
              (e/make-SR-frame 'incoming home
                               (up -1 0 0) (/ 24 25)
                               ((chart home)
                                traveller-at-turning-point-event))

              ;; After 50 years of home time the traveller reunites with the
              ;; homebody:
              end-event
              ((point home)
               (e/make-SR-coordinates home (up (* 'c 50) 0 0 0)))]
          ;; Indeed, the traveller comes home after 7 more years in the incoming
          ;; frame:
          (is (= '(up 0 0 0 0)
                 (simplify
                  (- ((chart incoming) end-event)
                     (e/make-SR-coordinates incoming
                                            (up (* 'c 7) 0 0 0))))))

          (is (= '(up 0 0 0 0)
                 (simplify
                  (- ((chart home) end-event)
                     ((chart home)
                      ((point incoming)
                       (e/make-SR-coordinates incoming
                                              (up (* 'c 7) 0 0 0))))))))

          ;; The traveller ages only 7 years on the return segment, so his
          ;; total aging is 14 years:
          (is (= '(* 14 c)
                 (simplify
                  (+ (e/proper-time-interval
                      (- ((chart outgoing) traveller-at-turning-point-event)
                         ((chart outgoing) start-event)))
                     (e/proper-time-interval
                      (- ((chart incoming) end-event)
                         ((chart incoming) traveller-at-turning-point-event)))))))

          ;; But the homebody ages 50 years:
          (is (= '(* 50 c)
                 (simplify
                  (e/proper-time-interval
                   (- ((chart home) end-event)
                      ((chart home) start-event))))))

          ;; At the turning point of the traveller the homebody is at
          (let [home-at-incoming-turning-point-event
                ((point incoming)
                 (e/make-SR-coordinates incoming
                                        (up 0 (* 7 (/ -24 25) 'c) 0 0)))]
            ;; The time elapsed for the homebody between the reunion and
            ;; the turning point of the homebody, as viewed by the incoming
            ;; traveller, is about 2 years.

            (is (= '(* (/ 49 25) c)
                   (simplify
                    (e/proper-time-interval
                     (- ((chart home) end-event)
                        ((chart home) home-at-incoming-turning-point-event))))))

            ;; Thus the aging of the homebody occurs at the turnaround, from
            ;; the point of view of the traveller.
            ))))))
