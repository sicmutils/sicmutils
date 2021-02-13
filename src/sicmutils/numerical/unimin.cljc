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

(ns sicmutils.numerical.unimin
  "`unimin` is a module of functions and methods designed to find minimal (or
  maximal) values of single variable functions.")

(defn local-maxima
  " Given a function f on [a, b] and N > 0, examine f at the endpoints a, b, and
  at N equally-separated interior points. From this form a list of brackets (p
  q) in each of which a local maximum is trapped. Then apply Brent to all these
  brackets and return a list of pairs (x fx) representing the local maxima.
  "
  [f a b n ftol])

#_
(define (local-maxima f a b n ftol)
  (let* ((h (/ (- b a) (+ n 1)))
         (xlist (generate-list
                 (+ n 2)
                 (lambda (i) (if (= i (+ n 1)) b (+ a (* i h))))))
         (flist (map f xlist))
         (xi (lambda(i) (list-ref xlist i)))
         (fi (lambda(i) (list-ref flist i)))
         (brack1 (if (> (fi 0) (fi 1))
                   (list (list (xi 0) (xi 1)))
                   '()))
         (brack2 (if (> (fi (+ n 1)) (fi n))
                   (cons (list (xi n) (xi (+ n 1))) brack1)
                   brack1))
         (bracketlist
          (let loop ((i 1) (b brack2))
               (if (> i n)
                 b
                 (if (and (<= (fi (- i 1)) (fi i))
                          (>= (fi i) (fi (+ i 1))))
                   (loop (+ i 1) (cons (list (xi (- i 1))
                                             (xi (+ i 1))) b))
                   (loop (+ i 1) b)))))
         (locmax (lambda (int) (gsmax f (car int) (cadr int)
                                      'function-tol ftol))))
    (map locmax bracketlist)))

(defn local-minima [f a b n ftol])

#_
(define (local-minima f a b n ftol)
  (let* ((g (lambda (x) (- (f x))))
         (result (local-maxima g a b n ftol))
         (flip (lambda (r) (list (car r) (- (cadr r)) (caddr r)))))
    (map flip result)))

(defn estimate-global-max
  "Refer to the previous two functions and find the max of all of those."
  [f a b n ftol])

#_
(define (estimate-global-max f a b n ftol)
  (let ((local-maxs (local-maxima f a b n ftol)))
    (let loop ((best-so-far (car local-maxs))
               (unexamined (cdr local-maxs)))
         (if (null? unexamined)
           best-so-far
           (let ((next (car unexamined)))
             (if (> (cadr next) (cadr best-so-far))
               (loop next (cdr unexamined))
               (loop best-so-far (cdr unexamined))))))))

(defn estimate-global-min
  "Refer to the previous two functions and find the min."
  [f a b n ftol])

#_
(define (estimate-global-min f a b n ftol)
  (let ((local-mins (local-minima f a b n ftol)))
    (let loop ((best-so-far (car local-mins))
               (unexamined (cdr local-mins)))
         (if (null? unexamined)
           best-so-far
           (let ((next (car unexamined)))
             (if (< (cadr next) (cadr best-so-far))
               (loop next (cdr unexamined))
               (loop best-so-far (cdr unexamined))))))))
