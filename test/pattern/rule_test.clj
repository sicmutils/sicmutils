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
;;; This software is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this code; if not, see <http://www.gnu.org/licenses/>.
;;

(ns pattern.rule-test
  (:require [pattern.rule :as r]))

(defmacro rule-1
  "Compiling a rule produces an arity 2 function which takes the data to match
  and a success continuation. For testing we provide this arity-1 wrapper which
  provides a continuation that immediately returns."
  [& pattern-components]
  `(let [compiled-rule# (r/rule ~@pattern-components)]
     (fn [data#]
       (compiled-rule# data# identity))))
