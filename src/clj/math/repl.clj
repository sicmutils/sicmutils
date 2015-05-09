;; Copyright (C) 2015 Colin Smith.
;; This work is based on the Scmutils system of MIT/GNU Scheme.
;;
;; This is free software;  you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or (at
;; your option) any later version.

;; This software is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this code; if not, see <http://www.gnu.org/licenses/>.

(ns math.repl
  (:refer-clojure :exclude [+ - * / zero?])
  (:require [clojure.main :as m]
            [math.generic :refer :all]
            [math.structure :refer :all]
            [math.expression :refer :all]
            [math.numbers]
            [math.numsymb]
            [math.function :refer :all]
            [math.operator]
            [math.simplify :refer [print-expression]]
            [math.numerical.integrate]
            [math.numerical.minimize :refer :all]
            [math.calculus.derivative :refer :all]
            [math.mechanics.lagrange :refer :all]))

(defn -main
  [& args]
  (println "Won't you sign in, stranger?")
  (m/with-bindings
    (in-ns 'math.repl)
    (if args
      ;; read and eval the contents of the supplied files
      (doseq [a args]
        (prn "arg" a))
      (m/repl :print print-expression))
    (println "Home at last.")))
