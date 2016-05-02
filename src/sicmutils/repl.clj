;
; Copyright (C) 2016 Colin Smith.
; This work is based on the Scmutils system of MIT/GNU Scheme.
;
; This is free software;  you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3 of the License, or (at
; your option) any later version.
;
; This software is distributed in the hope that it will be useful, but
; WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
; General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this code; if not, see <http://www.gnu.org/licenses/>.
;

(ns sicmutils.repl
  (:refer-clojure :exclude [+ - * / zero? ref partial])
  (:require [clojure.main :as main]
            [clojure.tools.nrepl.transport]
            [clojure.tools.nrepl.middleware :as mw]
            [clojure.tools.nrepl.middleware.pr-values :as pr-values]
            [sicmutils
             [env :refer :all]
             [simplify :as simp]])
  (:import [clojure.tools.nrepl.transport Transport])
  (:gen-class))

(defn math-printer
  "NRepl middleware to apply the simplifier and prettyprinter to print
  values. See the documentation for the pr-values default middleware to
  see how this is done by updating :value in a response object and
  setting :printed-value."
  [h]
  (fn [{:keys [^Transport transport] :as msg}]
    (h (assoc msg
              :transport (reify Transport
                           (recv [_] (.recv transport))
                           (recv [_ timeout] (.recv transport timeout))
                           (send [this response]
                             (.send transport
                                    (if (find response :value)
                                      (-> response
                                          (update-in [:value] simp/expression->string)
                                          (assoc :printed-value true))
                                      response))
                             this))))))

;; Interpose our middleware between evaluation and printing.
(mw/set-descriptor! #'math-printer
                    {:requires #{#'pr-values/pr-values}
                     :expects #{"eval"}
                     :handles {}})

(defn -main
  "A simple main that runs Clojure's internal REPL in the math environment."
  [& _]
  (println "Won't you sign in, stranger?")
  (main/with-bindings
    (in-ns 'sicmutils.env)
    (main/repl :print simp/print-expression)
    (println "Home at last.")))
