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
;; This software is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this code; if not, see <http://www.gnu.org/licenses/>.
;;

(defproject sicmutils "0.21.1"
  :description "A port of the scmutils computer algebra/mechanics system to Clojure."
  :url "http://github.com/sicmutils/sicmutils"
  :scm {:name "git" :url "https://github.com/sicmutils/sicmutils"}
  :license {:name "GPLv3"
            :url "http://www.opensource.org/licenses/GPL-3.0"}
  :dependencies [[org.clojure/clojure "1.10.1" :scope "provided"]
                 [org.clojure/clojurescript "1.11.4" :scope "provided"]
                 [org.clojure/core.match "1.0.0"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [borkdude/sci "0.2.7"]
                 [com.taoensso/timbre "5.1.2"
                  :exclusions [org.clojure/clojurescript]]
                 [dm3/stopwatch "0.1.1"]
                 [org.apache.commons/commons-math3 "3.6.1"]
                 [cljsjs/bigfraction "4.1.1-0"]
                 [cljsjs/complex "2.0.11-0"]
                 [cljsjs/odex "2.0.4-0"]
                 [hiccup "1.0.5"]
                 [potemkin "0.4.5"]]
  ;; :global-vars {*warn-on-reflection* true}
  :jvm-opts ["-Djava.util.logging.config.file=logging.properties"]
  :repl-options {:prompt (fn [ns] (str "[" ns "] > "))
                 :welcome (set! nrepl.middleware.print/*print-fn*
                                sicmutils.expression/expression->stream)
                 :init-ns sicmutils.env}
  :target-path "target/%s"
  :test-selectors {:short (complement :long)
                   :benchmark :benchmark}
  :profiles {:dev
             {:plugins [[lein-cloverage "1.2.1"]]
              :source-paths ["dev"]
              :jvm-opts ["-Xms6g" "-Xmx8g" "-server"]
              :repl-options {:nrepl-middleware
                             [cider.piggieback/wrap-cljs-repl]}
              :dependencies [[org.clojure/test.check "1.1.0"]
                             [cider/piggieback "0.5.3"]
                             [com.taoensso/tufte "2.2.0"]
                             [com.gfredericks/test.chuck "0.2.13"]
                             [io.github.nextjournal/clerk "0.5.346"
                              :exclusions [org.clojure/tools.deps.alpha]]
                             [nrepl "0.9.0"]
                             [same/ish "0.1.4"]
                             [thheller/shadow-cljs "2.17.4"]]}}
  :deploy-repositories [["clojars"
                         {:url "https://repo.clojars.org"
                          :username :env/clojars_username
                          :password :env/clojars_password
                          :sign-releases false}]])
