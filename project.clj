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

(def cljsbuild '[lein-cljsbuild "1.1.8"
                 :exclusions [org.clojure/clojurescript]])

(defproject sicmutils "0.18.0"
  :description "A port of the Scmutils computer algebra/mechanics system to Clojure."
  :url "http://github.com/sicmutils/sicmutils"
  :scm {:name "git" :url "https://github.com/sicmutils/sicmutils"}
  :license {:name "GPLv3"
            :url "http://www.opensource.org/licenses/GPL-3.0"}
  :dependencies [[org.clojure/clojure "1.10.1" :scope "provided"]
                 [org.clojure/clojurescript "1.10.773" :scope "provided"]
                 [org.clojure/core.match "1.0.0"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [borkdude/sci "0.1.1-alpha.8"]
                 [com.google.guava/guava "23.0"]
                 [com.taoensso/timbre "5.1.2"
                  :exclusions [org.clojure/clojurescript]]
                 [dm3/stopwatch "0.1.1"]
                 [org.apache.commons/commons-math3 "3.6.1"]
                 [cljsjs/bigfraction "4.0.12-0"]
                 [cljsjs/complex "2.0.11-0"]
                 [cljsjs/odex "2.0.4-0"]
                 [hiccup "1.0.5"]
                 [nrepl "0.7.0"]
                 [potemkin "0.4.5"]]
  :global-vars {*warn-on-reflection* true}
  :jvm-opts ["-Djava.util.logging.config.file=logging.properties"]
  :repl-options {:prompt (fn [ns] (str "[" ns "] > "))
                 :welcome (sicmutils.env/sicmutils-repl-init)
                 :init-ns sicmutils.env}
  :target-path "target/%s"
  :test-selectors {:default (complement :long)
                   :benchmark :benchmark}
  :profiles {:dev {:plugins [~cljsbuild
                             [lein-cloverage "1.2.1"]
                             [lein-doo "0.1.11"]]
                   :jvm-opts ["-Xms6g" "-Xmx8g" "-server"]
                   :repl-options {:nrepl-middleware
                                  [cider.piggieback/wrap-cljs-repl]}
                   :dependencies [[org.clojure/test.check "1.1.0"]
                                  [com.taoensso/tufte "2.2.0"]
                                  [com.gfredericks/test.chuck "0.2.10"]
                                  [same/ish "0.1.4"]
                                  [criterium "0.4.5"]
                                  [cider/piggieback "0.5.0"]
                                  [lein-doo "0.1.11"]
                                  [thheller/shadow-cljs "2.11.6"]]}
             :test {:jvm-opts ["-Xms6g" "-Xmx8g"]
                    :dependencies [[org.clojure/test.check "1.0.0"]
                                   [criterium "0.4.5"]]}}
  :aliases {"test-cljs"
            ["doo" "node" "test" "once"]}
  :deploy-repositories [["clojars" {:url "https://repo.clojars.org"
                                    :username :env/clojars_username
                                    :password :env/clojars_password
                                    :sign-releases false}]]
  :cljsbuild {:builds
              {:test
               {:source-paths ["src" "test"]
                :compiler
                {:main sicmutils.runner
                 :optimizations :none
                 :target :nodejs
                 :output-dir "target/main"
                 :output-to "target/main/main.js"}}}})
