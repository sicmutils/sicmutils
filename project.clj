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

(defproject net.littleredcomputer/sicmutils "0.12.2-SNAPSHOT"
  :description "A port of the Scmutils computer algebra/mechanics system to Clojure"
  :url "http://github.com/littleredcomputer/sicmutils"
  :scm {:name "git" :url "https://github.com/littleredcomputer/sicmutils"}
  :license {:name "GPLv3"
            :url "http://www.opensource.org/licenses/GPL-3.0"}
  :dependencies [[org.clojure/clojure "1.10.1" :scope "provided"]
                 [org.clojure/clojurescript "1.10.773" :scope "provided"]
                 [org.clojure/core.match "1.0.0"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [dm3/stopwatch "0.1.1"]
                 [com.google.guava/guava "23.0"]
                 [com.taoensso/timbre "4.11.0-alpha1"
                  :exclusions [org.clojure/clojurescript]]
                 [org.apache.commons/commons-math3 "3.6.1"]
                 [cljsjs/complex "2.0.11-0"]
                 [hiccup "1.0.5"]
                 [nrepl "0.7.0"]
                 [potemkin "0.4.5"]]
  :jvm-opts ["-Djava.util.logging.config.file=logging.properties"]
  :repl-options {:prompt (fn [ns] (str "[" ns "] > "))
                 :welcome (sicmutils.env/sicmutils-repl-init)
                 :init-ns sicmutils.env}
  :target-path "target/%s"
  :test-selectors {:default (complement :long)
                   :benchmark :benchmark}
  :profiles {:uberjar {:aot :all}
             :travis {:jvm-opts ["-Xmx512M"]}
             :dev {:plugins [~cljsbuild
                             [lein-cloverage "1.1.2"]
                             [lein-doo "0.1.11"]]
                   :cloverage {:ns-exclude-regex [#"sicmutils.rules"
                                                  #"sicmutils.simplify"]}
                   :repl-options {:nrepl-middleware
                                  [cider.piggieback/wrap-cljs-repl]}
                   :dependencies [[org.clojure/test.check "1.1.0"]
                                  [com.gfredericks/test.chuck "0.2.10"]
                                  [same/ish "0.1.4"]
                                  [criterium "0.4.5"]
                                  [cider/piggieback "0.5.0"]
                                  [lein-doo "0.1.11"]]}
             :test {:jvm-opts ["-Xmx512m"]
                    :dependencies [[org.clojure/test.check "1.0.0"]
                                   [criterium "0.4.5"]]}}
  :aliases {"test-cljs"
            ["doo" "node" "test" "once"]}
  :deploy-repositories [["clojars" {:sign-releases false}]]
  :cljsbuild {:builds
              {:test
               {:source-paths ["src" "test"]
                :compiler
                {:main sicmutils.runner
                 :optimizations :none
                 :target :nodejs
                 :output-dir "target/main"
                 :output-to "target/main/main.js"}}}})

(comment
  ;; NOTE - if you do decide to run tests in :advanced optimization mode, you'll
  ;; need to move to phantom, and an older version of clojurescript:

  [org.clojure/clojurescript "1.10.597"]

  ;;Don't ask me why. You may also need to remove `:output-dir`, so try that if
  ;;you run into problems. Do this, change `:optimizations` to `:advanced` and
  ;;you should be good to go.
  )
