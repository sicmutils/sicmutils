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

(defproject net.littleredcomputer/sicmutils "0.12.0"
  :description "A port of the Scmutils computer algebra/mechanics system to Clojure"
  :url "http://github.com/littleredcomputer/sicmutils"
  :license {:name "GPLv3"
            :url "http://www.opensource.org/licenses/GPL-3.0"}
  :dependencies [[org.clojure/clojure "1.10.1" :scope "provided"]
                 [org.clojure/clojurescript "1.10.773" :scope "provided"]
                 [org.clojure/tools.logging "0.4.0"]
                 [org.clojars.sritchie09/complex "2.0.11-0"]
                 [hiccup "1.0.5"]
                 [com.google.guava/guava "22.0"]
                 [org.clojure/core.match "0.3.0-alpha4"]
                 [org.apache.commons/commons-math3 "3.6.1"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [nrepl "0.6.0"]
                 [potemkin "0.4.3"]]
  :main sicmutils.repl
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
                                                  #"sicmutils.simplify"
                                                  #"sicmutils.calculus.manifold"]}
                   :repl-options {:nrepl-middleware
                                  [cider.piggieback/wrap-cljs-repl]}
                   :dependencies [[org.clojure/test.check "0.9.0"]
                                  [criterium "0.4.4"]
                                  [cider/piggieback "0.5.0"]
                                  [lein-doo "0.1.11"]]}
             :test {:jvm-opts ["-Xmx512m"]
                    :dependencies [[org.clojure/test.check "0.9.0"]
                                   [criterium "0.4.4"]]}}
  :aliases {"test-cljs"
            ["doo" "node" "test" "once"]}
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
