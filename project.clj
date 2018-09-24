;
; Copyright © 2017 Colin Smith.
; This work is based on the Scmutils system of MIT/GNU Scheme:
; Copyright © 2002 Massachusetts Institute of Technology
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

(defproject net.littleredcomputer/sicmutils "0.12.0-SNAPSHOT"
  :description "A port of the Scmutils computer algebra/mechanics system to Clojure"
  :url "http://github.com/littleredcomputer/sicmutils"
  :license {:name "GPLv3"
            :url "http://www.opensource.org/licenses/GPL-3.0"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/tools.logging "0.4.0"]
                 [hiccup "1.0.5"]
                 [com.google.guava/guava "22.0"]
                 [org.clojure/core.match "0.3.0-alpha4"]
                 [org.apache.commons/commons-math3 "3.6.1"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [org.clojure/tools.nrepl "0.2.13"]
                 [org.clojars.didiercrunch/clojupyter "0.1.5"]
                 [potemkin "0.4.3"]]
  :main sicmutils.repl
  :jvm-opts ["-Djava.util.logging.config.file=logging.properties"]
  :repl-options {:prompt (fn [ns] (str "[" ns "] > "))
                 :welcome "clojure algebra system"
                 :init-ns sicmutils.env
                 :nrepl-middleware [sicmutils.repl/math-printer]}
  :target-path "target/%s"
  :test-selectors {:default (complement :long)
                   :benchmark :benchmark}
  :profiles {:uberjar {:aot :all}
             :travis {:jvm-opts ["-Xmx512M"]}
             :dev {:dependencies [[org.clojure/test.check "0.9.0"]
                                  [criterium "0.4.4"]]}
             :test {:jvm-opts ["-Xmx512m" "-Djava.awt.headless=true"]
                    :dependencies [[org.clojure/test.check "0.9.0"]
                                   [criterium "0.4.4"]]}})
