(defproject net.littleredcomputer/sicmutils "0.10.0-SNAPSHOT"
  :description "A port of the Scmutils computer algebra/mechanics system to Clojure"
  :url "http://github.com/littleredcomputer/sicmutils"
  :license {:name "GPLv3"
            :url "http://www.opensource.org/licenses/GPL-3.0"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/tools.logging "0.3.1"]
                 [hiccup "1.0.5"]
                 [com.google.guava/guava "20.0"]
                 [org.clojure/core.match "0.3.0-alpha4"]
                 [org.apache.commons/commons-math3 "3.6.1"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [org.clojure/tools.nrepl "0.2.12"]
                 [potemkin "0.4.3"]]
  :main sicmutils.repl
  :jvm-opts ["-Djava.util.logging.config.file=logging.properties"]
  :repl-options {:prompt (fn [ns] (str "[" ns "] > "))
                 :welcome "clojure algebra system"
                 :init-ns sicmutils.env
                 :nrepl-middleware [sicmutils.repl/math-printer]}
  :target-path "target/%s"
  :test-selectors {:default (complement :long)}
  :profiles {:uberjar {:aot :all}
             :travis {:jvm-opts ["-Xmx512M"]}
             :dev {:dependencies [[org.clojure/test.check "0.9.0"]]}
             :test {:dependencies [[org.clojure/test.check "0.9.0"]]}})
