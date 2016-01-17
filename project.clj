(defproject net.littleredcomputer/math "0.0.1-SNAPSHOT"
  :description "A port of the Scmutils computer algebra/mechanics system to Clojure"
  :url "http://github.com/littleredcomputer/math"
  :license {:name "GPLv3"
            :url "http://www.opensource.org/licenses/GPL-3.0"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/tools.logging "0.3.1"]
                 [hiccup "1.0.5"]
                 [com.google.guava/guava "18.0"]
                 [org.apache.commons/commons-math3 "3.6"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [org.clojure/tools.nrepl "0.2.11"]]
  :main net.littleredcomputer.math.repl
  :jvm-opts ["-Djava.util.logging.config.file=logging.properties"]
  :repl-options {:prompt (fn [ns] (str "[" ns "] > "))
                 :welcome "clojure algebra system"
                 :init-ns net.littleredcomputer.math.env
                 :nrepl-middleware [net.littleredcomputer.math.repl/math-printer]}
  :target-path "target/%s"
  :test-selectors {:default (complement :long)}
  :profiles {:uberjar {:aot :all}})
