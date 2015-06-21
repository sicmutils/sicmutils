(defproject net.littleredcomputer/math "0.0.1-SNAPSHOT"
  :description "A port of the Scmutils computer algebra/mechanics system to Clojure"
  :url "http://github.com/littleredcomputer/math"
  :license {:name "GPLv3"
            :url "http://www.opensource.org/licenses/GPL-3.0"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/tools.logging "0.3.1"]
                 [com.google.guava/guava "18.0"]
                 [org.apache.commons/commons-math3 "3.4.1"]
                 [org.clojure/math.numeric-tower "0.0.4"]]
  :jvm-opts ["-Djava.util.logging.config.file=logging.properties"]
  :repl-options {:prompt (fn [ns] (str "[" ns "] > "))
                 :welcome "clojure algebra system"
                 :init-ns math.repl}
  :target-path "target/%s"
  :test-selectors {:default (complement :long)}
  :profiles {:uberjar {:aot :all}})
