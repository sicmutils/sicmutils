(defproject net.littleredcomputer/sicmutils "0.9.0"
  :description "A port of the Scmutils computer algebra/mechanics system to Clojure"
  :url "http://github.com/littleredcomputer/sicmutils"
  :license {:name "GPLv3"
            :url "http://www.opensource.org/licenses/GPL-3.0"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/tools.logging "0.3.1"]
                 [hiccup "1.0.5"]
                 [com.google.guava/guava "18.0"]
                 [org.apache.commons/commons-math3 "3.6"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [org.clojure/tools.nrepl "0.2.11"]]
  :main net.littleredcomputer.sicmutils.repl
  :jvm-opts ["-Djava.util.logging.config.file=logging.properties"]
  :repl-options {:prompt (fn [ns] (str "[" ns "] > "))
                 :welcome "clojure algebra system"
                 :init-ns net.littleredcomputer.sicmutils.env
                 :nrepl-middleware [net.littleredcomputer.sicmutils.repl/math-printer]}
  :target-path "target/%s"
  :test-selectors {:default (complement :long)}
  :profiles {:uberjar {:aot :all}})
