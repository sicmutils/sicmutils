(defproject sicmutils "0.22.0"
  :description "A port of the scmutils computer algebra/mechanics system to Clojure."
  :url "http://github.com/sicmutils/sicmutils"
  :scm {:name "git" :url "https://github.com/sicmutils/sicmutils"}
  :license {:name "GPLv3"
            :url "http://www.opensource.org/licenses/GPL-3.0"}
  :dependencies [[org.clojure/clojure "1.10.1" :scope "provided"]
                 [org.clojure/clojurescript "1.11.4" :scope "provided"]
                 [org.clojure/core.match "1.0.0"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [org.babashka/sci "0.3.2"]
                 [com.taoensso/timbre "5.1.2"
                  :exclusions [org.clojure/clojurescript]]
                 [dm3/stopwatch "0.1.1"]
                 [org.apache.commons/commons-math3 "3.6.1"]
                 [cljsjs/bigfraction "4.1.1-0"]
                 [cljsjs/complex "2.0.11-0"]
                 [cljsjs/odex "2.0.4-0"]
                 [hiccup "1.0.5"]
                 [potemkin "0.4.5"]]
  :deploy-repositories [["clojars"
                         {:url "https://repo.clojars.org"
                          :username :env/clojars_username
                          :password :env/clojars_password
                          :sign-releases false}]])
