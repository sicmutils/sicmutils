(ns build
  "Build for the sicmutils library."
  (:require [clojure.tools.build.api :as b]))

;; ## Variables

(def lib 'sicmutils/sicmutils)
(def version (slurp "resources/SICMUTILS_VERSION"))

(defn- ->version
  ([] version)
  ([suffix]
   (if suffix
     (format "%s-%s" version suffix)
     version)))

(def snapshot (->version "SNAPSHOT"))

;; source for jar creation.
(def class-dir "target/classes")

(defn ->jar-file [version]
  (format "target/%s-%s.jar" (name lib) version))

(def basis (b/create-basis {:project "deps.edn"}))

(def built-jar-version-file "target/built-jar-version.txt")

;; ## Tasks

(defn clean [opts]
  (println "\nCleaning target...")
  (b/delete {:path "target"})
  opts)

(defn jar
  "Build library jar file.

  Also writes built version to target/built-jar-version.txt for easy peasy
  pickup by any interested downstream operation.

  We use the optional :version-suffix to optionally distinguish local installs
  from production releases.

  For example, when installing for a cljdoc preview suffix is: cljdoc-preview."
  [{:keys [version-suffix] :as opts}]
  (let [version  (->version version-suffix)
        jar-file (->jar-file version)]
    (b/write-pom {:class-dir class-dir
                  :lib lib
                  :version version
                  :donkey "punch"
                  :scm {:tag version}
                  :basis basis
                  :src-pom "template/pom.xml"
                  :src-dirs ["src"]
                  :resource-dirs ["resources"]})
    (doseq [f ["README.md" "LICENSE" "deps.edn"]]
      (b/copy-file {:src f :target (format "%s/%s" class-dir f)}))
    (b/copy-dir {:src-dirs ["src" "resources"]
                 :target-dir class-dir})
    (b/jar {:class-dir class-dir
            :jar-file jar-file})
    (spit built-jar-version-file version)
    (println (str "Created " jar-file "."))
    (assoc opts
           :jar-file jar-file
           :built-jar-version version)))

(defn install [opts]
  (clean opts)
  (let [{:keys [built-jar-version jar-file]} (jar opts)]
    (b/install {:class-dir class-dir
                :lib lib
                :version built-jar-version
                :basis basis
                :jar-file jar-file})
    (println (str "Installed " jar-file " to local maven repository."))
    opts))

(defn publish [opts]
  (clean opts)
  (let [{:keys [jar-file]} (jar opts)]
    (println (str "Publishing " jar-file " to Clojars!"))
    ((requiring-resolve 'deps-deploy.deps-deploy/deploy)
     (merge {:installer :remote
             :sign-releases? false
             :artifact jar-file
             :pom-file (b/pom-path {:lib lib :class-dir class-dir})}
            opts))
    ;; TODO put a catch here if the version already exists?
    (println "Published.")
    opts))


;; (ns build
;;   "sicmutils build script. TODO finish conversion!

;;   clojure -T:build ci

;;   clojure -T:build run-doc-tests :aliases '[:cljs]'

;;   Run tests:
;;   clojure -X:test
;;   clojure -X:test:master

;;   For more information, run:

;;   clojure -A:deps -T:build help/doc"
;;   (:refer-clojure :exclude [test])
;;   (:require [clojure.tools.build.api :as b]
;;             [org.corfield.build :as bb]))

;; (def lib 'sicmutils/sicmutils)
;; (defn- the-version [patch] (format "0.22.%s" patch))
;; (def version (the-version (b/git-count-revs nil)))
;; (def snapshot (the-version "999-SNAPSHOT"))

;; (defn eastwood [opts]
;;   (bb/run-task opts [:eastwood]))

;; (defn gen-doc-tests
;;   "Generate tests from doc code blocks."
;;   [opts]
;;   (bb/run-task opts [:gen-doc-tests]))

;; (defn run-doc-tests
;;   "Generate and run doc tests.

;;   Optionally specify :aliases vector:
;;   [:1.9] -- test against Clojure 1.9 (the default)
;;   [:1.10] -- test against Clojure 1.10.3
;;   [:1.11] -- test against Clojure 1.11.0
;;   [:master] -- test against Clojure 1.12 master snapshot
;;   [:cljs] -- test against ClojureScript"
;;   [{:keys [aliases] :as opts}]
;;   (gen-doc-tests opts)
;;   (bb/run-tests
;;    (assoc opts :aliases
;;           (-> [:test-doc]
;;               (into aliases)
;;               (into (if (some #{:cljs} aliases)
;;                       [:test-doc-cljs]
;;                       [:test-doc-clj])))))
;;   opts)

;; (defn test
;;   "Run basic tests."
;;   [opts]
;;   (-> opts
;;       (update :aliases (fnil conj []) :1.11)
;;       (bb/run-tests)))

;; (defn ci
;;   "Run the CI pipeline of tests (and build the JAR).

;;   Default Clojure version is 1.9.0 (:1.9) so :elide
;;   tests for #409 on that version."
;;   [opts]
;;   (-> opts
;;       (bb/clean)
;;       (assoc :lib lib :version (if (:snapshot opts) snapshot version))
;;       (as-> opts
;;           (reduce (fn [opts alias]
;;                     (run-doc-tests (assoc opts :aliases [alias])))
;;                   opts
;;                   [:cljs :elide :1.10 :1.11 :master]))
;;       (eastwood)
;;       (as-> opts
;;           (reduce (fn [opts alias]
;;                     (bb/run-tests (assoc opts :aliases [alias])))
;;                   opts
;;                   [:cljs :elide :1.10 :1.11 :master]))
;;       (bb/clean)
;;       (assoc :src-pom "template/pom.xml")
;;       (bb/jar)))

;; (defn deploy "Deploy the JAR to Clojars." [opts]
;;   (-> opts
;;       (assoc :lib lib :version (if (:snapshot opts) snapshot version))
;;       (bb/deploy)))
