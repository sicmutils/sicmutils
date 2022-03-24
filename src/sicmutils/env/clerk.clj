(ns sicmutils.env.clerk
  (:import (java.io FileNotFoundException)))

(defn- try-resolve [sym]
  (try (requiring-resolve sym)
       (catch FileNotFoundException _ nil)))

(defmacro examples [& body]
  (when (some-> (try-resolve 'nextjournal.clerk.config/*in-clerk*)
                deref)
    `(nextjournal.clerk/with-viewer
       {:render-fn
        '(fn [l r]
           (v/html
            (into [:div.flex.flex-col]
                  (v/inspect-children r)
                  l)))}
       [~@body])))

;; similar thing for the other clerk functions, with-viewers, `clerk/tex` etc.

(examples
 ;; So this will be totally elided
 (def goofy 10)
 (+ 1 2))
