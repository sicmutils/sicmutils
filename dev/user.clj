(ns user
  (:require [nextjournal.clerk :as clerk]
            [sicmutils.env]))

(comment
  ;; Activate this line to start the clerk server.
  (clerk/serve!
   {:browse? true :port 7778}))

(comment
  ;; call clerk/show on files to be rendered:
  (clerk/show! "src/sicmutils/calculus/derivative.cljc")
  (clerk/show! "src/sicmutils/differential.cljc"))
