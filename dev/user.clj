(ns user
  (:require  sicmutils.env
             [nextjournal.clerk :as clerk]))


(comment
  ;; set :browse? to true to open a clerk browser tab automatically
  (clerk/serve! {:browse? false :port 7778}) 

  ;; call clerk/show on files to be rendered
  (clerk/show! "src/sicmutils/calculus/derivative.cljc")
  (clerk/show! "src/sicmutils/differential.cljc")

  )
