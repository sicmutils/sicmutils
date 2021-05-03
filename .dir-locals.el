((clojurescript-mode
  ;; You use a shadow-cljs to build the project
  ;; This answers the question "which command should be used?"
  (cider-preferred-build-tool . shadow-cljs)
  ;; This sets a default repl type and  answers the question "select cljs repl type".
  (cider-default-cljs-repl . shadow)
  ;; This tells shadow cljs what to build and should match a key in your shadow-cljs.edn
  ;; build map. e.g :builds {:<some-key> {...}}
  ;; params passed to shadow-cljs to start nrepl via cider-jack-in
  ))
