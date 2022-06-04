;; These dependencies are required for the cljs build of the library. They are
;; also included as cljsjs dependencies in the build... I THINK the cljsjs
;; versions only matter for the externs they provide, but my confusion is on
;; full display here so please file an issue if you run into trouble.
{:npm-deps
 {"complex.js" "^2.0.11"
  "fraction.js" "^4.0.12"
  "odex" "^2.0.4"}}
