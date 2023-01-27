# Minimal SICMUtils demo

This demostrates some basic ways of using SICMUtils in your project.

## Direct ClojureScript builds

Follow the [ClojureScript Quick Start](https://clojurescript.org/guides/quick-start) guide to set up your environment.

* Run `clj -M --main cljs.main --compile demo.minimal --repl` which uses `deps.edn`, `src/demo/minimal.cljs` and `index.html` to create a basic browser based demo with a REPL.
* Run `clj -M -m cljs.main -co release-browser.edn -c demo.minimal` to create a single `out/main.js` file with the optimizations in `build.edn`. Manually open `index.html` to see the results.
* Run `clj -M -m cljs.main --target node --output-to out/demo.js --compile demo.minimal` to create a Node.js build and run `node out/demo.js`.

## Using shadow-cljs

[shadow-cljs](https://github.com/thheller/shadow-cljs) is the preferred way to build projects with SICMUtils. Run `npm install` to install the dependencies listed in `package.json`.

* Run `shadow-cljs watch sicm-browser` to run the demo in the browser using the config in `shadow-cljs.edn`. Open http://localhost:9000 and the page will automatically update as the file changes.
* Run `shadow-cljs release sicm-browser` to create a release build and open `index.html`.
* Run `shadow-cljs watch sicm-esm` and open `esmdemo.html` to code the demo in the browser in JavaScript. SICMUtils is also a JavaScript library after all. (For good measure, we also provide a little Scheme compiler :-).
