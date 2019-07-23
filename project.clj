(defproject cue-db "0.1.0-SNAPSHOT"
  :description "Code to accompany Building Re-frame Components, a course on PurelyFunctional.tv"
  :url "https://purelyfunctional.tv/courses/building-re-frame-components/"
  :license {:name "CC0 1.0 Universal (CC0 1.0) Public Domain Dedication"
            :url "http://creativecommons.org/publicdomain/zero/1.0/"}

  :min-lein-version "2.7.1"

  :dependencies [[day8.re-frame/http-fx "0.1.4"]
                 [reagent "0.8.1"]
                 [re-frame "0.10.8"]
                 ;; [day8.re-frame/re-frame-10x "0.4.1"]
                 [re-com "2.5.0"]
                 [binaryage/devtools "0.9.10"]
                 [recalcitrant "0.1.2"]
                 ;; [binaryage/dirac "1.3.9"]
                 [figwheel-sidecar "0.5.18"]
                 [cider/piggieback "0.4.1"]
                 [org.clojure/clojure "1.10.1"]
                 [org.clojure/clojurescript "1.10.520"]
                 [org.clojure/core.async  "0.3.442"
                  :exclusions [org.clojure/tools.reader]]]

  :plugins [[cider/cider-nrepl "0.21.1"]
            [lein-figwheel "0.5.18"]
            [lein-cljsbuild "1.1.7" :exclusions [[org.clojure/clojure]]]]

  ;; :clean-targets ^{:protect false} [:target-path "out" "resources/public/cljs"] ;; need to add the compiled assets to the :clean-targets]})
  :clean-targets ^{:protect false} ["target"  "out" "resources/public/js/compiled"] ;; need to add the compiled assets to the :clean-targets]})
  ;; :source-paths ["src"]
  :source-paths ["src/clj" "src/cljs" "dev"] ;; need to add dev source path here to get user.clj loaded

  :repl-options {;:init-ns tolglow.core
                 :welcome (println "U IN PRJ CLJ")
                 :nrepl-middleware [cider.piggieback/wrap-cljs-repl] ;ok weirdest thing main diff(?) between reference project and this is ref lacked this! wtf?
                 }
  :cljsbuild
  {:builds
   [{:id "dev"
     :source-paths ["src/cljs"]
     ;; the presence of a :figwheel configuration here will cause figwheel to inject the figwheel client into your build
     :figwheel {:on-jsload "tolglow.core/mount-root"
                ;; :open-urls will open app in default browser once Figwheel has started and compiled it
                #_:open-urls #_["http://localhost:3449/index.html"]}

     :compiler {:main tolglow.core
                :asset-path "js/compiled/out"
                :output-to "resources/public/js/compiled/app.js"
                :output-dir "resources/public/js/compiled/out"
                :source-map-timestamp true
                ;; To console.log CLJS data-structures make sure you enable devtools in Chrome
                ;; :preloads [#_dirac.runtime.preload devtools.preload]
                :preloads [devtools.preload]
                :external-config {:devtools/config
                                  ;; {:features-to-install :all}}
                                  {:features-to-install [:formatters :hints]}
                                  :fin-symbol "F"
                                  :print-config-overrides true}
                }} ;; https://github.com/binaryage/cljs-devtools

    ;; This next build is an compressed minified build for production. You can build this with:
    ;; lein cljsbuild once min
    {:id "min"
     :source-paths ["src/cljs"]
     :compiler {:output-to "resources/public/js/compiled/app.js"
                :main tolglow.core
                :optimizations :advanced
                :pretty-print false}}]}

  :figwheel
  {;; :http-server-root "public" ;; default and assumes "resources"
   ;; :server-port 3449 ;; default
   ;; :server-ip "127.0.0.1"
   :css-dirs ["resources/public/css"] ;; watch and update CSS

   ;; Server Ring Handler (optional)
   ;; if you want to embed a ring handler into the figwheel http-kit
   ;; server, this is for simple ring servers, if this doesn't work for you just run your own server :) (see lein-ring)
   ;; :ring-handler hello_world.server/handler

   ;; To be able to open files in your editor from the heads up display you will need to put a script on your path.
   ;; that script will have to take a file path and a line number ie. in  ~/bin/myfile-opener
   ;; #! /bin/sh
   ;; emacsclient -n +$2 $1
   ;; :open-file-command "myfile-opener"

   ;; :nrepl-port 6000 ;;Start an nREPL server into the running figwheel process
   ;; :repl false ;;if you want to disable the REPL

   :server-logfile "logs/figwheel-logfile.log"  ;; to configure a different figwheel logfile path
   #_:server-logfile #_false } ;;to pipe all the output to the repl

  ;; https://github.com/bhauman/lein-figwheel/wiki/Using-the-Figwheel-REPL-within-NRepl
;;   :profiles ;; for setting up nREPL for Figwheel and ClojureScript dev work
;;   {:dev {:dependencies [[day8.re-frame/http-fx "0.1.4"]
;;                         [re-frame "0.9.4"]
;;                         [binaryage/devtools "0.9.2"]
;;                         ;; [binaryage/dirac "1.3.9"]
;;                         [figwheel-sidecar "0.5.18"]
;;                         [cider/piggieback "0.4.1"]]}
;;          :source-paths ["src" "dev"] ;; need to add dev source path here to get user.clj loaded
;;          :plugins [[cider/cider-nrepl "0.21.1"]] ;; for CIDER
;; ;; By default you should run it on port 8230 and with dirac.nrepl/middleware middleware.
;; ;; Please note that Dirac middleware was implemented as a Piggieback middleware fork, so
;; ;; --> you cannot run both. <-  Think of Dirac middleware as a Piggieback middleware replacement with
;; ;; some extra features specific to Dirac DevTools.
;;          :repl-options {:nrepl-middleware [cider.piggieback/wrap-cljs-repl]}
;;          ;; :repl-options {;:nrepl-middleware [dirac.nrepl/middleware]
;;                         ;; :port 8230
;;                         ;; :init (fig-start)
;;                         ;; :init (do (require 'dirac.agent) (dirac.agent/boot!))
;;                  }
)
