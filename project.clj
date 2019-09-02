(defproject tolglow-web-web "0.1.0-SNAPSHOT"
  :description "tolglow-web web frontend wip"
  :url "no"
  :license {:name "CC0 1.0 Universal (CC0 1.0) Public Domain Dedication"
            :url "http://creativecommons.org/publicdomain/zero/1.0/"}

  :min-lein-version "2.7.1"

  :dependencies [[reagent "0.8.1"]
                 [re-frame "0.10.9"]
                 [day8.re-frame/http-fx "0.1.6"] ;ajax n stuff
                 [cljs-ajax "0.8.0"]
                 [re-com "2.5.0"] ;components. but hmm don't really like the vibe...
                 ; [re-graph "0.1.12-SNAPSHOT" ] ;graphql in re-frame
                 [re-graph "0.1.10" :exclusions [stylefruits/gniazdo]] ;graphql in re-frame
                 [stylefruits/gniazdo "1.1.2" ] ;updated re-graph dep fixing jetty dep
                 [reanimated "0.6.1"]
                 [recalcitrant "0.1.2"] ;contains something to wrap react so broken component doesnt take down entire page
                 [cljsjs/react-color "2.13.8-0"] ;color-pickers
                 [cljsjs/react-flip-move "3.0.1-1"] ;auto-animate list reordering etc
                 [thi.ng/color "1.3.2-SNAPSHOT"]
                 ; [thi.ng/geom "1.0.0-RC3"] ;geometry stuff
                 ; [thi.ng/morphogen "0.2.0-SNAPSHOT"]
                 ; [viebel/klipse "7.9.1"] ;in-page repl. was clashing with something have to take out quick
                 ; [meander/delta "0.0.149"] ;data transforms
                 [garden "1.3.9"] ;like hiccup but for css
                 [quil "3.0.0"]
                 [tolglow "0.0.1-SNAPSHOT"]

                 [com.cognitect/transit-clj "0.8.313"]
                 [ring "1.7.1"]
                 [bk/ring-gzip "0.3.0"]
                 [ring-cors "0.1.13"]
                 [environ "1.1.0"]
                 ; [org.webjars.npm/bulma "0.7.5"]
                 ; [org.webjars.npm/material-icons "0.3.0"]

                 [cheshire "5.9.0"]
                 [expound "0.7.2"]
                 [funcool/struct "1.4.0"]
                 ;
                 [luminus-http-kit "0.1.6"]
                 [luminus-migrations "0.6.5"]
                 [luminus-transit "0.1.1"]
                 [luminus/ring-ttl-session "0.3.3"]
                 ; [markdown-clj "1.10.0"]
                 [metosin/muuntaja "0.6.4"]
                 [metosin/reitit "0.3.9"]
                 [metosin/ring-http-response "0.9.1"]

                 [mount "0.1.16"]
                 [conman "0.8.3"]
                 [cprop "0.1.14"]

                 [chourave/reagi "0.11.0-SNAPSHOT"] ;async FRP stuff

                 [com.walmartlabs/lacinia "0.34.0"]
                 [com.walmartlabs/lacinia-pedestal "0.12.0"]
                 [district0x/graphql-query "1.0.6"]

                 [io.pedestal/pedestal.jetty "0.5.7"] ;still needed explicitly?
                 ;  :exclusions [org.eclipse.jetty.websocket/websocket-client org.eclipse.jetty/jetty-client org.eclipse.jetty/jetty-xml org.eclipse.jetty/jetty-util]]
                 [hodur/engine "0.1.6"]
                 [hodur/datomic-schema "0.1.0"]
                 [hodur/lacinia-schema "0.1.2"]

                 [org.xerial/sqlite-jdbc "3.28.0"] ;db whoo
                 [seancorfield/next.jdbc "1.0.6"]
                 [datascript "0.18.4"]

                 ; [mvxcvi/whidbey "2.1.1"]

                 [org.clojure/tools.logging "0.5.0"]
                 [io.aviso/pretty "0.1.37"] ;pretty exceptions, pretty logging...
                 [com.taoensso/timbre "4.10.0"]
                 [com.fzakaria/slf4j-timbre "0.3.14"] ;route java logging through timbre. but dunno. hard to manage
                 [clj-logging-config "1.9.12"]
                 [radicalzephyr/ring.middleware.logger "0.6.0"]
                 ; [ch.qos.logback/logback-classic "1.2.3"]

                 [org.webjars/webjars-locator "0.37"]
                 [ring-webjars "0.2.0"]
                 [ring/ring-core "1.7.1"]
                 [ring/ring-defaults "0.3.2"]

                 [clojure.java-time "0.3.2"]
                 [org.clojure/data.json "0.2.6"]
                 [org.clojure/tools.cli "0.4.2"]


                 [org.clojure/clojure "1.10.1"]
                 [org.clojure/clojurescript "1.10.520"]
                 [org.clojure/core.async  "0.4.500"
                  :exclusions [org.clojure/tools.reader]]]

  :plugins [[lein-figwheel "0.5.18"]
            [lein-cljsbuild "1.1.7" :exclusions [[org.clojure/clojure]]]
            [lein-sassc "0.10.4"]
            [lein-auto "0.1.2"]]

   ; :sassc
   ; [{:src "resources/scss/screen.scss"
   ;   :output-to "resources/public/css/screen.css"
   ;   :style "nested"
   ;   :import-path "resources/scss"}]
   ; :auto {"sassc" {:file-pattern #"\.(scss|sass)$" :paths ["resources/scss"]}}

  :clean-targets ^{:protect false} ["target" "out" "resources/public/js/compiled"] ;; need to add the compiled assets to the :clean-targets]})
  :source-paths ["src/clj" "src/cljs" "src/cljc" #_"dev"] ;; XXX why do is dev needed here when it's in dev profile? need to add dev source path here to get user.clj loaded
  ;; :source-paths ["src/clj" "dev"] ;; does cljs make sense? this is jvm part...

  :cljsbuild
  {:builds
   [{:id "dev"
     :source-paths ["src/cljs" "src/cljc" "dev"] ; :source-paths ["src/clj" "src/cljs"] ;can we put clj in here to find macro thing?
     :figwheel {;:on-jsload "tolglow-web.core/init" ;; the presence of a :figwheel configuration here will cause figwheel to inject the figwheel client into your build
                ;"If you supply :on-jsload the name of a function, that function will be called after new code gets reloaded"
                ;^ so might not want that full ext init (that isnt currently working anyways...) bc will reload everything?
}

     :compiler {:closure-defines {"re_frame.trace.trace_enabled_QMARK_" true ;10x
                                  #_process.env/NODE_ENV #_"development"}
                ;; :npm-deps {:react "16.8.6" :react-dom "16.8.6"}
                :main "tolglow-web.core"
                :asset-path "js/compiled/out"
                :output-to "resources/public/js/compiled/app.js"
                :output-dir "resources/public/js/compiled/out"
                :source-map-timestamp true

                :foreign-libs [{:file "resources/lib/react-rotary-knob.js"
                                ; :file-min "lib/ReactDnD.min.js"
                                :provides ["react-rotary-knob"]
                                :requires ["cljsjs/react"]}]
                ; :preloads [devtools.preload day8.re-frame-10x.preload] ;#_dirac.runtime.preload
                :preloads [devtools.preload] ;
                #_:external-config #_{:devtools/config
                {:features-to-install [:formatters :hints]
                :fin-symbol "F"
                :print-config-overrides true}} }} ;; https://github.com/binaryage/cljs-devtools
                                   ;; {:features-to-install :all}}

    {:id "min" ;; compressed minified production build, do: lein cljsbuild once min
     :source-paths ["src/cljs"]
     :compiler {:output-to "resources/public/js/compiled/app.js"
                :main tolglow-web.core
                :optimizations :advanced
                :pretty-print false}}]}

  :hooks [leiningen.sassc leiningen.cljsbuild] ;cljsbuild one for quil apparently. check whether needed? hooks deprecated...
  :figwheel
  {;; :server-port 3449 ;; default
   :css-dirs ["resources/public/css"] ;; watch and update CSS
   :ring-handler tolglow-web.handler/app-routes ;Embed ring handler in figwheel http-kit server, for simple ring servers, if it doesn't work for you just run your own (see lein-ring)
   ;; when compiler emits warnings figwheel blocks reloading files. To disable:
   ;; :load-warninged-code true

   ;; To open files in editor from website HUD need launcher/script in $PATH, taking file path + line number ie.
   ;; #! /bin/sh
   ;; emacsclient -n +$2 $1
   ;; :open-file-command "~/.local/bin/nvim-linenum"
   :server-logfile #_false "logs/figwheel-logfile.log"}  ;; false sends to repl

  ;; https://github.com/bhauman/lein-figwheel/wiki/Using-the-Figwheel-REPL-within-NRepl
  :profiles ;; for setting up nREPL for Figwheel and ClojureScript dev work
  {:dev {:dependencies [[day8.re-frame/re-frame-10x "0.4.2"]
                        [binaryage/devtools "0.9.10"]
                        ;; [binaryage/dirac "1.3.9"]
                        [figwheel-sidecar "0.5.18"]
                        [cider/piggieback "0.4.1"]
                        [pjstadig/humane-test-output "0.9.0"]
                        [prone "2019-07-08"]
                        [re-frisk "0.5.4.1"]
                        [ring/ring-devel "1.7.1"]
                        [ring/ring-mock "0.4.0"]]

         :jvm-opts ["-Dconf=dev-config.edn"]
        :resource-paths ["env/dev/resources"]
         :source-paths ["dev" "env/dev/clj"] ;; XXX assuming merge? need to add dev source path here to get user.clj loaded
         :plugins [[cider/cider-nrepl "0.21.1"] ] ;; for CIDER
         :repl-options {:welcome #_"in DEV profile" (println "in DEV profile") ;dont get why println the fn is showing up in the output
                        :nrepl-middleware [cider.piggieback/wrap-cljs-repl]
                        :timeout 300000
                        :init (do (fig-start)
                                  (start))}
         ;; DIRAC: run on port 8230 with dirac.nrepl/middleware middleware (piggieback fork, so cant run both)
         ;; Think of Dirac middleware as a Piggieback middleware replacement with some extra features specific to Dirac DevTools.
         ;; :repl-options {;:nrepl-middleware [dirac.nrepl/middleware]
                        ;; :port 8230
                        ;; :init (do (require 'dirac.agent) (dirac.agent/boot!))}
                 } ;;END DEV PROFILE
   } ;; END PROFILES
)
