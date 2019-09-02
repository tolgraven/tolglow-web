(ns user
  (:require
   [tolglow-web.config :refer [env]]
   [tolglow-web.core :refer [start-app]]
   [tolglow-web.db.core]
   [clojure.spec.alpha :as s]
   [expound.alpha :as expound]
   [mount.core :as mount]
   [conman.core :as conman]
   [luminus-migrations.core :as migrations]
   [figwheel-sidecar.repl-api :as f]
   [cider.piggieback]
   [cljs.repl.nashorn]))
;; user is a namespace that the Clojure runtime looks for and loads when available
;; place helper fns here. great for starting and stopping servers and other dev services
;; defs here will be available from "lein repl" or other clojure repls


(defn piggieback-manual []
  (cider.piggieback/cljs-repl (cljs.repl.nashorn/repl-env)))

(defn fig-start "This starts the figwheel server and watch based auto-compiler." []
  (f/start-figwheel!)) ;guess we dont want to start the cljs-repl if trying to let vim do the conversion?
(defn fig-stop "Stop the figwheel server and watch based auto-compiler." []
  (f/stop-figwheel!))
(defn cljs-repl "Launch a ClojureScript REPL that is connected to your build and host environment." []
  (f/cljs-repl))



(alter-var-root #'s/*explain-out* (constantly expound/printer))
(add-tap (bound-fn* clojure.pprint/pprint))

(defn start "Starts application.  You'll usually want to run this on startup." []
  (mount/start)) ; (mount/start-without #'tolglow-web.core/repl-server))

(defn stop "Stops application." []
  (mount/stop)) ; (mount/stop-except #'tolglow-web.core/repl-server))

(defn restart "Restarts application." []
  (stop)
  (start))

(defn restart-db "Restarts database." []
  (mount/stop #'tolglow-web.db.core/*db*)
  (mount/start #'tolglow-web.db.core/*db*)
  (binding [*ns* 'tolglow-web.db.core]
    (conman/bind-connection tolglow-web.db.core/*db* "sql/queries.sql")))

(defn reset-db "Resets database." []
  (migrations/migrate ["reset"] (select-keys env [:database-url])))

(defn migrate "Migrates database up for all outstanding migrations." []
  (migrations/migrate ["migrate"] (select-keys env [:database-url])))

(defn rollback "Rollback latest database migration." []
  (migrations/migrate ["rollback"] (select-keys env [:database-url])))

(defn create-migration "Create a new up and down migration file with a generated timestamp and `name`."
  [name]
  (migrations/create name (select-keys env [:database-url])))

; (create-migration "export")
