(ns tolglow-web.core
  (:require
    [tolglow-web.log] ;want this mounted first to silence logs
    [tolglow-web.handler :as handler]
    [tolglow-web.config :refer [env]]
    [tolglow-web.tolglow-interop]

    [luminus.http-server :as http]
    [luminus-migrations.core :as migrations]
    [clojure.tools.cli :refer [parse-opts]]
    [clojure.tools.logging :as log]
    [taoensso.timbre :as timbre]
    [mount.core :as mount])
  (:gen-class))

;; log uncaught exceptions in threads
;; how combine this with aviso??
(Thread/setDefaultUncaughtExceptionHandler
  (reify Thread$UncaughtExceptionHandler
    (uncaughtException [_ thread ex]
      ; (log/error {:what :uncaught-exception
      (timbre/error {:what :uncaught-exception
                  :exception ex
                  :where (str "Uncaught exception on" (.getName thread))}))))

(def cli-options
  [["-p" "--port PORT" "Port number"
    :parse-fn #(Integer/parseInt %)]])

(mount/defstate ^{:on-reload :noop} http-server
  :start
  (http/start
    (-> env
        (assoc  :handler (handler/app))
        (update :io-threads #(or % (* 2 (.availableProcessors (Runtime/getRuntime)))))
        (update :port #(or (-> env :options :port) %))))
  :stop
  (http/stop http-server))


(defn stop-app []
  (doseq [component (:stopped (mount/stop))]
    (log/info component "stopped"))
  (shutdown-agents))

(defn start-app [args]
  (doseq [component (-> args
                        (parse-opts cli-options)
                        mount/start-with-args
                        :started)]
    (log/info component "started"))
  (.addShutdownHook (Runtime/getRuntime) (Thread. stop-app)))

(defn -main [& args]
  (mount/start #'tolglow-web.config/env)
  (cond
    (nil? (:database-url env))
    (do
      (log/error "Database configuration not found, :database-url environment variable must be set before running")
      (System/exit 1))
    (some #{"init"} args)
    (do
      (migrations/init (select-keys env [:database-url :init-script]))
      (System/exit 0))
    (migrations/migration? args)
    (do
      (migrations/migrate args (select-keys env [:database-url]))
      (System/exit 0))
    :else
    (start-app args)))

(println "ah") ;why is this ns loaded twice on startup?
(defn stuff []
  (-main)
  (start-app ""))
