(ns tolglow-web.log
  (:require [clojure.java.io :as io]
            [taoensso.timbre :as timbre]
            [taoensso.encore :as enc]
            [taoensso.timbre.appenders.3rd-party.rotor :as rotor]
            [mount.core :refer [defstate]]))

(defn ns-filter [fltr] (-> fltr enc/compile-ns-filter enc/memoize_))
(defn log-by-ns-pattern
  [ns-patterns & [{:keys [?ns-str config level] :as opts}]]
  (let [namesp   (or (some->> ns-patterns
                              keys
                              (filter #(and (string? %)
                                            ((ns-filter %) ?ns-str)))
                              not-empty
                              (apply max-key count))
                     :all)
        loglevel (get ns-patterns namesp (get config :level))]
    (when (and (timbre/may-log? loglevel namesp)
               (timbre/level>= level loglevel))
      opts)))

(defn error-be-red "Make errors red middleware" [])

(defn init-timbre []
 ; (timbre/merge-config! {:enabled false}))
 (let [ns-levels {"tolglow-web.*" :debug
                  "afterglow.show" :error
                  ; "tolglow-web.routes.services.graphql" :trace
                  "org.apache.http" :warn
                  "org.xnio.nio" :warn
                  "com.zaxxer.hikari" :warn
                  "org.eclipse.jetty.server*" :warn
                  "io.pedestal*" :info #_:debug #_:warn
                  :all :info}]
  (timbre/merge-config!
   {:enabled true
    :level :debug
    :timestamp-opts {:pattern "yyMMdd HH:mm:ss.SSS"
                     :locale :jvm-default
                     :timezone (java.util.TimeZone/getDefault)}
    :appenders {:println {:min-level :debug #_:info}
                :rotor (merge (rotor/rotor-appender
                               {:path "logs/tolglow-web.log"
                                :max-size 10000000 ;was set to just 100kb... thats like 10 stacktraces lol
                                :backlog 5})
                              {:rate-limit [[1 250] [2 1000] [4 10000]]}) }
    :middleware [(partial log-by-ns-pattern ns-levels)]}))
 )

(defstate logger-timbre
 :start (init-timbre))

