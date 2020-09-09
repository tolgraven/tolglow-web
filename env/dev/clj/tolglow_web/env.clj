(ns tolglow-web.env
  (:require
    [io.aviso.ansi :as ansi :refer :all]
    [taoensso.timbre :as timbre]
    [tolglow-web.dev-middleware :refer [wrap-dev]]))

(def defaults
  {:init
   (fn []
     (timbre/info bold-blue-font "tolglow-web started successfully using DEV profile" reset-font)) ;i mean how do we know its successful does that mean this is only run if successful? so last/outer then
   :stop
   (fn []
     (timbre/info bold-yellow-font "tolglow-web has shut down successfully" reset-font))
   :middleware wrap-dev})
