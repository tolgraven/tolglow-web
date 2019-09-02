(ns tolglow-web.middleware.formats "What's this even for?"
  (:require
    [cognitect.transit :as transit]
    [luminus-transit.time :as time]
    [muuntaja.core :as m]))

(def instance
  (m/create
    (-> m/default-options
        (update-in
          [:formats "application/transit+json" :decoder-opts]
          (partial merge time/time-deserialization-handlers))
        (update-in
          [:formats "application/transit+json" :encoder-opts]
          (partial merge time/time-serialization-handlers))
        #_(assoc-in "application/graphql" couldve done something here to sort my previous issue or?))))
