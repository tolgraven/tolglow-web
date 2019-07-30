(ns tolglow.db
  (:require [reagent.core :as r]
            [re-frame.core :as rf]))

(def data
 {:cues
 [{:id :1    :color "blue" :text "Run shite" :position [0 0] :fx "Strobe"}
  {:id :3    :color "red" :text "Extra sparkle" :position [1 0] :fx "Color"}
  {:id :11   :color "green" :text "Moving" :position [2 0] :fx "Pan"}
  {:id :10   :color "seagreen" :text "Moving" :position [3 0] :fx "Aim"}
  {:id :0    :color "purple" :text "Fog" :position [4 0] :fx "Fsshh!"}
  {:id :200  :color "orange" :text "Stripe" :position [0 1] :fx "Tilt"}
  {:id :19   :color "black" :text "Wash" :position [1 1] :fx "Shutter"}
  {:id :100  :color "orangered" :text "Pointy" :position [2 1] :fx "Blame"}

  {:id :108  :color "seagreen" :text "2nd ROW" :position [6 0] :fx "YO"}
  {:id :109  :color "purple" :text "2nd ROW"   :position [6 1] :fx "YO"}
  {:id :110  :color "seagreen" :text "2nd ROW" :position [6 2] :fx "YO"}
  {:id :111  :color "seagreen" :text "2nd ROW" :position [6 3] :fx "YO"}
  {:id :112  :color "seagreen" :text "2nd ROW" :position [6 4] :fx "YO"}
  {:id :113  :color "seagreen" :text "2nd ROW" :position [6 5] :fx "YO"}
  ]})

(defn init "Initialize the database. Only once unless forced..."
 [& force?]
 (rf/reg-event-db :initialize
  (fn [db _] {:cues (:cues data)}))
 (defonce _init (rf/dispatch-sync [:initialize]))
 (rf/clear-subscription-cache!))

