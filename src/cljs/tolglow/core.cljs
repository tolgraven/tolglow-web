(ns tolglow.core
  (:require [reagent.core :as r]
            [re-frame.core :as rf]
            [recalcitrant.core :refer [error-boundary]]
            ;; [tolglow.cue :as cue]
            [tolglow.db :as db]
            #_[tolglow ;cljs cant do this??
             [cue :as cue]
             [db :as db]]))

(defn dev-setup []
  (enable-console-print!)) ;what else?

(defn safe "Wrap whatever with an error boundary"
 [elem]
 (r/as-element [(fn [] [error-boundary elem])]))

#_(defn mount-root []
  (rf/clear-subscription-cache!)
  (when-some [el (js/document.getElementById "cue-db")]
    (r/render [cue/ui] el)))

(defn ^:export init []
  (dev-setup)
  (println "LOADED")
  #_(rf/dispatch-sync [:initialize])
  #_(mount-root))


;shortcuts:
;;   (html [:div#foo.bar.baz "bang"])
;;   ->
;;   "<div id=\"foo\" class=\"bar baz\">bang</div>"

;;   [:div>p>b "Nested Element"]
;;   ->
;;   [:div
;;     [:p
;;       [:b "Nested Element"]]]
