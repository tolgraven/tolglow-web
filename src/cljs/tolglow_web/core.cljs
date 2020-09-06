(ns tolglow-web.core
  (:require [reagent.core :as r]
            [re-frame.core :as rf]
            [re-graph.core :as rg]
            [goog.events :as events]
            [mount.core :as mount :refer [defstate]]
            [clojure.core.async :refer [pub sub chan go go-loop >! <! timeout close! unsub unsub-all sliding-buffer]]
            ; [reitit.core :as reitit]
            [tolglow-web.subs]
            [tolglow-web.events :as tevents]
            [tolglow-web.util :as util]
            [tolglow-web.views :as views]
            [tolglow-web.quil]))

(defn dev-setup []
 (enable-console-print!)
 (println "LOAD")
 ;for advanced compilation, instead of preload can do:
 #_(do (require 'devtools.core :as devtools)
     (devtools/install!)))


(defn load-page "Completely load or reload page" []
 ; (when ^boolean goog/DEBUG
 ;  (dev-setup)) ;once is enough
 (defonce _init_ (do
                  (dev-setup)
                  (rf/dispatch-sync [:initialize]))) ;on full reload
 (rf/dispatch [:init

               [[:diag/new :debug "Initialize" "Complete init"]
                [:init-re-graph "nu.local" #_3449 3001] ;tho hmm no reason to do this each reload...
                [:init-keyboard]
                [:set [:ch :fixtures] (chan (sliding-buffer 100))]
                [:set [:ch :shaders]  (chan (sliding-buffer 4))]
                [:send-fetch-events]]

               [[:diag/new :debug "Initialize" "JS loaded"]
                #_[:send-fetch-events] ;temp, for shaders...
                #_[:clear-rf-cache]] ])) ;lets see if slightly less errors? nope, many more? issue is then clears each time ugh


(defn ^:export init []
 (when-some [el (js/document.getElementById "cue-db")]
  ; (defonce _init_ (load-page))
  ; (rf/clear-subscription-cache!) ;trytry
  (load-page)
  (r/render [views/ui] el)))

(init)

;EX
; (defn nav-link [uri title page]
;   [:a.navbar-item
;    {:href   uri
;     :class (when (= page @(rf/subscribe [:page])) :is-active)}
;    title])

; (defn navbar []
;   (r/with-let [expanded? (r/atom false)]
;     [:nav.navbar.is-info>div.container
;      [:div.navbar-brand
;       [:a.navbar-item {:href "/" :style {:font-weight :bold}} "hmm"]
;       [:span.navbar-burger.burger
;        {:data-target :nav-menu
;         :on-click #(swap! expanded? not)
;         :class (when @expanded? :is-active)}
;        [:span][:span][:span]]]
;      [:div#nav-menu.navbar-menu
;       {:class (when @expanded? :is-active)}
;       [:div.navbar-start
;        [nav-link "#/" "Home" :home]
;        [nav-link "#/about" "About" :about]]]]))

; (defn about-page []
;   [:section.section>div.container>div.content
;    [:img {:src "/img/warning_clojure.png"}]])

; (defn home-page []
;   [:section.section>div.container>div.content
;    (when-let [docs @(rf/subscribe [:docs])]
;      [:div {:dangerouslySetInnerHTML {:__html (md->html docs)}}])])

; (def pages
;   {:home #'home-page
;    :about #'about-page})

; (defn page []
;   [:div
;    [navbar]
;    [(pages @(rf/subscribe [:page]))]])

; ;; -------------------------
; ;; Routes
#_(def router
  (reitit/router
    [["/" :home]
     ["/about" :about]
     ["/visualizer" :visualizer]
     ["/repl" :repl]]))

; ;; -------------------------
; ;; History
; ;; must be called after routes have been defined
#_(defn hook-browser-navigation! []
  (doto (History.)
    (events/listen
      HistoryEventType/NAVIGATE
      (fn [event]
        (let [uri (or (not-empty (string/replace(.-token event) #"^.*#" "")) "/")]
          (rf/dispatch
            [:navigate (reitit/match-by-path router uri)]))))
    (.setEnabled true)))

; ;; -------------------------
; ;; Initialize app
; (defn mount-components []
;   (rf/clear-subscription-cache!)
;   (r/render [#'page] (.getElementById js/document "app")))

; (defn init! []
;   (rf/dispatch-sync [:navigate (reitit/match-by-name router :home)])
;   (ajax/load-interceptors!)
;   (rf/dispatch [:fetch-docs])
;   (hook-browser-navigation!)
;   (mount-components))

