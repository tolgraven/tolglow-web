(ns tolglow-web.core
  (:require [reagent.core :as r]
            [re-frame.core :as rf]
            [re-com.core :as rc :refer-macros [handler-fn]]
            [recalcitrant.core :refer [error-boundary]]
            [goog.events :as events]
            [clojure.pprint :as pprint]
            [clojure.string :as string]
            [thi.ng.color.core :as clr]
            [thi.ng.math.core :as cmath]
            [mount.core :as mount :refer [defstate]]
            ; [reitit.core :as reitit]
            [tolglow-web.db :as db]
            [tolglow-web.util :as util :refer [css-str css-arg cmod make-key cs <sub]]
            [tolglow-web.events :as tevents]
            [tolglow-web.subs]
            [tolglow-web.views :as views :refer [cue-page active-effects nav-btns sync-sources header-bar]] ;need?
            [tolglow-web.quil]))

(defn dev-setup []
  (enable-console-print!)) ;what else?

(defn safe "Wrap whatever with an error boundary"
 [elem]
 (r/as-element [(fn [] [error-boundary elem])]))


(defn load-page "Completely load or reload page" []
  (dev-setup) ;once is enough
  (println "LOAD") ;always
  (rf/dispatch-sync [:initialize]) ;on full reload
  (tevents/ql-init true) ;init re-graph. full
  (rf/dispatch [:init-page (util/on-key-init)]))

(defn soft-reset "Reload after exception" []
  (println "RELOAD SUBSCRIPTION CACHE!")
  (rf/clear-subscription-cache!) ;after react exception. so a third, nedium reload?? trigger how, by rf event request?
 )
; (defstate ^{:on-reload :noop} init-js
(defstate init-js
 :start load-page)

(defn ^:export init []
 (mount/start #'init-js))


(when-some [el (js/document.getElementById "cue-db")]
  (defonce _init_ (load-page))
  (r/render [views/ui] el))

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

