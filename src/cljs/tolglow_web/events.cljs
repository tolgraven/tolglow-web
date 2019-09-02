(ns tolglow-web.events
 (:require [re-frame.core :as rf]
           [re-frame.interceptor :refer [->interceptor get-effect get-coeffect assoc-coeffect assoc-effect]]
           [re-graph.core :as rg]
           [reagent.core :as r]
           [thi.ng.math.core :as cmath]
           [thi.ng.color.core :as clr]
           [clojure.string :as string]
           [clojure.edn :as edn]
           [tolglow-web.db :as db]
           [tolglow-web.util :as util]))

(defn debug [] (when ^boolean goog.DEBUG rf/debug))
; XXX  HEAVY ws reconnect attempt spam when server down (like 20 errors a second in console?)
; is from this or something else?? afterglow + safari p sure
(defn ql-init "Initialize re-graph"
 [& [ws?]]
 (rf/dispatch [::rg/init ;; override the websocket url (defaults to /graphql-ws, nil to disable)
  {:ws-url (if ws? "ws://localhost:3001/graphql-ws") ;; override the websocket url (defaults to /graphql-ws, nil to disable)
   :http-url     "http://localhost:3001/graphql"   ;; override the http url (defaults to /graphql)
   :http-parameters {:content-type "application/json"
                     ; :accept "application/transit+json"
                     :with-credentials? false   ; makes allowed-origins work!
                     #_:oauth-token #_"Secret"} ;; any parameters to be merged with the request, see cljs-http for options
   :ws-reconnect-timeout    10000  ;; attempt reconnect n milliseconds after disconnect (default 5000, nil to disable)
   :resume-subscriptions?   true   ;; start existing subscriptions again when websocket is reconnected after a disconnect
   :connection-init-payload {}}])) ;; the payload to send in the connection_init message, sent when a websocket connection is made
; with current luminus setup: octet-stream and string both fine, json NOT since not actually sending json!!
; fuck knows with all that logging why there was no "UH BAD JSON SO COMPLETELY DROPPED INPUT

(defn ql-query [query argmap & cb-path]
 (let [cb-path (or (when (seq cb-path) (vec cb-path))
                   [::graph-ql-response])]
  (rf/dispatch [::rg/query query argmap cb-path])))

(defn ql-query-map [query argmap & cb-path]
 (let [cb-path (or (when (seq cb-path) (vec cb-path))
                   [::graph-ql-response])]
  {:dispatch [::rg/query query argmap cb-path]}))

(rf/reg-event-db ::graph-ql-response
  (fn [db [_ {:keys [data errors] :as payload}]]
   ; (println data)
   (when (seq errors) (println errors))
   (assoc db :ql payload))) ;; do things with data e.g. write it into the re-frame database

(defn ql-subscribe [query argmap id & cb-path]
 (let [cb-path (or cb-path [::graph-ql-response])]
  (rf/dispatch [::rg/subscribe id query argmap cb-path])
  id)) ;return id for easy unsub?

(defn ql-unsubscribe [id] (rf/dispatch [::rg/unsubscribe id]))

(defn queries []
 (db/get [:ql])
 (db/get [:re-graph])
 (db/get [:cues])
 (ql-query "{ cue_for_id { name variables { min name } }}" {:id 1} )
 (ql-query "{ cues { name }}" {} )
 (ql-query "{ sin }" {} )
 (ql-subscribe "{ sins }" {} :sinner)
 (ql-unsubscribe :cue-stream)
 (ql-query "query GetCues($size: Int) {
            cues(size: $size) {
            id text:name variables { name min max type } color { h s l} }}"
           {:size 2})
 (rf/dispatch [:cues-for-view]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Plan: graphql subs and all that jazz.
;;  Try to adapt most of the same strategies on server as well - so messy atm
;;
;;  But will maybe go something like:
;;  1.  Cue is launched in app
;;  2.  Web server quickly pushes out info so clients can react, as a one-off
;;  3.  Client makes further request (or likely, already has a spec of what data it wants)
;;  4.  Server sets up stream for cue (vars, vis, color...) and keeps updating until
;;      end of cue or goes out of scope or similar.
;;   4b. Client streams back any changes from its end, and any in-page running code
;;       or other zany stuff it wants affecting a param value...
;;  5.  Afterglow already only pushes changes, but should make it a bit more sophisticated
;;      a la re-frame. And not push a billion empty packets if nothing changed...
;;      Just keepalive...
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Btw, interceptor pattern for params and similar ought to be pretty good.
;;  So don't need to wrap all individual params but can set up like
;;  "Wrap params like this"
;;  "With this"
;;
;;  Or would work with effects as well...eg could implement "solo effect"
;;  without actually implementing it through interceptor chucking everyone
;;  elses assigners...
;;
;;  Event cue:
;;  Use above to create "novement" towards certain state, as an animation.
;;  Not as in, set up effect and slowly fade in (tho that's nice), but
;;  mod others. Including param/input level! Think putting a cubic-bezier
;;  on all "positional "
;;
;;  Animations by keyframe:
;;  Instead of a gen-fn calculating each frame work in immutable state and queued transformers
;;  (that create animation through fading and interpolation)
;;  ->INIT    1beats,  [ 1 0 0]XYZ
;;  ->Target: 16beats, [-1 1 5]XYZ
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn fix-query [cell-offset size] ;fix that lib with helpers that the jap was using
 (str "{ cues(cell_offset: " (vec cell-offset)
      ", size: " size ") {
      id text:name position priority
      variables { name min max type } held
      color current_color style_color
      }}"))

(def pure "Interceptor so dont have to return db... also trim-v"
 [rf/trim-v
  (rf/->interceptor :id :pure
   :before  (fn [context]
             (let [db (rf/get-coeffect context :db)]
              (assoc context :db-stash/db db)))
   :after   (fn [context]
             (rf/assoc-coeffect context :db (:db-stash/db context))))])


(rf/reg-event-fx :cues-for-view [pure]
 (fn [_ _]
  (ql-query-map
   (fix-query @(rf/subscribe [:view :cell-offset])
              @(rf/subscribe [:view :size]))
     {} ::save-cues)))


(rf/reg-event-db ::save-cues ;[rf/debug]
 (fn [db [_ {:keys [data errors] :as payload}]]
  ; (println "Cues INCOMING" data)
  (when (seq errors) (println errors))
  (let [view-offset @(rf/subscribe [:view :cell-offset])
        cues (into {} (for [cue (:cues data) ;cue row
               :let [id (keyword (:id cue))]
               :when (and (:text cue) (not (contains? (:cues-m db) id)))]
   {id (merge cue
              {:id id}
              (when (:color cue)
               {:color (clr/hsla (:color cue))}))}))] ;should actually be the string keep still to match afterglow? and then use cue key for id...
   (-> db
       (update :cues-m merge cues)))))

(rf/reg-event-fx :init-page
 (fn [cofx [_ init-keys-fn dispatches]]
  (init-keys-fn) ;keyboard navigation. more global stuff?
  {:db (:db cofx)
   :dispatch-n (into [[:cues-for-view]] dispatches)}))
; (events/listen js/window events/EventType.DEVICEORIENTATION
;                #(rf/dispatch [:set [:device-orientation] %]))


(rf/reg-event-fx :keypress [pure]
 (fn [_ [key-id]]
  (let [x (case key-id
                "ArrowLeft" -1
                "ArrowRight" 1
                0)
        y (case key-id
                "ArrowUp"    1
                "ArrowDown" -1
                0)]
   {:dispatch [:view-change :page [x y]]}))) ;testing


(rf/reg-event-fx :view-change
 (fn [{:keys [db]} [_ k change]] ;page/individual. should also keep track of zoom etc? all viewport stuff
  {:db (let [do-move #(map (comp (partial max 0) +) %1 change)] ;ensure >= 0
               (update-in db [:view k] do-move))
   :dispatch [:cues-for-view]}))


(rf/reg-event-db :delete-cue
 (fn [db [_ cue]] (update db :cues (fn [cues] (remove #(= (:id %) (:id cue)) cues))))) ;can just delete from list and will be replaced by a dummy. tho bit inefficient to trigger entire process from one dunno

(defonce temp-id-counter (atom 114))
(rf/reg-cofx :new-cue-id #(assoc % :new-cue-id (keyword (str (swap! temp-id-counter inc))))) ;tbh why not put a starting cue in db instead and update that?

(rf/reg-event-fx :create-cue [#_rf/debug (rf/inject-cofx :new-cue-id)]
 (fn [{:keys [db] :as cofx} [_ position]] ; open a modal interface yeah?
  ; ev when effect-fns register themselves etc will show a list of available ones, pick,
  ; get shown list of param inputs etc, choose, yada yada
  (let [selected @(rf/subscribe [:cues-for-ids [:get :macro :checked]])]
   (merge {:db db}
          (when (seq selected)
    {:db (let [id (:new-cue-id cofx)
             variables (for [cue selected
                             cue-var (:variables cue)]
                         (assoc cue-var :starting @(rf/subscribe [:cue-var-value (:id cue) cue-var])))
             cue {:id id
                  :text @(rf/subscribe [:macro :save-with-name]) ;to cofx i guess if adding ticking id when empty...
                  :fx (->> selected (map :fx) (clojure.string/join " / "))
                  :variables variables
                  :priority 10 ;w/e
                  :position position
                  :color @(reduce #(cmath/mix %1 %2 (/ 1 (count selected))) (map clr/css (map :color selected)))}
             ; existing @(rf/subscribe [:get-cue :position position])
             cues (remove #(= position (:position %)) @(rf/subscribe [:cues]))]
        (assoc db :cues (conj cues cue)))
   :dispatch-n [[:macro :check-all false]
                [:macro :save-with-name ""]]})))))

; (rf/reg-event-db :change-cue [(rf/path [:cues-m])] ;[rf/debug]
;  (fn [cues [_ id new-data]]
;   (map (fn [cue]
;         (if (= (:id cue) id) new-data cue))
;        cues)))
(rf/reg-event-db :change-cue #_[(rf/path [:cues-m])] [rf/debug]
 (fn [db [_ id new-data]] ;bc path change it''s broken on save. plus now gotta update in two places or how will handle?
  (update db :cues-m
           #(map (fn [cue]
                  (if (= (:id cue) id) new-data cue))
                 %))))

(rf/reg-event-fx :cue-drag [rf/debug]
 (fn [{:keys [db] :as cofx} [_ event arg]]
  (merge
   {:db db}
   (case event ;XXX BUGGY: -leave oversensitive so often end up with nil'd :over...
    :start {:db (assoc-in db [:cue-drag :id] arg)}
    :over  {:db (assoc-in db [:cue-drag :over] arg)} ;pos
    :end   {:db (dissoc db :cue-drag)
            :dispatch-n
            (let [{:keys [id over]} (:cue-drag db)
                  cue @(rf/subscribe [:get-cue :id id])]
             (when (and over (not= over (:position cue))) ;dont try to move to nothing or self
              (list [:change-cue id (assoc cue :position over)]
                    (if-let [existing @(rf/subscribe [:get-cue :position over])]
                     [:change-cue (:id existing)
                      (assoc existing :position (:position cue))]))))}))))


(rf/reg-event-db :inc-done
 (fn [db [_ nr]]
  (update db :done #(if (> 100 (+ (or % 0) nr)) (+ (or % 0) nr) 0))))
;; (defonce _interval2 (js/setInterval #(rf/dispatch-sync [:inc-done 1]) 500)) ;so would set up intervals to match bpm?

; (rf/reg-event-fx :for-all
;  (fn [{:keys [db] :as cofx} [_ event items]]
;   (merge
;    {:db db}
;    {:dispatch-n (mapv (fn [item] [:macro :checked [id value]])
;                       items)})))
; (db/get [:macro])

(rf/reg-event-fx :macro #_[rf/debug]
 (fn [{:keys [db] :as cofx} [_ command value]]
  (merge {:db db}
         (case command
          :checked {:db (let [[k state] value
                              path [:macro :checked k]
                              updated (assoc-in db path state)]
                              (if-not (get-in updated path)
                               (update-in db [:macro :checked] dissoc k)
                               updated))}
          :check-all {:dispatch-n (mapv (fn [id] [:macro :checked [id value]])
                                        (keys (db/get [:active])))} ;prob makes no sense - rather use generic ways of toggling all!
          :save-with-name {:db (assoc-in db [:macro :save-with-name] value)}))))

(rf/reg-event-fx :filter
 (fn [{:keys [db] :as cofx} [_ command value]]
  (merge {:db db}
         (case command
          :save {:db (update-in db [:filter :saved] #(conj (set %) value))
                 :dispatch [:filter :set ""]}
          :delete {:db (update-in db [:filter :saved] #(disj % value))}
          :set {:db (assoc-in db [:filter :value] value)}))))


(rf/reg-event-db :unhandled ;makes sense to store as much or do we just log straight out or
 (fn [db [_ id data]]
  (println "Unhandled event:" id ", data has been stored in db.")
  (assoc db [:unhandled id] data)))


(rf/reg-event-fx :cue-interaction [#_rf/debug #_undo-interceptor]
 (fn [{:keys [db #_options] :as cofx} [_ id action-event position]] ;last one optional for dummies
  (let [cue @(rf/subscribe [:get-cue :id id]) ;i guess we can call this "ok" since cue ids are supposed to be set in stone? like it's technically impure but in practice same input -> same output. wait actually we dont even want that, what with different actions on click/toggle, create...
        active @(rf/subscribe [:active id])
        macro? @(rf/subscribe [:get :macro :is-selecting])]
   (merge {:db db}
    (case action-event
     "click" {:dispatch [:cue-interaction
                         id (if (:id cue)
                             (if active "delete" "start")
                             (if macro? "create"))
                         position]}
     "delete" {:dispatch-n (list (when (get-in db [:options :auto-save-vars])
                                  [:cue-interaction id "save"])
                                 [:macro :checked [id false]] ;ensure doesnt stay selected for macro
                                 [:cue-interaction id "end"])}
     "save" {:dispatch-n (vec (for [[var-name value] active #_(get-in db [:active (:id cue) :values])]
                          [:cue-var-save id var-name value]))}
     ; below only reachable from self-dispatch
     "end" {:db (update db :active dissoc id)}
     "start" {;:dispatch-n [[:start-an-intense-animation] [:start-lowkey-isrunning-animation]]
              ;:dispatch-later {:ms 300 :dispatch [:stop-the-intense-animation]}
              :db (update db :active assoc id {})}
     "create" {:dispatch [:create-cue position]}
     {:dispatch [:unhandled (keyword action-event) cue]})))))


(def undos (r/atom ()))
(def redo (r/atom ()))

(def undo-interceptor
  (rf/->interceptor :id :undo
    :before (fn [context] ;; we take the interceptor context
              ;; (swap! undos conj (-> context :coeffects :db))
              (swap! undos (fn [stack incoming]
                            (conj (take 5 stack) incoming))
                     (-> context :coeffects :db))
              context))) ;; return the context unmodified

(rf/reg-event-fx :undo ;; [redo-interceptor]
  (fn [_ _]
    (let [undo-values @undos]
      (if (empty? undo-values)
        (do (println "Nothing to undo.")
            {}) ;; return no effects
        (let [[latest & older] undo-values]
          (swap! redo conj latest)
          (reset! undos older)
          {:db latest}))))) ;; update the db
;^ something like this undo should prob limit its restore to same paths it monitors, no?
; or deep merge. else bunch of other stuff in db will disappear.


(def interceptors [rf/debug rf/trim-v rf/after rf/enrich rf/path])
(def ensure-bounds ;yeah ok not a good candidate since this is for checkin the input, not the result
  (rf/->interceptor :id :ensure-bounds
   :after (fn [context]
            #_(let [trim-fn (fn [event] (-> event rest vec))]
             (update-in context [:coeffects :event] trim-fn)))))

