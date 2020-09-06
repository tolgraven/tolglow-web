(ns tolglow-web.events
 (:require [re-frame.core :as rf]
           [re-frame.interceptor :refer [->interceptor get-effect get-coeffect assoc-coeffect assoc-effect]]
           [vimsical.re-frame.cofx.inject :as inj]
           [re-graph.core :as rg]
           [day8.re-frame.http-fx]
           [ajax.core :as ajax]
           [reagent.core :as r]
           [thi.ng.math.core :as cmath]
           [thi.ng.color.core :as clr]
           [cljs-time.core :as ct]
           [graphql-query.core :as ql-q :refer [graphql-query]]
           [clojure.core.async :refer [pub sub chan go go-loop >! <! timeout close! unsub unsub-all]]
           [clojure.string :as string]
           [clojure.edn :as edn]
           [tolglow-web.db :as db]
           [tolglow-web.util :as util]
           ; [tolglow-web.quil :as quil]
           [graphql-builder.parser :refer-macros [defgraphql]]
           [graphql-builder.core :as qlb-core]))

; XXX remember: avoid subs here, use inj when needed, and best not - lean on db
; goes double for subs not also used in/tracked through components...

(def debug (when ^boolean goog.DEBUG rf/debug))

(defn log "Log to both console and app" ;XXX should add an endpoint to timbre instead.
 ([message] (log :debug message))
 ([level message & messages]
  (let [msg (string/join " " (into [message] messages))]
   (println (string/capitalize (name level)) "\t" message)
   (rf/dispatch [:diag/new level "Log" msg]))))

(defn log-errors [errors]
 (when (seq errors)
  (log :debug errors))) ;debug instead of error, dont want to spam the hud

(def log-ql-errors
 (->interceptor
    :id     ::handle-ql-errors
    :before (fn [{:keys [coeffects] :as context}]
              (log-errors (-> coeffects :event second :errors))
              context)))


(rf/reg-event-fx :init-re-graph
  (fn [_ [_ domain port]]
   (log :debug "Send re-graph init event" domain port)
   {:dispatch
    [::rg/init ;; override the websocket url (defaults to /graphql-ws, nil to disable)
     {:ws-url   (str "ws://"   domain ":" port "/graphql-ws") ;; override the websocket url (defaults to /graphql-ws, nil to disable)
      :http-url (str "http://" domain ":" port "/graphql")   ;; override the http url (defaults to /graphql)
      :http-parameters {:content-type "application/json" ; :accept "application/transit+json"
                        :with-credentials? false   ; makes allowed-origins work!
                        #_:oauth-token #_"Secret"} ;; any parameters to be merged with the request, see cljs-http for options
      :ws-reconnect-timeout    10000  ;; attempt reconnect n milliseconds after disconnect (default 5000, nil to disable)
      :resume-subscriptions?   true   ;; start existing subscriptions again when websocket is reconnected after a disconnect
      :connection-init-payload {}}]}))

;should have a default setter where can still decide path gets saved...
(rf/reg-event-db ::default-graph-ql-handler [rf/debug log-ql-errors]
  (fn [db [_ {:keys [data errors] :as payload}]]
   (assoc db :ql payload))) ;; do things with data e.g. write it into the re-frame database

(defn ql-query-map [query argmap & cb-path]
 (let [cb-path (or (when (seq cb-path) (vec cb-path))
                   [::default-graph-ql-handler])]
  {:dispatch [::rg/query query argmap cb-path]}))

(defn ql-query [query argmap & cb-path]
 (let [cb-path (or (when (seq cb-path) (vec cb-path))
                   [::default-graph-ql-handler])]
  (rf/dispatch [::rg/query query argmap cb-path])))

(defn ql-subscribe [query argmap id & cb-path]
 (let [cb-path (or cb-path [::default-graph-ql-handler])]
  (rf/dispatch [::rg/subscribe id query argmap cb-path])
  id)) ;return id for easy unsub?

(defn ql-unsubscribe [id] (rf/dispatch [::rg/unsubscribe id]))


(rf/reg-event-fx :code ;basic, just run any code in afterglow context. prob rip out existing afterglow code and make something with it (and more repl featurez)
 (fn [cofx [_ code id mutation?]]
  (let [query (str "{ eval_code(code: \"" code "\") }")] ;ugh figure out why variables are broken in re-graph tho...
   (rf/dispatch [(if mutation? ::rg/mutate ::rg/query)
                 query {} [:code-result id]] ))))

(rf/reg-event-db :code-result [rf/debug log-ql-errors]
 ; (fn [db [_ {:keys [data errors] :as payload}]]
 ; (fn [db {:keys [data errors] :as payload}]
 (fn [db data]
  (log data)
 ; (fn [db [_ all #_[id {:keys [data errors] :as payload}]]]
  (assoc-in db [:code #_id] data))) ;eh whatevs
; semi-generic should be prepped for ids like :start :stop :cue-blabla
; depending on orig query(id) and result (not error) we also change
; guess we want an equiv query tho so not using mutations to just get data... hmm
;
(rf/reg-event-fx :afterglow-action ;basic, just run any code in afterglow context. prob rip out existing afterglow code and make something with it (and more repl featurez)
 ; (fn [cofx [_ kind action arg]]
 (fn [cofx [_ argmap]]
  (let [query "mutation AfterglowAction($kind: String, $action: String, $arg: String) {
                 afterglow_action(kind: $kind, action: $action, arg: $arg)
               }"
        #_(str "{ afterglow_action(kind: \"" kind
                   "\", action: \"" action
                   "\", arg: \""    arg
                   "\") }")
        _ (log query)]
   ; (rf/dispatch [::rg/mutate query {:kind kind :action action :arg arg} [:code-result]]))))
   (rf/dispatch [::rg/mutate query argmap [:code-result]]))))

(defn queries []
 (db/get [:ql])
 (db/get [:code])
 (ql-query "{ shaders { file_name text kind }}" {} )
 (ql-query "{ retspec(i: 3) { file }}" {})
 (rf/dispatch [::rg/mutate "{ eval_code(code: \"(inc 2)\") }" {} [::default-graph-ql-handler]] )
 (rf/dispatch [::rg/mutate "{ afterglow_action(kind: \"cue\", action: \"start\", arg: \"[0 1]\") }" {} [::default-graph-ql-handler]] )
 ; (rf/dispatch [:afterglow-action :cue :start "[0 5]"] )
 (rf/dispatch [:afterglow-action "cue" "start" "[0 3]"] )
 ; (rf/dispatch [:run-code :start "afterglow.show/start!"] )
 (rf/dispatch [:code "inc 4" :running false] )
 (rf/dispatch [:code "(afterglow.show/start!)" :running true] )
 ; (rf/dispatch [:run-code "tolglow.util/clear!"] )
 (rf/dispatch [:run-code "taoensso.timbre/debug :ffs"] )
 (ql-query "{ cues { name variables { min name start } }}" {} )
 (ql-query "{ sin }" {} )
 (ql-query "{ fixtures { name id key x y z } }" {} )
 (ql-query "query GetShader($path: String) {
              shaders(path: $path) {
                kind text
              }
            }" {:path "resources/public/glsl"} )
 (ql-subscribe "{ sins }" {} :sinner)
 (ql-query (graphql-query {:queries [[:cues {:size 4 :cell_offset [0 16]}
                            [:id {:field/data [:name] :field/alias :text}
                                 [:variables [:name :type [:min [:value]] [:max [:value]] [:starting [:value]] ]]
                             :color]]]}) {}))


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

(defn cue-query [cell-offset size] ;fix that lib with helpers that the jap was using
 (str "{ cues(cell_offset: " (vec cell-offset)
      ", size: " size ") {
      id key text:name position
      variables { name type key min max start }
      color held priority
      }}"))

; (ql-query
;   ; (:graphql (load-query {:size 8 :cue_offset [0 16]}))
;   (:query (:graphql (load-cues {:size 8 :cue_offset [0 16]})))
;   {} ::save-cues)

#_(let [size @(rf/subscribe [:view :size])
      cue-offset @(rf/subscribe [:view :cell-offset])]
 (-> (load-query {:size size :cue_offset cue-offset})
     (get-in [:graphql :query])
     (string/replace #"^query\s?" "")
     (ql-query
      {:size size :cue_offset cue-offset} ::save-cues)))
; re-frame/reg-event-fx
;    ::dynamic-handler
;    [(re-frame/inject-cofx ::inject/sub
;       (fn [[_ arg1 arg2]]
;         ...
;         [::dynamic arg1 arg2]))]
;    (fn [{:as cofx {::keys [dynamic]} [_ arg1 arg-2]]
;      ...)
;      ; XXX ^ possible to inject a sub sending params to it, from receiving event...

(rf/reg-event-fx :request-cues-for-view
 (fn [{:as cofx :keys [db]} [_ view-override]]
  (let [{:keys [size page offset]} (or view-override (get-in db [:view]))
        cell-offset (mapv (fn [page offset]
                          (+ offset (* page size)))
                         page offset)]
   {:dispatch [::rg/query (cue-query cell-offset size)
               {} [::save-cues (not view-override)]]}))) ;propagate to surrounding only if was first in chain


(rf/reg-event-fx :send-fetch-events
 (fn [{:as cofx :keys [db]} _]
  (let [{:keys [fixtures shaders]} (get-in db [:schema :query])]
   {:dispatch-n [[::rg/query (:query fixtures) {} (:event fixtures)] ;uh, fixtures
                 [::rg/query (:query shaders)  (:args shaders) (:event shaders)] ;both should be replaced with init-included streams tho
                 [:request-cues-for-view]]})))

(rf/reg-event-fx :subscribe ;rename like ql-subscribe
 (fn [{:keys [db]} [_ action id]] ;no var support yet...
  (let [{:keys [query event]} (get-in db [:schema :subscription id])]
   (case action
    :start {:db (update-in db [:subscription :active] #(conj (set %1) id) )
            :dispatch [::rg/subscribe   id query {} event]}
    :stop  {:db (update-in db [:subscription :active] disj id)
            :dispatch [::rg/unsubscribe id]}))))

(defn parse-variables "Parse values GraphQL refuses to handle on its own, because they lack a fixed type"
 [variables]
 (into []
  (for [cue-var variables
        :let [parse (case (:type cue-var)
                      ("boolean" "integer" nil) edn/read-string
                      ("color") #(-> (string/split % #"HSLA") ;ouff...
                                     second edn/read-string vals
                                     clr/hsla))]] ;fugly but what to do...
   (util/remove-nils
    (reduce (fn [m k] (cond-> m (k m) (update k parse)))
            cue-var
            [:min :max :start])))))


(rf/reg-event-fx ::save-cues [#_rf/debug log-ql-errors] ;rename store-cues?
 (fn [{:as cofx :keys [db]} [_ propagate {:keys [data errors] :as payload}]]
  (let [cues (into {} ;seems best if can avoid all this running when not needed?
              (for [cue (:cues data) ;cue row
                    :let [[x y] (:position cue)
                          id (keyword (str x "-" y "-" (:key cue)))
                          {:keys [color variables]} cue]
                    :when (and (:text cue) (not (contains? (:cues-m db) id)))]
               {id (merge cue       {:id id}
                    (when color     {:color (clr/hsla color)}) ;works as got :Color in schema
                    (when variables {:variables (parse-variables variables)}))}))] ;should actually be the string keep still to match afterglow? and then use cue key for id...
   (merge
    {:db (update db :cues-m merge cues)}
    (when propagate
     {:dispatch-later
      (mapv (fn [off]
             {:ms 150 :dispatch [:request-cues-for-view
                                 (update (:view db) :page (partial mapv +) off)]})
            [[1 0] [1 1] [0 1]])}))))) ;want to dispatch more _if these cues came from view-change_ and not another auto-propagation...
; should maybe do something fancier like check all around then just prune anything invalid, dunno...
; then experiment with having all ready and adding softzoom-around to buttons...


(rf/reg-event-db ::save-fixtures [rf/debug log-ql-errors]
 (fn [db [_ {:keys [data errors]}]]
  (let [fixtures (into {} ;XXX make a utility fn to do the map-by-id dance
                       (for [fixture (:fixtures data)]
                        {(:id fixture) fixture}))] ;should actually be the string keep still to match afterglow? and then use cue key for id...
   (update db :fixtures merge fixtures))))

;rename like pass-fixture-state... and make template for streams to ch?
(rf/reg-event-fx :save-fixture-state [log-ql-errors] ;yup - AVOID saving to app-db. much better now. keep js state in js...
 (fn [{:as cofx :keys [db]} [_ {:keys [data errors]}]]
  (let [ch (get-in db [:ch :fixtures])
        fixtures (doseq [{:keys [id color dimmer] :as fixture} (:fixture_state data)
                        :let [data (merge fixture
                                          (when color {:color (clr/hsla color)})
                                          (when dimmer {:dimmer (/ dimmer 100)}))]] ;will just have to add rotation parsing later
                   (when ch (go (>! ch data)))) ;test this...
        #_(->>
                  (for [{:keys [id color dimmer] :as fixture} (:fixture_state data)
                        :let [data (merge fixture ;ugh inefficient shouldnt be sending nil crap in first place. but then gotta change to not graphql.
                                          (when color {:color (clr/hsla color)})
                                          (when dimmer {:dimmer (/ dimmer 100)})) ;normalize
                              _ (when ch (go (>! ch data)))]] ;will just have to add rotation parsing later
                   {id data}) ;maybe just send all at once on ch? lots of extra work i guess
                  (into {}) ;prob use reduce for less mem? but how can use so much...
                  util/remove-nils)]
   ; (update db :fixture-state merge fixtures)))) ;will need deep-merge?
   #_(update db :fixture-state util/deep-merge fixtures)))) ;do we need state atm for anything apart from vis? would help lots not touching app-db tbh...


(rf/reg-event-db :save-shaders [rf/debug log-ql-errors]
  (fn [db [_ {:keys [data errors] :as payload}]]
   (let [ch (get-in db [:ch :shaders])]
    (update db :shaders merge
            (into {}
                  (for [m (or (:shaders data)
                              [(:live_shader data)])
                        :let [m (update m :kind keyword)
                              _ (when ch (go (>! ch m)))]] ;maybe better?
                   {(:kind m) m})))))) ;XXX fix support for named shaders
                  ; (for [{:keys [kind text]} (or (:shaders data)
                  ;                               [(:live_shader data)])
                  ;       :let [kind (keyword kind)
                  ;             m {:text text :kind kind}
                  ;             _ (when ch (go (>! ch m)))]] ;maybe better?
                  ;  {kind m})))))) ;this is the one on init...


(rf/reg-event-fx :init
 (fn [{:as cofx :keys [db]} [_ dispatch-once dispatch-each]]
  (let [dispatches (concat dispatch-each ;try each first?
                           (when-not (get-in db [:done-init])
                            (conj dispatch-once
                                  [:set [:done-init] true])))]
   {:dispatch-n dispatches})))
; (events/listen js/window events/EventType.DEVICEORIENTATION
;                #(rf/dispatch [:set [:device-orientation] %]))

(rf/reg-event-fx :clear-rf-cache
 (fn [_ _]
   (log "Reload subscription cache.")
   (rf/clear-subscription-cache!))) ;after react exception. so a third, nedium reload?? trigger how, by rf event request?


(rf/reg-event-fx :init-keyboard
 (fn on-key-init [_ [_ options]]
  (log "Generate keyboard listeners")
  (let [;captured '("ArrowLeft" "ArrowRight" "ArrowUp" "ArrowDown")
        ondown (fn down [e]
                (case (.-key e)
                 ("ArrowLeft" "ArrowRight" "ArrowUp" "ArrowDown")
                 (.preventDefault e)
                 nil))
        onup (fn up [e]
                (case (.-key e)
                 ("ArrowLeft" "ArrowRight" "ArrowUp" "ArrowDown")
                 (do (.preventDefault e)
                     (rf/dispatch [:keypress (.-key e)])) ;but here should also send modifiers...
                 nil))]
   (util/remove-on-document "keydown" ondown) ;ensure no duplicates.
   (util/remove-on-document "keyup"   onup)
   (util/on-document "keydown" ondown)
   (util/on-document "keyup"   onup))))


(rf/reg-event-fx :keypress
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


(rf/reg-event-fx :view-change ;here should run the cell-offset update...
 (fn [{:keys [db]} [_ k change]] ;page/individual. should also keep track of zoom etc? all viewport stuff
  {:db (let [do-move #(map (comp (partial max 0) +) %1 change)] ;ensure >= 0
        (update-in db [:view k] do-move))
   :dispatch [:request-cues-for-view]})) ;fetch any needed new cues from server



(rf/reg-event-db :delete-cue
 (fn [db [_ cue]] (update db :cues (fn [cues] (remove #(= (:id %) (:id cue)) cues))))) ;can just delete from list and will be replaced by a dummy. tho bit inefficient to trigger entire process from one dunno

(defonce temp-id-counter (atom 114))
(rf/reg-cofx :new-cue-id #(assoc % :new-cue-id (keyword (str (swap! temp-id-counter inc))))) ;tbh why not put a starting cue in db instead and update that?

(rf/reg-event-fx :create-cue [#_rf/debug (rf/inject-cofx :new-cue-id)
                              (rf/inject-cofx ::inj/sub [:cues-for-ids [:get :macro :checked]])
                              (rf/inject-cofx ::inj/sub [:cues])
                              (rf/inject-cofx ::inj/sub [:macro :save-with-name])]
 (fn [{:as cofx :keys [db new-cue-id cues-for-ids cues macro]} [_ position]] ; open a modal interface yeah?
  ; ev when effect-fns register themselves etc will show a list of available ones, pick,
  ; get shown list of param inputs etc, choose, yada yada
  ; (let [selected @(rf/subscribe [:cues-for-ids [:get :macro :checked]])]
  (let [selected cues-for-ids]
   (when (seq selected)
    {:db (let [variables (doall (for [cue selected
                             cue-var (:variables cue)]
                         (assoc cue-var :starting @(rf/subscribe [:cue-var-value (:id cue) cue-var]))))
             cue {:id new-cue-id ;eh no
                  :text macro ;to cofx i guess if adding ticking id when empty...
                  :variables variables
                  :priority 10 ;w/e
                  :position position
                  :color @(reduce #(cmath/mix %1 %2 (/ 1 (count selected)))
                                  ; (map clr/css (map :color selected)))}
                                  (map :color selected))}
             ; existing @(rf/subscribe [:get-cue :position position])
             ; cues (remove #(= position (:position %)) @(rf/subscribe [:cues]))]
             cues (remove #(= position (:position %)) cues)]
        (assoc db :cues (conj cues cue)))
   :dispatch-n [[:macro :check-all false]
                [:macro :save-with-name ""]]}))))
;i mean this is mostly fake. shouldnt do much more than send a mutation to server
;then wait for new state to come back...


(rf/reg-event-db :change-cue #_[(rf/path [:cues-m])] #_[rf/debug]
 (fn [db [_ id new-data]]
  (assoc-in db [:cues-m id] new-data)))


(rf/reg-event-fx :cue-drag [rf/debug]
 (fn [{:keys [db] :as cofx} [_ event arg]]
  (case event ;XXX BUGGY: -leave oversensitive so often end up with nil'd :over...
    :start {:db (assoc-in db [:cue-drag :id]   arg)}
    :over  {:db (assoc-in db [:cue-drag :over] arg)} ;pos
    :end   {:db (dissoc   db  :cue-drag)
            :dispatch-n
            (let [{:keys [id over]} (:cue-drag db)
                  cue @(rf/subscribe [:get-cue :id id])]
             (when (and over (not= over (:position cue))) ;dont try to move to nothing or self
              [[:change-cue id (assoc cue :position over)]
               (if-let [existing @(rf/subscribe [:get-cue :position over])]
                [:change-cue (:id existing)
                 (assoc existing :position (:position cue))])]))})))


(rf/reg-event-fx :cue-var-set
 (fn [{:as cofx :keys [db]} [_ cue-id cue-var v]]
  (let [validator @(rf/subscribe [:cue-var-validator (:type cue-var)])
        validated (cond-> v
                   validator (validator (:min cue-var) (:max cue-var)))]
   (when-not (or (nil? validated)
                 (and (number? validated) (js/isNaN validated)))
     {:db (assoc-in db [:active cue-id (:name cue-var)] validated)
      #_:dispatch #_[::rg/mutate :ffs]})))) ;makes sense if they all have ids? and they do in afterglow, when running anyways

; (rf/reg-event-fx :cue-var-save
;  (fn [{:as cofx :keys [db]} [_ cue-id value var-name]]
;   (let [path [:session cue-id var-name]
;         value (or value (get-in db [:active cue-id var-name]))]
;    {:db (assoc-in db path value)})))

(rf/reg-event-fx :cue-var-save [rf/debug] ;session save, that is... but would happen on server tbh
 (fn [{:keys [db] :as cofx} [_ cue-id value & var-name]]
  (let [path (into [:session cue-id] var-name) ;no name = save all
        value (or value (get-in db (into [:active cue-id] var-name)))]
   {:db (assoc-in db path value)})))


(rf/reg-event-fx :cue-interaction [rf/debug #_undo-interceptor]
 (fn [{:keys [db] :as cofx} [_ id action-event position]] ;last one optional for dummies
  (let [cue    @(rf/subscribe [:get-cue :id id]) ;i guess we can call this "ok" since cue ids are supposed to be set in stone? like it's technically impure but in practice same input -> same output. wait actually we dont even want that, what with different actions on click/toggle, create...
        active (get-in db [:active id])
        macro? (get-in db [:macro :is-selecting])
        ; afterglow-ev [:afterglow-action "cue" action-event (str position)]]
        afterglow-ev [:afterglow-action {:kind "cue" :action action-event :arg (str position)}]]
   (case action-event
    "click"  {:dispatch [:cue-interaction
                         id (if (:id cue)
                             (if active "delete" "start")
                             (if macro? "create"))
                         position]}
    "delete" {:dispatch-n [(when (get-in db [:options :auto-save-vars]) ;not a proper action just the name of the icon lol
                            [:cue-interaction id "save"])
                           [:macro :checked [id false]] ;ensure doesnt stay selected for macro
                           [:cue-interaction id "end"]]}
    "save"   {:dispatch [:cue-var-save id active]} ;sent mutation to aglow as well...
    ; below only reachable from self-dispatch
    "end"    {:db (update db :active dissoc id)
              :dispatch-n [afterglow-ev]}
    "start"  {:db (update db :active assoc id {})
              :dispatch-n [afterglow-ev] ;[[:start-an-intense-animation] [:start-lowkey-isrunning-animation]]
              ;:dispatch-later [{:ms 300 :dispatch [:stop-the-intense-animation]}]
              }
    "create" {:dispatch [:create-cue position]}
    {:dispatch [:unhandled (keyword action-event) cue]}))))



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

(rf/reg-event-fx :macro [rf/debug]
 (fn [{:keys [db] :as cofx} [_ command value]]
  (case command
   :checked {:db (let [[id state] value
                       path [:macro :checked id]
                       updated (assoc-in db path state)]
                  (if-not (get-in updated path)
                   (update-in db [:macro :checked] dissoc id)
                   updated))}
   :check-all {:dispatch-n (mapv (fn [id] [:macro :checked [id value]])
                                 (keys (db/get [:active])))} ;prob makes no sense - rather use generic ways of toggling all!
   :save-with-name {:db (assoc-in db [:macro :save-with-name] value)})))

(rf/reg-event-fx :filter
 (fn [{:keys [db] :as cofx} [_ command value]]
  (case command
   :save   {:db (update-in db [:filter :saved] #(conj (set %) value))
            :dispatch [:filter :set ""]}
   :delete {:db (update-in db [:filter :saved] #(disj % value))}
   :set    {:db (assoc-in db  [:filter :value] value)})))


(rf/reg-event-db :unhandled ;makes sense to store as much or do we just log straight out or
 (fn [db [_ id data]]
  (println "Unhandled event:" id ", data has been stored in db.")
  (assoc-in db [:unhandled id] data)))

; (db/get [:ch :fixtures])
(rf/reg-event-db :set
 (fn [db [_ path value]]
  (assoc-in db path value)))
(rf/reg-event-db :unset
 (fn [db [_ path]]
  (update-in db (butlast path) dissoc (last path))))
(rf/reg-event-db :toggle
 (fn [db [_ path]]
  (update-in db path not)))
(rf/reg-event-db :conj
 (fn [db [_ path value]]
  (update-in db path conj value)))
(rf/reg-event-db :pop
 (fn [db [_ path]]
  (update-in db path pop)))


(rf/reg-event-fx :quil
 (fn [{:keys [db]} [_ action]]
  (case action
   true   {:db (assoc-in db [:quil :show] true)
           :dispatch [:quil :start]}
   false  {:db (update-in db [:quil] dissoc :show)
           :dispatch [:quil :stop]}
   :start {:dispatch-n [[:subscribe :start :fixture-state]
                        [:quil-control :start]]}
   :stop  {:dispatch-n [[:subscribe :stop  :fixture-state]
                        [:quil-control :stop]]}
          {:dispatch    [:quil-control action]})))

(rf/reg-event-fx :quil-control
 (fn [{:keys [db]} [_ action]]
  #_(quil/controls action)))


(rf/reg-event-fx ::http-get
  (fn [{:keys [db]} [_ opts & [handler error]]]
    {:db (assoc db :show-twirly true)   ;; set something to indicate request is underway
     :http-xhrio
     (merge
      {:method          :get
       :uri             "https://api.github.com/orgs/day8"
       :timeout         2000                                           ;; optional see API docs
       :response-format (ajax/json-response-format {:keywords? true})  ;; IMPORTANT!: You must provide this.
       :on-success      [(or handler :default-http-result) :success]
       :on-failure      [(or error   :default-http-error)  :error]}
      opts)}))

(rf/reg-event-fx ::http-post
  (fn [{:keys [db]} [_ opts & [handler error]]]
    (let [timeout 5000]
     {:http-xhrio
      {:method          :post
       :uri             (str "http://localhost:3449/api"
                             "/plus") #_"https://httpbin.org/post"
       ; :uri             (str "http://localhost:16000/"
       ;                       "ui-event/3/" "stopButton") #_"https://httpbin.org/post"
       ; :params          {:x 1 :y 2}
       :params          "x 1 y 2"
       :timeout         timeout
       ; :format          (ajax/json-request-format)
       :response-format (ajax/json-response-format {:keywords? true})
       :on-success      [:default-http-result]
       :on-failure      [:default-http-error]}
      :timeout [:yada-yada
                :timeout timeout]})))

; To make multiple requests, supply a vector of options maps:
; {:http-xhrio [ {...}
;                {...}]}

(rf/reg-event-fx :default-http-result
 (fn [db [_ res]] (println res)))
(rf/reg-event-fx :default-http-error
 (fn [db [_ res]] (println res)))

; (rf/dispatch [::http-get ])

; remember: we could keep everything mapped by :id, even cue-vars
; just use a sep key vector for display order!
; {:min {:start 0 :type :integer} :max {:osv true}}
; :cue-var-order [:min :max]


(rf/reg-cofx :now         #(assoc % :now (ct/now)))

(defonce diag-id-counter (atom 0))
(rf/reg-cofx :diag/gen-id #(assoc % :id (swap! diag-id-counter inc)))

(rf/reg-event-fx :diag/new  ;this needs a throttle lol
 [#_debug (rf/inject-cofx :now) (rf/inject-cofx :diag/gen-id)]
 (fn [{:keys [db now id]} [_ level title message actions]] ;error, warning, info
  (merge
   {:db (update-in db [:diagnostics :messages]
                  assoc id {:level   level
                            :id      id
                            :title   title
                            :message message
                            :time    now
                            :actions actions})}
   (when (not= level :debug)
    {:dispatch    [:diag/unhandled :add    id]
     :dispatch-later
     [{:dispatch  [:diag/unhandled :remove id]
       :ms (* 1000 (get-in db [:options :hud :timeout]))}]})))) ;tho can always get removed earlier by us...
;;random maybe:
;{:origin :server ;first thought just react but yeah nice
; :strategies {:undo [[:an-undo-button]]
;              :ignore [:silence]
;              :open-ide [:get-line-last-error error]} }

(rf/reg-event-db :diag/unhandled #_[debug]
 (fn [db [_ action id]]
  (case action
   :add    (update-in db [:diagnostics :unhandled] conj id)
   :remove (update-in db [:diagnostics :unhandled] #(-> % set (disj id))))))

(rf/reg-event-db :hud
 (fn [db [_ action id]]
  (case action
   :modal (if (= id :remove)
           (update db :hud dissoc :modal)
           (assoc-in db [:hud :modal] id)))))



; Undos:
; (day8.re-frame.undo/undo-config! {:harvest-fn h :reinstate-fn r})
; h is a function which "obtains" that state which should be remembered for undo/redo purposes. It takes one argument, which is app-db. The default implementation of h is deref which, of course, simply harvests the entire value in app-db.
; r is a function which "re-instates" some state (previously harvested). It takes two arguments, a ratom (app-db) and the value to be re-instated. The default implementation of r is reset!.

; With the following configuration, only the [:a :b] path within app-db will be undone/re-done:
; (day8.re-frame.undo/undo-config!
;   {:harvest-fn   (fn [ratom] (some-> @ratom :a :b))
;    :reinstate-fn (fn [ratom value] (swap! ratom assoc-in [:a :b] value))})
; so yeah just save eg view, subset of eg vars easy to pass entire
; structure rather than "undo" bunch of http hehe
; could also keep track of (afa eg launched cues) whether we caused
; an action or server did.
;
; ALSO:
; App Triggered Undo
; Apparently, some people's apps throw exceptions in production, sometimes. To gracefully handle this, I've heard that they write an Unhandled Exception Handler which triggers an undo:
; (dispatch [:undo])
; They want their application to step back to the last-known-good state. Potentially from there, the user can continue on.
; ^ also applies to dev tho haha (think stepping back and enabling diffs...)

;
;
; (def undos (r/atom ()))
; (def redo (r/atom ()))

;(def undo-interceptor
;  (rf/->interceptor :id :undo
;    :before (fn [context] ;; we take the interceptor context
;              ;; (swap! undos conj (-> context :coeffects :db))
;              (swap! undos (fn [stack incoming]
;                            (conj (take 5 stack) incoming))
;                     (-> context :coeffects :db))
;              context))) ;; return the context unmodified

;(rf/reg-event-fx :undo ;; [redo-interceptor]
;  (fn [_ _]
;    (let [undo-values @undos]
;      (if (empty? undo-values)
;        (do (println "Nothing to undo.")
;            {}) ;; return no effects
;        (let [[latest & older] undo-values]
;          (swap! redo conj latest)
;          (reset! undos older)
;          {:db latest}))))) ;; update the db
;;^ something like this undo should prob limit its restore to same paths it monitors, no?
;; or deep merge. else bunch of other stuff in db will disappear.


(def interceptors [rf/debug rf/trim-v rf/after rf/enrich rf/path])

