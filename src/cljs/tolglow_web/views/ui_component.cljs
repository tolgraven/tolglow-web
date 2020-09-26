(ns tolglow-web.views.ui-component "UI bits and pieces, gizmos, buttons..."
 (:require [re-frame.core :as rf]
           [reagent.core :as r]
           [re-com.core :as rc]
           [clojure.pprint :as pprint]
           [clojure.string :as string]
           [clojure.edn :as edn]
           [goog.string :as gstring]
           [thi.ng.math.core :as cmath]
           [thi.ng.color.core :as clr]
           [tolglow-web.util :as util :refer [at css-str css-arg cmod make-key cs <sub]]
           [tolglow-web.db :as db]

            [cljsjs.react]
            [cljsjs.react-color]
            [cljsjs.react-flip-move]))

;should have a common format for all components.
;first arg a map like can be passed to regular [:div] etc
;makes sense to me. Can we copy their way of avoiding it mandatory?
;Also should generally be able to pass a single path acting as Model
;for component - where val is fetched, and changes dispatched to.
;But also overridable when divergance or fancier behavior required.
;some kinda deep-merge anyways

(defn log "Show an expandable log thingy. Prob dumb here but good base for any sorta feed thingy I guess!"
  [options {:keys [messages] :as content}]
 (let [time-format (formatters :hour-minute-second)
       table-ref (atom nil) ; scroll (r/atom nil)
       log-line (fn [{:keys [time level title message] :as msg}]
                  [:tr.log-message
                   [:td (unparse time-format time)]
                   [:td {:class (str "message " (name level))} (name level)]
                   [:td title]
                   [:td.message #_{:style {:position :relative :left "1em"}} (str message)]])]
  (r/create-class
   {:display-name "Log"
    :component-did-update (fn [this] ; (r/dom-node this)
                           ; (println "Log updated!" (.-scrollHeight @table-ref))
                           ; (reset! scroll (.-scrollHeight @table-ref))
                           (set! (.-scrollTop @table-ref) (.-scrollHeight @table-ref))) ;resort to this since :scroll-top @ratom in the actual element doesnt work...
    :reagent-render
    (fn []
     [:div.log-container
      [minimize [:options :display :log]] ;this also needs to send an event to scroll-top the fucker...
      [:div.log-inner {:ref (fn [el] (reset! table-ref el))
                       :style {:max-height (if (:minimized @options) "1.2em" "20em")}
                       ; :scroll-top @scroll ;wonder why this doesnt work
                       ; :style {:max-height @scroll}
                       }
       [:table>tbody.log
        (for [msg (map messages (sort (keys messages))
                       #_(if (:minimized @options) ;upside-down?
                           [(count messages)]
                           (sort (keys messages))))]
         ^{:key (str (:id msg))}
         [log-line msg])]]])})))


(defn modal "Container for anything modal, taking care of common stuff"
 [component & [on-outside-click]]
 (let []
  (db/set [:modal] true)
  [:div#modal-container
   [:div#modal-bg {:on-click on-outside-click
                   :style {:position :fixed
                           :width "100%" :height "100%" :top 0 :left 0
                           :background "rgb(30, 30, 30, 0.5)"}}]
   [:div#modal {:class (when (db/get [:modal]) "modal-is-open")}
    component]]))

(defn hud-modal "Show more info about a specific HUD message"
 [] ;doesnt really have to be modal but wanted to implement that, so...
 (if-let [msg @(rf/subscribe   [:hud :modal])]
  (let [to-close #(rf/dispatch [:hud :modal :remove])]
   [modal [:div.hud-modal-main
           {:class (str "hud-message " (name (:level msg)))}
           [:h3  (:title   msg)]
           [:p   (str (:message msg))]
           [:p   (str (:time    msg))]
           [ui/close to-close]]
    to-close])
  (db/set [:modal] false))) ;eww gross

; (rf/dispatch [:diag/new :error "Some info" "much further info..."])
; (rf/dispatch [:diag/new :error "Title" "veryerysdbhafjasbdfhjasdbfhj ashjdfbahsdjf adjfhsabdfjhsdabfja asdhjfashdfsajdhfg hkasdfh"])


(defn hud "Render a HUD sorta like figwheel's but at reagent/re-frame level" []
 (let [to-show @(rf/subscribe [:hud])
       one-msg (fn [{:keys [level title message time actions id]}]
                (let [class (str "hud-message " (name level))]
                 [:div.hud-message
                  {:class class
                   :style {:position :relative}
                   :on-click #(rf/dispatch (or (:on-click actions)
                                               [:hud :modal id])) }
                  [:span title]
                  [ui/close (fn [e]
                             (.stopPropagation e) ;it's causing a click on hud-message as well...
                             (rf/dispatch [:diag/unhandled :remove id]))]]))]
  [:div.hud.hidden
   {:class (when (seq to-show) "visible")}
   [ui/flip-move
    {:class "hud-messages"
     :duration 200 :staggerDelayBy 20 :staggerDurationBy 30}
    (for [msg to-show]
     [one-msg msg])]]))


(defn toggle "A nice little (but not native checkbox little) toggle"
 ([model-path]
  ; (let [on-change #(db/toggle model-path)
  ; (let [on-change #(rf/dispatch (into [:set] model-path))
  (let [on-change #(rf/dispatch [:set model-path %])
        model (rf/subscribe (into [:get] model-path))]
  [toggle {} model on-change]))
 ([model on-change]
  [toggle {} model on-change])
 ([attrs model on-change] ;this is double triggering...
  (let []
   [:label.toggle-switch
    (util/deep-merge
     {:style {:margin "2px 2px"
              :width  "1.5rem"
              :height "1.3rem"
              :z-index 2} }
     attrs)
    [:input {:type :checkbox :default-checked @model
             :on-click (fn [e] ;ok, only triggers once now
                 ; (.preventDefault e) ;broke it! :O what
                 (on-change (not @model)))}]
    [:span.toggle-slider.round]
    #_"label"])))
;then build radio version as well for some settings...

(defn button [attrs model on-change])


(defn quick-wrapper
 [f model-path & [attrs]]
 (let [on-change #(db/toggle model-path)
       model (rf/subscribe (into [:get] model-path))]
  [f {} model on-change]))


(defn material-toggle
 [model-path [on-state off-state & [prefix]]]
 (let [model (rf/subscribe (into [:get] model-path))]
  [:i.zmdi
   {:class (str "zmdi-"
                (when prefix (str prefix "-"))
                (if @model on-state off-state))
    :style {:margin "0.1em 0.2em"}
    ; :style {:margin "0.2em 0.4em"}
    :on-click #(db/toggle model-path)}]))

(defn minimize [model-path]
 [material-toggle
  (into model-path [:minimized])
  ["maximize" "minimize" "window"]])

(defn close [on-click]
 [:i.zmdi.zmdi-close
  {:style {:position :absolute
           ;:right "0.4rem" :top "0.2rem"
           :right -5 :top -5
           :background "rgb(30, 30, 30, 0.7)"
           :color "#edc"
           :border-radius "50%"
           :padding "0.1em"
           :font-size "1.0rem"
           ; :line-height "1em"
           }
   :on-click on-click}])

(defn formatted-data [title path-or-data]
 (let [data (if (vector? path-or-data)
             (db/get path-or-data)
             path-or-data)]
  [:div {:style {:text-align :left}}
  [:h5 title]
  [:pre (pprint/write data :stream nil)]]))

; ; if keep handlers outside to avoid recreating, id etc gets captured and not updated within. so no go...
(defn drag-handlers "Create on-drag-* handlers for grid-housed component. `path` is the base event path events will be dispatched and subscribed (XXX curr using :get ie. pure db path). TODO: Touch stuff..."
 [path {:keys [position id] :as obj}]
 (let [at-path (fn [& args] (into path args))
       dispatcher (fn [& args] #(rf/dispatch-sync (into path args)))]
  {:on-drag-start (dispatcher :start id)
   :on-drag-end   (dispatcher :end)
   :on-drag-enter (dispatcher :over position) ;-enter is like -over but fires only once!
   :on-drag-over   (fn [e]
                    (.preventDefault e) ;not sure why but it's said it's mandatory
                    (if (not= (db/get (at-path :over)) position) ;prevent event spam...
                     (rf/dispatch-sync (at-path :over position)))) ;sync or it'll send 100 events before we have the chance to catch up...
   :on-drag-leave  (dispatcher :over nil)
   :on-drop        (dispatcher :over position) ;-leave can be rather overzealous, ensure valid :over
   ; :on-touch-start (dispatcher :start id) ;would need a timer to check length of touch in case just tapping yeah?
   ; :on-touch-move  (dispatcher :end)
   ; :on-touch-end   (dispatcher :end)
   :draggable      (if id true)})) ;all are valid drop targets, but empties cant be dragged...


(defn- xy-in-rect [e dimension rect]
 (let [m {:x (- (.-clientX e) (.-left rect)) ;XXX shouldnt do unneccessary work tho
          :y (- (.-clientY e) (.-top rect))}]
  (map m dimension))) ;ok so now will return vec even for one dim


(defn mouse-handlers "Handle mouse events for xy/x/y control (fix so generic)"
 [on-change var-spec direction] ;[:x] [:y] [:x :y]...
  (let [s (r/atom {})
        run (fn [e]
             (.preventDefault e)
             (when (:pressed? @s)
              (let [now (xy-in-rect e direction (:rect @s))]
               (when (not= now (:was @s)) ;filters out loads of duplicate events...
                (swap! s assoc :was now)
                (let [size (map {:x (.-width  (:rect @s))
                                 :y (.-height (:rect @s))} direction)
                      minmax (mapv var-spec [:min :max])
                      scaled (if (= 1 (count now)) ;single element vector
                              (as-> (cmath/map-interval-clamped
                                     (first now) [0 (first size)]
                                     minmax)
                               res (if (= direction [:y])
                                    (- (:max var-spec) res)
                                    res)) ;flip when :y, dirty...
                              (map #(cmath/map-interval-clamped
                                     %1 [0 %2] %3)
                                   now size minmax))] ;might want a further check in case int-constrained?
                 (on-change scaled))))))
        end (fn [e]
             (when (:pressed? @s)
              (run e)) ;do we always wanna run one last time?
             (reset! s {}))
        start (fn [e] ;need some other way than checking .-buttons, bc touch...
               (.preventDefault e)
               (when
                ; true
                (or (= (.-buttons e) 1) (= (.-type e) "touchstart"))
                (swap! s assoc :pressed? true
                               :rect (util/bounding-rect e)) ;cache rect bc loads of event spam...
                (util/on-move-then-cleanup run end) ;test, works fine for knob just attaching this right away...
                #_(run e)))
        ; enter (fn [e] (when (:rect @s) (start e))) ;dont really need this anymore?
        out (fn [e]
             (when (:pressed? @s)
              (util/on-move-then-cleanup run end)))] ;on purpuse we're sending end as cleanup?
    {:on-mouse-down   start
     :on-mouse-up     end
     ; :on-mouse-leave  out ;-leave seems better than -out?  ;XXX if exit element, then exit window, then mouse up, when re-enter still running...
     :on-mouse-move   run
     ; :on-mouse-enter enter ;use -enter not -over, dont have to deal with spam...
     ; :on-mouse-out   out
     ; :on-drag-start   start ;causing NaN...
     :on-touch-start  start ;not working with iphone hmm...
     ; :on-touch-leave  out ;no such thing
     ;"calling preventDefault() on a touchstart or the first touchmove event of a series prevents the corresponding mouse events from firing"
     ;but im not calling it...
     :on-touch-move   run
     :on-touch-end    end
     :on-touch-cancel end
     })) ;keep -move registered or if end up with tons better to remove listener? not that it gets invoked unless above but...


;also fix something modal/popout, have 2-3 different pickers to switch between etc
(defn color-picker [model on-change & options]
 (let [on-slider (fn [k] (fn [e]
                          (->> e .-target .-value (* 0.01)
                               (assoc @model k)
                               on-change)))
       grad (fn [k]
             (str "linear-gradient(to right, "
                  @(-> @model (assoc k 0.1)  clr/as-css) ","
                  @(-> @model (assoc k 0.85) clr/as-css) ")"))
      get-slider (fn [k]
                  [:input.control-color-slider
                   {:type :range :min 0 :max 100 :value (* 100 (k @model))
                    :on-change (on-slider k)
                    :style {:background-image (grad k)
                            :height 16}}])]
  (fn []
   [:div.control-color ;.control-bg
   {:style {}}
   [ui/hue-picker ; [ui/slider-color-picker [ui/circle-picker [ui/chrome-picker
    {:color @(clr/as-css @model)
     ; :pointer (r/reactify-component (fn [] [:div "[]"]))
     :onChange (fn [color e]
                (->> (.-hex color)
                     clr/css
                     clr/hue
                     (assoc @model :h)
                     on-change))}]
   [get-slider :l] [get-slider :s]])))



(defn input-text "Returns markup for a basic text input label"
 [& {:as args :keys [value path on-enter]}]
 (let [external-model (r/atom (or (rf/subscribe path) (at value))) ;ok so why does (sub in ratom...) work, straight subscribe not...
       internal-model (r/atom (if (nil? @external-model) "" @external-model))] ;; Create a new atom from the model to be used internally (avoid nil)
  (fn [& {:as args :keys [value path on-enter on-change placeholder width height change-on-blur? disabled? class style attr]}] ;to pass through from outer
   (let [latest-ext-model (or @(rf/subscribe path) (at value)) ;how repl this if not passing model but sub?
         disabled?        (at disabled?)
         change-on-blur?  (at change-on-blur?)]
    (when (not= @external-model latest-ext-model) ;; Has model changed externally?
     (reset! external-model latest-ext-model)
     (reset! internal-model latest-ext-model))
    [:input.form-control
     (merge attr ; (merge args ;would seem smart but react gets pissed off :/
            {:class class, :type "search" ;for clear button ;"text"
             :style       (merge style
                                 {:display "inline-flex" :flex "1 1 auto"
                                  :width (or width 100)
                                  :height height})
             :placeholder placeholder
             :value       @internal-model
             :disabled    disabled?
             :on-change (fn [e]
                         (let [new-val (-> e .-target .-value)]
                          (when (and on-change (not disabled?))
                           (reset! internal-model new-val)
                           (when-not change-on-blur?
                            (reset! external-model @internal-model) ;uhh cant do that obviously
                            (on-change @internal-model)))))
             :on-blur   (fn [e]
                         (when (and on-change change-on-blur?
                                    (not= @internal-model @external-model))
                          (reset! external-model @internal-model) ;nor here...
                          (on-change @internal-model)))
             :on-key-up (fn [e]
                         (if disabled? (.preventDefault e)
                          (case (.-key e)
                           "Enter" (if change-on-blur?
                                    (do (reset! external-model @internal-model)
                                        (when on-change (on-change @internal-model)))
                                    (when (and on-enter (not= "" @internal-model))
                                     (on-enter @internal-model)))
                           "Escape" (if change-on-blur?
                                     (reset! internal-model @external-model)) ;already loses focus automatically when press esc
                           true)))})]))))



(defn toggle-all-btns "Create a button that dispatches events toggling an entire category of things"
 [toggle-path ids & {:keys [style class] :as args}]
 (let [get-btn (fn [on?]
               [rc/button
                :label (if on? "all" "none")
                :style {:height "80%" :font-size "70%"}
                :class (str "btn-sm " "btn-" (if on? "secondary" "dark"))
                :on-click #(doall (for [id ids]
                                   (rf/dispatch (into toggle-path [on?]))))])]
                ;; :disabled? "if all checked..."
  [:div.toggle-btns (get-btn true) (get-btn false)]))
; like so:   but figure out when, like below, not unbroken path...
    ; [:i.zmdi.zmdi-window-minimize
    ;   {:on-click #(doall (for [id (keys (db/get [:active]))]
    ;                       (rf/dispatch [:set [:active id :minimized] true])))}]
; (defn minimize-all
;  [:div.collapse-btn.darkened
;  [:i.zmdi.zmdi-window-minimize {:on-click #(doall (for [id (keys (db/get [:active]))] (rf/dispatch [:set [:active id :minimized] true])))}]])

(defn svg-test []
 [:svg {:width 120 :height 120 :viewBox "0 0 100 100" :stroke-dasharray 300 :stroke-dashoffset 20 :stroke-width 20}
  [:defs
   [:linearGradient#gradient {:x1 "0%" :y1 "0%" :x2 "0%" :y2 "100%"}
    [:stop {:offset "0%" :stop-color "#50bc9b"}]
    [:stop {:offset "100%" :stop-color "#5eaefd"}]]]
  [:circle {:cx "50" :cy "50" :r "45" :stroke "url(#gradient)" :stroke-width "20" :fill "none" :transform "rotate(90 50 50)"}]])


; (defn beat-viz "maybe three progress bars showing beat/bar/phrase progress?" []
;  (let [progress (rf/subscribe [:done])] ;so this should be ok with sub, no form2 needed. but setInterval is fucked?
;  [rc/progress-bar :model progress :striped? true]))

; (rf/reg-cofx :start-value #(assoc % :start-value (rand-int 100)))


#_[rc/box :child
    [rc/scroller :scroll :on ;:size "50%" ;size only does anything on small devices (phone), why
     :child [:div#quil (as-> (tolglow-web.quil/run-sketch) sketch
                             (if (pos? (count sketch))
                              sketch
                              [:div "Borken"]))]]]


;; (defn codehl-render [content]
;;   [:div {:dangerouslySetInnerHTML {:__html (-> content str js/marked)}}])
;;
;; (defn highlight-code [html-node]
;;   (let [nodes (.querySelectorAll html-node "pre code")]
;;     (loop [i (.-length nodes)]
;;       (when-not (neg? i)
;;         (when-let [item (.item nodes i)]
;;           (.highlightBlock js/hljs item))
;;         (recur (dec i))))))
;;
;; (defn codehl-did-mount [this] (highlight-code (r/dom-node this)))
;; (defn codehl-component [content]
;;   (r/create-class
;;    {:reagent-render      codehl-render
;;     :component-did-mount codehl-did-mount}))


;; [codehl-component
;; (for [cue (db/get [:active-filtered])]
;;  (when-let [content [:pre [:code cue]]]
;;   [codehl-component content]))))]


(def flip-move (r/adapt-react-class js/FlipMove))
;; (def slider (r/adapt-react-class js/ReactSlider))
;; ^ this one has ranged/minmax stuff so def go for that. just figure out how...

(def slider-color-picker (r/adapt-react-class js/ReactColor.SliderPicker))
(def alpha-picker (r/adapt-react-class js/ReactColor.AlphaPicker))
(def block-picker (r/adapt-react-class js/ReactColor.BlockPicker))
(def chrome-picker (r/adapt-react-class js/ReactColor.ChromePicker))
(def circle-picker (r/adapt-react-class js/ReactColor.CirclePicker))
(def compact-picker (r/adapt-react-class js/ReactColor.CompactPicker))
(def github-picker (r/adapt-react-class js/ReactColor.GithubPicker))
(def hue-picker (r/adapt-react-class js/ReactColor.HuePicker))
(def material-picker (r/adapt-react-class js/ReactColor.MaterialPicker))
(def photoshop-picker (r/adapt-react-class js/ReactColor.PhotoshopPicker))
(def sketch-picker (r/adapt-react-class js/ReactColor.SketchPicker))
(def swatches-picker (r/adapt-react-class js/ReactColor.SwatchesPicker))
(def twitter-picker (r/adapt-react-class js/ReactColor.TwitterPicker))
(def custom-picker (r/adapt-react-class js/ReactColor.CustomPicker))
