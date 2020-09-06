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
           [cljsjs.react-flip-move])
 (:require-macros
           [tolglow-web.macros :as macros]))

;should have a common format for all components.
;first arg a map like can be passed to regular [:div] etc
;Also should generally be able to pass a single path acting as Model
;for component - where val is fetched, and changes dispatched to.
;But also overridable when divergance or fancier behavior required.
;some kinda deep-merge anyways

(defn toggle "A nice little (but not native checkbox little) toggle"
 ([model-path]
  ; (let [on-change #(db/toggle model-path)
  ; (let [on-change #(rf/dispatch (into [:set] model-path))
  (let [on-change #(rf/dispatch [:set model-path %])
        model (rf/subscribe (into [:get] model-path))]
  [toggle {} model on-change]))
 ([model on-change]
  (toggle {} model on-change))
 ([attrs model on-change] ;this is double triggering...
  (let []
   [:label.toggle-switch
    (util/deep-merge
     {:style {:margin "2px 2px"
              :width  "1.5rem"
              :height "1.3rem"
              :z-index 2} }
      ; :on-click #(on-change (not @model))} ;wrong place!
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
 (let [at-path     (fn [& args] (into path args))
       dispatcher  (fn [& args] #(rf/dispatch (into path args)))]
  {:on-drag-start  (dispatcher :start id)
   :on-drag-end    (dispatcher :end)
   :on-drag-enter  (dispatcher :over position) ;-enter is like -over but fires only once! and doesnt actually work for finishing off... lol
   :on-drag-over   (fn [e] (.preventDefault e) ;prevent "Reset the current drag operation to "none"."
                    ;guess still need below as -leave is sluggish (arrives after incoming -enter)
                    (when (not= position (db/get (at-path :over))) ;prevent event spam...
                     (rf/dispatch (at-path :over position)))) ;sync or it'll send 100 events before we have the chance to catch up...
   :on-drag-leave  (dispatcher :over nil)
   :on-drop        (fn [e] (.preventDefault e) ;prevent "open as link for some elements"
                    (rf/dispatch (at-path :over position))) ;-leave can be rather overzealous, ensure valid :over
   ; ^ so over-sensitive with touch. how best throttle?
   :on-touch-start (fn [e]
                    (dispatcher :start id)) ;would need a timer to check length of touch in case just tapping yeah?
   ; :on-touch-move  (dispatcher :end)
   ; :on-touch-end   (dispatcher :end)
   :draggable      (if id true)})) ;all are valid drop targets, but empties cant be dragged...


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
     (merge {:class class, :type "search" ;for clear button ;"text"
             :style (merge {:display "inline-flex" :flex "1 1 auto"
                            :width (or width 100) ; how do like "min-width 'chars in str model + 10' up til 200 pixels yada?"
                            :height height}       ; user best wrap in div or pass class for more fine grained control either way
                           style)
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
                           true)))}
            attr)])))) ;after not before, want to be able to override stuff duh



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
; (def custom-picker (r/adapt-react-class js/ReactColor.CustomPicker))
