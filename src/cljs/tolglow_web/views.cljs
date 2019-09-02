(ns tolglow-web.views
 (:require [re-frame.core :as rf]
           [reagent.core :as r]
           [re-com.core :as rc]
           [clojure.pprint :as pprint]
           [clojure.string :as string]
           [clojure.edn :as edn]
           [goog.string :as gstring]
           [taoensso.timbre :as timbre]
           [thi.ng.math.core :as cmath]
           [thi.ng.color.core :as clr]
           ; [klipse.run.plugin.plugin] ;; this namespace initializes Klipse. We require it for its side effects
           ; [klipse.plugin :as klipse-plugin]
           ; [tolglow-web.macros :refer [->some->]]
           [tolglow-web.db :as db]
           [tolglow-web.views.ui-component :as ui]
           [tolglow-web.views.variable-control :as vc]
           [tolglow-web.util :as util :refer [at css-str css-arg cmod make-key cs <sub]]
           [tolglow-web.events :as events]))

;; ********************
;; CUE/EFFECT LIST VIEW
;; ********************
(def row-id :1)
(defn row-buttons "Replace recom shit soon enough tho."
 [row-id]
  (into [:div.effect-row-buttons {:style {:visibility "hidden"}}] ;then unhide by css, instead of ratom bs...
         (mapv (fn [action]
                #_[:i.zmdi.effect-row-button ;somehow with this the old :last-child selector goes for the child of this instead of all these??
                 {:class (str "zmdi-" action)
                  :on-click #(rf/dispatch [:cue-interaction row-id action])}]
                [rc/row-button :mouse-over-row? true
                 :md-icon-name (str "zmdi zmdi-" action)
                 :class "effect-row-button" ;cant use this to actually style button...
                 ; :tooltip (str action " cue") ;buggy as shit ends up bottom of page...
                 ; :tooltip-position :left-below
                 :on-click #(rf/dispatch [:cue-interaction row-id action])])
               ["save" "edit" "delete"])))

(defn cue-var-value->str "Format cue var value human readable"
 [v]
 (let [f #(if (float? %)
           (->> % (gstring/format "%.2f") edn/read-string str) ;str drops trailing zeroes so good enough!
           (str %))]
      (if (coll? v) (string/join " " (map f v)) (f v))))

(defn xy-in-rect [e dimension rect]
 (let [m {:x (- (.-clientX e) (.-left rect)) ;XXX shouldnt do unneccessary work tho
          :y (- (.-clientY e) (.-top rect))}]
  (map m dimension))) ;ok so now will return vec even for one dim


(defn mouse-handlers "Handle mouse events for xy/x/y control (fix so generic)"
 [on-change var-spec direction] ;[:x] [:y] [:x :y]...
  (let [s (r/atom {})
        run (fn [e]
             (when (:pressed? @s)
              (let [now (xy-in-rect e direction (:rect @s))]
               (when (not= now (:was @s)) ;filters out loads of duplicate events...
                (swap! s assoc :was now)
                (let [size (map {:x (.-width (:rect @s)) :y (.-height (:rect @s))} direction)
                      scaled (if (= 1 (count now)) ;single element vector
                              (as-> (cmath/map-interval-clamped
                                     (first now) [0 (first size)]
                                     [(:min var-spec) (:max var-spec)])
                               res (if (= direction [:y])
                                    (- (:max var-spec) res)
                                    res)) ;flip when :y, dirty...
                              (map #(-> (cmath/map-interval-clamped %1 [0 %2] [%3 %4]))
                                   now size (:min var-spec) (:max var-spec)))] ;might want a further check in case int-constrained?
                 ;; (scale-control now )
                 (on-change scaled))))))
        start (fn [e] ;need some other way than checking .-buttons, bc touch...
               (when (= (.-buttons e) 1)
                (swap! s assoc :pressed? true
                       :rect (.getBoundingClientRect (.-target e))) ;cache rect bc loads of event spam...
                (run e)))
        enter (fn [e] (when (:rect @s) (start e))) ;dont really need this anymore?
        end (fn [e]
             (when (:pressed? @s) (run e)) ;@pressed? ;do we always wanna run one last time?
             (reset! s {}))
        out (fn [e]
             (when (:pressed? @s)
              (util/on-move-til-mouse-up run end)))] ;helper fn for this pattern?
    {:on-mouse-down  start
     :on-mouse-up    end
     :on-mouse-leave out ;-leave seems better than -out?  ;XXX if exit element, then exit window, then mouse up, when re-enter still running...
     :on-mouse-move  run
     ; :on-mouse-enter enter ;use -enter not -over, dont have to deal with spam...
     ; :on-mouse-out   out
     :on-touch-start start
     :on-touch-move  run
     :on-touch-end   end })) ;keep -move registered or if end up with tons better to remove listener? not that it gets invoked unless above but...

(defn xy-pointer [model {:keys [min max] :as var-spec} dunno scale]
 (let [[x y] (map #(cmath/map-interval-clamped %1 [%2 %3] [0 scale]) @model min max)
       size 20]
  [:div.noselect.control-xy-pointer
   {:style {:pointer-events "none" ;ok this was apparently sometimes considered a rect and spammed crap....
            :width size :height size
            :left (- x (/ size 2)) :top (- y (/ size 2))}}
  [:div.control-xy-pointer-outer ]]))

(defn xy-control
 [model on-change var-spec {:keys [width height] :or {width 100 height 100} :as config}]
 (let [mouse-control (mouse-handlers on-change var-spec [:x :y])]
  (fn [] ;neither model nor on-change ought to change right?
   [:div.control-xy.control-bg
    (merge mouse-control {:style {:width width :height height}})
     [xy-pointer model var-spec [:x :y] width]])))

(defn slider [model on-change {:keys [min max] :as var-spec} direction] ;also need slider with two sides (min/max), int/segmented (ratio)
 (let [mouse-control (mouse-handlers on-change var-spec [direction])]
  (fn []
   (let [[dir cross length across] (case direction :x [:width :height "auto" 22] :y [:height :width 100 28]) ;well get from settings but yeah
         thumb-width 15
         v (cmath/map-interval-clamped @model [min max] [0 100]) ;need to calc width...
         style {:position :relative dir length #_"100%" #_100 cross across}
         common {:position :absolute :pointer-events "none"}] ;avoid junk events
    [:div.control-slider.control-bg
     (merge mouse-control {:style style})
     [:div.control-slider-fill ;behind thumb
      {:style (merge common {:left 0 :bottom 0 cross "100%" dir (str v "%")})}]
     [:div.control-slider-thumb
      {:style (merge common
                     (let [where (if (= direction :x) :left :bottom)]
                      {where (str "calc(" v "%" " - (" thumb-width "px) / 2)")}) ;adjust for thumb size
                     {cross "105%" dir thumb-width :transform (str "scale" (if (= direction :x) "Y" "X") "(" 1.18 ")")})}
      [:div.control-slider-value
      {:style (merge common {:position :absolute :left "120%"
                             :top "16%" :color "rgb(190, 210, 220, 0.55)" :transform "scale(0.75)"})}
      (cue-var-value->str @model)]] ]))))


(defn pie-knob [model radius deg-span & {:keys [start-angle scale outer-style inner-style stroke]
                                         :or {scale 100 start-angle 45}}]
 (let [full (* 2 Math/PI radius) ;2pi :r of :circle
       scale (fn [y] (* (/ deg-span 360) (* y (/ full scale))))]
  [:svg.knob {:style (merge outer-style {:transform (util/css-rot (+ 90 start-angle)) :border-radius "100%"})
              :width (* 2 radius) :height (* 2 radius)}
   [:defs [:linearGradient#gradient {:x1 "0%" :y1 "30%" :x2 "50%" :y2 "100%"}
           [:stop {:offset "0%"   :stop-color "rgb(120, 75, 65, 0.8)"}]
           [:stop {:offset "100%" :stop-color "rgb(190, 45, 40, 0.6)"}]]]
   [:circle.knob-pie {:r radius :cx radius :cy radius
                      :stroke-dasharray full
                      :stroke-dashoffset (- full (scale (at model)))
                      :stroke-width (* 2 radius)
                      :style (merge inner-style {:stroke (or stroke "url(#gradient)")})}]]))


(defn knob [model on-change {:keys [size ticks deg-used]
                             :or {size 100 ticks 0 deg-used 270}} var-spec]
 (let [start-angle (/ (- 360 deg-used) 2) ;45 at 270
       end-angle   (+ start-angle deg-used) ;315 at 270
       bounds [[(:min var-spec) (:max var-spec)] [start-angle end-angle]]
       [n->deg deg->n] (map (fn [dir] #(apply cmath/map-interval-clamped % dir)) [bounds (reverse bounds)])
       xy->deg (fn [position center]
                (let [[x y] (map - position center)  ;actual relative pos
                      offset (if (pos? x) 270 90)    ;really hardcoded? or dep on deg-used?
                      deg (-> (Math/atan (/ y x))
                              (* 180) (/ Math/PI) (+ offset))]
                 (when (< start-angle deg end-angle) deg))) ;this returns nil for the nether zone -> dont update
       start (fn [e] (.preventDefault e)
              (let [rect    (util/bounding-rect e)
                    center  [(+ (.-left rect)  (/ (.-width  rect) 2))
                             (+ (.-top  rect)  (/ (.-height rect) 2))]
                    handler
                    (fn [e] (.preventDefault e)
                     (when-let [n (some-> ; we ignore invalid positions
                                   (xy->deg (util/client-xy e) center)
                                   deg->n)]
                      (and (not= n (at model))
                           (on-change n))))]
               (handler e) ;call once manually for click... but likely to fuck up drags? need to check on mouse-up...
               (util/on-move-til-mouse-up handler)))
       scroll (fn [e] (.preventDefault e)
               (let [d (.-deltaY e)
                     slowed (* 1.5 (Math/sqrt (Math/sqrt (Math/abs d))))
                     n (-> (n->deg (at model))        ;convert back and forth to see if we should really send update event..
                           ((if (pos? d) + -) slowed) ;transformed abs offset added or subtracted
                           (cmath/clamp start-angle end-angle)
                           deg->n)]
                (and (not= n (at model))
                     (on-change n))))] ;bit silly but if not deg would need some other unified measure...
  (r/create-class
   {:component-did-mount (fn [this]
                          (util/on-event (r/dom-node this)
                                         "wheel" scroll true)) ;need to override "passive" or cant hijack even individual element
   :reagent-render
   (fn []
    (into
     [:div.control-knob.control-bg
      {:draggable true :on-drag-start start :on-mouse-down start :on-touch-start start
       :style {:width size :height size :border-radius "100%" }}]
     (let [radius (/ size 2)
           tick-width 8, center-size 15
           get-tf (fn [angle] {:transform (str "rotate(" angle "deg)")})
           common {:position :absolute :pointer-events "none"}
           more {:transform-origin "top" :left radius :top radius :height radius}]

       [[:div.control-knob-bull {:style (merge common more {:top (- radius (/ center-size 2)) :left (- radius (/ center-size 2)) :width center-size :height center-size})}]
        [pie-knob model radius deg-used :start-angle start-angle :scale (:max var-spec)]

        [pie-knob 100 radius 90 :start-angle -45 :outer-style (merge common {:left 0})
         :stroke "rgb(30, 40, 50, 0.6)"]
        [:div.control-knob-tick {:style (merge common more (get-tf (n->deg (at model)))
                                                {:left (- radius (/ tick-width 2)) :width tick-width :z-index 5})}]
        [:div.control-knob-limit.limit-left  {:style (merge common more (get-tf start-angle) )}]
        [:div.control-knob-limit.limit-right {:style (merge common more (get-tf end-angle) )}]
        [:div.control-knob-value {:style (merge common more
                                                {:text-align :right :transform "scale(0.9)"
                                                 :font-size "80%" :color "rgb(190, 210, 220, 0.55)" :left "40%" :top "72%"})}
         (cue-var-value->str (at model))]])))})))

; (db/toggle [:active :cue-4-4])
; (db/get [:active :cue-4-4])
; (db/get [:options :display :control])
; (satisfies? IDeref (clr/hsla 0.5 0.5 0.3))
(satisfies? IDeref (rf/subscribe [:cue-var-value :cues-35-13]))
(defn cue-var-control [cue-id {:as cue-var :keys [name min max]}]
 (let [;_ (println cue-id cue-var)
       value (rf/subscribe [:cue-var-value cue-id cue-var])
       ; on-change #(do (println %) (rf/dispatch [:cue-var-set cue-id (:name cue-var) %]))
       on-change #(rf/dispatch [:cue-var-set cue-id (:name cue-var) %])
       ; custom (db/get [:options :display :control (:name cue-var) :size])] ;putting this here works fine for all except KNOB. does update in db, but no re-render...
       ; _ (println (filterv some? (mapv #(get cue-var %) [:name :type])))
       custom @(rf/subscribe [:cue-var-control-options :size cue-var]) #_(:size @(rf/subscribe (into [:options [:display :control]]
                                          (mapv #(get cue-var %) [:name :type]))))
       _ (println custom)] ;putting this here works fine for all except KNOB. does update in db, but no re-render...
  ; __XXX ^^^^ fix keyword/str and itll work
 (fn [] ;can help redraw/mouse spasm?
  [:div.cue-var-control
   {:style (when custom {:width (/ custom 0.9) ;ugly hack test control+name/val - how proper dynamic?
                         :flex-flow :column})}
   [:div.cue-var-control-edit ;some button(s) to edit var display stuff? swap controller type, also maximize biggg
    {:style {:position :absolute :left 0 :top 0}}
    [:button.darkened {:style {:opacity 0.3}}
     [:i.zmdi.zmdi-window-minimize]]]
   [:div.cue-var-control-name.darkened
    {:class (when-not custom "cue-var-field")} ;force min width when row-wise, not column
    (:name cue-var)]
   [:div.cue-var-control-main ;guess we should have a standard height for a control? one line and padding basically, 25ish pixels... then can have bigger as well ofc
    {:class (when-not custom "auto-expand")}
    ; then also want maximize. could use popover, also overflow: overlay creates scroller...
    ; (condp = (:type cue-var) ; add :controller or similar - would have a default one per-type but many other options
    ; (if (satisfies? IDeref value) ;hopefully satisfies not as buggy-slow on cljs
    (if (some? value) ;oh yeah sub is always valid, me thinking dumly. check for nil...
     (case (keyword (:type cue-var)) ; add :controller or similar - would have a default one per-type but many other options
         (:integer :number :double nil)
         (case (:name cue-var)
          "Snippet" [:div "eh"] #_[code-box value]
          "Alpha"   [slider value on-change cue-var :y]
          ("Beats" "Cycles" "Chance")     [knob value on-change {} cue-var]
          [slider value on-change cue-var :x])

         :knob [knob value on-change {} cue-var]
         ; :boolean [rc/checkbox :model value :on-change on-change]
         :boolean [ui/toggle {} value on-change]

         :alpha [slider value on-change cue-var :y]
         :ratio [:p "Like make something with boxes?"]
         :color [:div {:style {:margin-top "5px !important"}}
                 [ui/slider-color-picker
                 {:color @value
                  :pointer (r/reactify-component (fn [] [:div "[]"]))
                  :onChange (fn [color e] (on-change (.-hex color)))}]]
                  ;also fix something modal/popout, have 2-3 different pickers to switch between etc
         :space (str "xyz")
         :xy [xy-control value on-change cue-var]
         :else "dunno type")
     [:div "Var model invalid"])]
   #_[:div.cue-var-control-value {:class (when-not custom "cue-var-field")} (cue-var-value->str @value)]
#_[:td.buttons []] ;want to be able to edit stuff like min/max, starting value, display-as, everything except for maybe type/id really?
   ; afterglow itself obviously needs interface for that but yeah. to be an app obviously everything needs to be in ui
   ; this would really just bring up the create-cue-var view with values already pre-filled...  except they'll always be prefilled by defaults lols
   ; ALSO oh yeah like attach param / mod... which just wraps param in something that could bring up a select (list existing show vars) / create-param (templates with lfos etc,)

   ; introduce new concept of param-modifier? an abstraction - now to mod something you create new param,
   ; have it take value from first param, and apply fn, etc. Would still actually work like that but it'd be
   ; more like "original param plus middleware" as user doesnt care meaning before going to fx cue-var param will be intercepted
   ; so could have like a straight level cue set at 50, add mod of sine-lfo of +-10
   ; result equivalent to a level sine-lfo cue :min 30 :max 50 as transforms will be reused some might be a bit crazy, so result will still have to conform to original param min/max etc
   ;
   ; basically thing is params can be lots of values, but eg a uniform float level number always >= 0.0 x 1.0 mods of same could be negative (if )
   ; a la re-frame, before doing all that work check whether input value has changed (+ somehow know whether mod is pure?) and only run wrappers then, else use cached
   ])))
(defn cue-vars-summary [{:keys [id variables] :as cue}]
 (let [values (doall (for [cue-var variables] @(rf/subscribe [:cue-var-value id cue-var])))]
  (into [:div.cue-var-control]
        (mapcat (fn [variable value]
                 [[:div.cue-var-summary
                    [:div.cue-var-name.darkened (:name variable)]
                    [:div.cue-var-value (cue-var-value->str value)]]])
                variables values))))


(defn effect-row "Render one active cue/effect"
 [{:keys [id variables] :as cue}]
 (let [color (or @(rf/subscribe [:cue-color id]) (clr/css "#333"))
       color (try (cond (< 0.55 (clr/luminance color)) (assoc color :l 0.55)
                        (> 0.3 (clr/luminance color)) (assoc color :l 0.3)
                        :else color) ;capping lum
                  (catch js/TypeError e (clr/css "#222") #_(println "Color fucked:" color)))
       ; color (try (cond (< 0.55 (clr/luminance color)) (assoc color :l 0.55)
       ;                  (> 0.3 (clr/luminance color)) (assoc color :l 0.3)
       ;                  :else color) ;capping lum
       ;            (catch js/TypeError e (clr/css "#222") #_(println "Color fucked:" color)))
       minimized (db/get [:active id :minimized])]
  ; XXX simply interacting with any cue-var control causes a shitton (specifically: all)
  ; of [:active id] subs to run.  figure out why!!  i mean guess important thing is ensure they dont diff but still.
  [:div.effect-row ;.collapsable-section
   {:class (when minimized "effect-row-minimized")
    :style {:background-color @(cmod color :adj-l -0.5 :adj-s -0.3) ;we're using both -color and -image to get gradient + animation...
            :background-image (css-str "linear-gradient"
                                       @(cmod color :adj-s -0.35 :adj-l -0.15 :adj-a -0.40)
                                       @(cmod color :adj-s -0.20 :adj-l -0.07 :adj-a -0.50))}}
   [:div.effect-info
    [:div.effect-info-line1.parent-of-wrapping
     [:div.collapse-btn.darkened
      [:i.zmdi {:class (str "zmdi-window-" (if minimized "maximize" "minimize"))
                :on-click #(db/toggle [:active id :minimized])}]]
     [:div.effect-info-name.child-that-wraps (str (:text cue))]
     [:div.hidden.ml-auto.macro-check
      {:class (if (db/get [:macro :is-selecting]) "visible")} ;:display :none doesnt occupy space. opacity 0 is third option
      [ui/toggle {} ;tho these should maybe figure as universal "select" toggles rather than just for macro?
       (rf/subscribe [:macro :checked id])
       #(rf/dispatch [:macro :checked [id %]])]
      #_[rc/checkbox ;tho these should maybe figure as universal "select" toggles rather than just for macro?
       :model (db/get [:macro :checked id])
       :on-change #(rf/dispatch [:macro :checked [id %]])]] ;better if dissocs tho... leaves garbage
     ] ;put a iniize roe button. prob also expand? eg turn xy to like 400x400. Expand box or hang over?

    (when-not minimized
     [:div.effect-info-line2
      [:div.darkened (:id cue)]])]

   [ui/flip-move
     {:class "cue-var-controls"
      :duration 200 :easing "cubic-bezier(0.56, -0.2, 0.13, 1.26)" ;; "cubic-bezier(.23, -0.09, .23, .97)"
      :staggerDelayBy 20 :staggerDurationBy 30} ;weird thing now where it paints change THEN removes then animates...
     (if minimized
      ^{:key (make-key "cue-active-summary" id)}
      ; [:div.cue-var-control [cue-vars-summary cue]] ;so this one works much better even non-wrapped than the other which fully glitches out, wonder why. still, better do it
      [cue-vars-summary cue] ;so this one works much better even non-wrapped than the other which fully glitches out, wonder why. still, better do it
      ; still, should wrap these - but complicates flexing
      ; [:div [cue-vars-vertical cue] ] ;eg alpha as per-effect "global" control, not causing extra rows...
      (for [cue-var variables] ;lack of doall here works only bc we are only passing the static bit
       ^{:key (make-key "cue-active-var" id (:name cue-var))}
       ; [:div.cue-var-control [cue-var-control id cue-var]]))] ;we can send entire cue right since the changing values are somewhere else...
       [cue-var-control id cue-var]))] ;we can send entire cue right since the changing values are somewhere else...

   [row-buttons id]])) ; where put value/lfo viz?


(defn select-all-btns []
 (let [get-btn (fn [on?]
               [rc/button
                :label (if on? "all" "none")
                :style {:height "80%" :font-size "70%"}
                :class (str "btn-sm " "btn-" (if on? "secondary" "dark"))
                :on-click #(rf/dispatch [:macro :check-all on?])])] ;; :disabled? "if all checked..."
  [:div.macro-select-all-btns
   [:label {:style {:margin-right "5px"}} "Select"]
   (get-btn true) (get-btn false)]))

(defn macro-btns []
 (let [active? (db/get [:macro :is-selecting])]
  [:div.macro-btns.flex-center
   [rc/button :label "MACRO" :class "btn-success btn-sm"
    :on-click #(db/toggle [:macro :is-selecting])
    :style {:z-index 2}
    :tooltip-position :right-below
    :tooltip (if-not active? "Macro saving mode")]

   [:div.macro-selecting-ui.flex-center.slide-in-left-hidden
     {:style {:opacity (if active? 0.8 0.0)}
      :class (when active? "slide-in-left-visible")}
     [select-all-btns]
     [rc/input-text
      :class "input-dark" :placeholder "Name this macro..."
      :model     (db/get    [:macro :save-with-name])
      :on-change (db/setter [:macro :save-with-name])]

     [:div {:style {:margin-left 5 :font-size 12}}
      (let [selected @(rf/subscribe [:macro :checked])
            ct (count selected)]
       [:span (if-not (zero? ct)
               (str ct " cue" (when-not (= 1 ct) "s") " selected...")
               "Select some cues.")])]]]))


(defn stop-cues-btn [target]
 (let [[btn-style sub getter]
       (case target
        :all ["danger" [:active] keys]
        :visible ["warning"
                  [:filter-maps [:cues-for-ids [:active]]]
                  (partial map :id)]
        :hidden ["info" [:diff-maps
                         [:cues-for-ids [:active]]
                         [:filter-maps [:cues-for-ids [:active]]] ]
                 (partial map :id)])] ;ook
  [rc/button
     :label (name target)
     :class (str "btn-sm " "btn-" btn-style)
     :on-click (fn [e] (doall (for [id (getter @(rf/subscribe sub))]
                               (rf/dispatch [:cue-interaction id "delete"]))))])) ;haha silly. also need visible to kill filter tho

(defn filter-and-control []
 [:div.cue-list-top-bar
  [ui/input-text :class "input-dark" :placeholder "filter..."
   :path [:get :filter :value] ;ahh wait this way no deref here = wont rerender further down... never gets called and given chance to prove it will diff
   :on-change (db/setter [:filter :value]) :change-on-blur? false
   :on-enter #(rf/dispatch [:filter :save %])] ;fuzzy search fx? later also eg search vars, params etc... develop into interactive replish fields as well

  (into [:div.filters-saved]
        (mapv (fn [text]
               [:li.filter-saved
                {:on-click #(rf/dispatch [:filter :set text])}
                text
                [:button {:style {:font-size "60%" :border "0px" :color "#a45" :background "rgb(0,0,0,.1)"}
                          :on-click #(rf/dispatch [:filter :delete text])} "x"]])
              (db/get [:filter :saved])))
  [:div.stop-btns.float-right.ml-auto ;ml-auto works for pushing (implied) cols right
   {:style {:opacity 0.8 :right 0 :font-size "80%"}}
   [:span {:style {:margin-right 5}} "Stop"]
   [stop-cues-btn :all] [stop-cues-btn :visible] [stop-cues-btn :hidden]]])


(defn active-effects []
 (let [cues @(rf/subscribe [:sort-maps [:filter-maps [:cues-for-ids [:active]]]])]
     ;;[cues (for [cue @(rf/subscribe [:sort-maps [:filter-maps [:cues]
     ;;                                       [:get :filter :value]]
     ;;                           [:id #dentity]])]] ;ugly-looking... but whoa flexible. and cpuld find better mechanism for stuff
  [:div.cue-list
   [filter-and-control]

   [:div.cue-list-header
    ; [:div.collapse-btn.darkened
    [:<>
     [:i.zmdi.zmdi-window-minimize {:on-click #(doall (for [id (keys (db/get [:active]))] (rf/dispatch [:set [:active id :minimized] true])))}]
     [:i.zmdi.zmdi-window-maximize {:on-click #(doall (for [id (keys (db/get [:active]))] (rf/dispatch [:unset [:active id :minimized]])))}]]
    [:div "Cues r here"]]

   [ui/flip-move
    {:class "cue-list-main"
     :duration 250 :easing "cubic-bezier(0.46, 0.11, 0.6, 1.01)"
     ; :appearAnimation "accordionVertical" :enterAnimation "fade" :leaveAnimation "accordionVertical"
     :staggerDelayBy 15 :staggerDurationBy 25}
    (for [cue cues]
       ^{:key (make-key "cue-active-row" (:id cue))}
       [:div [effect-row cue]])] ; smooth. maybe try making a proper react class for effect-row...
       ; [effect-row cue])] ;BAD LAG SLOW!! settles that! no div = jitter.

   [macro-btns]]))


;; ********************
;; NAVIGATION (put page nav stuff here as well)
;; ********************

(defn nav-btns "Draw navigation buttons"
 [target-key]
 (into [:div.nav-btns.flex-center]
       (for [y (reverse (range -1 2)), x (range -1 2)]
        (if (= 0 x y) ;middle button shows pos
         (let [[x-pos y-pos] (db/get [:view target-key])]
          [:div.nav-btn (str x-pos "  " y-pos)])
         [:div.nav-btn [rc/md-icon-button :class "nav-btn" :md-icon-name "zmdi-arrow-left"
           :on-click (fn [e] (rf/dispatch [:view-change target-key [x y]])) ;heh why not thie in the ctual button?
           :style {:transform (str "rotate(" (+ (* 45 x) (* 90 y)
                                                (if (and (= -1 x) (>= 0 y)) 45))
                                   "deg)")}]])))) ;haha close enough almost


(defn sync-sources []
 (let [sources (into [] (db/get :sync :sources))]
  [:div.sync-sources.flex-center
   [:div {:style {:text-align :right :flex "1 4 auto"}} ;XXX fixes iphone overflow bs for unknown reasons. solve better!
    "Tempo sync"]
   (if (seq sources)
    [:div ;build something ourselves? remember :visibility :collapsed thing to set width according to items inside...
     [rc/single-dropdown :class "sync-sources input-dark"
      :choices sources
      :model      (db/get [:sync :selected-id])
      :on-change #(db/set [:sync :selected-id] %)
      :placeholder "Not syncing"]]
    [:div "No sources"])]))

;; ********************
;; CUE CELL / ROW / PAGE VIEWS
;; ********************
(defn cue-cell [{:keys [text position id] :as cue}] ;wait dont need to pass pos empty cues have it duh
 (let [drag-target-class (when (= position (db/get [:cue-drag :over])) "cue-cell-droppable")
       on-click (fn [e] (rf/dispatch [:cue-interaction id "click" position])) ;could just send cue object, contains pos (always) and id (if not empty)
       on-drag (ui/drag-handlers [:cue-drag] cue)]
  ; (fn [{:keys [text position id] :as cue}] ;try agai, handlers outside.... ;form2 here to avoid regenerating on-click event handler...
   [:div.cue-cell.noselect
     (merge on-drag #_(ui/drag-handlers [:cue-drag] cue) ;not sure how reasonable this is... could we go low-level react and only regen this after (since so rare) id changes?
     {:style {:min-width (str (/ 100 (db/get [:view :size])) "%")}
      :on-click on-click
      :class  (str drag-target-class)}) ;"handle right click, shift click, anything else?")} ;; :on-mouse-over mouse-over :on-mouse-out mouse-out mouse-down / mouse-move / mouse-up, touch-start / touch-move / touch-end, for click-and-hold a la push?
     (if text ; scroll on cue button to adjust main param? haha. device-orientation / device-motion also for cue-controls?
      [:div.cue-cell-within ;.cue-cell-flat ;also needs applying to outer cue-cell...
       {:class (when (db/get [:active id]) "cue-cell-running")
        :style ;look more at timers and make some animations with opacity...
        (let [color @(rf/subscribe [:cue-color id true])];; :border "v cool animation when toggling grid border off then back on in chrome. try to do that on load..."
         {:color @(deref (rf/subscribe [:text-color color]))
          :background-color @color
          :background-image @(rf/subscribe [:bg-gradient color])})}
       [:div.cue-cell-text (:text cue)]]
      [:div.cue-cell-empty
       {:class (when (and (db/get [:macro :is-selecting]) ;shouldnt make check here but yeah nice!
                          (seq (db/get [:macro :checked])))
                "cue-cell-empty-saveable")}])]))
 ; )


(defn cue-row [y cues] ;doesn't need to know location of self, just draws...
 ; [:div.cue-row
   [ui/flip-move
    {:class "cue-row"
     :duration 250 :easing "cubic-bezier(0.46, 0.11, 0.6, 1.01)"
     :staggerDelayBy 15 :staggerDurationBy 25}
  (doall (map-indexed
   (fn [x cue ]
    ^{:key (make-key "cue-cell" x y #_(:id cue))} ;figure out best approach
    [cue-cell cue]) ;could/should the ID crap be in cue-cell instead tho?
    ; (let [[_ pos] (string/split (:id cue) #"-" 2)]
    ;  ^{:key (make-key "cue-cell" x y #_(:id cue))} ;figure out best approach
    ;  [cue-cell (merge cue {:position pos})])) ;could/should the ID crap be in cue-cell instead tho?
   cues))
  ; ]
   ])

(defn cue-page "Show a fixed number of cues: a page"
 [{:keys [page offset size] :as view}] ;where is page located on larger grid? what's zoom level?
 (let [[x y] (map #(+ %1 (* size %2)) offset page)
       [end-x end-y] (map #(+ size %) [x y])
       ; rows (for [y (range y end-y) x (range x end-x)] ; y/x since we do by row
       ;             (or @(rf/subscribe [:get-cue :position [x y]])
       ;                 {:position [x y]}))]
       rows (->> (for [y (range y end-y) x (range x end-x)] ; y/x since we do by row
                   (or @(rf/subscribe [:get-cue :position [x y]])
                       {:position [x y]})) ;empty cue... remember to add page n stuff later for proper
                 (partition size) reverse)]
  [:div.cue-page
   [ui/flip-move
    {:class "cue-page"
     :duration 250 :easing "cubic-bezier(0.46, 0.11, 0.6, 1.01)"
     :staggerDelayBy 15 :staggerDurationBy 25}
    (map-indexed (fn [y row]
                  ^{:key (make-key "cue-row" (- size y 1))}
                  [cue-row (- size y 1) row]) rows)]]))
  ; (println rows)
  ; (println (count rows))
  ; [:div.cue-page
  ;  (map-indexed (fn [i cue]
  ;                ^{:key (make-key (:id cue))}
  ;                [cue-cell cue])
  ;               rows)]))
; (def cues (count (db/get [:cues-m])))
; (def view (db/get [:view])
; (rf/subscribe [:get-cue :position [0 0]])

; so obviously now when view changes we recreate all the rows/cells
; if we kept them we could animate them and stuff when zooming or moving by offset.
; question is how that'd work... honestly quite useless but would be cool.

;; (defn cue-grid "Free-scrolling cue view (post all the fuckers in there and make it a scrollable container)" []

(defn graphql-view []
 (let [subs (:subscriptions (:re-graph.internals/default (db/get [:re-graph])))
       sin (rf/subscribe [:get :ql :data :sins])]
  [:div [:div
   [:div>button.zmdi.zmdi-plus
    {:on-click #(events/ql-subscribe "{ sins }" {} :sinner)
     :disabled (when (seq subs) true)}]
   [:div>button.zmdi.zmdi-minus
    {:on-click #(events/ql-unsubscribe :sinner)
     :disabled (when-not (seq subs) true)}] ]
  [:div.flex-center {:style {:justify-content :space-around}}
   [:div.slow-transition-child.
    [slider sin (fn [& _]) {:min -1 :max 1} :y]] ]
  [ui/formatted-data "re-graph" subs]
   [:div (str @sin)]
   [:div (str subs)]
  [ui/toggle {}
   (rf/subscribe [:macro :is-selecting])
   #(db/toggle [:macro :is-selecting])] ]))


(defn header-bar []
 [:div.header-bar
   #_[:span (pr-str (db/get [:device-orientation]))]
   [ui/formatted-data "Session" [:session]]
   [ui/formatted-data "Active" [:active]]
    #_(into [:div.map-pprint]
          (for [cue @(rf/subscribe [:sort-maps [:filter-maps [:cues-for-ids [:active]]]])]
      [:pre [:code.map (pprint/write cue :stream nil)]])) ])
  ;; [rc/popover-tooltip] [rc/popover-border]

(defn nav-bar []
 [:div.nav-bar
  [:nav.naver.flex-center
  [:a "home"] [:a "cfg"] [:a "repl"] [:a "ide"] [:a "log"] [:a "visualize"] [:a "patch"]
  [:div]
  [:div {:on-click #(db/toggle [:options :display :flat])} "flat"]
  [:div.show-control.flex-center {:style {:margin-left :auto}}
   (let [[label class-suffix] (if (db/get [:show :started])
                               ["started" "success"] ["stopped" "danger"])]
    [:button
     {:style {:font-size "80%" :opacity 0.6 }
      ; :class (str "btn-sm btn-" class-suffix)
      :on-click #(db/toggle [:show :started])}
     label])]]
   [:div.nav-bar-fill]])

(defn ui []
 [:div [:title "CUE-db"]
   [nav-bar]
   [:div.nav-and-utils
    [nav-btns :page] [nav-btns :offset] [sync-sources]]
   [cue-page (db/get [:view])]

   [active-effects] ;; [beat-viz] ;[metronome-controls]
   [graphql-view]
   [header-bar]
   [ui/svg-test] ])


