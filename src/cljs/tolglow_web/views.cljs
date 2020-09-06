(ns tolglow-web.views
 (:require [re-frame.core :as rf]
           [reagent.core :as r]
           [re-com.core :as rc]
           [re-graph.core :as rg]
           [clojure.string :as string]
           [clojure.edn :as edn]
           [goog.string :as gstring]
           [taoensso.timbre :as timbre]
           [thi.ng.math.core :as cmath]
           [thi.ng.color.core :as clr]
           [cljs-time.core :as ct]
           [cljs-time.format :refer [formatters formatter unparse]]
           ; [klipse.run.plugin.plugin] ;; this namespace initializes Klipse. We require it for its side effects
           ; [klipse.plugin :as klipse-plugin]
           ; [tolglow-web.macros :refer [->some->]]
           [tolglow-web.db :as db]
           [tolglow-web.views.ui-component :as ui]
           [tolglow-web.views.variable-control :as vc]
           [tolglow-web.util :as util :refer [at css-str css-arg cmod make-key cs <sub]]
           [tolglow-web.events :as events]
           [tolglow-web.three :as tol-three]
           #_[tolglow-web.quil :as quil]))

; iphone no-worky:
; - slider:
;     touch causes ##NaN
; - cue-cell:
;     drag is too sensitive
;     font size/wrap
; - setting a knob, then flipping a toggle, causing some kind of ghost touch?
;   sometimes on multiple knobs. only knobs.
; - on touch there's a weird sorta re-render in effects list?
;   more box-shadow appears and quickly disappears..

(defn unravel-object
 [[component state]]
 (println ((js->clj component) state))
 (def obj obj))

(defn safe [component]
 (let [is-safe (r/atom true)] ;or reg not ratom or would cause etra re-render?
  (r/create-class
  {:display-name "Error boundary"
   ;:get-derived-state-from-error ;"defined as instance method and will be ignored. define as static"
   ;(fn [error] ;this should update state to serve like, an error page (for render)
   ; ;"if using getDerivedState methods, the state has to be plain JS object as React implementation uses Object.assign to merge partial state into the current state."
   ; )
   :component-did-catch ;this can log
   (fn [this error info]
    (reset! is-safe false))
   :reagent-render
   (fn [args]
    (if @is-safe   ;state change downstream? then it gets easier to debug "in-page",
     ; component    ;what causes broken state and what restores valid...
     args
     (do (unravel-object args)
         [:div "Click to attempt reload"
          [:button {:on-click #(reset! is-safe true)}]])))})))


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

(defn scale-value "Scale between cue-var range and control range"
 [var-spec])

(defn cue-var-value->str "Format cue var value human readable"
 [v]
 (let [f #(cond
           (float? %) (->> % (gstring/format "%.2f") edn/read-string str) ;str drops trailing zeroes so good enough!
           :else      (pr-str %))]
      (if (coll? v) (string/join " " (map f v)) (f v))))
; (map pr (clr/css "#222"))
; (map pr (clr/hsla 0.5 0.3 0.6 0.8))
; (pr (clr/hsla 0.5 0.3 0.6 0.8))
; (deref (clr/hsla 0.5 0.3 0.6 0.8))
; (map pr {:a 3})

(defn xy-in-rect [e dimension rect]
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



; should btw use input range and for Y simply rotate 90 deg...
; so no need custom mouse handlers etc. only for xy and min-max double sliders.
; although making it work on all browsers seems a bit impossible so eh maybe keep this
(defn slider [model on-change {:keys [min max] :as var-spec} direction] ;also need slider with two sides (min/max), int/segmented (ratio)
 (let [mouse-control (mouse-handlers on-change var-spec [direction])]
  (fn []
   (let [[dir cross length across] (case direction
                                  :x [:width :height, "auto" "1.5rem"]
                                  :y [:height :width, 80     "1.8rem"]) ;well get from settings but yeah
      thumb-width 15
      integer (= "integer" (:type var-spec))
      v (cmath/map-interval-clamped @model [min max] [0 100]) ;need to calc width...
      style  {:position :relative, dir length, cross across}
      common {:position :absolute :pointer-events "none"}
      int-ticks (when (and integer (>= 16 (- max min)))
                 (for [i (range min max)
                       :let [each (/ 100 (- (inc max) min))]]
                  [:div.control-slider-tick
                   {:style (merge common
                                  {:left (str (* each i) "%")
                                   :width (when (zero? (mod i 4)) 3)})}]))
      v-str [:div.control-slider-value
              (let [[anchor-1 anchor-2 anchor-3]
                    (case direction
                     :x [:left :right :top]
                     ; :y [:bottom :top :left])]
                     :y [:fart :fart :fart])]
               {:style (merge common
                              {:text-align :center :font-size "0.60rem"
                               :color "rgb(190, 210, 220, 0.55)"
                               :position (when (= direction :x) :absolute) ;relative doesnt leave space for ourselves...
                               (if (< v 80) anchor-1 anchor-2) "150%"
                               anchor-3 "16%"})}) ;transform prob fucks justification etc...
              (cue-var-value->str @model)]
      thumb [:div.control-slider-thumb
             {:style (merge common
                            (let [where (if (= direction :x) :left :bottom)]
                             {where (if integer
                                     (str (- v (/ v (- max min))) "%")
                                     (str "calc(" v "%" " - (" thumb-width "px) / 2)"))}) ;adjust for thumb size
                            {cross "105%" dir thumb-width
                             :transform (str "scale" (if (= direction :x) "Y" "X") "(" 1.18 ")")})}
             v-str]] ;avoid junk events
  [:div.control-slider.control-bg
   (merge mouse-control {:style style})
   int-ticks
   [:div.control-slider-fill
    {:style (merge common {:left 0 :bottom 0 cross "100%" dir (str v "%")})}]
   thumb]))))

(defn ranged-slider [model on-change {:keys [min max] :as var-spec}]
 (let []
  ))


(defn draw-knob-svg [model radius deg-span &
                     {:keys [start-angle scale start-scale outer-style inner-style stroke]
                      :or   {scale 100 start-angle 45}}]
 (let [full (* 2 Math/PI radius) ;2pi :r of :circle
       scaler (fn [n] (* (/ deg-span 360)
                         (/ full scale)
                         (- n start-scale)))]
  [:svg.knob {:style (merge outer-style {:transform (util/css-rot (+ start-angle (- 360 deg-span )))
                                         :border-radius "100%"})
              :width (* 2 radius) :height (* 2 radius)}
   [:defs [:linearGradient#gradient {:x1 "0%" :y1 "30%" :x2 "50%" :y2 "100%"}
           [:stop {:offset "0%"   :stop-color "rgb(120, 75, 65, 0.8)"}]
           [:stop {:offset "100%" :stop-color "rgb(190, 45, 40, 0.6)"}]]]
   [:circle.knob-pie {:r radius :cx radius :cy radius
                      :stroke-dasharray full
                      :stroke-dashoffset (- full (scaler (at model)))
                      :stroke-width (* 2 radius)
                      :style (merge inner-style {:stroke (or stroke "url(#gradient)")})}]]))


(defn knob [model on-change {:keys [size ticks deg-used]
                             :or {size 100 ticks 0 deg-used 270}} var-spec]
 (let [start-angle (/ (- 360 deg-used) 2) ;45 at 270
       end-angle   (+ start-angle deg-used) ;315 at 270
       bounds [[(:min var-spec) (:max var-spec)]
               [start-angle end-angle]]
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
               (util/on-move-then-cleanup handler)))
       scroll (fn [e] (.preventDefault e) ;will need a way to handle int knobs. will mean having an internal model...
               (let [d (.-deltaY e)
                     slowed (* 1.5 (Math/sqrt (Math/sqrt (Math/abs d))))
                     n (-> (n->deg (at model))        ;convert back and forth to see if we should really send update event..
                           ((if (pos? d) + -) slowed) ;transformed abs offset added or subtracted
                           (cmath/clamp start-angle end-angle)
                           deg->n)]
                (and (not= n (at model))
                     (on-change n))))] ;bit silly but if not deg would need some other unified measure...
  (r/create-class
   {:display-name "Knob"
    :component-did-mount (fn [this]
                          (util/on-event (r/dom-node this)
                                         "wheel" scroll true)) ;need to override "passive" or cant hijack even individual element
    :reagent-render
    (fn []
     (into
      [:div.control-knob.control-bg
       {;:draggable true :on-drag-start start
        :on-mouse-down start :on-touch-start start
        :style {:width size :height size :border-radius "100%"}}]
      (let [radius (/ size 2)
            tick-width (+ 3 (/ radius 10)), center-size (+ 4 (/ radius 3))
            get-tf (fn [angle] {:transform (str "rotate(" angle "deg)")})
            common {:position :absolute :pointer-events "none"}
            more   {:transform-origin "top" :left radius :top radius :height radius}]

       [[:div.control-knob-bull
         {:style (merge common more {:top  (- radius (/ center-size 2))
                                     :left (- radius (/ center-size 2))
                                     :width center-size :height center-size})}]
        [draw-knob-svg model radius deg-used :start-angle start-angle
         :scale (- (:max var-spec) (:min var-spec)) :start-scale (:min var-spec)]
        [draw-knob-svg 100 radius (- 360 deg-used) :start-angle (+ 90 start-angle) ;this math makes NO sense. cleanup needed
         :outer-style (merge common {:left 0}) :stroke "rgb(30, 40, 50, 0.6)"] ;inactive/bottom area

        [:div.control-knob-tick
         {:style (merge common more (get-tf (n->deg (at model)))
                        {:left (- radius (/ tick-width 2))
                         :width tick-width :z-index 5})}]
        [:div.control-knob-limit.limit-left
         {:style (merge common more (get-tf start-angle))}]
        [:div.control-knob-limit.limit-right
         {:style (merge common more (get-tf end-angle))}]
        [:div.control-knob-value
         {:style (merge common more
                        {:text-align :center #_:right :transform "scale(0.9)"
                         :font-size "80%" :color "rgb(190, 210, 220, 0.55)"
                         :left "40%" :top "72%"})}
         (cue-var-value->str (at model))]])))})))


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

(defn cue-var-control [cue-id {:as cue-var :keys [name min max]}]
 (let [value      (rf/subscribe [:cue-var-value cue-id cue-var])
       on-change #(rf/dispatch  [:cue-var-set   cue-id cue-var %])
       custom    @(rf/subscribe [:cue-var-control-options :size cue-var])] ;putting this here works fine for all except KNOB. does update in db, but no re-render...
 (fn [] ;can help redraw/mouse spasm?
  [:div.cue-var-control
   {:style (when custom {:width (/ custom 0.9) ;ugly hack test control+name/val - how proper dynamic?
                         #_:flex-flow #_:column})}
   [:div.cue-var-control-edit ;some button(s) to edit var display stuff? swap controller type, also maximize biggg
    {:style {:position :absolute :left "0.4rem" :top "0.2rem"}}
    [ui/minimize [:active cue-id :cue-var (:name cue-var)]]]
   [:div.cue-var-control-name.darkened
    {:class (when-not custom "cue-var-field")} ;force min width when row-wise, not column
    (:name cue-var)]
   [:div.cue-var-control-main ;guess we should have a standard height for a control? one line and padding basically, 25ish pixels... then can have bigger as well ofc
    {:class (when-not custom "auto-expand")}
    ; then also want maximize. could use popover, also overflow: overlay creates scroller...
    ; (condp = (:type cue-var) ; add :controller or similar - would have a default one per-type but many other options
    (if (some? @value) ;oh yeah sub is always valid, me thinking dumly. check for nil...
     (case (keyword (:type cue-var)) ; add :controller or similar - would have a default one per-type but many other options
         (:integer :number :double nil)
         (case (:name cue-var)
          "Snippet" [:div "eh"] #_[code-box value]
          ; "Alpha"   [slider value on-change cue-var :y]
          "Alpha"   [knob value on-change {:size custom} cue-var]
          ("Beats" "Cycles" "Chance")     [knob value on-change {:size 75} cue-var]
          [slider value on-change cue-var :x])

         :knob [knob value on-change {:size 50} cue-var]
         :boolean [ui/toggle {} value on-change]

         :alpha [slider value on-change cue-var :y]
         :ratio [:p "Like make something with boxes?"]
         :color [color-picker value on-change]

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
  ; [] ;testing error boundaries...
  ; (into [:div.cue-var-control]
  (into [:<>]
        (mapcat (fn [variable value]
                 [[:div.cue-var-summary
                    [:div.cue-var-name.darkened (:name variable)]
                    [:div.cue-var-value (cue-var-value->str value)]]])
                variables values))))

;flip-move makes undock weird, so will have to sep things...
(defn effect-row "Render one active cue/effect"
 [{:keys [id variables] :as cue}]
 (let [color (or @(rf/subscribe [:cue-color id]) (clr/css "#333"))
       color (try (cond (< 0.55 (clr/luminance color)) (assoc color :l 0.55)
                        (> 0.3  (clr/luminance color)) (assoc color :l 0.3)
                        :else color) ;capping lum
                  (catch js/TypeError e (clr/css "#222") #_(println "Color fucked:" color)))
       {:keys [minimized undocked]} (db/get [:active id])]
  ; XXX simply interacting with any cue-var control causes a shitton (specifically: all)
  ; of [:active id] subs to run.  figure out why!!  i mean guess important thing is ensure they dont diff but still.
  [:div.effect-row ;.collapsable-section
   {:class (string/join " " [(when minimized "effect-row-minimized")
                             (when undocked  "effect-row-undocked")])
    :style (merge {:background-color @(cmod color :adj-l -0.5 :adj-s -0.3) ;we're using both -color and -image to get gradient + animation...
                   :background-image (css-str "linear-gradient"
                                              @(cmod color :adj-s -0.35 :adj-l -0.15 :adj-a -0.40)
                                              @(cmod color :adj-s -0.20 :adj-l -0.07 :adj-a -0.50))}
                  (when undocked {:bottom (:y undocked 0)
                                  :left   (:x undocked 0)}))}
   [:div.effect-info
    [:div.effect-info-line1.parent-of-wrapping
     [:div.darkened
      [ui/minimize [:active id]]
      [ui/material-toggle [:active id :undocked] ["fullscreen-exit" "collection-item"]]] ;minimize should be generalized tho...and used with toggle
     [:div.effect-info-name.child-that-wraps
      (str (:text cue))]
     ; [:div.hidden.ml-auto.macro-check
     ;  {:class (if (db/get [:macro :is-selecting]) "visible")} ;:display :none doesnt occupy space. opacity 0 is third option
     [:div.removed.macro-check
      {:class (if (db/get [:macro :is-selecting]) "restored")} ;:display :none doesnt occupy space. opacity 0 is third option
      [ui/toggle [:macro :checked id]]
      #_[ui/toggle {:style {:height "1rem"}} ;tho these should maybe figure as universal "select" toggles rather than just for macro?
       (rf/subscribe [:get :macro :checked id])
       #(rf/dispatch [:macro :checked [id %]])]] ] ;put a iniize roe button. prob also expand? eg turn xy to like 400x400. Expand box or hang over?

    (when-not minimized
     [:div.effect-info-line2.flex-center
      #_[ui/material-toggle [:active id :undocked] ["fullscreen-exit" "collection-item"]] ; undock floating div!
      [:div.darkened (str (:position cue)) #_(:id cue)]])]

   [ui/flip-move
     {:class "cue-var-controls"
      :duration 200 :easing "cubic-bezier(0.56, -0.2, 0.13, 1.26)" ;; "cubic-bezier(.23, -0.09, .23, .97)"
      :staggerDelayBy 20 :staggerDurationBy 30} ;weird thing now where it paints change THEN removes then animates...
     (if minimized
      ^{:key (make-key "cue-active-summary" id)}
      [:div.cue-var-control [cue-vars-summary cue]] ;so this one works much better even non-wrapped than the other which fully glitches out, wonder why. still, better do it
      ; [cue-vars-summary cue] ;so this one works much better even non-wrapped than the other which fully glitches out, wonder why. still, better do it
      ; still, should wrap these - but complicates flexing
      ; [:div [cue-vars-vertical cue] ] ;eg alpha as per-effect "global" control, not causing extra rows...
      (for [cue-var variables] ;lack of doall here works only bc we are only passing the static bit
       ^{:key (make-key "cue-active-var" id (:name cue-var))}
       ; [:div.cue-var-control [cue-var-control id cue-var]]))] ;we can send entire cue right since the changing values are somewhere else...
       [cue-var-control id cue-var]))] ;we can send entire cue right since the changing values are somewhere else...

   (if-not undocked ;second option should be some floating buttons that appear when yada...
    [row-buttons id])]))


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
 ;;[cues (for [cue @(rf/subscribe [:sort-maps [:filter-maps [:cues]
 ;;                                       [:get :filter :value]]
 ;;                           [:id #dentity]])]] ;ugly-looking... but whoa flexible. and cpuld find better mechanism for stuff
 (let [cues   @(rf/subscribe [:sort-maps [:filter-maps [:cues-for-ids [:active]]]]) ;TODO implement :-> for this would be cool? la
       active @(rf/subscribe [:active])
       [undocked in-place] ((juxt filter remove) #(get-in active [(:id %) :undocked]) cues)
       f (fn [cues]
          (for [cue cues]
           ^{:key (make-key "cue-active-row" (:id cue))}
            [:div [safe [effect-row cue]]]))] ; smooth. maybe try making a proper react class for effect-row...
          ; [effect-row cue])] ;BAD LAG SLOW!! settles that! no div = jitter. but dont get why, as reagent implements all components as classes?
  [:div.cue-list
   [filter-and-control]

   [:div.cue-list-header
    [:div.collapse-btn.darkened
     ; [:<>
     [:i.zmdi.zmdi-window-minimize {:on-click #(doall (for [id (keys (db/get [:active]))] (rf/dispatch [:set [:active id :minimized] true])))}]
     [:i.zmdi.zmdi-window-maximize {:on-click #(doall (for [id (keys (db/get [:active]))] (rf/dispatch [:unset [:active id :minimized]])))}]]
    [:div "Cues r here"]]

   [ui/flip-move
    {:class "cue-list-main"
     :duration 250 :easing "cubic-bezier(0.46, 0.11, 0.6, 0.95)"
     :appearAnimation "fade" :enterAnimation "accordionVertical" :leaveAnimation "accordionVertical"
     :staggerDelayBy 15 :staggerDurationBy 25}
    (f in-place)]

   (f undocked) ]))


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
;; XXX amount of db/get subs here is CRAZY. figure something out...
(defn cue-cell [{:keys [text position id] :as cue}] ;wait dont need to pass pos empty cues have it duh
 (let [drag-target-class (when (= position (db/get [:cue-drag :over]))
                           "droppable" #_"cue-cell-droppable")
       on-click (fn [e] (rf/dispatch [:cue-interaction id "click" position])) ;could just send cue object, contains pos (always) and id (if not empty)
       on-drag (ui/drag-handlers [:cue-drag] cue)]
  ; (fn [{:keys [text position id] :as cue}] ;try agai, handlers outside.... ;form2 here to avoid regenerating on-click event handler...
   ; (r/create-class {
     #_:component-will-unmount
     #_(fn [this]

      (let [node (r/dom-node this)]
       (println (r/props this)) ;entire cue data obj. why hmm?
       #_(r/set-state this {:style {:opacity 0}})))
       ; (r/merge-props {:style {:opacity 0}})))
     ; :reagent-render (fn []
      [:div.cue-cell.noselect
       (merge on-drag #_(ui/drag-handlers [:cue-drag] cue) ;not sure how reasonable this is... could we go low-level react and only regen this after (since so rare) id changes?
              {:style {"--cue-cell-height" (when (db/get [:display :cue-height]) "30px") ;want to depend on option. not at all necessary here (only if doing calcs in css) but anyways
                       :min-width (str (/ 100 (db/get [:view :size])) "%")}
               :on-click on-click
               :class  drag-target-class}) ;stopped working hmm
       (if text ; scroll on cue button to adjust main param? haha. device-orientation / device-motion also for cue-controls?
        [:div.cue-cell-within ;.cue-cell-flat ;also needs applying to outer cue-cell...
         {:class (when (db/get [:active id]) "cue-cell-running")
          :style ;look more at timers and make some animations with opacity...
          (let [color @(rf/subscribe [:cue-color id true])];; :border "v cool animation when toggling grid border off then back on in chrome. try to do that on load..."
           {:color @(deref (rf/subscribe [:text-color color])) ;prob also bad...
            :background-color @color
            ; :background-image @(rf/subscribe [:bg-gradient color])})} ;this we should prob do in css and just send a var?
            ; ;seemed implicated in many ms ^ also non-db sub is nono. yeah? unless nil trick works?
            ; but at least above ought to cache? this'd rerun each render regardless of what changed or? hmm
            :background-image (css-str "linear-gradient" "-30deg"
                                       (css-arg @(cmod color :adj-a -0.7  :adj-s -0.25 :adj-l -0.20) "10%")
                                       (css-arg @(cmod color :adj-a -0.45 :adj-s 0.075 :adj-l -0.20) "90%"))})} ;this we should prob do in css and just send a var?
         [:div.cue-cell-text (:text cue)]]
        [:div.cue-cell-empty
         {:class (when (and (db/get [:macro :is-selecting]) ;shouldnt make check here but yeah nice!
                            (seq (db/get [:macro :checked])))
         ; {:class (when-let [{:as macro :keys [is-selecting checked]} (db/get [:macro])]
         ;          (seq (db/get [:macro :checked]))
                  "cue-cell-empty-saveable")}])]
      ; )
     ; })
   ))
; )
 ; )


(defn cue-row [y cues] ;doesn't need to know location of self, just draws...
 ; [:div.cue-row
   [ui/flip-move
    {:class "cue-row"
     :duration 40 :easing "cubic-bezier(0.46, 0.11, 0.6, 1.01)"
     :staggerDelayBy 0 :staggerDurationBy 700}
  (doall (map-indexed
   (fn [x cue ]
    ^{:key (make-key "cue-cell" x y #_(:id cue))} ;figure out best approach
    [cue-cell cue]) ;could/should the ID crap be in cue-cell instead tho?
   cues))
  ; ]
   ])

(defn cue-page "Show a fixed number of cues: a page"
 [{:keys [page offset size] :as view}] ;where is page located on larger grid? what's zoom level?
 (let [[x y] (map #(+ %1 (* size %2)) offset page)
       ; rows (for [y (range y end-y) x (range x end-x)] ; y/x since we do by row
       ;             (or @(rf/subscribe [:get-cue :position [x y]])
       ;                 {:position [x y]}))]
       rows (->> (for [y (range y (+ y size))
                       x (range x (+ x size))] ; y/x since we do by row
                   (or @(rf/subscribe [:get-cue :position [x y]])
                       {:position [x y]})) ;empty cue... remember to add page n stuff later for proper
                 (partition size)
                 reverse)] ;change all this to fetch entire rows or some shit...
  [:div.cue-grid
   {:style {:position (when (db/get [:display :cue-height]) :sticky)} }

   [:div.cue-grid-gizmos
    {:style {:height 20}}
    [ui/toggle [:display :cue-height]]] ;should be a 3-way radio... auto, squareish (default) or rect (low height) cue cells
   [ui/flip-move
    {:class "cue-page"
     :duration 20 :easing "cubic-bezier(0.46, 0.11, 0.6, 1.01)"
     :staggerDelayBy 3 :staggerDurationBy 5}
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
  [:div
   [ui/toggle [:show-graphql]]
   [:div
    [:div>button.zmdi.zmdi-plus
     {:on-click #(events/ql-subscribe "{ sins }" {} :sinner)
      :disabled (when (seq subs) true)}]
    [:div>button.zmdi.zmdi-minus
     {:on-click #(events/ql-unsubscribe :sinner)
      :disabled (when-not (seq subs) true)}] ]
   [:div.flex-center {:style {:justify-content :space-around}}
    [:div.slow-transition-child
     [slider sin (fn [& _]) {:min -1 :max 1} :y]]]
   [ui/formatted-data "re-graph" (db/get [:re-graph])]
   [:div (str @sin)]
   [:div (str subs)]
   [ui/toggle [:macro :is-selecting]] ]))


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
   [:a "home"] [:a "cfg"] [:a "repl"] [:a "ide"] [:a "visualize"] [:a "patch"]
   ; [:div "/ "]
   ; [:div {:on-click #(db/toggle [:options :display :flat])} "flat"]
   [:div.show-control.flex-center {:style {:margin-left :auto}}
    (let [started? (rf/subscribe [:get :show :started])
          [label class-suffix] (if @started?
                                ["started" "success"] ["stopped" "danger"])]
     [:button
      {:style {:font-size "80%" :opacity 0.6 }
       ; :class (str "btn-sm btn-" class-suffix)
       ; :on-click #(db/toggle [:show :started])}
       :on-click #(rf/dispatch [:code @(rf/subscribe [:get :commands (if @started? :stop :start) :cmd]) :running true])}
      label])]]
   [:div.nav-bar-fill]])
; (rf/dispatch [:code @(rf/subscribe [:get :commands (if @started? :stop :start) :cmd])])

(defn log "Show an expandable log thingy" []
 (let [options (rf/subscribe [:get :options :display :log])
       diag    (rf/subscribe [:get :diagnostics])
       time-format (formatters :hour-minute-second)
       ; time-format (formatters "HH-mm-ss.SSS")
       table-ref (atom nil) ; scroll (r/atom nil)
       line (fn [{:keys [time level title message] :as msg}]
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
      [ui/minimize [:options :display :log]] ;this also needs to send an event to scroll-top the fucker...
      [:div.log-inner {:ref (fn [el] (reset! table-ref el))
                       :style {:max-height (if (:minimized @options) "1.2rem" "20em")}
                       ; :scroll-top @scroll ;wonder why this doesnt work
                       #_:style #_{:max-height @scroll}}
       [:table>tbody.log
        (for [msg (map (:messages @diag) (sort (keys (:messages @diag))) #_(if (:minimized @options)
                                          [(count (:messages @diag))]
                                          (sort (keys (:messages @diag)))))]
         ^{:key (str (:id msg))}
         [line msg])]]])})))

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


(defn quil [id]
 (let [show (rf/subscribe [:get :quil :show])]
  (r/create-class
   {:display-name (str "Quil " id)
    :component-did-mount (fn [this] ;we must wait until component mounts before invoking defsketch
                          (rf/dispatch [:quil :init]) ;fix id here too...
                          (when-not show
                           (rf/dispatch [:quil :stop])))
    :reagent-render
    (fn []
     [:div.quil-section
      [ui/toggle {} show ;this is double-triggering event. investigate.
       #(rf/dispatch [:quil %])] ;this both hides AND pauses sketch - otherwise it's always using resources.
      [:div.quil-container
       {:style {:overflow-x "scroll"}}
       [:div
        {:id id
         :style {:visibility (if @show :visible :hidden)
                 :max-height (when-not @show "2rem")}}]]])})))



(defn ui []
 [:<>
  [:div#page {:class (when (db/get [:modal]) "modal-is-open")}
   [:title "CUE-db"]

   [nav-bar]

   [:div.nav-and-utils
    [nav-btns :page] [nav-btns :offset] [sync-sources]]

   [:pre (pr-str @(rf/subscribe [:get :cue-drag :over]))]
   [cue-page (db/get [:view])]

   [active-effects] ;; [beat-viz] ;[metronome-controls]
   [macro-btns]

   [hud] ;ethereal...

   ; [quil "quil"]
   ; [tol-three/three-comp 600 (/ 4 3) @(rf/subscribe [:get :fixtures])]
   (if @(rf/subscribe [:get :fixtures]) ;gets p spammy
    (do ;(println "Now load")
     ; (rf/dispatch [::rg/query "{ shaders { file_name text kind }}" {} [:save-shaders]]) ;eh not sure where to put. also gotta start sub...
     [safe [tol-three/three-comp 600 (/ 4 3) #_(deref (rf/subscribe [:get :fixtures]))]])
    (do ;(println "Waiting for re-graph")
     [:div "Loading visualizer..."]))

   [log]
   [ui/svg-test]
   [header-bar]

   [graphql-view]]
  [hud-modal]])
; (tol-three/run)
#_(quil/init {:venue :fix-a-sub-yo
               :get-current-values (fn [] @(rf/subscribe [:get :fixture-state]))
               :get-fixtures (fn [] @(rf/subscribe [:get :fixtures]))})
; remember reagent does have the below...
; (defn my-div []
;   (let [this (r/current-component)]
;     (into [:div.custom (r/props this)]
;           (r/children this))))

