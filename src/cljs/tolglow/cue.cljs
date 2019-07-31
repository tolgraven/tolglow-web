(ns tolglow.cue
  (:require [reagent.core :as r]
            [re-com.core :as rc :refer-macros [handler-fn]]
            [re-frame.core :as rf]
            [tolglow.db :as db]
            [tolglow.prefab :as prefab]))

(defn cs [& names]
  (clojure.string/join " " (filter identity names)))

(rf/reg-sub :view
 (fn [db [_ k default]]
  (or (-> db :view k) default)))

(rf/reg-event-db :view-changed
 (fn [db [_ k pos]] ;page/individual. should also keep track of zoom etc? all viewport stuff
  (assoc-in db [:view k] pos)))

(rf/reg-sub :sync-sources (fn [db _] (-> db :sync :sources)))
(rf/reg-event-db :set-sync
 (fn [db [_ selected]] (assoc-in db [:sync :active] selected)))


(rf/reg-event-db :save-cue ;but like, what's point of re-render if thing is outside our viewport
 (fn [db [_ cue]] (update db :cues #(conj cue %))))
(rf/reg-event-db :delete-cue
 (fn [db [_ cue]] (update db :cues (fn [cues] (remove #(= (:id %) (:id cue)) cues))))) ;can just delete from list and will be replaced by a dummy. tho bit inefficient to trigger entire process from one dunno

(rf/reg-sub :cues (fn [db [_ cue-key]] (:cues db [])))
(rf/reg-sub :get-cue
 (fn [] (rf/subscribe [:cues])) ;so reason for this is it gets cached, so only dig in the db once!
 (fn [cues [_ k arg]] ; like k :id arg :100, k :position arg [0 0]
  (let [[cue] (filter #(= arg (k %)) cues)]
  cue)))

(rf/reg-sub :active
 (fn [db [_ id-key]]
  (if id-key
   (some #{id-key} (:active db))
   (:active db))))

(rf/reg-event-fx :cue-interacted ;good? everything from clicking cue itself to delete/edit button of running one, etc
 (fn [{:keys [db]} [_ id-key action-event]]
   (let [cue @(rf/subscribe [:get-cue :id id-key]) ;row id for cue-list is actually cue-id!
        hit (:id cue) ;should be same as id-key
       #_click #_(parse-input action-event)]
    (if (nil? hit) {:db db} ;here should check if we're in macro-adding mode or whatever...
     (case action-event
      ("click" "delete") ;rename stop/cancel? delete would mean deleting cue not stopping
      {:db (update db :active
              (if (or (some #{hit} (:active db)) (nil? hit)) ;howtf does this fix? already checking for nil above...
               #(disj (set %) id-key)
               #(conj % hit)))}
      ("edit" "save")  #_[rc/modal-panel] ;actually this should be a full fx fn and just dispatch further...
      {:db db
       :dispatch [action-event]}))))) ;not impure now. but need to figure out how to set stuff up... but bit by bit (filter null, handle toggle: more serious op? pass down. yeah?)


(defn cue-id-gen [] ;or no atom, init id with db and do it all through there...
 (let [id (atom 114)]
  #(swap! id inc)))
(def new-cue-id (cue-id-gen))

(defonce temp-id-counter (atom 114))
(rf/reg-cofx :temp-cue-id
  #(assoc % :temp-cue-id (swap! temp-id-counter inc))) ;I mean this wont make below work tho...
; still need either of above for proper cue ids
;; (defn create-blank-cue [x y] ;or can we do :id :none?
;;  {:id (keyword (str (new-cue-id))) :color "#282f32" :position [x y]})
(rf/reg-cofx :gen-react-key
  (fn[] #_hmm))


; oh yeah will have to make use of same stuff to replicate push functionality
; (color picker on grid etc) on web!
; basically reimplement existing functionality but now with non-shit code -> easy to then keep adding
(defn cue-cell [{:keys [color id] :as cue}]
 (let [s (r/atom cue)
       toggle (fn [k & [force?]] (swap! s update k #(or force? (not %))))
       a? #(:active @s)
       [mouse-over mouse-out] (map #(fn [e] (toggle :hovered %)) [true false])
       on-click (fn [e]
                 (if (seq cue) (toggle :active)) ;or we just dispatch and then sub or will everything get redrawn too much then?
                 (rf/dispatch [:cue-interacted (:id cue) "click"]))]
  (fn []
   (let [style
         {:box-shadow (str "4px 3px 10px 3px rgb(50, 50, 30,  0.3)")
          ;; :outline (str "solid 4px " color) ;; :outline (str "solid 4px #282828" )
          :border (str (if (a?) "3px" "1px") " solid " "rgb(31, 31, 31, 0.4)") ;cant use dyn color it seems...
          :border-radius (if id "10px" "1px")
          :color :white ;XXX should auto calc complement like in afterglow
          :background-color (if (a?) "rgb(200, 200, 200, 0.4)" (if color color "#282f32")) ;(cond->        #_color (@s :hovered)  #_(-> % (brighten 15%) (saturate 10%)) (@s :active)   #_(-> % (lighten 25%)) ;etc, other states: would kill other cue (way [too] faded in curr AG), maybe like "about to be included in new macro", a? color)
          ;; :background-color (if (a?) "rgb(100, 100, 100, 0.3)" (if color color "#282f32")) ;(cond->        #_color (@s :hovered)  #_(-> % (brighten 15%) (saturate 10%)) (@s :active)   #_(-> % (lighten 25%)) ;etc, other states: would kill other cue (way [too] faded in curr AG), maybe like "about to be included in new macro", a? color)
          :background-image (if-not (a?) (str "linear-gradient(-30deg, rgba(130, 130, 145, 0.3), " color ")")
                             color)
          :width (* 60 (if (:hovered @s) 1.25 1.0)), :max-width 100 ;in AG max 100 but resizing cells jump from 90 to 116?? and max 142. no zoom
          :height (* 50 (if (:active @s) 1.00 1.0)), :max-height 70
          :cursor :pointer
          :transition "0.7s"
          :font-size "60%"
          :align :center
          ;; :visibility (if (:fx @s) :visible :hidden)
          :word-wrap :break-word
          :display :table-cell ; w3schools.com/cssref/pr_class_display.asp :display :inline-block ;everything ends up in same :td with this...
          ;; :display (if (:hovered @s) :inline-block :table-cell) ; w3schools.com/cssref/pr_class_display.asp :display :inline-block ;everything ends up in same :td with this...
          }]
   [:td
    {:style style
     :on-mouse-over mouse-over :on-mouse-out  mouse-out :on-click on-click} ;"handle right click, shift click, anything else?")}
     (:text cue)
     #_[rc/title :label (:id cue) :style {:color "black" :font-size "40%" :line-height "1.0"}]]))))


(defn cue-row [cues & {:keys [wrap-tag cell-tag style]}] ;doesn't need to know location of self, just draws...
 ; :tbody, or :thead for top, or notthing?. any case looks like individual wraps work as well as whole table...
  (let [row [:tr {:style style} ;why centering not working for :th but is for :td?
             (doall (map-indexed  ;if want to abandon dummy cue route, how do I still render rest of table?
               (fn [i cue]
                (if cell-tag ;manual cell...
                 [cell-tag cue]
                 ^{:key (str "cue-cell-" i "-" (:id cue))} ;UGH
                 [cue-cell cue])) ;could/should the ID crap be in cue-cell instead tho?
               cues))]] ;could/should the ID crap be in cue-cell instead tho?
   (if wrap-tag
    [wrap-tag row]
    row)))

(defn cue-page [page-xy & {:keys [size offset] :or {size 8 offset [0 0]}}] ;where is page located on larger grid? what's zoom level?
 (let [[x y] (map #(+ %1 (* size %2)) offset page-xy #_@view)
       [end-x end-y] (map #(+ size %) [x y])
       rows (->> (for [y (range y end-y) x (range x end-x)] ; y/x since we do by row
                  @(rf/subscribe [:get-cue :position [x y]]))
                 vec (partition size) reverse)] ;table starts from up, cue grid down
  [:tbody (doall (for [row rows] [cue-row row]))]))


(defn cue-grid "Viewcontroller for [cue-page]" [#_page-data]
 (let [[page offset size :as view] (map (fn [k default] (rf/subscribe [:view k default]))
                                              [:page :offset :size] [[0 0] [0 0] 8])] ;so do we only have one grid and swap in and out cue-pages to simulate changing
  ; or do we have many instances of cue-page all ready to go? wanting other views than 8x8 sounds possible so making it super dynamic (if works performance-wise) soudns good
  ; also simplifies a bit if what is dispayed is what "exists", no throttling state updates for stuff out of view or whatever - it doesnt concern us until it does
  [rc/box :child [rc/scroller :size "50%" :scroll :on
    :child ;no go lols
    [:table.cue-grid
     {:style {:width "100%" :text-align "center" ;why do we need that?
              :border "2px solid #789" :background-color "#333"
              :table-layout :fixed :overflow :hidden}}
     [cue-row (range 1 9) :wrap-tag :thead :cell-tag :th :style {:text-align "center !important"}] ;align no work. also the unique ID thing. send along a more exact cell spec?
     [cue-page @page :size @size :offset @offset]
     [cue-row (range 1 9) :wrap-tag :tbody :cell-tag :td] ]]] )) ;bottom


(defn nav-btn "Draw one navigation button"
 [x-move y-move target-sub target-key]
 [:td
  {:style {:text-align :center :height 30 :width 30 #_:border #_"3px solid #678" }
   :on-click (fn [e] ;should go straight to dispatch...
              (rf/dispatch [:view-changed target-key
                            (map #(if (<= 0 (+ %1 %2)) (+ %1 %2) 0)
                                 @target-sub [x-move y-move])]))}
  (let [[x y] @target-sub]
   (if (= x-move y-move 0) ;middle button shows pos
    (str x "  " y)
    [rc/md-icon-button :md-icon-name "zmdi-arrow-left" ;:tooltip "hmm"
     :style {:display :inline-block :font-size 18
             :transform (str "rotate(" (+ (* 45 x-move) (* 90 y-move) (if (and (= -1 x-move) (>= 0 y-move)) 45)) "deg)")}]))])
     ;haha close enough almost

(defn nav-btns "Buttons to move viewport" [target-key] ; :page, :offset...
  (let [target-sub (rf/subscribe [target-key])] ;but i guess first push pos if non-0 0
   [:div>table.nav-btns>tbody {:style {:background-color "#282828"}}
    (for [y (reverse (range -1 2))]
     [:tr (for [x (range -1 2)]
           [nav-btn x y target-sub target-key])])]))


(defn sync-sources []
 (let [sources (rf/subscribe [:sync-sources])]
  [:div
   [:span "Tempo sync"]
   [rc/single-dropdown
     :choices     @sources :model       nil
     :placeholder "No sync" :title?      true
     :width       "30%" ;:max-height  "300px" :filter-box? true
     :style {:border "3px solid #789" :background-color "#222"}
     :on-change   #(rf/dispatch [:set-sync %])]]))


(defn row-button
 [row-id action mouse-over-row?]
   #_[rc/button :label action :tooltip (str row-id action) ;reg button does work!
              :style {:box-shadow (str "8px 6px 10px 5px rgb(50, 50, 30, 0.3)") :border-radius "40px"
                      :font-size 10
                      :visibility (if mouse-over-row? :visible :hidden)
                      :line-height "1.0em" :vertical-align :center
                      ;; :height "50%" :padding "10px 10px" :width 20  #_:background-color #_"rgb(50, 50, 50, 0.5)"}
                      :height 20 :width 45 :padding "5px 5px"  #_:background-color #_"rgb(50, 50, 50, 0.5)"}
              :on-click (fn [e] (rf/dispatch [:cue-interacted row-id action]))]
 [rc/row-button
  :md-icon-name    (str "zmdi zmdi-" action) ;must be something wrong with my font css file? it's there tho, and sourced in the html
  :mouse-over-row? mouse-over-row?
  :style   {:color "white" :font-size 32 :box-shadow (str "8px 6px 10px 5px rgb(50, 50, 30, 0.3)")}
  :on-click        #(rf/dispatch [:cue-interacted row-id action])])

(defn row-buttons
  [row-id mouse-over]
  (let [mouse-over-row? (identical? @mouse-over row-id)]
    [rc/v-box :align :start :gap "10px"
              ;; :attr {:on-mouse-over (rc/handler-fn (reset! mouse-over row-id))
              ;;        :on-mouse-out  (rc/handler-fn (reset! mouse-over nil))}
     :children  [[rc/h-box ;; :class    "rc-div-table-row" ;oh yeah set some base classes btw
                  :children (mapv (fn [action]
                                   [row-button row-id action mouse-over-row?])
                                  ["save" "edit" "delete"]) ]]]))



(rf/reg-event-db :inc-done
 (fn [db [_ nr]]
  (update db :done #(if (> 100 (+ (or % 0) nr)) (+ (or % 0) nr) 0))))
(rf/reg-sub :done (fn [db _] (:done db)))
;; (defonce _interval2 (js/setInterval #(rf/dispatch-sync [:inc-done 1]) 500)) ;so would set up intervals to match bpm?

(defn beat-viz "maybe three progress bars showing beat/bar/phrase progress?" []
 (let [progress (rf/subscribe [:done])] ;so this should be ok with sub, no form2 needed. but setInterval is fucked?
 [rc/progress-bar :model progress :striped? true
   :style {:color "#567" :background-color "#222" #_:transition #_"0.1s"}]))

(rf/reg-cofx :start-value
  #(assoc % :start-value (rand-int 100)))

(rf/reg-sub :cue-var-value
  (fn [db [_ cue-var]]
    (or (get-in db [:cue-vars (keyword (:name cue-var))] ) ;test...
        (:max cue-var)
        (:starting cue-var)
        (rand-int (:max cue-var))
        0)))
(rf/reg-event-db :cue-var-set
 (fn [db [_ cue-var v]] (assoc-in db [:cue-vars (keyword (:name cue-var))] v))) ;makes sense if they all have ids? and they do in afterglow, when running anyways
;;  (fn [db [_ cue-var v]] (update-in db [:cues] (:name cue-var)] v))) ;makes sense if they all have ids? and they do in afterglow, when running anyways
; yeah we're gonna need a map for this...

(defn cue-var-control [cue-var] ;fair play they rerender when others get moved but why is atom being recreated?
 (let [start (rf/subscribe [:cue-var-value cue-var])]
 (fn [cue-var]
  [:tr.cue-var-control
   [:td (:name cue-var)]
   [:td (condp = (:type cue-var)
         ;; :number (prefab/slider
         ;;          #js {:defaultValue [0, 100] :withBars true})
         :number [rc/slider :model start :style {:height "30px" :width "200px" :max-width "300px"}
                  :on-change (fn [v]
                              ;; (reset! current v)
                              (rf/dispatch [:cue-var-set cue-var v])) #_(fn [_] )
                  :min (:min cue-var 0) :max (:max cue-var 100) :step 1 :width "100%"]
         :ratio [:p "Like make something with boxes?"]
         :color (prefab/slider-picker
                  #js {;:width 268 :height 130 :background-color "#2C3C44"
                       :onChangeComplete (fn [color event]
                                          (println "color: " color)
                                          (rf/dispatch [:cue-var-set cue-var (:hex color)])
                                          #_(reset! current color))})
                  ;also make sure new color reflected yo...

         ;; :xy-pad [:make :self! :just a box and watch cursor pos when over?]
         #_(prefab/swatches-picker
                  #js {:width 268 :height 130 :background-color "#2C3C44"
                       :onChangeComplete (fn [color event]
                                          (println "color: " color)
                                          (rf/dispatch [:cue-var-set cue-var color])
                                          (reset! current color))})
         :space (str "xyz")
         "dunno type")]
   [:td (str @start)]]
  ; different depending on var type, settings etc. colorpicker, beat fraction ui,
  ; knob, slider, xy...
  ; quil for doing waveform visualizer like on push
  )))

(defn effect-row "Render one active cue/effect" []
 (let []
  ))


(defn active-effects []
 (let [active (rf/subscribe [:active])
       mouse-over (r/atom nil)] ;for now
  [:div
   [:table.cue-list
   {:style {:border "4px solid #222" :padding "10px 10px" ;:margin 10
            :text-align :center
            :background-color "#444" :color "#bcd"
            :table-layout :fixed
            :width "100%" :height "100%"
            :transition "0.3s"}}
   [cue-row ["Pos", "Name",  "Gizmos", "", "", "Buttons"]
    :wrap-tag :thead :cell-tag :th] ;align no work. also the unique ID thing. send along a more exact cell spec?
   [:tbody
    (doall (for [cue-id @active :let [cue @(rf/subscribe [:get-cue :id cue-id])]] ;i take it this is too ez to not go cpu hogging
           [:tr {:attr {:on-mouse-over (rc/handler-fn (reset! mouse-over cue-id))
                        :on-mouse-out  (rc/handler-fn (reset! mouse-over nil))} ;attr in tr went from seeing on cell hover to item hover heh... dont understand
                 :style {:background-color (:color cue)
                         :background-image (str "linear-gradient(rgb(50, 50, 65, 0.35), " (:color cue) ")")}}
            ;; [:td [rc/checkbox :style {:visibility :hidden}]] ;macro bull
            [:td.col1 (str (:position cue))]
            [:td.col2 (str (:text cue))]
            [:table>tbody {:style {:background-color (:color cue)}}
             [:div (doall (for [cue-var (:variables cue) ]
                           ^{:key (str "cue-var-" (:id cue) "-" (:name cue-var))}
                           [cue-var-control cue-var]))]]
            [:td.col4] [:td.col5]
            [:td.col6 [row-buttons cue-id mouse-over]
             #_[rc/button :label "x" :tooltip (str (:id cue))
              :style {:box-shadow (str "8px 6px 10px 5px rgb(50, 50, 30, 0.3)") :border-radius "80px"
                      :font-size 10
                      :line-height "1.0" :vertical-align :center
                      ;; :height "50%" :padding "10px 10px" :width 20  #_:background-color #_"rgb(50, 50, 50, 0.5)"}
                      :height 25 :width 25 :padding "5px 5px"  #_:background-color #_"rgb(50, 50, 50, 0.5)"}
              :on-click (fn [e] (rf/dispatch [:cue-clicked cue-id e]))]]
            ; save/forget vars button
            ; value/lfo viz
            ]))]]
   [rc/button :label "MACRO" :style {:size "30%"} :on-click #(rf/dispatch [:macro-button]) :tooltip "enter??"]]))


(defn header-bar []
 (let [cues (rf/subscribe [:cues])]
  [:div {:style {:font-size 12 :line-height 1.2
                 :height 40 :width "100%" :border "4px solid #222"}}
  #_[:p (count @cues) "/"]
  ;; [rc/single-dropdown] [rc/popover-tooltip] [rc/popover-border]
   ]))


(defn ui []
  [:div {:style {:background-color "#303031" :color "#bcd" }}
   [header-bar]
   [cue-grid]
   [:table>tbody>tr
    [:td [nav-btns :page]] [:td " - "] [:td [nav-btns :offset]]]
   [sync-sources]
   ;; [beat-viz] ;[metronome-controls]
   [active-effects]
   [:div#quil] ;animations!!!
   ])  ;could even send s along and have it swapped from there?

(when-some [el (js/document.getElementById "cue-db")]
  (db/init)
  (r/render [ui] el))
