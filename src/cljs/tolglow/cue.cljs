(ns tolglow.cue ;fix hehe
  (:require [reagent.core :as r]
            [recalcitrant.core :refer [error-boundary]]
            [re-com.core :as rc ]
            [re-frame.core :as rf]
            [tolglow.db :as db]))


(defn safe "Wrap whatever with an error boundary"
 [elem] (r/as-element [(fn [] [error-boundary elem])]))

(def data
 {:cues
 [{:id :1 :color :blue :text "Run shite" :position [0 0] :fx "Strobe"}
  {:id :3 :color :red :text "Extra sparkle" :position [1 0] :fx "Color"}
  {:id :11 :color :green :text "Moving" :position [2 0] :fx "Pan"}
  {:id :10 :color :seagreen :text "Moving" :position [3 0] :fx "Aim"}
  {:id :0 :color :purple :text "Fog" :position [4 0] :fx "Fsshh!"}
  {:id :200 :color :orange :text "Stripe" :position [0 1] :fx "Tilt"}
  {:id :19 :color :black :text "Wash" :position [1 1] :fx "Shutter"}
  {:id :100 :color :orangered :text "Pointy" :position [2 1] :fx "Blame"}

  {:id :108 :color :seagreen :text "2nd ROW" :position [6 0] :fx "YO"}
  {:id :109 :color :purple :text "2nd ROW"   :position [6 1] :fx "YO"}
  {:id :110 :color :seagreen :text "2nd ROW" :position [6 2] :fx "YO"}
  {:id :111 :color :seagreen :text "2nd ROW" :position [6 3] :fx "YO"}
  {:id :112 :color :seagreen :text "2nd ROW" :position [6 4] :fx "YO"}
  {:id :113 :color :seagreen :text "2nd ROW" :position [6 5] :fx "YO"}
  ]})

(rf/reg-event-db :initialize
 (fn [db _] {:cues (:cues data)}))

(rf/reg-event-db :create-cue
 (fn [db [_ c]] (update db :cues #(conj c %))))
(rf/reg-event-db :delete-cue
 (fn [db [_ c]] (update db :cues #(disj % c)))) ;can just delete from list and will be replaced by a dummy. tho bit inefficient to trigger entire process from one dunno

(rf/reg-sub :cues (fn [db [_ cue-key]] (:cues db [])))
;;  (fn [db [_ [max-x max-y]]] (:cues db [])))

(rf/reg-sub :cues-padded-blanks
 (fn [[_ max-x max-y]] (rf/subscribe [:cues])) ;idunno could we fwd x y and mod (limit) first fetch at all? sure
 (fn [cues [_ max-x max-y]]
  (-> cues #_println #_(println max-x)))) ;need to what, sort by position, pad between?

(rf/reg-event-db :cue-clicked
 (fn [db [_ id-key action-event]]
  (let [cue (filter #(= (:id %) id-key) (:cues db))
        click `(parse-input action-event)]
   (if (some #{id-key} (:active db))
    (update db :active #(disj % cue))
    (update db :active #(conj cue %)))))) ;pretty nifty...


(defn cue-id-gen [] ;or no atom, init id with db and do it all through there...
 (let [id (r/atom 114)]
  (fn [] (swap! id inc)
   @id)))
(def new-cue-id (cue-id-gen))

(defn create-blank-cue [x y] ;or can we do :id :none?
 {:id (keyword (str (new-cue-id))) :color "#575757" :text "" :position [x y] :fx nil})

(rf/reg-sub :cue-at
 (fn [db [x y]]
   (let [cue (filter #(= [x y] (:position %)) (:cues db))]
    (if (some? cue) cue (create-blank-cue x y))))) ;not sure what's wrong with this...

(def page (r/atom [0 0]))

;;   [:div>p>b "Nested Element"]
;;   ->
;;   [:div
;;     [:p
;;       [:b "Nested Element"]]]

(defn cue-cell [{:keys [color id] :as cue-data}]
 (let [s (r/atom cue-data)
       toggle (fn [k & force?] (swap! s update k #(or (first force?) (not %))))]
  (fn []
    [:td
     {:style
      {:box-shadow (str "1px 1px 6px 3px #345") ;how alpha?
       ;; :outline (str "solid 4px " color) ;; :outline (str "solid 4px #282828" )
       :border (str "1px solid #111") ;cant use dyn color it seems...
       :color :white, :background-color #_color (if (:active @s) "#434a51" color) ;(cond->        #_color (@s :hovered)  #_(-> % (brighten 15%) (saturate 10%)) (@s :active)   #_(-> % (lighten 25%)) ;etc, other states: would kill other cue (way [too] faded in curr AG), maybe like "about to be included in new macro", a? color)
       :width (* 60 (if (:hovered @s) 0.95 1.0)), :max-width 100 ;in AG max 100 but resizing cells jump from 90 to 116?? and max 142. no zoom
       :height (* 50 (if (:active @s) 1.15 1.0)), :max-height 70
       :transition "0.8s"
       :font-size "60%"
       :visibility (if (:fx @s) :visible :hidden)
       :text-align "center" :word-wrap :break-word, :overflow :hidden
       :display :table-cell ;:flex no  ;:grid no table-cell, etc. w3schools.com/cssref/pr_class_display.asp :display :inline-block ;everything ends up in same :td with this...
       :margin-left 5, :margin-top 5, :padding 5} ;; :position :relative
      :on-mouse-over (fn [e] (toggle :hovered true))
      :on-mouse-out  (fn [e] (toggle :hovered false))
      :on-click      (fn [e] (toggle :active) ;or we just dispatch and then sub or will everything get redrawn too much then?
                      (rf/dispatch [:cue-clicked id e]))} ;"handle right click, shift click, anything else?")}
     (:text cue-data) #_(:id cue-data)])))

(defn cue-row [cues & {:keys [wrap-tag cell-tag style]}] ;doesn't need to know location of self, just draws...
 (fn [] ; :tbody, or :thead for top, or notthing?. any case looks like individual wraps work as well as whole table...
  (let [row [:tr {:style style}
             (doall
              (for [cue cues]
               (if cell-tag ;manual cell...
                [cell-tag cue]
                ^{:key (:id cue)}[cue-cell cue])))] ]
   (if wrap-tag
    [wrap-tag row]
    row))))

(defn cue-page [page-xy] ;where is page located on larger grid?
 (let [[cell-x cell-y] (map #(* 8 %) page-xy)
       cues @(rf/subscribe [:cues]) ;should be limited to those on curr page
       dummies (vec (doall (for [y (range 8) x (range 8)] ; y/x since we do by row
                            (create-blank-cue x y)))) ;wanna go this approach since need to be able to handle on the fly creation and destruction anyways...
       idx-for-xy (fn [[x y]] (+ x (* 8 y))) ;this is to find index in monovector but should prob group em up earlier yet or?
       rows (->> (reduce
                  (fn [grid cue]
                   (assoc grid (idx-for-xy (:position cue)) cue))
                  dummies cues)
                 (partition 8)
                 reverse)]
  (fn [] [:tbody (doall (for [row rows] [cue-row row]))])))

(defn cue-grid "Viewcontroller for [cue-page]" [page-data]
 (let [s (r/atom {:view [0 0]})
       view (rf/subscribe [:view])] ;so do we only have one grid and swap in and out cue-rects to simulate changing
  ; or do we have many instances of cue-page all ready to go? wanting other views than 8x8 sounds possible so making it super dynamic (if works performance-wise) soudns good
  ; also simplifies a bit if what is dispayed is what "exists", no throttling state updates for stuff out of view or whatever - it doesnt concern us until it does
  (fn []
   (let [#_(:view @s)]
    [:table.cue-grid
     {:style {:width "100%" :text-align "center" ;"left" ;why do we need that?
              :border "2px solid #789" :background-color #_"hsl(20, 60%, 50%)" "#333"
              :overflow :hidden}}
     [cue-row (range 1 9) :wrap-tag :thead :cell-tag :th ;top
      :style {:text-align "center"}] ;align no work. also the unique ID thing. send along a more exact cell spec?

     [cue-page @page]

     [cue-row (range 1 9) :wrap-tag :tbody :cell-tag :td ;bottom
      :style {:text-align "center"}] ]) )))


(defn nav-btn "Single button"
 [x-move y-move s]
 [:td
  {:style {:text-align :center :height 60 :width 80 :padding 10
           :border "3px solid #789" :display :table-cell}
   :on-click (fn []
              (swap! page (fn [pos]
                           (map #(if (<= 0 (+ %1 %2)) (+ %1 %2) 0)
                                pos [x-move y-move])))
              (rf/dispatch [:view-change @page]))}
  (str x-move " " y-move)])  ;for now. fix a nice arrow and rotate somehow

(defn nav-btns "Buttons to change page" [s]
  (fn []
  [:div
    [:table.nav-btns {:style {:background-color "#282828" } }
    [:tbody
     (for [y (reverse (range -1 2))]
      [:tr (for [x (range -1 2)]
        [nav-btn x y s])])]]])) ; :arrow-left :arrow-right :arrow-bottom :arrow-left-bottom :arrow-right-top :long-arrow-up :long-arrow-down
; uhh rest? also what about just rotating one?


(defn header-bar []
 (let [cues @(rf/subscribe [:cues])]
  [:div {:style {:font-size 14 :line-height 1.4
                 :background-color "#303031" :color "#bcd"
                 :height 100 :width "100%" :border "4px solid #222"}}
  [:span "Inget snack va."]
  [:p (count cues)]
  (for [cue cues] (str (:position cue) ",  "))]))


(defn title []
  (let [name (rf/subscribe [:whoo])]
    [rc/title :label (str "Hello from " name #_"me") :level :level1]))

(def selecta (r/atom ""))
(def slida (r/atom 0))

(defn test-re-con []
  ;; (safe)

  [rc/h-box :height "50%" :width "25%"
   :children [(for [y (reverse (range -1 2))]
               [rc/h-box :size "33%"
                :children [(for [x (range -1 2)]
                            [rc/v-box :size "33%" :style {:border "2px solid" :background-color "green"}
                             :children [[rc/label :label (str x y)]]])] ])]]
  [rc/v-box
   :height "100%" :width "80%"
   :children [[title]
              [rc/md-icon-button :md-icon-name "zmdi-plus" #_:tooltip #_"this thing"]
              [rc/md-circle-icon-button :md-icon-name "zmdi-plus" #_:tooltip #_"this thing"]
              [rc/label :label "DO DO OD"]
              [rc/info-button :info "WAZAAA"]
              [rc/title :label "TEST"]
              [rc/slider :model slida :on-change #(reset! slida %) :min 0 :max 1 :step 0.01 :width "50%"
               :style {:height "50px"}]
              [rc/single-dropdown
               :choices     [{:id 1 :label "One"}
                             {:id 2 :label "Twoee"}
                             {:id 3 :label "Sriie"}]
               :model       nil ;selected-country-id
               :title?      true
               :placeholder "Several choice"
               :width       "300px"
               :max-height  "400px"
               :filter-box? false
               :on-change   #(reset! selecta %)]
              [:div
               [:strong "Damn: "]
               (if (nil? @selecta)
                "None"
                (str (:label @selecta ) " [" @selecta "]"))]
              [rc/button :label "Clicker" :on-click #(swap! selecta inc)
               :tooltip "HoverMe"]]]
  [rc/box :size "auto"
    :children [[rc/md-icon-button :md-icon-name "long-arrow-up"
                 :tooltip "this thing"]
               [rc/title :label "TEST"]]])



(defn ui []
  [:div
   ;; [header-bar]
   [cue-grid]
   [:p]
   [nav-btns page]
   #_[test-re-con]])  ;could even send s along and have it swapped from there?

(when-some [el (js/document.getElementById "cue-db")]
  (defonce _init (rf/dispatch-sync [:initialize]))
  (r/render [ui] el))
