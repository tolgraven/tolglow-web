(ns tolglow.cue ;fix hehe
  (:require [reagent.core :as r]
            [re-frame.core :as rf]))

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

(defn create-blank-cue [x y]
 {:id (keyword (str (new-cue-id))) :color :gray :text "" :position [x y] :fx nil})

(rf/reg-sub :cue-at
 (fn [db [x y]]
   (let [cue (filter #(= [x y] (:position %)) (:cues db))]
    (if (some? cue) cue (create-blank-cue x y))))) ;not sure what's wrong with this...

;shortcuts:
;;   (html [:div#foo.bar.baz "bang"])
;;   ->
;;   "<div id=\"foo\" class=\"bar baz\">bang</div>"

;;   [:div>p>b "Nested Element"]
;;   ->
;;   [:div
;;     [:p
;;       [:b "Nested Element"]]]

(defn cue-cell [cue-data]
 (let [s (r/atom cue-data)
       a? #(:active @s)
       toggle (fn [k & force] (swap! s update k #(if (seq force) (first force) (not %))))]
  (fn []
   (let [
         {:keys [color id whatever]} cue-data ;or do we create it blank and then sub to get from db I guess
         ;; mm yup cause color can change tons etc... and obviously cues can be added and deleted while empty cue-cell keeps existing in some state
          ;does that mean server pushes all colors at potentially 40fps tho sounds bit crazy
         ]

[:td
 {:style {;:background-color color ;"#86c"
          ;; :box-shadow (str "2px 3px 8px 1px " color)
          :box-shadow (str "2px 2px 7px 1px #678") ;how alpha?
          :outline (str "solid 3px " color)
          ;; :outline (str "solid 4px #282828" )
          :border (str "2px solid #111") ;cant use dyn color it seems...
          :bordercolor color ;"#ddd" #_(if a? "#484848" c)
          :color :white
          :background-color #_color (if (:active @s) "#434a51" color) #_(cond->        #_color
                                                                 (@s :hovered)  #_(-> % (brighten 15%) (saturate 10%))
                                                                 (@s :active)   #_(-> % (lighten 25%)) ;etc, other states: would kill other cue (way [too] faded in curr AG), maybe like "about to be included in new macro",
                                                                 a? color) ;
          :width (* 60 (if (:hovered @s) 0.95 1.0)), :max-width 100 ;in AG max 100 but resizing cells jump from 90 to 116?? and max 142. no zoom
          :height (* 50 (if (:active @s) 1.15 1.0)), :max-height 70
          :transition "0.8s" ;"height 0.5s width 1.5s" ;how set color transition and stuff?
          :font-size "60%"
          :text-align "center" :word-wrap :break-word :overflow :hidden
          ;; :display :inline-block ;everything ends up in same :td with this...
          :display :table-cell ;:flex no  ;:grid no table-cell, etc. w3schools.com/cssref/pr_class_display.asp
          ;; :position :relative
          :margin-left 5, :margin-top 5, :padding 5
          ;; :top 0 :left 0 :right 0 :bottom 0 ;or I guess grid keeps track of poses
          }
  :on-mouse-over (fn [e] (toggle :hovered true))
  :on-mouse-out (fn [e] (toggle :hovered false))
  :on-click (fn [e]
             (toggle :active) ;or we just dispatch and then sub or will everything get redrawn too much then?
             (rf/dispatch [:cue-clicked id e])
             (comment "handle right click, shift click, anything else?"))}

 (:text cue-data)] #_(:id cue-data) ))))

(def xy (r/atom (for [x (range 8) y (range 8)]
                 @(rf/subscribe [:cue-at x y]))))

(defn cue-page [pos-x pos-y] ;where is page located on larger grid?
 (let [s (r/atom {})]
  (fn [] ;oh
   (let [cues @(rf/subscribe [:cues #_show-key])
         active? #(get-in @s [(:id %1) :active])
         cues-padded @(rf/subscribe [:cues 8 8])
         indiviual-subs (doall (for [x (range 8) y (range 8)]
                                @(rf/subscribe [:cue-at x y])))]

    [:table {:style {:width "100%" :text-align "left" ;:position :absolute
                     :border "2px solid #789" :background-color "#333"
                     :visible (:active @s false)
                     :overflow :hidden}}
      [:thead ;row above table proper, probably should be in cue-grid?
       [:tr (for [i (range 8)] ;
             ^{:key (str "cue-header" i)}
             [:th
              {:style {:text-align "center"}}
              (inc i)])]]

       (let [dummies (vec (doall (for [y (range 8) x (range 8)] ; y/x since we do by row
                      (create-blank-cue x y)))) ;wanna go this approach since need to be able to handle on the fly creation and destruction anyways...
             idx-for-xy (fn [[x y]] (+ x (* 8 y))) ;]
             page (reduce
                    (fn [grid cue] (assoc grid (idx-for-xy (:position cue)) cue))
                    dummies cues)]

        ;; (reduce ;try do below nicer
        ;;  (fn [[row table] cues]
        ;;   (let [])))
      (loop [cues #_indiviual-subs page row [:tr], table []]
       (let [row (into row
                       (doall
                        (map-indexed ;; [:td ^{:key (:id cue)}[cue-cell cue]]
                         (fn [i cue]
                          ^{:key (:id cue)}[cue-cell cue])
                         (take 8 cues))))]
       (if (< 8 (count cues))
        (recur (drop 8 cues), [:tr], (concat table [row]))
        (concat table [row])))))


       [:tbody ;row below table proper
        [:tr (for [i (range 8)]
             ^{:key (str "cue-footer" i)}
             [:td
              {:style {:text-align "center"}}
              (inc i)])]]
       ]))))

;; (defn nav-btn "Button to change page" [x-offset y-offset]
;;  (let [thing (rf/)]
;;   (fn []
;;   [:div
;;    [:lala-presentation]
;;    {:style {}
;;     :on-click #(rf/dispatch [:view-change x-offset y-offset])}
;;    ])))

(defn cue-grid "Viewcontroller for [cue-page]" [page-data]
 (let [s (r/atom {:view [0 0]})] ;so do we only have one grid and swap in and out cue-rects to simulate changing
  ; or do we have many instances of cue-page all ready to go? wanting other views than 8x8 sounds possible so making it super dynamic (if works performance-wise) soudns good
  ; also simplifies a bit if what is dispayed is what "exists", no throttling state updates for stuff out of view or whatever - it doesnt concern us until it does
  (fn []
   [:table {:style {:width "100%" :text-align "left" ;:position :absolute
                    :border "2px solid #789" :background-color "#333"
                    :visible (:active @s false)
                    :overflow :hidden}}
      [:thead ;row above table proper, probably should be in cue-grid?
       [:tr (for [i (range 8)] ;
             ^{:key (str "cue-header" i)}
             [:th
              {:style {:text-align "center"}}
              (inc i)])]]

       [cue-page 0 0]

       [:tbody ;row below table proper
        [:tr (for [i (range 8)]
             ^{:key (str "cue-footer" i)}
             [:td
              {:style {:text-align "center"}}
              (inc i)])]]

       ;; [nav-btn :left-glyph #(swap! s #(update ))] ;still cooler to have one arrow and set rotation by move offset vector :P

       ;; [nav-btn 1  0 #(swap! s #(update s :view #(map + %)))]
       ;; [nav-btn 0  1]
       ;; [nav-btn -1 0]
       ;; [nav-btn 0 -1]
       ])))


(defn header-bar []
 (let [cues @(rf/subscribe [:cues])]
  [:div {:style {:font-size 14 :line-height 1.4
                 :background-color "#303031" :color "#bcd"
                 :height 100 :width "100%" :border "4px solid #222"}}
  [:span "Inget snack va."]
  [:p (count cues)]
  (for [cue #_(range 16) cues]
   (str (:position cue) ",  "))]))

(defn ui []
  [:div
   [header-bar]
   [cue-page]])

(when-some [el (js/document.getElementById "cue-db")]
  (defonce _init (rf/dispatch-sync [:initialize]))
  (r/render [ui] el))
