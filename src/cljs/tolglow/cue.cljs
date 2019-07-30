(ns tolglow.cue
  (:require [reagent.core :as r]
            ;; [recalcitrant.core :refer [error-boundary]]
            [re-com.core :as rc ]
            [re-frame.core :as rf]
            [tolglow.core :as core]
            [tolglow.db :as db]))

;; (core/safe ) ;time to try this on...

(rf/reg-event-db :save-cue ;but like, what's point of re-render if thing is outside our viewport
 (fn [db [_ cue]] (update db :cues #(conj cue %))))
(rf/reg-event-db :delete-cue
 (fn [db [_ cue]] (update db :cues (fn [cues] (remove #(= (:id %) (:id cue)) cues))))) ;can just delete from list and will be replaced by a dummy. tho bit inefficient to trigger entire process from one dunno

(rf/reg-sub :cues (fn [db [_ cue-key]] (:cues db []))) ;;  (fn [db [_ [max-x max-y]]] (:cues db [])))
(rf/reg-sub :get-cue ;merge with :cues and this is what does with args?
 (fn [db [_ k arg]] ; like k :id arg :100, k :position arg [0 0]
  (let [[cue] (filter #(= arg (k %)) (:cues db))]
  cue #_(or cue {}))))


(rf/reg-sub :view (fn [db _ #_[_ pos]] (:view-page db [0 0]))) ;;  (fn [db [_ [max-x max-y]]] (:cues db [])))
(rf/reg-sub :active
 (fn [db [_ id-key]]
  (if id-key
   (some #{id-key} (:active db))
   (:active db))))

(rf/reg-event-db :cue-clicked
 (fn [db [_ id-key action-event]]
   (let [cue @(rf/subscribe [:get-cue :id id-key])
        #_click #_(parse-input action-event)
         hit (:id cue)]
    (if (nil? hit)
     db ;start action to add new cue or something? dont forget to return db
     (update db :active
              ;; (if (some #{hit} (:active db)) ;avoid conj on nil id
              (if (or (some #{hit} (:active db)) (nil? hit)) ;howtf does this fix? already checking for nil above...
                       #(disj (set %) id-key)
                       #(conj % hit))))))) ;) ;pretty nifty... too nifty for myself. flipped order or it keeps nesting
;; (rf/reg-event-fx :cue-clicked
;;  (fn [cofx [_ id-key action-event]]
;;    ;; (let [[cue] (filter #(= (:id %) id-key) (:cues (:db cofx)))
;;    (let [cue @(rf/subscribe [:get-cue :id id-key]) ;[cue] (filter #(= (:id %) id-key) (:cues (:db cofx)))
;;         #_click #_(parse-input action-event)
;;          hit (:id cue)]
;;     (if (nil? hit)
;;      cofx ;start action to add new cue or something? dont forget to return db
;;      ;; (update db :active
;;      (update-in cofx [:db :active]
;;                  (if (or (some #{hit} (cofx :db :active)) (nil? hit)) ;avoid conj on nil id
;;                           #(disj (set %) id-key)
;;                           #(conj % hit))))))) ;) ;pretty nifty... too nifty for myself. flipped order or it keeps nesting

(rf/reg-event-db :view-changed
 (fn [db [_ pos]]
  (assoc db :view-page pos)))

(defn cue-id-gen [] ;or no atom, init id with db and do it all through there...
 (let [id (atom 114)]
  #(swap! id inc)))
(def new-cue-id (cue-id-gen))

(defonce temp-id-counter (atom 114))
(rf/reg-cofx :temp-cue-id
  #(assoc % :temp-cue-id (swap! temp-id-counter inc))) ;I mean this wont make below work tho...
; still need either of above for proper cue ids
(defn create-blank-cue [x y] ;or can we do :id :none?
 {:id (keyword (str (new-cue-id))) :color "#282f32" :position [x y]})


(defn cue-cell [{:keys [color id] :as cue}]
 (let [s (r/atom cue)
       ;; cue-sub (rf/subscribe [:get-cue :id id]) ;eh no lol. shiet non-dummy ones lack pos id etc
       toggle (fn [k & [force?]] (swap! s update k #(or force? (not %))))
       a? #(:active @s)]
  (fn []
   (let [style
         {:box-shadow (str "4px 3px 10px 3px rgb(30, 30, 30, 0.5)")
          ;; :outline (str "solid 4px " color) ;; :outline (str "solid 4px #282828" )
          ;; :border (str (if (a?) "3px" "1px") "solid #111") ;cant use dyn color it seems...
          :border (str (if (a?) "2px" "1px") " solid " "rgb(11, 11, 11, 0.4)") ;cant use dyn color it seems...
          :border-radius (if id "10px" "1px")
          :color :white ;XXX should auto calc complement like in afterglow
          :background-color (if (a?) "#333a41" (if color color "#282f32")) ;(cond->        #_color (@s :hovered)  #_(-> % (brighten 15%) (saturate 10%)) (@s :active)   #_(-> % (lighten 25%)) ;etc, other states: would kill other cue (way [too] faded in curr AG), maybe like "about to be included in new macro", a? color)
          :background-image (if-not (a?) (str "linear-gradient(rgb(30, 30, 45, 0.4), " color ")")
                             color)
          :width (* 60 (if (:hovered @s) 1.25 1.0)), :max-width 100 ;in AG max 100 but resizing cells jump from 90 to 116?? and max 142. no zoom
          :height (* 50 (if (:active @s) 1.00 1.0)), :max-height 70
          ;; :width (* 60 (if (:hovered @s) 0.90 1.0)), :max-width 100 ;in AG max 100 but resizing cells jump from 90 to 116?? and max 142. no zoom
          ;; :height (* 50 (if (:active @s) 0.95 1.0)), :max-height 70
          :cursor :pointer
          :transition "0.8s"
          :font-size "60%"
          :align :center
          ;; :visibility (if (:fx @s) :visible :hidden)
          :word-wrap :break-word
          ;; :display :table-cell ; w3schools.com/cssref/pr_class_display.asp :display :inline-block ;everything ends up in same :td with this...
          ;; :display (if (:hovered @s) :inline-block :table-cell) ; w3schools.com/cssref/pr_class_display.asp :display :inline-block ;everything ends up in same :td with this...
          ;; :display (if (a?) :inline-block :table-cell) ; w3schools.com/cssref/pr_class_display.asp :display :inline-block ;everything ends up in same :td with this...
          ;; :box-sizing (if (:hovered @s) :border-box )
          }]
   [:td
    {:style style
     :on-mouse-over (fn [e] (toggle :hovered true))
     :on-mouse-out  (fn [e] (toggle :hovered false))
     :on-click      (fn [e] (toggle :active) ;or we just dispatch and then sub or will everything get redrawn too much then?
                     (rf/dispatch [:cue-clicked (:id cue) e])) ;"handle right click, shift click, anything else?")}
     }
     #_[rc/button :label (:text cue "")
               :tooltip (:id cue "nope")
               :style style]
     ;; [rc/info-button :info (str (:id cue "no")) :color "red"]
     ;; [rc/info-button :info "yo" ]
     ;; [rc/box
     ;;  ;; :child [rc/close-button :tooltip "X" :color "red"]]
     ;;  ;; :child [rc/p :label "eh" :tooltip "X" :color "red"]]
     ;;  :child [rc/border :child [rc/p "X"]]]
     (:text cue)
     [rc/title :label (:id cue) :style {:color "black" :font-size "40%" :line-height "1.0"}]]))))


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

(defonce sizer (r/atom 8))

(defn cue-grid "Viewcontroller for [cue-page]" [#_page-data]
 (let [view (rf/subscribe [:view])
       size (rf/subscribe [:size])] ;so do we only have one grid and swap in and out cue-pages to simulate changing
  ; or do we have many instances of cue-page all ready to go? wanting other views than 8x8 sounds possible so making it super dynamic (if works performance-wise) soudns good
  ; also simplifies a bit if what is dispayed is what "exists", no throttling state updates for stuff out of view or whatever - it doesnt concern us until it does
  (fn []
   [rc/box :child
    [rc/scroller :size "50%" :scroll :on
     :child [:table.cue-grid
    {:style {:width "100%" :text-align "center" ;"left" ;why do we need that?
             :border "2px solid #789" :background-color "#333"
             :table-layout :fixed
             :overflow :hidden}}
    [cue-row (range 1 9) :wrap-tag :thead :cell-tag :th {:text-align "center !important"}] ;align no work. also the unique ID thing. send along a more exact cell spec?

    [cue-page @view :size @sizer :offset [0 0]] ;; [cue-page [0 0] @sizer [0 0]] [cue-page]

    [cue-row (range 1 9) :wrap-tag :tbody :cell-tag :td] ]]] ))) ;bottom


(defn nav-btn "Single button"
 [x-move y-move view-sub]
 [:td
  {:style {:text-align :center :height 40 :width 70 :padding 10
           :border "3px solid #789" :display :table-cell}
   :on-click (fn [e] ;should go straight to dispatch...
              (rf/dispatch [:view-changed
                            (map #(if (<= 0 (+ %1 %2)) (+ %1 %2) 0)
                                 @view-sub [x-move y-move])]))}
  (if (= x-move y-move 0) ;middle button shows pos
   (str @view-sub) "")])  ;for now. fix a nice arrow and rotate somehow

(defn nav-btns "Buttons to change page" [& [pos]]
  (let [view-sub (rf/subscribe [:view])] ;but i guess first push pos if non-0 0
   (fn [& [pos]]
    [:div
     [:table.nav-btns {:style {:background-color "#282828" } }
      [:tbody
       (for [y (reverse (range -1 2))]
        [:tr (for [x (range -1 2)]
              [nav-btn x y view-sub])])]]]))) ; :arrow-left :arrow-right :arrow-bottom :arrow-left-bottom :arrow-right-top :long-arrow-up :long-arrow-down

(rf/reg-event-db :inc-done (fn [db [_ nr]] (update db :done #(if (> 100 (+ (or % 0) nr)) (+ (or % 0) nr) 0))))
(rf/reg-sub :done (fn [db _] (:done db)))
(def _interval2 (js/setInterval #(rf/dispatch-sync [:inc-done 1]) 500))

(defn header-bar []
 (let [cues (rf/subscribe [:cues])
       active (rf/subscribe [:active])
       progress (rf/subscribe [:done])]
  [:div {:style {:font-size 14 :line-height 1.4
                 :background-color "#303031" :color "#bcd"
                 :height 100 :width "100%" :border "4px solid #222"}}
  [:p (count @cues) "/" @sizer]
  [:span @active]
  ;; [rc/single-dropdown ]
  #_[rc/progress-bar :model progress :striped? true
   :style {:color "#567" :background-color "#222" :transition "0.5s"}]
  ;; [rc/popover-tooltip]
  ;; [rc/popover-border]
  #_(for [cue @cues] (str (:position cue) ",  ")) ]))


(def selecta (r/atom ""))
(def slida (r/atom 0))

(defn test-re-com []
  ;; (safe)
  [rc/h-box :height "50%" :width "25%"
   :children [(for [y (reverse (range -1 2))]
               [rc/h-box :size "33%"
                :children [(for [x (range -1 2)]
                            [rc/v-box :size "33%" :style {:border "2px solid" :background-color "green"}
                             :children [[rc/label :label (str x y)]]])] ])]]
  [rc/v-box
   :height "100%" :width "80%"
   :children [#_[rc/md-icon-button :md-icon-name "zmdi-plus" #_:tooltip #_"this thing"]
              ;; [rc/info-button :info "WAZAAA"]
              [rc/slider :model slida :style {:height "50px"}
               :on-change #(reset! slida %)
               :min 0 :max 1 :step 0.01 :width "90%"]
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
               :tooltip "put into"]]])


(defn ui []
  [:div
   [header-bar]
   [cue-grid]
   [:p]
   [nav-btns #_page]
   [test-re-com]])  ;could even send s along and have it swapped from there?

(when-some [el (js/document.getElementById "cue-db")]
  (db/init)
  (r/render [ui] el))
