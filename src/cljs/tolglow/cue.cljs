(ns tolglow.cue
  (:require [reagent.core :as r]
            [recalcitrant.core :refer [error-boundary]]
            [re-com.core :as rc ]
            [re-frame.core :as rf]
            [tolglow.db :as db]))


(rf/reg-event-db :create-cue ;this needs to like send a :cues event as well no? for earlier subs to react...
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
