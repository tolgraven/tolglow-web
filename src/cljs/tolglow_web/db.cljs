(ns tolglow-web.db
  (:require [reagent.core :as r]
            [re-frame.core :as rf]
            [cljs.reader]))

(declare data)

(rf/reg-event-db :initialize [rf/debug]
  (fn [db _] data))

(defn init "Initialize the database. Only once unless forced..."
 [& force?]
 (defonce _init (rf/dispatch-sync [:initialize]))
 (rf/clear-subscription-cache!)) ;

(defn get "Quick subscription getter"
 [& path] ;default is better handled ext since this goes through :get -> we already know there is a valid sub
 (let [path (if (seqable? (first path)) (first path) path) ;handle both individual args and vector
       sub (rf/subscribe (into [:get] path))]
  @sub))

(defn set "Quick db setter. Returns passed value."
 [path value]
 (rf/dispatch [:set path value])
 value)

(defn setter "Quick db setter fn getter"
 [path]
 (fn [value & _] (set path value)))

(defn toggle "Toggle bool at path in db"
 [path]
 (rf/dispatch [:toggle path]))


(def user-key "tolglow-web-dev-storage")  ;; localstore key

(defn set-user-ls [user]
  (.setItem js/localStorage user-key (str user)))  ;; sorted-map written as an EDN map

(defn remove-user-ls  []
  (.removeItem js/localStorage user-key))

(rf/reg-cofx :local-store-user
 (fn [cofx _]
   (assoc cofx :local-store-user  ;; put the local-store user into the coeffect under :local-store-user
          (into (sorted-map)      ;; read in user from localstore, and process into a sorted map
                (some->> (.getItem js/localStorage user-key)
                         (cljs.reader/read-string))))))  ;; EDN map -> map

(def data
 {:cues
 [{:id :1    :color "#57c" :text "Run shite" :position [0 0] :fx "Strobe" :priority 100
   :variables [{:type :number :name "Level" :min 0 :max 100}
               {:type :number :name "Lightness" :min 0 :max 100}
               {:type :color :name "Color" :starting "#bbb"}]}
  {:id :3    :color "#c54" :text "Extra sparkle" :position [1 0] :fx "Color" :priority 5
   :variables [{:type :alpha :name "Alpha" :min 0 :max 100}
                {:type :color :name "Color" :starting "#000"}
               #_{:type :number :display [:slider :y] :name "Alpha" :min 0 :max 100}]}
  {:id :11   :color "#5a6" :text "Moving" :position [2 0] :fx "Pan" :priority 3
   :variables [{:type :number :name "Fade" :min 0 :max 3000 :starting 200}
               {:type :knob :name "Chance" :min 0 :max 0.3 :starting 0.001}
               {:type :xy :name "x y" :min [-1 -1] :max [1 1] :starting [0 0]}
               {:type :xy :name "pan tilt" :min [-180 -180] :max [180 180] :starting [45 90]}]}
  {:id :10   :color "#4ba" :text "Moving" :position [3 0] :fx "Aim"
   :variables [{:type :space :name "x" :min -10 :max 10 :starting 0}
               {:type :space :name "y" :min -2 :max 4 :starting 0}
               {:type :space :name "z" :min -2 :max 10 :starting 0}]}
  {:id :0    :color "#72c" :text "Fog" :position [4 0] :fx "Fsshh!"}
  {:id :200  :color "#a83" :text "Stripe" :position [0 1] :fx "Tilt"}
  {:id :19   :color "#222" :text "Wash" :position [1 1] :fx "Shutter"}
  {:id :100  :color "#c84" :text "Pointy" :position [2 1] :fx "Blame"
   :variables [{:type :ratio :name "beats" :starting [4 2]}]}
  {:id :108  :color "#4ab" :text "1st ROW" :position [6 0] :fx "YO"
   :variables [{:type :xy :name "x y" :min [0 0] :max [10 10] :starting [5 5]}]}
  {:id :109  :color "#82c" :text "2nd ROW" :position [6 1] :fx "YO"
   :variables [{:type :number :name "Snippet" :starting "(sin now)"}]} ;need some other property for display-as
  {:id :110  :color "#3ac" :text "Synth" :position [6 2] :fx "ADSR"
   :variables [{:type :knob :name "Attack"  :min 0 :max 10 :starting 2.5}
               {:type :knob :name "Decay"   :min 0 :max 10 :starting 5.0}
               {:type :knob :name "Sustain" :min 0 :max 10 :starting 5.0}
               {:type :knob :name "Release" :min 0 :max 10 :starting 5.0}]}
  {:id :111  :color "#55b" :text "4nd ROW" :position [6 3] :fx "YO"}
  {:id :112  :color "#cba" :text "5nd ROW" :position [6 4] :fx "YO"}
  {:id :113  :color "#789" :text "6nd ROW" :position [6 5] :fx "YO"} ]
 :filter {:method "regex"}
 :view {:page [0 0] :offset [0 0] :size 8}
 :commands {:start {:cmd "(afterglow.show/start!)"}
            :stop {:cmd "(afterglow.show/stop!)"}

            :cue-start {:cmd "afterglow.show/run-effect-from-cue-grid! " :args [:x :y]} ;ehhh or yeah just use schema
            }
 :schema {:query
          {:fixtures {:query "{ fixtures { name id key x y z } }"
                      :event [:tolglow-web.events/save-fixtures]}
           :shaders {:query "query GetShader($path: String) {
                               shaders(path: $path) {
                                 file_name kind text
                             } }"
                     :args {:path "resources/public/glsl"}
                     :event [:save-shaders]}}
          :subscription
          {:fixture-state {:query "{ fixture_state { id color dimmer strobe pan_tilt lookat } }"
                           :event [:save-fixture-state]}
           :fixture-diff  {:query "{ fixture_diff { id newdata_string } }" ;or something... prob abandon gql for this tbh
                           :event [:save-fixture-diff]}
           :live-shaders {:query "{ live_shader { file_name text kind }}"
                          :event [:save-shaders]}}}
 :options {:auto-save-vars true
           :hud {:timeout 30 :level :info}
           :display {:control {:knob    {:size 50}
                               :beats   {:size 75}
                               :cycles  {:size 75}
                               ; :gain {:size "50%"} ;maybe not now but will def work
                               :gain    {:size 100}
                               :offset  {:size 100}
                               :xy      {:size 100}
                               ; :alpha {:size 25}}} ;presumably a size here would= "dont fill all available" & can fit multiple same line...
                               :alpha   {:size 50}
                               :boolean {:size 30}}} ;presumably a size here would= "dont fill all available" & can fit multiple same line...
           }
 :sync {:selected-id nil
        :sources #_[]
        [{:id 1 :label "MIDI 1" :type :midi}
         {:id 2 :label "Beat Link" :type :beat-link}
         {:id 3 :label "Ableton Link" :type :ableton-link}
         {:id 4 :label "Audio analysis" :type :audio}]}})

