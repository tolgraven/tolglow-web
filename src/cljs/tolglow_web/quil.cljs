(ns tolglow-web.quil
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]
            [re-frame.core :as rf]
            [re-graph.core :as rg]
            [tolglow-web.db :as db]
            [tolglow.viz :as viz]
            [thi.ng.color.core :as clr]
            [thi.ng.geom.vector :as v :refer [vec2 vec3]]
            #_[quil.sketch :as a]))

(defn setup []
  (q/frame-rate 40)
  (q/color-mode :hsb)
  (q/stroke 0 20 20 90)
  (q/stroke-weight 1)
  {:fixtures {}
   :color 0 :angle 0})

(defn update-state [state]
  (let []
    (assoc {:color (mod (+ (:color state) 0.7) 255)
            :angle      (+ (:angle state) 0.025)}
           :fixtures (vals (db/get [:fixtures])))))

(defn draw-roundy [state]
  (let [fill [(:color state) 55 55 (+ 100 (rand 155))]
        angle (:angle state)
        x (* (+ 130 (rand 4)) (q/cos angle))
        y (* (+ 130 (rand 0)) (q/sin angle))]
    (q/color-mode :hsb)
    (apply q/fill fill)
    (q/with-translation [(/ (q/width) 2) (/ (q/height) 2)]
      (q/ellipse x y 100 100))))

(defn draw-fixtures [state]
  (q/with-translation [(/ (q/width) 2) (/ (q/height) 2)]
      (doseq [{:keys [x y key id]} (:fixtures state)
              :let [{:keys [color dimmer]} (get (db/get [:fixture-state]) id)]]
        (q/color-mode :hsb 1.0)
        (if color
          (apply q/fill @color)
          (q/fill 30 30 30))
        (q/ellipse (* 50 x) (* 50 y) 30 30))))

(defn draw-state [state]
  (q/background 0.2 0.2 0.2)
  (draw-roundy state)
  (draw-fixtures state))

(declare run-sketch)
(defn ^:export controls [action]
  (if-let [id (q/get-sketch-by-id "quil")]
    (q/with-sketch id
      (case action
        :stop   (q/no-loop)
        :start  (q/start-loop)
        :exit   (q/exit)
        :reload (do (controls :exit) (controls :init))
        (println "Invalid action"))
      nil) ;stop garbage print
    (if (= action :init)
      (run-sketch)
      (println "Can't find #quil"))) )

; (viz/init)

(defn ^:export run-sketch [] ;[w h]
  ; (let [w (or w 600)
  ;       h (or h 400)]
    (q/defsketch quil
      :host "quil" ;id of div
      :size [600 400] ;[w h]
      :setup setup
      :update update-state
      :draw draw-state
      ; :features [:no-start] ;no auto-start. doesnt work...
      :middleware [m/fun-mode]))


(defn init [& [setup-cfg]] ;so at least currently need :venue from cfg, and getters for previous-movement and visualizer-visible...
 (let [setup (viz/get-setup setup-cfg)]
 (q/defsketch     viz
  :title          "quil visualizer for tolglow"
  :host           "viz"
  :size           [600 400]
  :renderer       :p3d
  ; :middleware     [m/pause-on-error  m/fun-mode  m/navigation-3d]
  :middleware     [m/fun-mode m/navigation-3d]
  ; :middleware     [m/fun-mode ]
  :navigation-3d  (merge {:step-size 150} viz/starting-camera)
  :setup          setup
  :update         viz/update-state-timed
  :draw           viz/draw-state
  :mouse-wheel    viz/mouse-scroll
  :key-pressed    viz/key-pressed
  :on-close       #(println "CLOSING DIz"))))
;; (defmacro wa [f] `(with-applet quil ~f)) ;not in need anymore
#_(init {:venue :fix-a-sub-yo
       :cheat (rf/dispatch [:subscribe :start :fixture-state])
       :get-current-values (fn [] (rf/subscribe [:get :fixture-state]))
       :get-fixtures (fn [] (rf/subscribe [:get :fixtures]))})
