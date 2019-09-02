(ns tolglow-web.quil
  (:require [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

(defn setup []
  (q/frame-rate 40)
  (q/color-mode :hsb)
  (q/stroke 0 20 20 90)
  (q/stroke-weight 1)
  {:color 0 :angle 0})

(defn update-state [state]
  {:color (mod (+ (:color state) 0.7) 255)
   :angle (+ (:angle state) 0.025)})

(defn draw-state [state]
  ;; (q/background 53 42 43)
  ;; (q/background 39 39 41)
  (q/fill (:color state) 55 55 (+ 100 (rand 155)))
  (let [angle (:angle state)
        x (* (+ 130 (rand 4)) (q/cos angle))
        y (* (+ 130 (rand 0)) (q/sin angle))]
    (q/with-translation [(/ (q/width) 2) (/ (q/height) 2)]
      (q/ellipse x y 100 100))))

(defn ^:export run-sketch []
  (q/defsketch quil
    :host "quil"
    :size [600 400]
    :setup setup
    :update update-state
    :draw draw-state
    :middleware [m/fun-mode]))
