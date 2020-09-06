(ns tolglow-web.tolglow-interop
  (:require
   [tolglow.core :as tcore]
   [afterglow.web.routes.show-control :as aweb]
   [afterglow.show :as show]
   [thi.ng.color.core :as clr]
   [clojure.edn :as edn]
   [afterglow.rhythm :refer [metro-snapshot]]
   [afterglow.web.routes.web-repl :as web-repl]
   [afterglow
    [core :as core] [controllers :as ct]
    [midi :as midi :refer [sync-to-midi-clock]]
    [show :as show :refer [add-effect! all-fixtures end-effect! fixtures-named get-variable patch-fixture! set-cue! set-variable! start! stop! sync-to-external-clock]]
    [util :as util]
    [show-context :refer [*show* with-show]]]
   [afterglow.effects
    [channel :as chan-fx] [dimmer :as dimmer]
    [oscillators :as lfo :refer [build-oscillated-param sawtooth sine square triangle]]
    [cues :as cues :refer []]
    [params :as params :refer [param?]]
    [show-variable :as var-fx :refer [variable-effect]]]

   [mount.core :refer [defstate]]))

(defstate ^{:on-reload :noop} tolshow
  :start (tcore/activate-show))
; (mount.core/start #'tolshow)
; (show/find-cue-grid-active-effect *show* 8 8)

(def current-cue-color #'aweb/current-cue-color)
(defn cue-view "Returns a nested structure of rows of cue information starting at the specified origin, with the specified width and height. Ideal for looping over and rendering in textual form, such as in a HTML table.  Each cell contains a tuple [cue effect], the cue assigned to that grid location, and the currently-running effect, if any, launched from that cue. Cells which do not have associated cues still be assigned a unique cue ID (identifying page-relative coordinates, with zero at the lower left) so they can be updated if a cue is created for that slot while the page is still up."
  [left bottom height width holding]
  (let [show *show*
        active-keys (show/active-effect-keys show)
        snapshot (metro-snapshot (:metronome *show*))]
    (for [y (range (dec (+ bottom height)) (dec bottom) -1)
          x (range left (+ left width))]
      (if-let [[cue active] (show/find-cue-grid-active-effect show x y)]
        (let [held? (and holding (= holding [x y (:id active)]))
              color (current-cue-color show active-keys cue active held? snapshot)]
          (assoc cue
                 :current_color color
                 ; :style_color (str "style=\"background-color: "
                 ;                   @(-> color clr/as-int24 clr/as-css)
                 ;                   "; color: " (util/contrasting-text-color color) "\"")
                 :position [x y]))))))  ;; Add the ID whether or not there is a cue
      ; (assoc
      ;    (if-let [[cue active] (show/find-cue-grid-active-effect show x y)]
      ;      (let [held? (and holding (= holding [x y (:id active)]))
      ;            color (current-cue-color show active-keys cue active held? snapshot)]
      ;        (assoc cue
      ;               :current_color color
      ;               :style_color (str "style=\"background-color: "
      ;                                 @(-> color clr/as-int24 clr/as-css)
      ;                                 "; color: " (util/contrasting-text-color color) "\"")
      ;               :position [x y]))
      ;      {}) ;; No actual cue found, start with an empty map
      ;    :id (str "cue-" x "-" y)))))  ;; Add the ID whether or not there is a cue

(def cue-var-values #'aweb/cue-var-values)
(def effect-summary #'aweb/effect-summary)
; (defn add-hook [] (show/add-frame-fn! (fn previous-movement [snapshot] )))
; (defn previous-movement   []  (:previous @(:movement *show*)))
(defn calculated-values   [] @(:calculated-values *show*))
; (defn visualizer-visible  []  (:visualizer-visible @(:dimensions *show*) []))
;should drop :head recursion when sending...

;this sucks balls and will need alt solution sooner not later.
; (cue-var-values *show* (effect-summary @(:active-effects *show*)))


; (get @(:cues (:cue-grid *show*)) [8 8])
; (print-dup (clr/hsla 0.5 0.5 0.5) java.io.)
; (def as-str (binding [*print-dup* true] (prn (clr/hsla 0.5 0.5 0.5))))
; (type as-str)
;some basic data fetching sketch outline

;;so our equivalent of this should be reactive.
;;Guess that's where graphql subs really come in...
;(defn- record-page-grid "Stores the view of the cue grid last rendered by a particular web interface so that differences can be sent the next time the page asks for an update."
;  [page-id grid left bottom width height]
;  (swap! clients update-in [page-id] assoc :view [left bottom width height] :grid grid :when (now)))

;(def effect-summary aweb/effect-summary)
;(aweb/update-known-controllers page-id)


;(aweb/move-view) ;if want to move linked controller as well would send this instead of changing our request from client side?

;; his endpoint for a quick implementation:
;; (aweb/post-ui-event id kind req)
;(aweb/post-ui-event 1 "startButton" req)
;(aweb/post-ui-event 1 "set-cue-var"
;  {:params {:effect-key x :effect-id y :var-key o :value h}}) ;request...
;; except maybe not cause have to get ourselves onto list of clients and crsf blabla
;;
; example data from server:
  ; "grid-changes": [
  ;   {
  ;     "id": "cue-0-0",
  ;     "name": "All Strobe",
  ;     "color": "#e6e6e6",
  ;     "textColor": "#222"
  ;   },
  ;   ; ...
  ;   {
  ;     "id": "cue-6-0",
  ;     "name": "Tube Strobe",
  ;     "color": "#999999",
  ;     "textColor": "#edc"
  ;   }
  ; ]
