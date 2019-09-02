(ns tolglow-web.views.variable-control "UI components for controlling Afterglow cue/effect variables."
 (:require [re-frame.core :as rf]
           [reagent.core :as r]
           [re-com.core :as rc]
           [clojure.pprint :as pprint]
           [clojure.string :as string]
           [clojure.edn :as edn]
           [goog.string :as gstring]
           [thi.ng.math.core :as cmath]
           [thi.ng.color.core :as clr]
           ; [klipse.run.plugin.plugin] ;; this namespace initializes Klipse. We require it for its side effects
           ; [klipse.plugin :as klipse-plugin]
           [tolglow-web.db :as db]
           [tolglow-web.util :as util :refer [at css-str css-arg cmod make-key cs <sub]]
           [tolglow-web.events :as events]))


; (defn klipse-wrapper
;   [{:keys [scripts content settings]}]
;   (r/create-class
;    {:component-did-mount (fn [_] (klipse-plugin/init (clj->js settings)))
;     :reagent-render      (fn [{:keys [content]}] content)}))

; (defn klipse-snippet [starting setup loop-ms]
;  (let [#_starting #_(atom starting)] ;needs to retain last good value so doesnt go nuts while typing...
;   [klipse-wrapper
;    {:content [:div
;               [:div.klipse.language-klipse ;[:div.klipse.language-reagent "[:div {:style {:color \"red\"}} \"hello world!\"]"]
;                {:style {:background "#222" :opacity 0.7}
;                 :data-eval-idle-msec 25
;                 :data-max-eval-duration 200 ;ms, default 1000 sounds dangerous
;                 :data-preamble setup  ;hidden js code. setup ticks for animations...
;                 ;; :data-external-libs :"github,repos,list"
;                 :data-print-length 30 ;default 1000 lol
;                 :data-loop-msec loop-ms}
;                @starting]]
;     :settings {:selector ".language-klipse"
;                :selector_reagent ".language-reagent"
;                :codemirror_options_in {:lineWrapping true,
;                                        :lineNumbers false,
;                                        :autoCloseBrackets true
;                                        :theme "birds-of-paradise"}
;                :codemirror_options_out {:lineWrapping true
;                                         :lineNumbers false }}}]))
; HTMLElement.innerText ;<- rendered text...
;; (.getElementById "klipse") (.-innerHTML)
;; klipse_editors[i].setValue('let a = 1');
;; ^ for access. so
;; (println (.setValue (aget js/klipse_editors 0) "let a = 1"))

; (defn code-box "Control value through code live code snippet"
;  [model on-change spec]
;  [klipse-snippet
;   model
;   "(def sin Math/sin) (def now (js/Date.now))" nil #_250])

;; (defn scale-control [v var-bound control-size to-var?] ;direction
;;  (let [ bounds (if to-var? [control-bound var-bounds] [var-bounds control-size])
;;        ;; f (fn [v [from to]] (cmath/map-interval-clamped v from to))]
;;        f (fn [v [from to]] (cmath/map-interval-clamped v from to))]
;;  (if (seq? v) (map f v bounds) (f v bounds))))
;; (scale-control [30 50] [[0 100]] [[0 10] [0 10]] false)

;; The with-let macro looks just like let Ð but the bindings only execute once, and it takes an optional finally clause, that runs when the component is no longer rendered. This can be particularly useful because it can prevent the need for a form-2 component in many instances (like creating a local reagent atom in your component).
;; For example: here's a component that sets up an event listener for mouse moves, and stops listening when the component is removed.
; (defn mouse-pos-comp []
;   (r/with-let [pointer (r/atom nil)
;                handler #(swap! pointer assoc :x (.-pageX %) :y (.-pageY %))
;                _ (.addEventListener js/document "mousemove" handler)]
;     [:div "Pointer moved to: " (str @pointer)]
;     (finally (.removeEventListener js/document "mousemove" handler))))
;^ investigate.
