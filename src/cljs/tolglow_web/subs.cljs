(ns tolglow-web.subs
  (:require [re-frame.core :as rf]
            [thi.ng.math.core :as cmath]
            [thi.ng.color.core :as clr]
            [tolglow-web.db :as db]
            [tolglow-web.macros :as macros]
            [clojure.edn :as edn]
            [cljs-time.core :as ct]
            [tolglow-web.util :as util :refer [css-str css-arg cmod make-key cs <sub]]))

;since
; You see, all (currently instantiated) layer 2 subscriptions will
; run every time app-db changes in any way. All of them. Every time.
; really try minimize num end layer 2s...
(rf/reg-sub :get ;should this be discontinued? or only used transiently like migrate everything away once got a comp working?
 (fn [db [_ & path]]
  (get-in db (if (seqable? path) path [path])))) ;either way prob skip the destructuring and shit, runs too often...

(rf/reg-sub :nil (fn [_ _]))

(rf/reg-sub :view ;this one could use that interceptor that auto runs updates (for cell-offset)
 :<- [:get :view]
 (fn [{:keys [page offset size] :as view} [_ k]]
  (if (= k :cell-offset)
   (mapv (fn [page offset]
          (+ offset (* page size)))
         page offset)
   (get view k view))))

(rf/reg-sub :cues
 ; :<- [:get :cues-m]
 (fn [db [_ cue-key]] (vals (:cues-m db []))))

(rf/reg-sub :get-cue ;this should probably only return what's really needed for render? however we can know that...
 :<- [:cues]         ;since it's currently all passed to each cue in grid etc
 (fn [cues [_ k arg]] ; like k :id arg :100, k :position arg [0 0]
  (first (filter #(= arg (k %)) cues)))) ;or sep things, :id one gets :cues-m for direct get? investigate perf...

(rf/reg-sub :options
 :<- [:get :options]
 (fn [options [_ k args]] ;v useless and specific. fix...
  (let [hits (map (get-in options k) args)]
   (first hits))))

(rf/reg-sub :cue-var-control-options ;fix good evt
 :<- [:get :options]
 (fn [options [_ k cue-var]]
  (let [types-names (filterv some? (mapv #(get cue-var %) [:name :type]))
        sizes (map #(get-in options [:display :control (keyword (clojure.string/lower-case %)) :size]) types-names) ]
   (first sizes))))

(rf/reg-sub :active
 :<- [:get :active]
 (fn [m [_ id]]
  (if id (id m) m)))

; (rf/subscribe [:cue-var-value "cue-7-0" "Color"])

(rf/reg-sub :cue-color
 (fn [[_ id]]
  [(rf/subscribe [:get-cue :id id])
   (rf/subscribe [:active id])
   (rf/subscribe [:cue-var-value id "Color"])])
 (fn [[cue active? cue-var-value] [_ id adj-active?]]
  (let [value (or ;@(a-realtime-cue-color-fn-sub)
                  cue-var-value
                  (:color cue)
                  "#f03")]
    (when value
     (-> (cond-> value ;really gotta figure good way to retain hue pos when reaching 0 sat...
          (string? value) (clr/css)
          (record? value) (-> (clr/adjust-saturation -0.05)
                              (clr/adjust-luminance  -0.05))
          (and adj-active? active?) (-> (clr/adjust-saturation 0.1)
                                        ; (clr/adjust-brightness 0.1)
                                        (clr/adjust-luminance 0.1)))
         clr/as-css)))))

; XXX some things with clr:
; css rgba/hsla only parses when no spaces between args...
; straight update with assoc is dangerous since might forget to first convert to format that has correct key...
(rf/reg-sub :text-color
 :<- [:nil] ;eh, worth? assuming this is a wrong-sub with no db input and we do have a lot of subs for this so
 (fn [_ [_ bg-color]]
  (if (record? bg-color)
   (let [off (- (clr/luminance bg-color) 0.50)
         text (if (neg? off) (+ 1.01 off) off) ;else both 0.0 and 1.0 end up at 0.5...
         final (cond (>  0.85 text 0.50) 0.85
                     (<= 0.15  text 0.50) 0.15
                     :else text)]
    ; (println bg-color "/" final)
    (-> (clr/hsla 0.0 0.0 final 1.0) clr/as-css)
    #_(-> bg-color clr/as-hsla (clr/rotate-hue Math/PI) (assoc :l final :a 1.0) clr/as-css))
   (clr/css "#f03")))) ;rather overkill and looks meh just wanted to test :)

(rf/reg-sub :bg-gradient
 (fn [db [_ base-color]]
  (css-str "linear-gradient" "-30deg"
           (css-arg @(cmod base-color :adj-a -0.7  :adj-s -0.25 :adj-l -0.20) "10%")
           (css-arg @(cmod base-color :adj-a -0.45 :adj-s 0.075 :adj-l -0.20) "90%"))))


(rf/reg-sub :dim-color
 (fn [_ [_ id]] (rf/subscribe [:cue-color id]))
 (fn [color [_ id]]
  (cmod color :adj-s -0.10 :adj-l -0.15)))
(rf/reg-sub :bright-color
 (fn [_ [_ id]] (rf/subscribe [:cue-color id])) ;;  :<- [:cue-color [:<> :first]] ;would be cool to extend sugar to send args to sub-getter
 (fn [color [_ id]]
  (cmod color :adj-s 0.05 :adj-l 0.20)))



(rf/reg-sub :cue-var
 (fn [[_ cue-id _]]
  (rf/subscribe [:get-cue :id cue-id]))
 (fn [cue [_ cue-id var-name]]
  (let [res (filter #(= (:name %) var-name) (:variables cue))]
   (if (seq res) (into {} res)))))


(rf/reg-sub :cue-var-validator ;eh prob shouldnt be a sub...
 (fn [_ [_ kind]]
  (case kind
   ("double", nil) (fn [v mi ma]
                    (cond->> v
                     mi (max mi)
                     ma (min ma)))
   "integer" (fn [v mi ma]
              (cond->> (Math/round v)
               mi (max mi)
               ma (min ma)))
   "color" (fn [v mi ma]
           (if-not (or mi ma) v
            (let [[mi ma] (map deref [mi ma])]
             (-> (cond->> v
                  mi (map max mi)
                  ma (map min ma))
                 clr/hsla))))
   "xy" (fn [v mi ma]
        (cond->> v
         mi (map max mi)
         ma (map min ma)))
   nil)))

(rf/reg-sub :cue-var-value ;so eg _ALL_ these run each app-db change? crazy
  :<- [:get :active]
  :<- [:get :session]
  (fn [[active session] [_ cue-id var-or-name]] ;or just swap to name then sub...
    (let [cue-var (if (map? var-or-name)
                   var-or-name
                   @(rf/subscribe [:cue-var cue-id var-or-name]))]
     (or (get-in active  [cue-id (:name cue-var)])
         (get-in session [cue-id (:name cue-var)])
         (:start cue-var)
         (:max   cue-var)
         #_(when (= (:type cue-var) "color")
          (clr/css "#69c"))))))


(rf/reg-sub :macro
 :<- [:get :macro]
 (fn [data [_ action]]
  (case action
   :save-with-name (let [input (:save-with-name data)]
                    (if (and input (not= "" input)) input "New macro")) ;XXX add counter
   ;; :checked (:checked ))
   (action data)))) ;fallback lookup...


(rf/reg-sub :cues-for-ids
 (fn [[_ input-path]] (rf/subscribe input-path))
 (fn [ids [_]]
  (let [ids (if (map? ids) (keys ids) ids)] ;uncouth but since active returns a map...
   (for [id ids]
    @(rf/subscribe [:get-cue :id id])))))


(defn get-filter-pred [kind input-sub & [lookup]] ;uh cant remember why ended up passing inut like this instead of to returned fn?
 (if (= "" @input-sub) (fn [& _] true) ;no filtering. but wait that means this IS being recreated...
  (let [lookup (or lookup identity)]
  (case kind
  "regex" (fn [text]
           (if (lookup text)
            (try (re-find (re-pattern (str @input-sub ".*")) (lookup text))
                (catch js/SyntaxError. e))
            true)) ;tho likely means there are no candidates anyways...)
  "initial" (fn [text]
             (if (lookup text) ;shouldnt it be checking input-sub isnt "", rather??
              (clojure.string/starts-with? (lookup text) @input-sub)
              true))
  (fn [_] true)))))


(rf/reg-sub :filter-obj
 :<- [:get :filter :method]
 (fn [method [_ sub lookup]] ;; (println "new filter obj")
  (get-filter-pred method sub (or lookup :text))))

(rf/reg-sub :filter-maps
 (fn [[_ input-path filter-path]] ; could send a bunch more instructions
  [(rf/subscribe input-path)
   (rf/subscribe [:filter-obj (rf/subscribe (or filter-path [:get :filter :value]))])]) ;right so this is still triggering new without derefing...
 (fn [[coll flt] [_]]
  (filter flt coll)))

(rf/reg-sub :sort-maps
 (fn [[_ input-path]] (rf/subscribe input-path))
 (fn [coll [_ input-path [by-key direction]]]
  (let [by-key (or by-key :priority)
        direction-fn (if (ifn? direction) direction reverse)] ;or maybe identity default
   (->> coll (sort-by #(by-key %)) direction-fn))))

(rf/reg-sub :diff-maps ;eh sux
 (fn [[_ input-1 input-2]] [(rf/subscribe input-1) (rf/subscribe input-2)])
 (fn [[input-1 input-2] [_ ]]
  (let [ids-2 (set (map :id input-2))
        ids (apply disj (set (map :id input-1)) ids-2)
        cues (doall (for [id ids] @(rf/subscribe [:get-cue :id id])))]
   cues))) ;ugh fix...
        ;; res (remove (fn [tries] (some #{:id tries} ids-2)) input-2)]
        ;; res (remove (fn [in] (some (:id %) ids-2)) input-1)
        ;[a b both] (clojure.data/diff input-1 input-2)]
   ;; (filter identity res))))

(rf/reg-sub :not ;negate a sub result. for fns that require sub paths...
 (fn [[_ input]] (rf/subscribe input))
 (fn [input [_]]
  (not input)))

;; (rf/reg-sub  ;universal of above
;;  (fn [[_ input] (rf/subscribe input)])
;;  (fn [input [_]]
;;   (not input)))

(rf/reg-sub :hud ;so this should massage :diagnostics and only return relevant stuff
 :<- [:get :diagnostics]
 :<- [:get :options :hud]
 :<- [:get :hud]
 (fn [[{:keys [messages unhandled]}
       {:keys [timeout level]}
       hud]
      [_ & [request-key]]] ;could be like :modal, :error...
  (case request-key
   :modal (when (:modal hud) (get messages (:modal hud))) ;fetch message by id...
   (let [including (conj (take-while #(not= % level)
                                    [:error :warning :info]) level)]
   #_(filter #(ct/after? (ct/plus (:time %) (ct/seconds timeout)) (ct/now)) diag)
   (filter #(some #{(:level %)} including)
           (map messages unhandled))))))

(rf/reg-sub :drag
 :<- [:get :drag :touch-draggable]
 (fn [draggable [_ query]]
  (case query
   :draggable? (or :not-touch-device?
                   draggable))))
; or something. since :draggable in drag-handlers will have to query this.


(rf/reg-sub :schema
 :<- [:get :schema]
 (fn [schema [_ kind id]]
  (get-in schema [kind id])))

