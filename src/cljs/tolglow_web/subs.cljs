(ns tolglow-web.subs
  (:require [re-frame.core :as rf]
            [thi.ng.math.core :as cmath]
            [thi.ng.color.core :as clr]
            [tolglow-web.db :as db]
            [clojure.edn :as edn]
            [tolglow-web.util :as util :refer [css-str css-arg cmod make-key cs <sub]]))

(rf/reg-sub :view
 :<- [:get :view]
 (fn [{:keys [page offset size] :as view} [_ k]]
  (if (= k :cell-offset)
   (mapv (fn [page offset]
          (+ offset (* page size)))
         page offset)
   (get view k view))))

(rf/reg-sub :cues
 ; (fn [db [_ cue-key]] (:cues db [])))
 (fn [db [_ cue-key]] (vals (:cues-m db []))))
(rf/reg-sub :get-cue
 :<- [:cues] ;so reason for this is it gets cached, so only dig in the db once!
 (fn [cues [_ k arg]] ; like k :id arg :100, k :position arg [0 0]
  (first (filter #(= arg (k %)) cues))))

(rf/reg-sub :options
 :<- [:get :options]
 (fn [options [_ k args]] ;v useless and specific. fix...
  (let [hits (map (get-in options k) args)]
   (first hits))))
(rf/subscribe [:options [:display] [:control]])

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
   (rf/subscribe [:active id])])
 (fn [[cue active?] [_ id adj-active?]]
  (let [value (or @(rf/subscribe [:cue-var-value id "Color"])
                   (:color cue)
                   "#f03")]
    (when value
     (-> (cond-> value ;really gotta figure good way to retain hue pos when reaching 0 sat...
          (string? value) (clr/css)
          (record? value) (-> (clr/adjust-saturation -0.05)
                              (clr/adjust-luminance -0.07))
          (and adj-active? active?) (-> (clr/adjust-saturation 0.1)
                                        (clr/adjust-brightness 0.2)))
         clr/as-css)))))

; (clr/as-hsla (clr/hsla 0 0 0 1))
; XXX some things with clr:
; css rgba/hsla only parses when no spaces between args...
; straight update with assoc is dangerous since might forget to first convert to format that has correct key...
(rf/reg-sub :text-color
 (fn [_ [_ bg-color]]
  ; (clr/css "#ddd")
  (if (and (record? bg-color) #_(float? (:s bg-color)))
   (do ;(println bg-color)
       (let [off (- (clr/luminance bg-color) 0.50)
             text (if (neg? off) (+ 1.0 off) off)
             final (cond (>= 0.85 text 0.45) 0.85
                         (<= 0.15  text 0.45) 0.15
                         :else text)]
        (-> (clr/hsla 0.0 0.0 final 1.0) clr/as-css)
        #_(-> bg-color clr/as-hsla (clr/rotate-hue Math/PI) (assoc :l final :a 1.0) clr/as-css)))
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

(rf/reg-sub :cue-var-validator
 (fn [_ [_ kind]]
  (case kind
   :number (fn [v mi ma]
            (cond->> v
             mi (max mi)
             ma (min ma)))
   :color (fn [v mi ma]
           (if-not (or mi ma) v
            (let [[mi ma] (map #(-> % clr/css clr/as-hsla deref) [mi ma])]
             (-> (cond->> v
                  mi (map max mi)
                  ma (map min ma))
                 clr/hsla clr/as-css deref))))
   :xy (fn [v mi ma]
        (cond->> v
         mi (map max mi)
         ma (map min ma)))
   nil)))

(rf/reg-sub :cue-var-value
  (fn [db [_ cue-id var-or-name]] ;or just swap to name then sub...
    (let [cue-var (if (map? var-or-name)
                   var-or-name
                   @(rf/subscribe [:cue-var cue-id var-or-name]))]
     (or (get-in db [:active cue-id (:name cue-var)])
         (get-in db [:session cue-id (:name cue-var)])
         (:starting cue-var)
         (:max cue-var)
         #_(when (= (:type cue-var) "color")
          (clr/css "#69c")
          #_(let [cue @(rf/subscribe [:get-cue :id cue-id])]
           (println cue)
           (:color cue))) ;dirty hack!!

         ;; (rand-int (:max cue-var)) ;by cofx in that case
         #_0 ;nah just weird. if there's no value there isnt, thats better.
         ))))

; (clr/luminance (clr/css "#000"))
(rf/reg-event-db :cue-var-set
 (fn [db [_ cue-id var-name v & [save-session session-key]]]
  (let [cue-var @(rf/subscribe [:cue-var cue-id var-name])
        validator @(rf/subscribe [:cue-var-validator (:type cue-var)])
        validated (cond-> v
                   validator (validator (:min cue-var) (:max cue-var)))]
    (assoc-in db [:active cue-id var-name] validated)))) ;makes sense if they all have ids? and they do in afterglow, when running anyways

; (float? NaN%)
;; (rf/reg-cofx :session
;;  (fn [cofx] (assoc cofx :session @(rf/subscribe [:get :session]))))

(rf/reg-event-fx :cue-var-save
 (fn [{:keys [db] :as cofx} [_ cue-id var-name value]]
  (let [path [:session cue-id var-name]
        value (or value (get-in db [:active cue-id var-name]))]
   {:db (assoc-in db path value)})))


(rf/reg-sub :macro
 :<- [:get :macro]
 (fn [data [_ thing]]
  (case thing
   :save-with-name (let [input (:save-with-name data)]
                    (if (and input (not= "" input)) input "New macro")) ;XXX add counter
   ;; :checked (:checked ))
   (thing data)))) ;fallback lookup...


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

