(ns tolglow-web.util
  (:require [re-frame.core :as rf]
            [clojure.string :as s]
            [thi.ng.math.core :as cmath]
            [thi.ng.color.core :as clr]
            [mount.core :refer [defstate]]))
;; (make a cond-> where tests also get "threaded") !!!

(defn css-str "string builder"
 [f & args] (str f "(" (clojure.string/join ", " args) ")"))
(defn css-arg [& parts] (clojure.string/join " " parts))
(defn css-rot [angle] (str "rotate(" angle "deg)"))

(defn cmod
 [spec & {:keys [adj-h adj-s adj-l adj-a]}]
 ; (let [color (if (or (instance? thi.ng.color.core/CSS spec))
 ; (let [color (if (map? spec)
 ;              spec (clr/css spec))]
 ;  (-> color (clr/analog adj-h adj-s adj-l) (clr/adjust-alpha adj-a))))
 (-> (cond-> spec
      (not (record? spec)) (clr/css))
     (clr/analog adj-h adj-s adj-l)
     (clr/adjust-alpha adj-a)))

(defn cubic-bezier [& args] (apply css-str "cubic-bezier" args))

(defn make-key [& ids] ;could this return the meta straight away instead
 (s/join "-" (map #(if (keyword? %) (name %) (str %)) ids))
 #_(reduce #(str %1 (when-not (= "" %1) "-") %2) "" ids))

(defn cs [& names] (clojure.string/join " " (filter identity names)))
(def <sub  (comp deref rf/subscribe))


(defn item-for-id "From vector of maps 'v', returns first item whose id-fn (default :id) matches 'id', else nil"
  [id v & {:keys [id-fn] :or {id-fn :id}}]
  (first (filter #(= (id-fn %) id) v)))


(defn remove-for-id "Takes a vector of maps 'v', each of which has an id-fn (default :id) key.  Return v where item matching 'id' is excluded"
  [id v & {:keys [id-fn] :or {id-fn :id}}]
  (filterv #(not= (id-fn %) id) v))


(defn fmap "Apply 'f' to keys of 'm', returning updated map: (fmap inc {:a 4 :b 2}) => {:a 5 :b 3}"
 [f m]
 (into {} (for [[k v] m] [k (f v)])))

(defn mapor "Map that simply calls f when given non-colls"
 [f maybe-coll & maybe-colls]
 (let [#_coll #_(if (coll? maybe-coll) maybe-coll [maybe-coll]) ]
  (if (coll? maybe-coll)
   (apply map f maybe-coll maybe-colls)
   (apply f maybe-coll maybe-colls))))

(defn deep-merge "Recursively merge maps. If vals are not maps, the last value wins."
 [& vals]
 (if (every? map? vals)
  (apply merge-with deep-merge vals)
  (last vals)))

(defn at "Take symbol: if value, return it. If ratom, deref it, thereby avoiding nil derefing etc..."
 [val-or-atom]
 (if (satisfies? IDeref val-or-atom) ;is satisfies equally slow on js? investigate...
  @val-or-atom
  val-or-atom))

(defn element-by-id [id] (.getElementById js/document id))
(defn on-window [event f]
 (js/window.addEventListener event f))
(defn on-document [event f]
 (js/document.addEventListener event f))
(defn remove-document [event f]
 (js/document.removeEventListener event f))

(defn on-event [node event f & [active?]]
 (let [node (cond-> node
             ; (or (keyword? node) (string? node)) (element-by-id (name node)))]
             (or (keyword? node) (string? node)) (-> name element-by-id))]
  (.addEventListener node event f active?)))

(defn on-move-til-mouse-up [handler & [cleanup]]
 (js/document.addEventListener "mousemove" handler)
 (js/document.addEventListener
  "mouseup" (fn [e]
             (js/document.removeEventListener
              "mousemove" handler)
             (when cleanup (cleanup e)))
  #js{:once true}))

(defn on-key-init []
 (println "Generate listeners")
 (let [;captured '("ArrowLeft" "ArrowRight" "ArrowUp" "ArrowDown")
       ondown (fn down [e]
               ; (println "down")
               (case (.-key e)
                ("ArrowLeft" "ArrowRight" "ArrowUp" "ArrowDown")
                (.preventDefault e)
                nil))
       onup (fn up [e]
               ; (println "up")
               (case (.-key e)
                ("ArrowLeft" "ArrowRight" "ArrowUp" "ArrowDown")
                (do (.preventDefault e)
                    (rf/dispatch [:keypress (.-key e)]))
                nil))]
  (fn []
   (println "replacing listeners")
   (remove-document "keydown" ondown)
   (remove-document "keyup" onup)
   (on-document "keydown" ondown)
   (on-document "keyup" onup))))

; (defn on-key []
;  (on-document
;   "keydown" (fn [e] (.preventDefault e)))
;  (js/document.addEventListener
;   "keyup" (fn [e]
;            (case (.-key e)
;             ("ArrowLeft" "ArrowRight" "ArrowUp" "ArrowDown")
;             (do (.preventDefault e)
;                 (rf/dispatch [:keypress (.-key e)]))
;             nil))))
; (on-key)
; (defstate key-init
;  :start on-key-init)

(defn bounding-rect [e]
 (.getBoundingClientRect (.-target e)))
(defn client-xy [e]
 [(.-clientX e) (.-clientY e)])
