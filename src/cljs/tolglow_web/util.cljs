(ns tolglow-web.util
  (:require [re-frame.core :as rf]
            [clojure.string :as s]
            [thi.ng.math.core :as cmath]
            [thi.ng.color.core :as clr]
            [clojure.core.async :refer [<! >! timeout chan alt! go]]
            [clojure.walk :refer [postwalk]]
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
 (if (coll? maybe-coll)
   (apply map f maybe-coll maybe-colls)
   (apply f maybe-coll maybe-colls)))

(defn deep-merge "Recursively merge maps. If vals are not maps, the last value wins."
 [& vals]
 (if (every? map? vals)
  (apply merge-with deep-merge vals)
  (last vals)))

(defn remove-nils "Remove keys with nil or empty seq values, even in nested maps."
  [m]
  ; (let [f (fn [[k v]] (when v [k v]))]
  (let [f (fn [[k v]] (when (if (coll? v) (seq v) v) [k v]))]
    (postwalk
     (fn [x] (if (and (map? x) (not (record? x)))
               (into {} (map f x))
               x))
     m)))


(defn at "Take symbol: if value, return it. If ratom, deref it, thereby avoiding nil derefing etc..."
 [val-or-atom]
 (if (satisfies? IDeref val-or-atom) ;is satisfies equally slow on js? investigate...
  @val-or-atom
  val-or-atom))

(defn element-by-id [id] (.getElementById js/document id))
(defn on-window [event f]
 (js/window.addEventListener event f))
(defn on-document [event f & [opt-map]]
 (js/document.addEventListener event f (clj->js opt-map)))
(defn remove-on-document [event f & [opt-map]]
 (js/document.removeEventListener event f (clj->js opt-map)))


(defn on-event [node event f & [active?]]
 (let [node (if (or (keyword? node) (string? node))
              (element-by-id (name node))
              node)]
  (.addEventListener node event f active?)))

(defn on-move-then-cleanup [handler & [extra-cleanup-f]]
  (let [end (fn [event-name]
              (on-document event-name
                           (fn [e]
                             (remove-on-document "mousemove" handler)
                             (when extra-cleanup-f (extra-cleanup-f e)))
                           {:once true}))]
    (on-document "mousemove" handler)
    (end "mouseup")
    (end "touchend"))) ;but original handler still doesnt have to sort touchmove?


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
   (remove-on-document "keydown" ondown)
   (remove-on-document "keyup" onup)
   (on-document "keydown" ondown)
   (on-document "keyup" onup))))


(defn bounding-rect [e]
 (.getBoundingClientRect (.-target e)))
(defn client-xy [e]
 [(.-clientX e) (.-clientY e)])

(defn element-in-view? [el]
  (let [rect (.getBoundingClientRect el)
        [top bottom] [(.-top rect) (.-bottom rect)]]
    (and (< top js/window.innerHeight) (>= bottom 0))))


; JS bench testing shite
(defrecord XY [x y])
(def NUM 10000)

(def array-data (into-array
	(for [i (range NUM)]
    (let [o (js-obj)]
      (set! (.-x o) (rand))
      (set! (.-y o) (rand))
      o))))

(def vec-data (mapv #(hash-map :x (.-x %) :y (.-y %)) array-data))
; (def vec-data-array-map (mapv #(into cljs.core.PersistentArrayMap/EMPTY %) vec-data))
(def vec-data-array-map (mapv #(into (array-map) %) vec-data))
(def vec-data-records (mapv #(XY. (:x %) (:y %)) vec-data))
(def array-data-records (into-array vec-data-records))

(defn with-loop [data]
  (loop [x 0 y 0 data data]
    (if (seq data)
      (let [o (first data)]
        (recur (+ x (:x o)) (+ y (:y o)) (rest data)))
      [x y])))

(defn loop-and-native [data]
  (loop [x 0 y 0 data data]
    (if (seq data)
      (let [o (first data)]
        (recur (+ x (.-x o)) (+ y (.-y o)) (rest data)))
      [x y])))

(defn mutable-native-count [data]
  (let [x (doto (make-array 1)
            (aset 0 0))
        y (doto (make-array 1)
            (aset 0 0))]
    (dotimes [i (count data)]
      (let [o (data i)]
        (aset x 0 (+ (aget x 0) (.-x o)))
        (aset y 0 (+ (aget y 0) (.-y o)))))
    [(aget x 0) (aget y 0)]))

(defn mutable-native-length [data]
  (let [x (doto (make-array 1)
            (aset 0 0))
        y (doto (make-array 1)
            (aset 0 0))]
    (dotimes [i (alength data)]
      (let [o (aget data i)]
        (aset x 0 (+ (aget x 0) (.-x o)))
        (aset y 0 (+ (aget y 0) (.-y o)))))
    [(aget x 0) (aget y 0)]))

(defn with-reduce [data]
  (reduce
   (fn [[x y] d]
     [(+ x (:x d)) (+ y (:y d))])
   [0 0]
   data))

(defn loop-subvec [data]
  (loop [x 0 y 0 data data]
    (if (seq data)
      (let [o (first data)]
        (recur (+ x (:x o)) (+ y (:y o)) (subvec data 1)))
      [x y])))

(defn benchy []
; (simple-benchmark [data array-data] (js/sumXsAndYsJs data) 1000)
 (println (time (dotimes [i 1000] (mutable-native-length array-data)) ))
 (time (dotimes [i 1000] (mutable-native-length array-data-records)) )
 (time (dotimes [i 1000] (mutable-native-count vec-data-records)) )
 (time (dotimes [i 1000] (loop-and-native vec-data-records)) )
 (time (dotimes [i 1000] (with-loop vec-data-records)) )
 (time (dotimes [i 1000] (with-loop vec-data-array-map)) )
 (time (dotimes [i 1000] (with-loop vec-data)) )
 (time (dotimes [i 1000] (with-reduce vec-data)) )
 (time (dotimes [i 1000] (loop-subvec vec-data)) ))


; (require '[clojure.core.async :as async :refer :all]) #_[<! >! <!! timeout chan alt! go]
; (use '[clojure.core.async]) #_[<! >! <!! timeout chan alt! go]

(defn fake-search [kind]
  (fn [ch query]
    (go
     (<! (timeout (rand-int 100)))
     (>! ch [kind query]))))

(def web1 (fake-search :web1))
(def web2 (fake-search :web2))
(def image1 (fake-search :image1))
(def image2 (fake-search :image2))
(def video1 (fake-search :video1))
(def video2 (fake-search :video2))

(defn fastest [query & replicas]
  (let [ch (chan)]
    (doseq [replica replicas]
      (replica ch query))
    ch))

(defn google [query]
  (let [ch (chan)
        t (timeout 80)]
    (go (>! ch (<! (fastest query web1 web2))))
    (go (>! ch (<! (fastest query image1 image2))))
    (go (>! ch (<! (fastest query video1 video2))))
    (go (loop [i 0 ret []]
          (if (= i 3)
            ret
            (recur (inc i) (conj ret (alt! [ch t] ([v] v)))))))))

; (go (>! (<! (google "clojure"))))

