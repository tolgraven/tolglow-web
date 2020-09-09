(ns tolglow-web.utils "Move cljs util fns here..."
 (:require [clojure.walk :refer [postwalk]]))

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
