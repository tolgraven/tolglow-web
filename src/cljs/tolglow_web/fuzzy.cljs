;; (ns tolglow-web.fuzzy)
;;
;; (defn str-len-distance "normalized multiplier 0-1 measures length distance between strings, 1 = same length"
;;   [s1 s2]
;;   (let [c1 (count s1)
;;         c2 (count s2)
;;         maxed (max c1 c2)]
;;     (double (- 1 (/ (- maxed (min c1 c2)) maxed)))))
;;
;; (defn clean-str
;;   [^String s]
;;   (.replaceAll (.toLowerCase s) "[ \\/_]" ""))
;;
;; (defn score
;;   [oquery ostr :as args]
;;   (let [[query string] (map clean-str args)
;;         max-len 1000.0]
;;     (loop [query (seq (char-array query))
;;            string (seq (char-array string))
;;            mult 1
;;            idx max-len
;;            score 0]
;;       (cond
;;        ;; add str-len-distance to score, so strings with matches in same position get sorted by length
;;        ;; boost score if we have an exact match including punctuation
;;        (empty? query) (+ score
;;                      (str-len-distance query str)
;;                      (if (<= 0 (.indexOf ostr oquery)) max-len 0))
;;        (empty? string) 0
;;        :default (if (= (first query) (first string))
;;                   (recur (rest query)
;;                          (rest string)
;;                          (inc mult) ;; increase the multiplier as more query chars are matched
;;                          (dec idx) ;; decrease idx so score gets lowered the further into the string we match
;;                          (+ mult score)) ;; score for this match is current multiplier * idx
;;                   (recur query
;;                          (rest string)
;;                          1 ;; when there is no match, reset multiplier to one
;;                          (dec idx)
;;                          score))))))
;;
;; (defn search
;;   [file query & {:keys [limit] :or {limit 20}}]
;;   (println "Matching " query " in " file)
;;   (let [query (.toLowerCase query)]
;;     (let [data (get-dataset file)]
;;       (take limit
;;             (sort-by :score (comp - compare)
;;                      (filter #(< 0 (:score %))
;;                              (for [s data]
;;                                {:data s
;;                                 :score (score query (.toLowerCase s))})))))))
;;
;; (defn fuzzy-search
;;   [query col get-against]
;;   (let [query (to-lower query)]
;;     (take 1
;;       (sort-by :score
;;         (comp - compare)
;;         (filter #(< 0 (:score %))
;;           (for [doc col]
;;             {:data doc
;;              :score (score query (to-lower (get-against doc)))}))))))
;;
;; ;; (defn fuzzy-search-title[query col]
;; ;;   (:data (first (fuzzy-search query col (fn [m] (field))))))
;;
