(ns tolglow-web.routes.services.graphql
  (:require
    [com.walmartlabs.lacinia.util :refer [attach-resolvers attach-streamers attach-scalar-transformers]]
    [com.walmartlabs.lacinia.schema :as schema]
    [com.walmartlabs.lacinia :as lacinia]
    [com.walmartlabs.lacinia.pedestal :as lp]
    [io.pedestal.interceptor :refer [interceptor]]
    [io.pedestal.http :as http]
    [io.pedestal.http.cors :as pcors]
    ; [mvxcvi.whidbey :as w]
    [taoensso.timbre :as timbre]

    [afterglow.web.routes.show-control :as aweb]

    [tolglow-web.tolglow-interop :as to-aft]
    [thi.ng.color.core :as clr]

    [datascript.core :as d]

    [reagi.core :as reagi]
    [hodur-engine.core :as hodur]
    [hodur-lacinia-schema.core :as hodur-lacinia]
    [clojure.data.json :as json]
    [clojure.walk :as walk]
    [clojure.edn :as edn]
    [clojure.java.io :as io]
    [clojure.pprint :as pprint]
    [ring.util.http-response :refer :all]
    ; [ring.middleware.cors :as cors]
    [clojure.core.async :refer [pub sub chan go-loop go >! <!
                                timeout close! >!! <!! unsub unsub-all]]
    [clojure.spec.alpha :as s]
    [mount.core :refer [defstate]])
  (:import thi.ng.color.core.HSLA))

(defn edn->map [path]
  (-> (io/resource path) slurp edn/read-string))

(defn simplify "Converts all ordered maps nested within the map into standard hash maps, and sequences into vectors, which makes for easier constants in the tests, and eliminates ordering problems."
 [m]
 (walk/postwalk
  (fn [node]
    (cond (map? node) (into {} node)
          (seq? node) (vec node)
          :else node))
  m))


(defn log [& stuff]
  (clojure.tools.logging/info
   "_\n" (clojure.string/join
             "" (pprint/write (simplify stuff) :stream nil)) "_\n")
  stuff)


(defn get-control [ctx args value] {:id 1000 :name "knob" :usable_with ["NUMBER", "XY"]})
(defn get-all [& _] (edn->map "db-init.edn")) ;graphql surves + db storage yeah

; (to-aft/cue-view 0 0 8 8 nil)

(defn get-cue-grid
 [ctx {:keys [cell_offset size] :as args} value]
 ; (timbre/debug ctx args value)
 (timbre/debug args value)
 (let [[x y] cell_offset]
  (to-aft/cue-view x y size size nil)))

(defn get-cue-for-id [_ {:keys [id]} _]
 (timbre/debug id)
  (->> (get-all) :cues
       (filter #(or (= id (:id %))
                    (= id (name (:id %)))))
       first)) ;limit to one hit

(defn- literal-factory [literal-value]
  (fn [ctx args value] literal-value))

(defn gen-now-sin []
 (fn [& _] (Math/sin (.getTime (java.util.Date.)))))

(def now-sin (gen-now-sin))

(def fallback (fn [& _] (constantly {})))
(defn resolver-map []
  (let []
    {:get-app-db get-all
     :get-cue-grid get-cue-grid
     :get-cue-for-id get-cue-for-id
     :get-control get-control
     :resolve-with-show-yada-doo fallback
     :get-active-cue-var fallback
     ; :get-sin #(Math/sin (.now java.util.Date)))
     :get-sin now-sin
     ; :literal literal-factory
     :get-cue-var (constantly {:id 1 :name "Level" :min 0 :max 100}) }))
     ; :get-cue-var (fn [_ {:keys [cue-id var-name]} _] #_etc) }))


; (def sin-events (reagi/events))

; (defn new-reagi-stream [] "Dunno wtf that guy is doing"
;   (fn [ctx args source-stream]
;     (timbre/debug "New stream")
;     (let [new-value @sin-events] ;like I get this possibly blocks waiting for first event but then what? where's the loop??
;       (timbre/debug "Running stream!??")
;       (source-stream new-value))
;    #(timbre/debug "Ended stream")))

; (defn stuff []
;  (reagi/deliver sin-events (now-sin))
;  (println @sin-events))

; (defn reagi-looper []
;  (go-loop [ct 5]
;           (timbre/debug "Looping")
;           (timbre/debug @sin-events)
;           (Thread/sleep 1000)
;           (when (pos? ct) (recur [ct (dec ct)]))))

;;XXX also try as an infinite lazy seq being realized?

(defstate ^{:on-reload :noop} stream-channel
 :start (let [channel (chan)]
         {:channel channel :publication (pub channel :msg-type)})
 :stop (do (unsub-all (:publication stream-channel))
           (close! (:channel stream-channel))))

(defn update-sin-2 [& [end?]]
 (>!! (:channel stream-channel)
      {:msg-type :stream-update :data (if end? nil (now-sin))}))

(declare value-updater)
(defonce init-updater (atom (delay (value-updater))))
(defonce stop (atom (promise)))

(defn value-updater []
 (timbre/debug "Value updater loop starting...")
 (go-loop []
          (update-sin-2)
          ; (timbre/debug "In value updater") ;not seeing as many of these as actual delivered updates. weeird??
          (if (deref @stop 1000 nil)
           (update-sin-2 true) ;informs graphql client stream is ending
           (recur)))
 (timbre/debug "Value updater loop terminated!")
 ;this happens every 10s. only reason I can se is channel in lacinia is buffer size 10
 ;but how does that relate to anything? I mean values are popped off so cant be filling...
 (reset! init-updater (delay (value-updater))))

(defn new-async-stream []
  (timbre/debug "Create subscription fn.") ;this gets recreated every request when compiled-schema is passed as fn and not finished. seems a bit overkill...
  (fn [ctx {:keys [num]} source-stream]
    (timbre/debug "Start subscription.")
    ; (source-stream 0) ;some initial value
    ; (value-updater)
    (reset! stop (promise))
    (force @init-updater) ;and then I guess, reset it if and only if after we end?
    ;XXX this can still end up running multiples...
    (let [{:keys [publication]} stream-channel ;; Take the publication.
            subscription (chan)] ; and either way might be a sep event? setting up sub and actually making it stream...
      (sub publication :stream-update subscription) ;; Start to subscribe the publication.

      (go-loop []
               (when-let [{:keys [data]} (<! subscription)] ;; Wait for event.
                ; (timbre/debug "Subscription received data" data)
                (source-stream data) ;; Notify update to client.
                (recur)))

      #(do ;; Stop subscription.
        (timbre/debug "Stop subscription.")
        (unsub publication :stream-update subscription)
        (close! subscription)
        (deliver @stop true) ;ensures we end updater loop.
        ))))


; (defn class-name->ns-str "Turns a class string into a namespace string (by translating _ to -)"
;   [class-name] (clojure.string/replace class-name #"_" "-"))

; (defn edn-tag->constructor "Takes an edn tag and returns a constructor fn taking that tag's value and building an object from it."
;   [tag]
;   (let [c (resolve tag)]
;     (when (nil? c)
;       (throw (RuntimeException. (str "EDN tag " (pr-str tag) " isn't resolvable to a class")
;                                 (pr-str tag))))
;     (when-not ((supers c) clojure.lang.IRecord)
;       (throw (RuntimeException.
;              (str "EDN tag " (pr-str tag)
;                   " looks like a class, but it's not a record,"
;                   " so we don't know how to deserialize it."))))
;     (let [constructor-name (-> (name tag); Translate from class name "foo.Bar" to namespaced constructor fn "foo/map->Bar"
;                                class-name->ns-str
;                                (clojure.string/replace #"\.([^\.]+$)" "/map->$1"))
;           constructor (resolve (symbol constructor-name))]
;       (when (nil? constructor)
;         (throw (RuntimeException.
;                (str "EDN tag " (pr-str tag) " looks like a record, but we don't"
;                     " have a map constructor " constructor-name " for it"))))
;       constructor)))

; (def memoized-edn-tag->constructor (memoize edn-tag->constructor)) ; This is expensive, so we'll avoid doing it more than once

; (defn default-edn-reader "We use defrecords heavily and it's nice to be able to deserialize them."
;   [tag value]
;   (if-let [c (memoized-edn-tag->constructor tag)]
;     (c value)
;     (throw (RuntimeException.
;              (str "Don't know how to read edn tag " (pr-str tag))))))

; (defn read-record [rec-str]
;  (edn/read-string {:default default-edn-reader} rec-str))
 ; (prefer-method print-method ;setup right print method for default record transformer pr-str...
 ;                clojure.lang.IPersistentMap
 ;                clojure.lang.IDeref)

; (pr-str (clr/hsla 0.5 0.3 0.7))

(defn streamer-map []
 ; {:stream-sin (new-reagi-stream )})
 {:stream-sin (new-async-stream)
  :running (constantly false)
  #_:stream-vars-for-cue #_(constantly {})})

(defn transformer-map [schema]
 (attach-scalar-transformers schema
  {:str-to-clj-rec clr/hsla #_read-record
   :clj-rec-to-str deref #_pr-str}))

(defn attach-interceptors "Merge default interceptors with ours"
 []
 {:interceptors
  (into [(interceptor
          {:name ::logger
           :enter (fn [ctx] (timbre/debug ctx) ctx)
           :leave (fn [ctx] (timbre/debug ctx) ctx)})]
        (lp/default-interceptors compiled-schema options)
        #_(pcors/allow-origin (fn [_] true))
        #_(pcors/allow-origin {:allowed-origins ["http://localhost*"]}) )})

(defstate compiled-schema "Compile the GraphQL schema."
 :start #(-> (edn->map "graphql/schema.edn")
 ; :start (-> (edn->map "graphql/schema.edn")
             (attach-resolvers (resolver-map))
             (attach-streamers (streamer-map))
             (transformer-map) ;sort a simple -> str / edn/read-string
             schema/compile))

(defstate graphql-server "Exposes schema more easier + has GraphiQL web repl built in"
 :start (let [options {:subscriptions true
                       :graphiql true
                       :port 3001}]
         (-> compiled-schema ;we want this to return latest state not regen
             (lp/service-map (merge options
                                    (attach-interceptors)))
             (merge {::http/allowed-origins ["http://localhost:3449" "http://localhost:3001"
                                             "http://localdark:3001" "http://localhost:3001"]
                     ::http/allow-credentials "true"})
             http/create-server
             http/start))
  :stop     (http/stop graphql-server))

; (def meta-db (-> "hodur/schemas.edn" io/resource hodur/init-path))


(defn execute-request [req] ;
  (let [vars nil ctx nil ;delay getting :body and slurping because it was disappearing a lot due to missing headers(??) prob middleware fucking us up...
        slurped (try (slurp (:body req)) (catch Throwable t req))
        final   (try (get (json/read-str slurped) "query") (catch Throwable _))] ;attempt to extract value from map
    (-> (lacinia/execute (compiled-schema) (or final slurped) vars ctx) ;fall back to whatever step didnt fail...
        log
        (json/write-str))))

