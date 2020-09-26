(ns tolglow-web.routes.services.graphql
  (:require
    [com.walmartlabs.lacinia.util :refer [attach-resolvers attach-streamers attach-scalar-transformers]]
    [com.walmartlabs.lacinia.schema :as schema]
    [com.walmartlabs.lacinia :as lacinia]
    [com.walmartlabs.lacinia.pedestal :as lp]
    [com.walmartlabs.lacinia.resolve :as res]
    [io.pedestal.interceptor :refer [interceptor]]
    [io.pedestal.http :as http]
    [io.pedestal.http.cors :as pcors]
    [leona.core :as leona]
    [leona.util :as lutil]
    [clojure.spec.alpha :as s]
    ; [ring.middleware.cors :as cors]
    [taoensso.timbre :as timbre]

    [afterglow.web.routes.web-repl :as web-repl]
    [afterglow.show :as show]
    [afterglow.show-context :refer [*show*]]

    [tolglow-web.tolglow-interop :as to-aft]
    [tolglow-web.utils :as utils]
    [thi.ng.color.core :as clr]

    [datascript.core :as d]

    [hawk.core :as hawk]
    ; [reagi.core :as reagi]
    ; [hodur-engine.core :as hodur]
    ; [hodur-lacinia-schema.core :as hodur-lacinia]
    [clojure.data.json :as json]
    [clojure.walk :as walk]
    [clojure.edn :as edn]
    [clojure.java.io :as io]
    [clojure.pprint :as pprint]
    [clojure.core.async :refer [pub sub chan go go-loop >! >!! <! <!! thread timeout close! unsub unsub-all]]
    [clojure.spec.alpha :as s]
    [mount.core :refer [defstate]])
  (:import thi.ng.color.core.HSLA))

;; TODO SO we've clearly got a lot of messy bullshit here but some of it's semi-fit for purpose
;; clean out the very afterglow-specific crap into somewhere else
;; ideally we make a nice little reusable wrapper thing for async<->graphql
;; and lacinia/leona in general?
;;
;; resolver fns get defined in some other layer so that only that one has to be aware of underlying crap
;; and map of such are defined in a configuration file, similar to (once cleaned up...) tolglow config shiet

(defn edn->map [path]
  (-> (io/resource path)
      slurp
      edn/read-string))

(defn simplify "Converts all ordered maps nested within the map into standard hash maps, and sequences into vectors, which makes for easier constants in the tests, and eliminates ordering problems."
 [m]
 (walk/postwalk
  (fn [node]
    (cond (map? node) (into {} node)
          (seq? node) (vec node)
          :else node))
  m))

(defn log [& stuff]
  (timbre/info
   "_\n" (clojure.string/join
             "" (pprint/write (simplify stuff) :stream nil)) "_\n")
  stuff)

(defn get-all [& _] (edn->map "db-init.edn")) ; well stupid

;***********************
;  Resolver functions
;***********************
(defn get-cue-grid
 [ctx {:keys [cell_offset size] :as args} value]
 (timbre/debug args value)
 (let [[x y] cell_offset]
  (to-aft/cue-view x y size size nil)))

(defn get-cue-for-id [_ {:keys [id]} _]
 (timbre/debug id)
  (->> (get-all) :cues
       (filter #(or (= id (:id %))
                    (= id (name (:id %)))))
       first)) ;limit to one hit

(defn sane-fixture-map "Get rid of redundancy. Should probably go somewhere else..."
 [fixture-or-list]
 (if (sequential? fixture-or-list)
  (map (fn [fixture]
        (-> fixture
            (dissoc :function-map)
            (update :heads    #(map (fn [head] (dissoc head :fixture :function-map)) %))
            (update :channels #(map (fn [ch]   (dissoc ch :head)) %))))
       fixture-or-list)
  (sane-fixture-map [fixture-or-list])))

(defn get-fixtures [_ {:keys [include_heads]} _]
 (sane-fixture-map (show/all-fixtures)))

(defn- literal-factory [literal-value]
  (fn [ctx args value] literal-value))

(defn get-shaders "Should be shared with stream version tho..."
 [ctx {:keys [path] :as args} value]
 (for [kind ["vert" "frag"]
        :let [fname (str kind ".glsl")]]
 {:text (slurp (str path "/" fname)) :kind kind :file_name fname}))

(defn now-sin [& _]
 (-> (java.util.Date.) .getTime Math/sin))

(defn set-eval "Run any code. Great for security"
 [ctx {:keys [code] :as args} value]
 ; (let [code (eval (edn/read-string code))]
 (timbre/debug "Eval on server: " (str code))
 (let [code (web-repl/do-eval code :graphql)] ;remember to clean up the session btw
  (str code)))
; (web-repl/do-eval "(afterglow.show/stop!)" :graphql)

(defn afterglow-action "Run some mapped actions"
 [ctx {:keys [kind action arg] :as ehh} value]
 ; (let [code (eval (edn/read-string code))]
 (let [stuff {"cue" {:action {"start" "afterglow.show/add-effect-from-cue-grid!"
                              "stop" "hmm"
                              "toggle" "hmm"}
                    :arg [:x :y]}
              "show" [:action {"start" "afterglow.show/start!"
                               "stop" "afterglow.show/stop!"
                               "toggle" "hmm"}]}
       ; s (str "(apply " (get-in stuff [kind :action action])  ;eh dunno why was doing this=
       ;        " " (clojure.string/join " " arg) ")")
       s (str "(" (get-in stuff [kind :action action])
              " " (clojure.string/join " " arg) ")")
       _ (timbre/debug "Run " s)
       code (web-repl/do-eval s :graphql)] ;remember to clean up the session btw
  (str code)))
; (web-repl/do-eval "(apply afterglow.show/add-effect-from-cue-grid! [0 4])" :graphql)
(def fallback (fn [& _] (constantly {})))

(defn resolver-map []
  (let []
     ; :literal literal-factory
    {:get-cue-grid       get-cue-grid
     :get-cue-for-id     get-cue-for-id
     :get-fixtures       get-fixtures
     :get-active-cue-var fallback
     :get-sin            now-sin
     :get-shaders        get-shaders
     :eval-code          set-eval ;run code on server. like repl thing, also to run lots of of basic actions on server... but obviously not very good in long run hah tho if auth for web why not. maybe can rinse code from java interop and any file etc actions... on other end. wait this is the other end lol
     :afterglow-action   afterglow-action ;map some keys to common actions
     ; :set-afterglow      set-afterglow-action ;more specialized, maybe create an ns aggregating most common actions so dont need to ugh resolve? dunno
     ; could return like "and here's the simplified state change, you apply that to your app-db k?"
      }))


(defstate ^{:on-reload :noop} stream-channel            ;; Master ch for use with publication topics
 :start (let [ch (chan)]
         {:channel ch
          :publication (pub ch :msg-type)})
 :stop (do (unsub-all (:publication stream-channel))    ;; Unsubscribe any dangling subscriptions
           (close!    (:channel     stream-channel))))

(defn stream-update [ch data]
 (>!! ch {:msg-type :stream-update :data data}))

(defn put-topic [topic data]
 (>!! (:channel stream-channel) {:msg-type topic :data data}))

; other ways (?):
; -just create updater, let it block on >!! until needed...
; or talk to (to wrap up) updater by chan as well obviously...doesnt need recreating and al that
; use atom and watch-fn to get updated values?

(defn value-updater [ch update-fn freq-ms finish-promise]
 (timbre/debug "Value updater loop starting.")
 (let [cleanup (fn []
                (timbre/debug "Value updater loop terminating.")
                (stream-update ch nil))] ;send nil to stop subscription
  (loop [finished false] ;also guess this should be a new thread and not fuck up thread pool?
   (if finished     ;no need for go anyways.
    (cleanup)       ;and cant park cause not the put that gets stuck
    (and (stream-update ch (update-fn)) ;returns false if channel closed, so dont recur then
         (recur (deref @finish-promise freq-ms false)))))))


(defn get-stream-with-polling "Takes an updater fn, which determines by itself how often to run so bit misleading, as we're not the ones polling.
                               and takes a promise letting it know when to end/run internal cleanup?
                               I guess this was my first attempt and get-stream below is tidier and anyways it's the passed fn that determines how to feed us so..."
  [updater]
 (let [finish  (atom (promise))
       init    (atom (delay (thread (updater finish)))) ; so here we spawn thread but upon reset not?
       clients (atom 0)] ; this is the whole reason for the thing I guess? multiple clients not meaning multiple pollings
  (timbre/debug "Create subscription fn.") ;this gets recreated every request when compiled-schema is passed as fn and not finished. seems a bit overkill...
  (fn [ctx {:keys [num]} source-stream] ;ok so a true solution would stuff stuff in ctx or?
   (timbre/debug "Start subscription.")
   (swap! clients inc)
   (force @init) ; well it's nice this only triggers once...
   (let [{:keys [publication]} stream-channel ;; Take the publication.
         subscription (chan)] ; and either way might be a sep event? setting up sub and actually making it stream...
    (sub publication :stream-update subscription) ;; Start to subscribe the publication.

    (go-loop []
             (when-some [{:keys [data]} (<! subscription)] ;; Wait for event - nil cancels
              (source-stream data) ;; Notify update to client.
              (recur)))

    (fn [] ; better use fn not # also remember can name them!!
      (timbre/debug "Stop subscription.")
      (unsub publication :stream-update subscription)
      (close! subscription)
      (swap! clients dec)
      (when-not (pos? @clients)
       (deliver @finish true) ;ensures we end updater loop.
       (reset! finish (promise)) ;likely to work as updater mostly waiting but TERRIBLE code, could well skip!
       (reset! init (delay (updater finish))))))))) ;let value-updater restart when needed


(defn get-stream [{:keys [publication]} id attach-updater cleanup-updater]
 (timbre/debug "Create subscription fn" (name id))
 (fn [ctx args send-fn]
  (timbre/debug "Start subscription"    (name id))
  (let [subscription (chan)]              ;; New ch for our sub
   (sub publication id subscription)      ;; Start subscription to publication.
   (when attach-updater (attach-updater)) ;; side effects as al hell lol - run any provided fn to start updater

   (go-loop [] ;thought maybe add timeout to be safe, but realized breaks show paused.
            (when-some [{:keys [data]} (<! subscription)] ;; Park until event - nil cancels
             (send-fn data)                               ;; Notify update to client.
             (recur)))

   #(do
     (timbre/debug "Stop subscription"  (name id))
     (when cleanup-updater (cleanup-updater))
     (unsub publication id subscription)
     (close! subscription)))))            ;; Line for line just reverse of first 4 lines of fn


(defonce stream-sin (get-stream-with-polling
                     (partial value-updater
                              (:channel stream-channel)
                              now-sin 1000)))

(defn from-show [& path] (reduce #(-> %1 %2) *show* path))
(defn calculated-values   [& _] @(:calculated-values *show*))


(defn afterglow-stream "Create new stream updating by hooking into afterglow render pipeline as frame-fn"
 [id show-path & [transform]] ;i guess should also keep last state so only sends updates..
 (let [[frame-count how-often] [(atom 0) 3]
       last-sent (atom {})
       updater (fn [_]
                (when (>= (swap! frame-count inc) how-often) ;throttle...
                 (let [new-data (apply from-show show-path)
                       [changes old same] (clojure.data/diff new-data @last-sent)

                       ; solution: make own diff, dont look inside records or bottom-level shit (eg lookat ordinary vec)
                       proper (->> (doall (for [[id {:keys [color] :as fixture}] changes]
                                            {id (merge {:id id}
                                                       (dissoc fixture :color)
                                                       (when (instance? thi.ng.color.core.HSLA color)
                                                         {:color color}))})) ;shouldnt let this awful workaround stop me from figuring out what ive broken in fade-colors
                                  (into {})
                                  (utils/remove-nils))]
                  ; (def wtf {:new changes :old old :common same})
                  ; (def ouch proper)
                  ; (def last-sent @last-sent)
                  ; (def new-data new-data)
                  (when proper ;also prob check whether anyone curr listening
                   ;; with sente we just get a ch each side and roll. 'd be way to go for static basic queries eh
                   (put-topic id (cond-> proper #_changes #_new-data ;wah half colors n shit crapping out serializer, rethink
                                  transform (transform))))
                  (reset! last-sent proper))
                 (reset! frame-count 0)))]
  (get-stream stream-channel id
              #(show/add-frame-fn!   updater) ;passing these means delaying them until show's created (presumably)
              #(do
                (put-topic id nil) ;cancel subscribing go-loop
                (show/clear-frame-fn! updater)))))


(def stream-calculated (afterglow-stream :calculated-values [:calculated-values deref]
                                         (comp (partial filter #(< 2 (count %))) vals))) ;before sending, filter out vals that only mirror key (:id id)

(defn stream-shaders "Poll files in resources, serve when updated..."
 [path] ;base path pretty given, should rather be pair?
 (let [watcher (atom nil)
       start-polling
       (fn []
        (reset! watcher
                (hawk/watch!
                 [{:paths [path]
                   :filter hawk/modified? ;this POS includes dirs UGH duplicates.
                   :handler (fn [_ e]
                             (when-not (.isDirectory (:file e))
                              (timbre/debug "Shader changed: " e)
                              (let [fname (.getName (:file e))
                                    [kind _] (clojure.string/split fname #"\.")
                                    text (slurp (:file e))]
                               (put-topic :shaders {:text text :kind kind :file_name fname}))))}])))] ;hmm how deal with name etc?
  (get-stream stream-channel :shaders start-polling #(hawk/stop! @watcher))))


(def stream-shaders-def (stream-shaders "resources/public/glsl")) ;dunno just recreate bit less

(defn streamer-map "Weird shit atm have to reload before :fixture-state works" []
 {:stream-sin          stream-sin
  :fixture-state       stream-calculated ;investigate why need reload. *show* something or?
  :stream-shaders      stream-shaders-def
  ; :stream-afterglow-events probably... wont have individual for everything
  #_:running             #_(constantly false) })

;due to idiotic "neither is preferred":
; (extend-protocol IPrintWithWriter ;someones tip for cljs. could put in lib? but for now prefer manually...
;   mycool.newObj
;   (-pr-writer [new-obj writer _]
;     (write-all writer "#myObj \"" (:details new-obj) "\"")))
(prefer-method print-method clojure.lang.IDeref clojure.lang.IPersistentMap)
(prefer-method print-method clojure.lang.IDeref java.util.Map)
; prob cause of digg bug and thing that varies for eg bloom, sparkle, how fades are broken now... somehow shit turns to java???
; or im just talking nonsense
(defn attach-transformer-map [schema] ; obviously silly thing here is while we're serializing should do to something not text right...
 (attach-scalar-transformers schema
  {:str-to-hsla     clr/hsla
   ; :hsla-to-str     deref
   :hsla-to-str     #(if (instance? thi.ng.color.core.HSLA %)
                      (deref %)
                      (mapv (partial get %) [:h :s :l :a])) ;but trouble if anywhere near as slow as in param? check hmm. also total bs either way im disgusting
   :edn-read-str    edn/read-string
   :pr-str          pr-str}))
   ; :pr-str          #(if (instance? thi.ng.color.core.HSLA %)
   ;                    (deref %)
   ;                    (pr-str %))})) ;but wtf lol now even above not enough??


(defstate attached-schema "Precompile the GraphQL schema."
 :start (-> (edn->map "graphql/schema.edn") ; will rerun everything each request, for dev purposes. (map vs #fn is diff)
             (attach-resolvers (resolver-map)) ;also contains mutations
             (attach-streamers (streamer-map))
             (attach-transformer-map))
 :stop   (timbre/info "About to recompile GraphQL schema."))
; (mount.core/start #'attached-schema)
; (mount.core/stop #'attached-schema)

;; SPEC BULLSHIT FOR TESTING LEONA

(s/def ::none nil?)
(s/def ::i integer?)
(s/def ::qspec (s/keys :req-un [::i])) ;presumably by query they mean args??
(s/def ::file string?)
(s/def ::num number?)
(s/def ::bol true?)
(s/def ::merry-map (s/keys :req-un [::file ::num ::bol])) ;this also bc query name...
(s/def ::search string?)
(s/def ::union-search (s/keys :req-un [::search])) ;this also bc query name...
(s/def ::precious any?)
(s/def ::implementing string?)

(s/fdef resolver-no-arg
        :args ::none
        :ret ::merry-map)

(defn resolver-no-arg "Docstring goes here"
 [ctx args value]
 {:file "File" :num 5.0 :bol true})

(s/fdef resolver-fn-2
        :args ::qspec
        :ret (s/keys :req-un [::file ::num ::bol])
        ; :ret (s/cat :file string? :num number? :bol boolean?)
        ; :ret ::merry-map
        )
(defn resolver-fn-2 "Docstring goes here"
 [ctx {:keys [i]} value]
 {:file "File" :num (+ i 5.0) :bol true})

(defn resolver-fn
 [ctx {:keys [i]} value]
 {:file "File" :num (+ i 5.0) :bol true})

(s/fdef resolver-fn
        :args ::qspec
        ; :args (s/cat :ctx any?
        ;              ; :args (s/keys :req-un [::i]) ;so I what have to break out s/keys??
        ;              :args ::qspec ;so I what have to break out s/keys??
        ;              :value any?)
        :ret ::merry-map)

; (var resolver-fn-2)
; (leona/parse-fspec #'resolver-fn-2)
; (leona/parse-fspec #'resolver-fn)
; (leona/parse-fspec #'resolver-no-arg)

(defn union-resolver
 [ctx {:as args :keys [resolver-search i]} value]
 (let [[r tag] (case resolver-search
                "fdef" [resolver-fn-2 ::merry-map]
                ""     [resolver-fn ::merry-map]
                nil    [resolver-no-arg ::merry-map])]
  ; (res/with-context )
  ; (res/resolve-as)
  (schema/tag-with-type
   (r ctx {:i i} value)
   ; (lutil/clj-name->gql-object-name tag))))
   (lutil/clj-name->gql-name tag))))

; ::lacinia/container-type-name in ctx provided with parent conrete type key

; (defn union-resolver
;  [ctx {:as args :keys [resolver-search i]} value]
;  (let [res (case resolver-search
;             "fdef" resolver-fn-2
;             "normal" resolver-fn)]
;   ; (res/with-context )
;   ; (res/resolve-as)
;   ((res) ctx {:i i} value)))


; (s/def-impl :args/ spec spec)
; (leona/attach-union-query (leona/create) ::union-search ::precious [:resolver-fn-2 ::merry-map] union-resolver)
;     ; (s/explain ::leona/pre-compiled-data (leona/create))
; (-> (leona/create)
;     (leona/attach-union-query ::union-search ::precious [:resolver-fn-2 ::merry-map] union-resolver)
;     #_((partial s/explain ::leona/pre-compiled-data)))
; (leona/attach-query (leona/create) resolver-fn-2) ;also contains mutations
; ; (s/get-spec ::precious)
; (s/get-spec :far/yoda)
; (s/def ::precious any?)
; (s/def ::precious nil)
(defn logg [m] (timbre/debug m) (def m m) m)
; (eval
;  `(s/def ~(keyword (name :far)
;                     (name :yoda)) any?))
; (defmacro atta
;  [resolver]
;  `(if (symbol? ~resolver) (var ~resolver) ~resolver))
; (eval (atta (constantly nil)))
; ; (if (clojure.core/symbol? (constantly nil)) `(var (constantly nil)) (constantly nil))
; (if (clojure.core/symbol? (constantly nil)) `(var (constantly nil)) false)
; (if (clojure.core/symbol? (constantly nil)) true 0)
;; Sorry, frustrations with how far I've yet to go and the reality
;; that getting a job isn't as simple as acing some little test
;; got me acting very strange indeed.
;; I'll have enough experience and know-how soon enough if I get involved in projects
;; and keep going at it and don't act like a dick. Cheers!
;;
; (eval `(let [resolver# (fn [ctx query value]
;                          {:file "File" :num 5.0 :bol true})]
;     (symbol resolver#)))

; (let [resolver (fn [ctx query value]
;                    {:file "File" :num 5.0 :bol true})
;         compiled-schema (-> (leona/create)
;                             (leona/attach-query ::qspec ::merry-map resolver)
;                             #_(s/explain :leona/compiled-data)
;                             #_(leona/compile))
;         _ (println compiled-schema)
;         result (leona/execute (leona/compile compiled-schema) "query test($input: InputInput!) { test(input: $input) { num }}" {:input {:num 1, :nums [2 3]}} {})]
;     (= 6 (get-in result [:data :test :num])))
; (do `(symbol? (constantly nil)))
; (leona/attach-query (leona/create) ::qspec ::merry-map (constantly nil)  )
; ; (def fux (constantly nil))
; (leona/attach-query (leona/create) ::qspec ::merry-map resolver-fn-2)
; (leona/attach-query (leona/create) ::qspec ::merry-map nil)
(defstate schema "Compile the GraphQL schema from Leocina and Leona"
 :start #(-> (leona/create) ; will rerun everything each request, for dev purposes
             (leona/attach-query resolver-fn-2) ;also contains mutations
             (leona/attach-query resolver-fn) ;also contains mutations
             ; (leona/attach-query nil) ;also contains mutations
             ; (leona/attach-query resolver-no-arg)
             ; (leona/attach-query ::qspec ::merry-map resolver-fn :op-name "QUERYH" :doc "docstring atempt!") ;also contains mutations
             ; (leona/attach-query ::qspec ::merry-map (constantly nil) :op-name "QUERYH" :doc "docstring atempt!") ;also contains mutations
             (leona/attach-query ::qspec ::merry-map resolver-fn :doc "docstring atempt!") ;also contains mutations
             #_(leona/attach-union-query ::union-search ::precious
                                       [:resolver-fn-2 ::merry-map] union-resolver)
             (leona/attach-object ::implementing :input? true)
             ; (leona/attach-union-query :precious [:resolver-fn-2 ::merry-map])
             ; yeah. how could you generalize extracting union types from queries?
             (logg)
             (leona/attach-schema attached-schema))
 :stop   (timbre/info "About to recompile GraphQL leona spec schema."))
; (mount.core/stop #'schema)
; (mount.core/start #'schema)

; (defmacro attach-union-query
;   "Adds a query and associated union into the provided pre-compiled data structure.
;    List of results-specs only for typing purposes and must be defined independently."
;   [m query-spec result-spec results-specs resolver & {:keys [doc]}]
;   `(-> ~m
;        (update :unions assoc (util/clj-name->gql-object-name ~result-spec)
;                {:members (mapv util/clj-name->gql-object-name ~results-specs)})
;        (attach-internal ~query-spec ~result-spec (var ~resolver) :queries :doc ~doc)))

(defn get-compiled "Universal extract compiled schema, since path differs"
 [executor]
 (case executor
  :leona (leona/compile (schema))
  :lacinia (:compiled (leona/compile (schema)))))

; (leona/execute (get-compiled :leona) "{ retspec(i: 3) { file } }")
; (leona/execute (get-compiled :leona) "{ resolverfn2(i: 3) { file } }")
; (leona/execute (get-compiled :leona)
;   "{ precious(search: \"\") { ... on ResolverNoArg { file } } }")
; (leona/execute (get-compiled :leona)
;   "{ resolvernoarg { file } }")

;; "rules":
;; currently - both query and type take name of results-spec
;;
;; optimal:
;; TYPE name from results-spec keyword
;; QUERY name something else - manual or from fdef
;;       worst case name after type...
;;
;; always:
;; Either have at least a results-spec OR a fn name
;; Confusing then is having to name TYPE from results-spec named after fn
;;
;; fdef:
;; --possibly inline ret... or should that be disallowed?
;;   Though otherwise not much point in grabbing from fn haha.
;; * if is then must def to keyword-spec to follow reg flow
;; * if not then don't rereg for no reason!
;;   Name query after fn name
;;
;; regular:
;; --allow naming query.



(defn attach-interceptors "Merge default interceptors with ours"
 [options schema]
 (let [logger (interceptor
               {:name ::logger
                :enter (fn [ctx]
                        (timbre/debug (str (get-in ctx [:request :body])))
                        ctx) ;these aint workin too good?
                :error (fn [& what]
                        (timbre/error "something badwrong log interceptor!!"))})]
 (assoc options :interceptors
  (-> (lp/default-interceptors schema options)
      (lp/inject logger :before ::lp/json-response)))))



(defstate graphql-server "Exposes schema more easier + has GraphiQL web repl built in"
 :start  (let [schema (get-compiled :lacinia)
               options {:subscriptions true ;maybe its angry at this? no support leona
                        :graphiql      true
                        :port          3001}
               service-opts (attach-interceptors options schema)
               pedestal-options {::http/allowed-origins (constantly true)
                                 ::http/allow-credentials true}]
          ; (def service-opts service-opts)
          (-> schema ;we want this to return latest state not regen
              (lp/service-map service-opts)
              (merge pedestal-options)
              http/create-server ;jetty throws EofException on page reload heh - fix?
              http/start))
  :stop  (http/stop graphql-server)) ;XXX BROKEN ON LOAD - needs reloading ns after start. Get on that lib for ordering events...

; (mount.core/stop #'graphql-server)
; (mount.core/start #'graphql-server)
; (def meta-db (-> "hodur/schemas.edn" io/resource hodur/init-path))

(defn execute-request "Not used currently, using lacinia-pedestal" [req] ;
  (let [vars nil ctx nil ;delay getting :body and slurping because it was disappearing a lot due to missing headers(??) prob middleware fucking us up...
        slurped (try (slurp (:body req))
                     (catch Throwable t req))
        final   (try (get (json/read-str slurped) "query")
                     (catch Throwable _))] ;attempt to extract value from map
    #_(-> (lacinia/execute (get-compiled :lacinia)
                         (or final slurped)
                         vars ctx) ;fall back to whatever step didnt fail...
        log
        (json/write-str))))

;; below yanked from some internets when I thought I could jvm js or whatever?

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

