
; (s/def ::allow-origin string?)
; (s/def ::allow-methods (s/coll-of keyword? :kind set?))
; (s/def ::allow-credentials boolean?)
; (s/def ::allow-headers (s/coll-of string? :kind set?))
; (s/def ::expose-headers (s/coll-of string? :kind set?))
; (s/def ::max-age nat-int?)
; (s/def ::access-control
;   (s/keys :opt-un [::allow-origin
;                    ::allow-methods
;                    ::allow-credentials
;                    ::allow-headers
;                    ::expose-headers
;                    ::max-age]))

; (s/def ::cors-interceptor
;   (s/keys :opt-un [::access-control]))

; (def cors-interceptor
;   (interceptor
;    {:name    ::cors
;    :spec    ::access-control
;    :compile (fn [{:keys [access-control]} _]
;               (when access-control
;                 (let [access-control (cors/normalize-config (mapcat identity access-control))]
;                   )))}))

; (def access-control
;  {:allow-origin "http://localhost"})
; (interceptor
;  {:name    ::cors
;   :spec    ::access-control
;   :enter (fn cors-interceptor-enter
;           [{:as ctx :keys [request]}]
;           (if (or (and (cors/preflight? request)
;                        (cors/allow-request? request access-control)))
;             (let [resp (cors/add-access-control
;                         request
;                         access-control
;                         cors/preflight-complete-response)]
;              (assoc ctx
;                     :response resp
;                     :queue nil))
;             ctx))
;   :leave (fn cors-interceptor-leave
;           [{:as ctx :keys [request]}]
;           (if (and (cors/origin request)
;                    (cors/allow-request? request access-control))
;            (if-let [response (:response ctx)]
;             (assoc ctx
;                    :response
;                    (cors/add-access-control
;                     request
;                     access-control
;                     response)))
;            ctx))})
