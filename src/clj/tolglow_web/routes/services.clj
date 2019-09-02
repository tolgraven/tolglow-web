(ns tolglow-web.routes.services
  (:require
   [reitit.swagger :as swagger]
   [reitit.swagger-ui :as swagger-ui]
   [reitit.ring.coercion :as coercion]
   [reitit.coercion.spec :as spec-coercion]
   [reitit.ring.middleware.muuntaja :as muuntaja]
   [reitit.ring.middleware.multipart :as multipart]
   [reitit.ring.middleware.parameters :as parameters]
   [tolglow-web.routes.services.graphql :as graphql]
   [tolglow-web.middleware.formats :as formats]
   [tolglow-web.middleware.exception :as exception]
   [ring.util.http-response :refer :all]
   [clojure.java.io :as io]))
; (require '[reitit.core :as ri])
; (require '[reitit.ring :as ring])
; (-> (ring/get-router tolglow-web.handler/app-routes)
;     (ri/match-by-name ::ping)
;     :result :get :middleware
;     (->> (mapv :name))

(defn service-routes []
  ["/api"
   {:coercion spec-coercion/coercion
    :muuntaja formats/instance
    :swagger {:id ::api}
    :middleware
    [parameters/parameters-middleware     ;; query-params & form-params
     muuntaja/format-negotiate-middleware ;; content-negotiation
     muuntaja/format-response-middleware  ;; encoding response body
     exception/exception-middleware       ;; exception handling
     muuntaja/format-request-middleware   ;; decoding request body
     coercion/coerce-response-middleware  ;; coercing response bodys
     coercion/coerce-request-middleware   ;; coercing request parameters
     multipart/multipart-middleware]}     ;; multipart
   ; I think somewhere up there it forgets about the binary thing, unless we are explicit...

   ["" {:no-doc true ;; swagger documentation
        :swagger {:info {:title "my-api"
                         :description "https://cljdoc.org/d/metosin/reitit"}}}
    ["/swagger.json" {:get (swagger/create-swagger-handler)}]
    ["/api-docs/*"   {:get (swagger-ui/create-swagger-ui-handler
             {:url "/api/swagger.json"
              :config {:validator-url nil}})}]]

   ["/ping" {:get (constantly (ok {:message "pong"}))}]

   ["/graphql" {:post
                (fn [req]
                  (ok (graphql/execute-request req))
                  ; (graphql/log (:headers req))
                  #_(ok (-> (update req :headers merge
                                  {"Content-Type" "application/octet-stream"}) ;thought that did it but no doesnt actually reach here?
                          graphql/log
                          graphql/execute-request)))}]

   ["/math" {:swagger {:tags ["math"]}}
    ["/plus"
     {:get {:summary "plus with spec query parameters"
            :parameters {:query {:x int?, :y int?}}
            :responses {200 {:body {:total pos-int?}}}
            :handler (fn [{{{:keys [x y]} :query} :parameters}]
                       {:status 200 :body {:total (+ x y)}})}
      :post {:summary "plus with spec body parameters"
             :parameters {:body {:x int?, :y int?}}
             :responses {200 {:body {:total pos-int?}}}
             :handler (fn [{{{:keys [x y]} :body} :parameters}]
                        {:status 200 :body {:total (+ x y)}})}}]]

   ["/files" {:swagger {:tags ["files"]}}

    ["/upload"
     {:post {:summary "upload a file"
             :parameters {:multipart {:file multipart/temp-file-part}}
             :responses {200 {:body {:name string?, :size int?}}}
             :handler (fn [{{{:keys [file]} :multipart} :parameters}]
                        {:status 200
                         :body {:name (:filename file)
                                :size (:size file)}})}}]

    ["/download"
     {:get {:summary "downloads a file"
            :swagger {:produces ["image/png"]}
            :handler (fn [_]
                       {:status 200
                        :headers {"Content-Type" "image/png"}
                        :body (-> "public/img/warning_clojure.png"
                                  (io/resource)
                                  (io/input-stream))})}}]]])
