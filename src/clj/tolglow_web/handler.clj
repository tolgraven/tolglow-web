(ns tolglow-web.handler
  (:require
    [tolglow-web.middleware :as middleware]
    [tolglow-web.routes.services :refer [service-routes]]
    [tolglow-web.env :refer [defaults]]

    [reitit.swagger-ui :as swagger-ui]
    [reitit.ring :as ring]
    [reitit.dev.pretty :as pretty]
    [muuntaja.core :as m]
    [ring.middleware.content-type :refer [wrap-content-type]]
    [ring.middleware.webjars :refer [wrap-webjars]]
    [mount.core :as mount]))

(mount/defstate init-app ;makes no sense whatsoever to me that init app at large is coupled to web handler ns??
  :start ((or (:init defaults) (fn [])))
  :stop  ((or (:stop defaults) (fn []))))
;right we should just rename these init-server, server-routes, server...

(mount/defstate app-routes
  :start
  (ring/ring-handler
    (ring/router
     [["/" {:get {:handler (constantly
                            {:status 301
                             :headers {"Location" "/api/api-docs/index.html"}})}}]
       (service-routes)]

     #_{;:reitit.interceptor/transform dev/print-context-diffs ;; pretty context diffs
       ;;:validate spec/validate ;; enable spec validation for route data
       ;;:reitit.spec/wrap spell/closed ;; strict top-level validation
       :exception pretty/exception
       :data {;:coercion reitit.coercion.spec/coercion
              :muuntaja m/instance
              #_:interceptors
              #_[swagger/swagger-feature ;; swagger feature
               (parameters/parameters-interceptor) ;; query-params & form-params
               (muuntaja/format-negotiate-interceptor) ;; content-negotiation
               (muuntaja/format-response-interceptor) ;; encoding response body
               (exception/exception-interceptor) ;; exception handling
               (muuntaja/format-request-interceptor) ;; decoding request body
               (coercion/coerce-response-interceptor) ;; coercing response bodys
               (coercion/coerce-request-interceptor) ;; coercing request parameters
               (multipart/multipart-interceptor)]}}) ;; multipart
    (ring/routes
      (ring/create-resource-handler {:path "/"})
      (wrap-content-type (wrap-webjars (constantly nil)))
      (ring/create-default-handler))))

(defn app []
  (middleware/wrap-base #'app-routes))
