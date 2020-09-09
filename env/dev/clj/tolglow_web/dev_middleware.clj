(ns tolglow-web.dev-middleware
  (:require
    [ring.middleware.reload :refer [wrap-reload]]
    ; [selmer.middleware :refer [wrap-error-page]]
    [ring.middleware.cors :refer [wrap-cors]]
    [prone.middleware :refer [wrap-exceptions]]))

(defn wrap-dev [handler]
  (-> handler
      wrap-reload
      ; wrap-error-page
      ; (wrap-cors :access-control-allow-origin [#"localhost"]
      ;            :access-control-allow-methods [:get :put :post :delete])
      ; ^ hopefully allow cross-port requests on localhost...
      (wrap-exceptions {:app-namespaces ['tolglow-web]})))
