(ns cld.slides
  (:require [compojure.handler :as handler]
            [compojure.route :as route]
            [clojure.pprint :as pprint]
            [clojure.java.io :as io]
            [ring.util.response :as response]
            )
  (:use [compojure.core]))




(defroutes main-routes
  (GET "/" [] (io/resource "public/index.html"))
  (route/resources "/"))


(def app (handler/site main-routes))

#_  (do
      (use 'ring.adapter.jetty)
      (defonce server (run-jetty #'app {:port 8000 :join? false})))

