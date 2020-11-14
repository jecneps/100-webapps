(ns figtest.core
    (:require [rum.core :as rum]
    		  [figtest.torust3.core :as tttt]
    		  [figtest.torust3.logic :as logic]
  			  [figtest.torust3.tests :as tests]))

(enable-console-print!)

(println "This text is printed from src/figtest/core.cljs. Go ahead and edit it and see reloading in action.")

;; define your app data so that it doesn't get over-written on reload

(defonce app-state (atom {:text "Get Fuc!"}))


(rum/defc hello-world []
  [:div
   [:h1 (:text @app-state)]
   [:h3 "Edit this and watch it change!"]
   [:h4 "heheheh"]])

(rum/mount (tttt/selectMode)
           (. js/document (getElementById "app")))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
