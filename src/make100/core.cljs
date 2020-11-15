(ns make100.core
    (:require [rum.core :as rum]
    		  [make100.torust3.core :as tttt]
    		  [make100.torust3.logic :as logic]
  			  [make100.torust3.tests :as tests]))

(enable-console-print!)

;;#########################################
(defn splitUrl []
  (clojure.string/split 
    (.. js/window -location -href)
    #"#"))

(defn getRoute []
  (if-let [r (second (splitUrl))]
    r
    "home"))

(defn getBaseUrl []
  (first (splitUrl)))

(defn path->hash [path]
  (str (getBaseUrl) "#" path))

;;#########################################
;; HOME
;;#########################################

(rum/defc home []
  [:div
    [:h1 "Welcome to Make100 Webapps!"]
    [:a {:href (path->hash "torustictactoe")} "Click for torus tic tac toe"]])

;;#########################################
;; ROUTING CODE
;;#########################################

(def routeMap {
  "home" home
  "torustictactoe" tttt/selectMode
  })

;;#########################################



(defn handleHash []
  (rum/mount ((routeMap (getRoute)))
           (. js/document (getElementById "app"))))

(. js/window addEventListener "hashchange" handleHash false)

;;#########################################


(handleHash)
