(ns make100.core
    (:require [rum.core :as rum]
      		    [make100.torust3.core :as tttt]
              [make100.link-stats.core :as ls]
              [make100.gpm.core :as gpm]))

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
  [:div {:style {:display "flex" :flex-direction "column"}}
    [:h1 "Welcome to Make100 Webapps!"]
    [:a {:href (path->hash "torustictactoe")} "Click for torus tic tac toe"]
    [:a {:href (path->hash "linkstats")} "Click for Blog Link Stats"]
    [:a {:href (path->hash "gpm")} "Click for Graphical Probabalistic Models"]])

;;#########################################
;; ROUTING CODE
;;#########################################

(def routeMap {
  "home" home
  "torustictactoe" tttt/selectMode
  "linkstats" ls/link-stats-page
  "gpm" gpm/gpmView
  })

;;#########################################

(println (type getRoute))

(defn handleHash []
  (rum/mount ((routeMap (getRoute)))
           (. js/document getElementById "app")))

(. js/window addEventListener "hashchange" handleHash false)

;;#########################################


(handleHash)