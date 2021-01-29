(ns make100.core
    (:require [rum.core :as rum]
      		    [make100.torust3.core :as tttt]
              [make100.link-stats.core :as ls]
              [make100.gpm.core :as gpm]
              [make100.evosim.core :as evo]))

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
    [:a {:href (path->hash "gpm")} "Click for Graphical Probabalistic Models"]
    [:a {:href (path->hash "evo")} "Click for Evolution Simulator"]])

;;#########################################
;; ROUTING CODE
;;#########################################

(def routeMap (let [app (. js/document getElementById "app")]
  {
  "home" {:view home :trg app}
  "torustictactoe" {:view tttt/selectMode :trg app}
  "linkstats" {:view ls/link-stats-page :trg app}
  "gpm" {:view gpm/gpmView :trg (. js/document -body)}
  "evo" {:view evo/topLevel :trg app}
  }))

;;#########################################

(println (type getRoute))

(defn handleHash []
  (let [m (routeMap (getRoute))]
    (rum/mount ((:view m))
               (:trg m))))

(. js/window addEventListener "hashchange" handleHash false)

;;#########################################


(handleHash)