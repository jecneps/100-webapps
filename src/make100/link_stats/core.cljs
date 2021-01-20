(ns make100.link-stats.core
	(:require [rum.core :as rum]
			  [ajax.core :refer [GET]]
			  [cljs.reader]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn splitUrl []
  (clojure.string/split 
    (.. js/window -location -href)
    #"#"))

(defn getBaseUrl []
	(first (splitUrl)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def linkStatState (atom
	{:blogs {
			"Samzdat" nil
			;"The Last Psychiatrist" nil
			"Melting Asphalt" nil}
	 :selected-blog "Samzdat"
	 :host nil
	 :data-loaded false
	 :width 0}))

(defn resetLinkStatState [st]
	(assoc st :host nil :selected-blog "Samzdat"))

(def DEFAULT_STEP 3)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn error-handler [{:keys [status status-text]}]
  (.log js/console (str "something bad happened: " status " " status-text)))

(def eventHandlingMap (atom {}))

(defn registerEventHandler [event handler]
	(reset! eventHandlingMap (assoc @eventHandlingMap event handler)))

(defn send-event [event data]
	(let [em @eventHandlingMap]
		((get em event) data)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(registerEventHandler :blog-selected 
					  (fn [{blog :blog}]
							(let [st @linkStatState]
								(reset! linkStatState (assoc st 
															:selected-blog blog
															:host nil)))))

(registerEventHandler :host-selected
					  (fn [host]
					  		(let [st @linkStatState]
					  			(reset! linkStatState (assoc st :host host)))))

(registerEventHandler :reset-host
					  (fn [_]
					  		(reset! linkStatState (assoc @linkStatState :host nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def PATH_MAP {
	"Samzdat" "/data/blogData/Samzdat.clj"
	;"The Last Psychiatrist" "/data/blogData/Tlp.clj"
	"Melting Asphalt" "/data/blogData/Simler.clj"})



(def DUMMY_DATA (into {} (map (fn [i] [(str "blog" i) (* i 5)]) (range 10))))


(defn dataHandler [response]
	(->> (cljs.reader/read-string response)
		 (map :title)
		 (take 10)))

;(GET SAMZDAT_PATH {:handler dataHandler})

(defn loadData [blog response]
	(let [posts (cljs.reader/read-string response)
		  state @linkStatState]
		  (println "in load data")
		 (as-> (assoc-in state [:blogs blog] posts) $
		 	  (assoc $ :data-loaded true)
		 	  (reset! linkStatState $))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn calculateStep [maxData nameWidth]
	(let [windowWidth (. js/window -innerWidth)
	  	  maxWidth (- windowWidth nameWidth 400)]
	  	  (if (< maxWidth (* maxData DEFAULT_STEP))
	  	  	(/ maxWidth maxData)
	  	  	DEFAULT_STEP)))

(defn mergeLinkMap [m1 m2]
	{:cnt (+ (:cnt m1) (:cnt m2))
	 :links (merge-with + (:links m1) (:links m2))})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn posts->linkStats [posts]
	(->> (map :links posts)
		 (apply (partial merge-with mergeLinkMap))
		 (reduce (fn [acc [host {cnt :cnt}]]
		 			(assoc acc host cnt))
		 		 {})
		 (sort-by second)
		 (reverse)))

(defn blog->linkStats [blog]
	(-> (:blogs @linkStatState)
		 (get , blog)
		 (posts->linkStats)))


(rum/defc bar-graph <
	rum/reactive
	{:did-mount (fn [state]
					(reset! linkStatState (assoc @linkStatState 
												:width 
												(-> (. js/document getElementById "names") (. -offsetWidth))))
					state)}
	[data] 
	(let [step (calculateStep (second (first data))
							  (:width (rum/react linkStatState)))]
		[:div {:style  {:display "flex"}}
			[:div {:id "names"}
					(map (fn [host] 
							[:div {:style {:border "grey 1px solid" 
										   :height "30px"
										   :display "flex"
										   :align-items "center"
										   :padding-left "2px"}
								   :on-click (fn [] 
								   				(if (nil? (:host @linkStatState))
								   					(send-event :host-selected host)))} 
								  [:div host]])
					   (map first data))]
			[:div {:style {:height "100%" :background-color "green"}}]
			[:div {:style {:display "flex" :flex-direction "column"}}
					(map (fn [cnt]
							[:div {:style {:display "flex" :flex-direction "row" :align-items "center"}}
								[:div {:style { :height "20px"
											:margin "5px"
											:background-color "#ff6300"
										   	:width (* cnt step)}}]
								[:div cnt]])
					   (map second data))]]))

(defn statsData [blog]
	(let [posts (get-in @linkStatState [:blogs blog])
		  linkStats (->> (map :links posts)
		  				 (apply (partial merge-with mergeLinkMap)))]
		[["Posts: " (count posts)]
		 ["Inbound links: " (reduce (fn [acc [host {cnt :cnt}]]
		 			 						 (if (= host (:host (:link (first posts))))
		 			 						 	(+ acc cnt)
		 			 						 	acc))
		 								 0
		 		 						 linkStats)]
		 ["Outbound links: " (reduce (fn [acc [host {cnt :cnt}]]
		 			 						 (if (not= host (:host (:link (first posts))))
		 			 						 	(+ acc cnt)
		 			 						 	acc))
		 								 0
		 		 						 linkStats)]]))

(defn hostGraphStats [blog host]
	(let [posts (get-in @linkStatState [:blogs blog])]
		(as-> (map :links posts) $
			  (apply (partial merge-with mergeLinkMap) $)
			  (get-in $ [host :links])
			  (reduce (fn [acc [{h :host p :path} cnt]]
			  				(assoc acc (str h p) cnt))
			  		  {}
			  		  $)
			  (sort-by second $)
			  (reverse $))))

(rum/defc blog-stats [blog]
	[:div {:style {:display "flex" :flex-direction "column"}}
		(map (fn [[s n]] [:div (str s n)]) (statsData blog))])

(defn selectedVal [name]
	(let [sel (. js/document getElementById name)]
		(-> (. sel -options)
			(aget , (. sel -selectedIndex))
			(. , -value))))

(rum/defc select [name options]
	[:select {:style {:width "fit-content"}
			  :name name 
		      :id name 
		      :on-change (fn []
	      					(send-event :blog-selected {:blog (selectedVal name)}))}
		(map (fn [option]
				[:option {:value option} option])
			 options)])


;BUG-FOUND problem has to do with init actual sets the value of something (or at least
; the ret val matters), and also the doall map with GET doesn't seem to be evaluating
;BUG_SOLVED height not at 100% is fucky. Not sure why...
(rum/defc link-stats-page < 
	rum/reactive
	{:init (fn [state]
				(swap! linkStatState resetLinkStatState)
				(doall
					(if (not (:data-loaded @linkStatState))
						(map (fn [[name path]]
								(println "about to make GET")
								(GET path {:handler (partial loadData name) :error-handler error-handler}))
							PATH_MAP)))
				state)}
	[]
	[:div {:style {:display "flex" :flex-direction "column" :height "100%" :margin "200px"}}
		[:h1 "This is the Link stat page!"]
		[:p "Select the blog to see a histogram of its links, grouped by top level domain. You can click on a top level domain to see a histogram of links to that domain."]
		(select "blogs" (keys (:blogs (rum/react linkStatState))))
		(blog-stats (:selected-blog (rum/react linkStatState)))
		(if-let [host (:host (rum/react linkStatState))]
			[[:button {:style {:margin "5px" :width "fit-content"}
					   :on-click (fn [_] (send-event :reset-host nil))}"Back to Blog"]
						(bar-graph (hostGraphStats (:selected-blog (rum/react linkStatState)) host))]
			(bar-graph (blog->linkStats (:selected-blog (rum/react linkStatState)))))])

;;TODO: make events update stuff