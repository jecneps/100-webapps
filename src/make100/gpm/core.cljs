(ns make100.gpm.core
	(:require [rum.core :as rum]
			  [cljs.core.match :refer-macros [match]]))

;;############################



(def globalState (atom {
	:sampleTree nil
	:hiddenModel nil
	:guessed? nil
	}))

(def GREEN "#6aa84f")

(def RED "#cc0000")


;;#######################
;; UTIL
;;#######################

(defn remove-index [v i]
	(into (subvec v 0 i) (subvec v (inc i) (count v))))

(defn probCycle [r i]
	(-> (* r 10)
		(+ i)
		(mod 10)
		(/ 10)))

(defn probCompliment [r]
	(probCycle r 5))




;;################################################################################################################
;;  PROBABILITY LOGIC
;;################################################################################################################

;return-> {:n num :rations {"name" # "name" # ...}}
(defn sample->ratios [sample]
	(->> (reduce (fn [cntAcc dataPoint]
				 	(map (fn [num b] 
				 			(if b 
				 				(inc num)
				 				num)) 
				 		 cntAcc 
				 		 dataPoint))
				 (repeat (:labelCnt sample) 0)
				 (:data sample))
		 (map vector (:labels sample))
		 (into {})
		 (assoc {:n (:n sample)} :ratios)))

(defn conditionSample [sample label value]
	(let [i (.indexOf (:labels sample) label)
		  newData (reduce (fn [acc item] 
		  					(if (= value (nth item i)) 
		  						(conj acc (remove-index item i))
		  						acc))
		  				  []
		  				  (:data sample))]
		{:n (count newData)
		 :labels (remove #(= label %) (:labels sample))
		 :labelCnt (dec (:labelCnt sample))
		 :data newData}))

;;#############################

(defn newChain []
	{"A" {:trigger {#{} (rand)}
		  :needs #{}
		  :next #{"B"}}
	 "B" {:trigger (let [r (rand)]
	 					{#{["A" true]} r
	 	 				 #{["A" false]} (probCompliment r)})
	 	  :needs #{"A"}
	 	  :next #{"C"}}
	 "C" {:trigger (let [r (rand)]
	 					{#{["B" true]} r
	 	 			 	 #{["B" false]} (probCompliment r)})
	 	  :needs #{"B"}
	 	  :next #{}}
	 :roots #{"A"}
	 :type :chain})

(defn newFork []
	{"A" {:trigger (let [r (rand)]
	 					{#{["B" true]} r
	 	 			 	 #{["B" false]} (probCompliment r)})
		  :needs #{"B"}
		  :next #{}}
	 "B" {:trigger {#{} (rand)}
	 	  :needs #{}
	 	  :next #{"C" "A"}}
	 "C" {:trigger (let [r (rand)]
	 					{#{["B" true]} r
	 	 			 	 #{["B" false]} (probCompliment r)})
	 	  :needs #{"B"}
	 	  :next #{}}
	 :roots #{"B"}
	 :type :fork})

(defn newCollider []
	{"A" {:trigger {#{} (rand)}
		  :needs #{}
		  :next #{"B"}}
	 "B" {:trigger (let [r (rand)]
	 					{#{["A" true] ["C" true]} r
	 	 				 #{["A" true] ["C" false]} (probCycle r 2.5)
	 	 			     #{["A" false] ["C" true]} (probCycle r 2.5)
	 	 			     #{["A" false] ["C" false]} (probCompliment r)})
	 	  :needs #{"C" "A"}
	 	  :next #{}}
	 "C" {:trigger {#{} (rand)}
	 	  :needs #{}
	 	  :next #{"B"}}
	 :roots #{"A" "C"}
	 :type :collider})

(defn runGraph [g]
	(loop [nodes (:roots g) computedVars {}]
		(if (empty? nodes)
			computedVars
			(let [nv (map (fn [n] 
							(let [needs (get-in g [n :needs])
								  cvNeeded (into #{} (filter (fn [[k, _]] (contains? needs k)) computedVars))
								  p (get-in g [n :trigger cvNeeded])
								  v (< (rand) p)]
								 [n v])) 
						  nodes)
				  nextNodes (reduce #(into %1 (get-in g [%2 :next])) #{} nodes)]
				(recur nextNodes (into computedVars nv))))))

(defn g->sample [g]
	(->> (runGraph g)
		 (sort-by first) ;TODO make more general
		 (reduce #(conj %1 (second %2)) [])))

(defn graph->sample [g n]
	{:labels ["A" "B" "C"]
	 :labelCnt 3
	 :n n
	 :data (repeatedly n (partial g->sample g))})

(defn randomGraph []
	((rand-nth [newFork newChain newCollider])))

;;################################################################################################################
;; DUMMY DATA
;;################################################################################################################
;;;;;;;;;;;;;;;;;;;;;
;; a sample: {:labels ["bla" "ble"]
;			  :n x
;   		  :labelCnt x
;             :data '([b b b] [f t t] [f f f])}
(def DUMMY_SAMPLE {
	:n 50
	:labels ["A" "B" "C"]
	:labelCnt 3
	:data (map (fn [_] [(< 0.5 (rand)) (< 0.5 (rand)) (< 0.5 (rand))]) (range 50))
	})

(def DUMMY_TREE {:node DUMMY_SAMPLE
		 :children [{:node DUMMY_SAMPLE
					 :children [{:node DUMMY_SAMPLE}
					 			{:node DUMMY_SAMPLE}
					 			{:node DUMMY_SAMPLE
					 			 :children [{:node DUMMY_SAMPLE}
								 			{:node DUMMY_SAMPLE}
								 			{:node DUMMY_SAMPLE}]}]}
		 			{:node DUMMY_SAMPLE
					 :children [{:node DUMMY_SAMPLE}
					 			{:node DUMMY_SAMPLE}
					 			{:node DUMMY_SAMPLE}]}
		 			{:node DUMMY_SAMPLE
					 :children [{:node DUMMY_SAMPLE}
					 			{:node DUMMY_SAMPLE}]}]})

(def DUMMY_PEN {
	:sample DUMMY_SAMPLE
	:sampleId [["A" false]]
	;TODO: variables that have been conditioned on
	})

(def DMT {:node DUMMY_PEN
		  :children {["A" false] {:node DUMMY_PEN
		  						  :children {["B" true] {:node DUMMY_PEN}
		  						  			 ["C" true] {:node DUMMY_PEN}}}
		  			 ["B" true] {:node DUMMY_PEN
		  						  :children {["A" true] {:node DUMMY_PEN}
		  						  			 ["C" true] {:node DUMMY_PEN}}}
		  			 ["C" false] {:node DUMMY_PEN
		  						  :children {["B" true] {:node DUMMY_PEN}
		  						  			 ["A" true] {:node DUMMY_PEN}}}}})

(def DT {:node 1
		 :children [{:node 1
		 			 :children [{:node 1}]}]})

(def INIT_TREE
	{:node {:sample (graph->sample (newChain) 100)
			:sampleId []
			:selectedLabels {}}})

(def sampleTree (atom INIT_TREE))

(defn newTree [sample]
	{:node {:sample sample
			:sampleId []
			:selectedLabels {}}})


;;################################################################################################################
;; EVENTS
;;################################################################################################################
(def eventMap (atom {}))

(defn registerEventHandler [label handler]
	(reset! eventMap (assoc @eventMap label handler)))

(defn sendEvent 
	([label]
		(if-let [handler (get @eventMap label)]
			(handler)
			(println "Event ERROR: label does not exist")))
	([label data]
		(if-let [handler (get @eventMap label)]
			(handler data)
			(println "Event ERROR: label does not exist"))))

;;###################

(registerEventHandler :test (fn [] (println "event triggered!")))

;example structure of sampleTree
;(def DMT {:node $
;		  :children {["A" false] {:node $
;		  						  :children {["B" true] {:node $}
;		  						  			 ["C" true] {:node $}}}
;		  			 ["B" true] {:node $
;		  						  :children {["A" true] {:node $}
;		  						  			 ["C" true] {:node $}}}
;		  			 ["C" false] {:node $
;		  						  :children {["B" true] {:node $}
;		  						  			 ["A" true] {:node $}}}}})

(defn updateSelectedLabels [selectedLabels label value]
	(if-let [s (get selectedLabels label)]
		(assoc selectedLabels label (conj s value))
		(assoc selectedLabels label #{value})))

(defn removeSelectedLabel [selectedLabels [label value]]
	(let [s (disj (get selectedLabels label) value)]
		(if (empty? s)
			(dissoc selectedLabels label)
			(assoc selectedLabels label s))))

(defn condition-event [{sampleId :sampleId [label value :as id] :lv}]
	(let [tree (:sampleTree @globalState)
		  node (get-in tree (conj (into [] (interleave (repeat :children) sampleId)) :node))
		  newNode (update node :selectedLabels updateSelectedLabels label value)
		  newSampleId (conj sampleId id)
		  nextNode {:sample (conditionSample (:sample node) label value)
		  		   :sampleId newSampleId
		  		   :selectedLabels {}}]
		(as-> (assoc-in tree 
					   (conj (into [] (interleave (repeat :children) newSampleId)) :node) 
					   nextNode) $
			  (assoc-in $
			 		   (conj (into [] (interleave (repeat :children) sampleId)) :node)
			 		   newNode)
			  (assoc @globalState :sampleTree $)
			  (reset! globalState $))))

(registerEventHandler :condition-on condition-event)

(defn remove-sample-event [sampleId]
	(let [pathToParent (into [] (interleave (repeat :children) (butlast sampleId)))]
		(as-> (update-in (:sampleTree @globalState) (conj pathToParent :children) dissoc (last sampleId)) $
			  (update-in $ (conj pathToParent :node :selectedLabels) removeSelectedLabel (last sampleId))
			  (assoc @globalState :sampleTree $)
			  (reset! globalState $))))

(registerEventHandler :remove-sample remove-sample-event)

(defn restart []
	(let [state @globalState
		  model (randomGraph)]
		(as-> (assoc state :hiddenModel model) $
			  (assoc $ :sampleTree (newTree (graph->sample model 100)))
			  (assoc $ :guessed? nil)
			  (reset! globalState $))))

(registerEventHandler :restart restart)

(defn makeGuess [guess]
	(reset! globalState (assoc @globalState :guessed? guess)))

(registerEventHandler :guess makeGuess)

;;################################################################################################################
;; UI COMP
;;################################################################################################################

(rum/defc bar [n color onclick]
	(let [[class click] (if onclick ["" onclick] ["disabledBar" (fn [])])]
		[:div {:style {:background-color color
								   :padding "2px 0"
								   :display "flex"
								   :justify-content "center"
								   :cursor "pointer"
								   :width n}
				:on-click click
				:class class}
						[:div {:style {:width "fit-content"}} (if (< 50 n) (str n "%"))]]))

(rum/defc ratioBar [label n sampleId disabled]
	[:div {:style {:display "flex" :margin "10px 0"}}
		[:div (str label ":")]
		[:div {:style {:display "flex"}}
			(let [greenClick (fn [] (sendEvent :condition-on {:sampleId sampleId :lv [label true]}))
				  redClick (fn [] (sendEvent :condition-on {:sampleId sampleId :lv [label false]}))
				  noClick nil
				  [green red] (match [disabled]
								  [#{true false}] [noClick noClick]
								  [#{true}] 	  [noClick redClick]
								  [#{false}]	  [greenClick noClick]
								  [nil]			  [greenClick redClick])]
				(seq [(bar n GREEN green)
					  (bar (- 100 n) RED red)]))]])

(rum/defc ratios [sampleId data selectedLabels]
	(let [n (:n data)
		  disableAll (if (= 1 (count (:ratios data))) #{true false})]
		[:div
			(map (fn [[label cnt]]
					(let [vis (if-let [dis (get selectedLabels label)]
							  		dis
							  		disableAll)]
						(ratioBar label (. js/Math round (* 100 (/ cnt n))) sampleId vis)))
				 (:ratios data))]))

(rum/defc dataPoint [point]
	[:div {:style {:display "flex" :flex-direction "column" :margin "5px"}}
		(map (fn [b] 
				[:div.dataBar {:style {:width "30px" 
									   :height "10px" 
									   :background-color (if b GREEN RED)}}]) 
			 point)])

(rum/defc dataBlock [sample]
	[:div {:style {:display "flex" :flex-wrap "wrap" :border "black" "2px" "solid" :width "500px"}}
		(map dataPoint (:data sample))])

(rum/defc samplePen [{sample :sample sampleId :sampleId selectedLabels :selectedLabels :as s}]
	[:div {:style {:display "flex" :flex-direction "column" :margin "0px 20px"}}
		(if (seq sampleId)
			[:div {:style {:display "flex" :justify-content "start" :align-items "center"}}
					[:div {:style {:text-align "center"}}
						 (let [[l v] (last sampleId)] (str l ": " v))]
					[:button.gButton {:on-click (fn [_] (sendEvent :remove-sample sampleId))}"DELETE"]]
			nil)
		[:div {:style {:display "flex"}}
			(dataBlock sample)
			(ratios sampleId (sample->ratios sample) selectedLabels)]])

;;################################################################################################################
;; TREE UI
;;################################################################################################################


(rum/defc testView [f]
	[:div {:style {:width "30px" :height "30px" :background-color "#f36300"}}])

(rum/defc nodeView [makeView tree]
	[:div.node {:style {:display "flex" :flex-direction "column" :align-items "center"}}
    	(let [children (:children tree)]
    		(if (empty? children)
    			[:div.container.leaf (makeView (:node tree))]
	    		(seq 
	    			[[:div.container.inner (makeView (:node tree))]
	    			    			[:div {:style {:display "flex" :justify-content "space-between"}}
	    			    				(map (partial nodeView makeView) (vals children))]])))])

(rum/defc treeView [tree makeView]
	[:div.tree
		(nodeView makeView tree)])

;;################################################################################################################
;; GRAPH UI
;;################################################################################################################

(rum/defc probTable [label triggers dir]
	(let [dependentLabels (->> (ffirst triggers)
							   (map first)
							   (sort)
							   (into []))]
		[:table {:style {:position "absolute" dir "100%"}}
			[:tr 
				(map (fn [l] [:th l]) (conj dependentLabels label))]
			(map (fn [[s p]]
					[:tr
						(map (fn [[_, v]] 
								(if v
									[:td.t (str v)]
									[:td.f (str v)])) (sort-by first s))
						[:td (str "P(x=true)=" (. js/Math round (* 100 p)) "%")]])
			 triggers)]))


(defn arrow [degrees x y]
	[:div {:style {:transform (str "translate(" x "px, " y "px) rotate(" degrees "deg)")
				   :display "flex" 
				   :flex-direction "column" 
				   :align-items "center"}}
		[:div.arrowHead]
		[:div.arrowBody]])

(rum/defc graphNode [label]
	[:div.graphNode 
		[:div label]])

(rum/defc chainGraph 
	([]
		[:div.demoGraph {:style {:display "flex" :flex-direction "column" :align-items "center"}
						 :on-click (fn [] (sendEvent :guess :chain))}
			(graphNode "C")
			(arrow 0 0 0)
			(graphNode "B")
			(arrow 0 0 0)
			(graphNode "A")])
	([g]
		[:div {:style {:display "flex" :flex-direction "column" :align-items "center"}}
			[:div {:style {:display "flex" :flex-direction "row" :position "relative"}}
				(probTable "C" (get-in g ["C" :trigger]) :left)
				(graphNode "C")]
			(arrow 0 0 0)
			[:div {:style {:display "flex" :flex-direction "row" :position "relative"}}
				(probTable "B" (get-in g ["B" :trigger]) :left)
				(graphNode "B")]
			(arrow 0 0 0)
			[:div {:style {:display "flex" :flex-direction "row" :position "relative"}}
				(probTable "A" (get-in g ["A" :trigger]) :left)
				(graphNode "A")]]))

(rum/defc forkGraph 
	([]
		[:div.demoGraph {:style {:display "flex" :flex-direction "column" :align-items "center"}
			   :on-click (fn [] (sendEvent :guess :fork))}
			[:div {:style {:display "flex" :flex-direction "row"}}
				(graphNode "A")
				(graphNode "C")]
			[:div {:style {:display "flex" :flex-direction "row"}}
				(arrow -30 -10 0)
				(arrow 30 10 0)]
			(graphNode "B")])
	([g]
		[:div {:style {:display "flex" :flex-direction "column" :align-items "center"}}
			[:div {:style {:display "flex" :flex-direction "row"}}
				[:div {:style {:display "flex" :flex-direction "row" :position "relative"}}
					(probTable "A" (get-in g ["A" :trigger]) :right)
					(graphNode "A")]
				[:div {:style {:display "flex" :flex-direction "row" :position "relative"}}
					(probTable "C" (get-in g ["C" :trigger]) :left)
					(graphNode "C")]]
			[:div {:style {:display "flex" :flex-direction "row"}}
				(arrow -30 -10 0)
				(arrow 30 10 0)]
			[:div {:style {:display "flex" :flex-direction "row" :position "relative"}}
				(probTable "B" (get-in g ["B" :trigger]) :left)
				(graphNode "B")]]))

(rum/defc colliderGraph 
	([]
		[:div.demoGraph {:style {:display "flex" :flex-direction "column" :align-items "center"}
						 :on-click (fn [] (sendEvent :guess :collider))}
			(graphNode "B")
			[:div {:style {:display "flex" :flex-direction "row"}}
				(arrow 30 -10 0)
				(arrow -30 10 0)]
			[:div {:style {:display "flex" :flex-direction "row"}}
				(graphNode "A")
				(graphNode "C")]])
	([g]
		[:div {:style {:display "flex" :flex-direction "column" :align-items "center"}}
			[:div {:style {:display "flex" :flex-direction "row" :position "relative"}}
				(probTable "B" (get-in g ["B" :trigger]) :bottom)
				(graphNode "B")]
			[:div {:style {:display "flex" :flex-direction "row"}}
				(arrow 30 -10 0)
				(arrow -30 10 0)]
			[:div {:style {:display "flex" :flex-direction "row"}}
				[:div {:style {:display "flex" :flex-direction "row" :position "relative"}}
					(probTable "A" (get-in g ["A" :trigger]) :right)
					(graphNode "A")]
				[:div {:style {:display "flex" :flex-direction "row" :position "relative"}}
					(probTable "C" (get-in g ["C" :trigger]) :left)
					(graphNode "C")]]]))

(defn graph [g]
	((get {:fork forkGraph :collider colliderGraph :chain chainGraph} (:type g)) g))

;;################################################################################################################

(rum/defc instructions []
	[:div {:style {:width "800px"}}
		[:p "This mini-game is designed to help you build an intuition for simple " 
			[:a {:href "https://en.wikipedia.org/wiki/Bayesian_network"}  "causal models"] 
			"."]
		[:p (str "There are three binary random variables, A B and C. Each \"round\" a random graph will be selected to represent the causal "
			     "relationship between the three variables. Click on the model that you think generated the data and see if you were right!")]
		[:p (str "The secretly chosen graph is used to generate a 100 data samples, shown below. Each square represents a sample, with each horizontal "
			     "bar representing a binary value for a variable (red for false, green for true). Next to each sample is a sidebar that tells "
			     "you overall, how often did a given variable turn out true? Click on the bar to condition on that variable.")]
		[:p (str "By snooping around and exploring how the ratios change based on what variables you condition on, you should be "
				 "able to make a decent guess at what the causal relationship is. Crazy right!")]])



(rum/defc gpmView <
	rum/reactive
	{:did-mount (fn [s]
				(sendEvent :restart)
				s)}
	[]
	[:div {:style {:width "fit-content" :height "fit-content" :margin "auto"}}

		[:div.nonTree

			[:h1 "Causal Bayes Nets!"]
			(instructions)
			[:div {:style {:display "flex"}}
				(chainGraph)
				(forkGraph)
				(colliderGraph)]

			(if-let [guess (:guessed? (rum/react globalState))]
				[:div {:style {:margin "50px" :display "flex" :flex-direction "column" :align-items "center"}}
					(graph (:hiddenModel (rum/react globalState)))
					(if (= guess (:type (:hiddenModel (rum/react globalState))))
						[:div "Nice, you did it!"]
						[:div "Nopes, try again."])
					[:button.gButton {:on-click (fn [_] (sendEvent :restart))} "Restart"]]
				[:div.QMark "?"])]
		(treeView (:sampleTree (rum/react globalState)) samplePen)])

;;######################################


