(ns make100.gpm.core
	(:require [rum.core :as rum]))


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

(defn remove-index [v i]
	(into (subvec v 0 i) (subvec v (inc i) (count v))))

;return-> {:n num "name" # "name" # ...}
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
		  						acc
		  						(conj acc (remove-index item i))))
		  				  []
		  				  (:data sample))]
		{:n (count newData)
		 :labels (remove #(= label %) (:labels sample))
		 :labelCnt (dec (:labelCnt sample))
		 :data newData}))

;;######################################
;;
;;######################################
;example structure of sampleTree


(defn condition-event [sampleId [label value :as id]]
	(let [tree @sampleTree
		  sample (get-in tree sampleId)]
		(->> (assoc-in tree (conj sampleId id :sample) (conditionSample sample label value))
			 (reset! sampleTree))))

(defn remove-sample-event [sampleId]
	(->> (update-in @sampleTree (butlast sampleId) dissoc (last sampleId))
		 (reset! sampleTree)))

;;######################################
;; UI COMP
;;######################################

(rum/defc bar [n color]
	[:div {:style {:background-color color
							   :padding "2px 0"
							   :display "flex"
							   :justify-content "center"
							   :width n}}
					[:div {:style {:width "fit-content"}}
						(str n "%")]])

(rum/defc ratioBar [label n]
	[:div {:style {:display "flex" :margin "10px 0"}}
		[:div (str label ":")]
		[:div {:style {:display "flex"}}
				(bar n "green")
				(bar (- 100 n) "red")]])

(rum/defc ratios [data]
	(let [n (:n data)]
		[:div
			(map (fn [[label cnt]]
					(ratioBar label (. js/Math round (* 100 (/ cnt n)))))
				 (:ratios data))]))

(rum/defc dataPoint [point]
	[:div {:style {:display "flex" :flex-direction "column" :margin "5px"}}
		(map (fn [b] 
				[:div {:style {:width "30px" 
							   :height "10px" 
							   :background-color (if b "green" "red")}}]) 
			 point)])

(rum/defc dataBlock [sample]
	[:div {:style {:display "flex" :flex-wrap "wrap" :border "black" "2px" "solid" :width "500px"}}
		(map dataPoint (:data sample))])

(rum/defc samplePen [sample]
	[:div {:style {:display "flex"}}
		(dataBlock sample)
		(ratios (sample->ratios sample))])

;;######################################
(def DUMMY_TREE {:node 1
		 :children [{:node 1
					 :children [{:node 1}
					 			{:node 1}
					 			{:node 1
					 			 :children [{:node 1}
								 			{:node 1}
								 			{:node 1}]}]}
		 			{:node 1
					 :children [{:node 1}
					 			{:node 1}
					 			{:node 1}]}
		 			{:node 1
					 :children [{:node 1}
					 			{:node 1}]}]})

(rum/defc testView [f]
	[:div {:style {:width "30px" :height "30px" :background-color "#f36300"}}])

(rum/defc nodeView [tree]
	[:div.node {:style {:display "flex" :flex-direction "column" :align-items "center"}}
    	(if-let [children (:children tree)]
    		(seq 
    			[[:div.container.inner (testView (:node tree))]
    			    			[:div {:style {:display "flex" :justify-content "space-between"}}
    			    				(map nodeView children)]])
    		[:div.container.leaf (testView (:node tree))])])

(rum/defc treeView [tree]
	[:div.tree
		(nodeView tree)])

;;######################################

(rum/defc node [children]
	[:div.node {:style {:display "flex" :flex-direction "column" :align-items "center"}}
		[:div {:style {:width "30px" :height "30px" :background-color "#f36300" :margin-bottom "30px"}}]
		[:div {:style {:display "flex" :justify-content "space-between"}} children]])

(rum/defc treeTest []
	[:div.tree (node (seq
					[(node (seq 
							[(node nil)
							(node nil)
							(node nil)
							(node nil)
							(node nil)
							(node nil)]))
				(node (seq 
							[(node nil)
							(node nil)
							(node nil)]))
				(node (seq 
							[(node nil)
							(node nil)
							(node 0)]))]))])

(rum/defc gpmView [] 
	(treeView DUMMY_TREE))

