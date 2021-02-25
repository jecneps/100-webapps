(ns make100.evosim.core
	(:require [rum.core :as rum]
			  [js-combinatorics :as combo]
			  [make100.evosim.test :as test]
			  [make100.evosim.logic :as logic]))

;; {0 n 1 n1 2 n2...}
(defn dummy-pop [width cap]
	(as-> (map #(vector % 0) (range 0 360 width)) $
		  (into {} $)
		  (reduce (fn [acc _] 
		  	    		(let [n (quot (. js/Math round (* 360 (rand))) width)]
		    	  			 (assoc acc (* n width) (inc (get acc (* n width)))))) 
		 		  $ 
		 		  (range cap))))
	

;#############################################;

(test/testAll)

(defn canvas-update[state]
	(let [[id _ _ draw] (:rum/args state)
		 canvas (. js/document getElementById id)
		 ctx (. canvas getContext "2d")]
		 (draw ctx canvas))
		 state)

(rum/defc canvasElem <
	{:did-mount canvas-update
	 :after-render canvas-update}
	[id w h draw]
	[:canvas {:id id :width w :height h}])

(defn do-with-args 
	([f g]
		(fn [& args]
			(do
				(apply f args)
				(apply g args)
				nil)))
	([f g & more]
		(fn [& args]
			(do
				(apply f args)
				(apply g args)
				(doall (map (fn[h] (apply h args)) more))
				nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn box [x y w h ctx canvas]
	(set! (. ctx -fillStyle) "#ff6300")
	(. ctx fillRect x y w h))

(defn points->curve [points ctx canvas]
	(let [[x y] (first points)]
		 (. ctx beginPath)
		 (. ctx moveTo x y)
		 (doall (map (fn [[x y]]
		 				 (. ctx lineTo x y))
		 			 points))
		 (. ctx stroke)))

(defn clear [ctx canvas]
	(. ctx clearRect 0 0 (. canvas -width) (. canvas -height)))


(defn polar-graph-axis [cx cy rStep rMax thetaStep color ctx canvas]
	(set! (. ctx -strokeStyle) color)
	(doall
		(map (fn [r]
				(. ctx beginPath)
				(. ctx arc cx cy r 0 (* 2 (. js/Math -PI)))
				(. ctx stroke))
			 (range rStep (inc rMax) rStep)))
	(doall
		(map (fn [theta]
				(. ctx beginPath)
				(. ctx moveTo cx cy)
				(let [[x y] (logic/polar->cart [rMax (logic/deg->rad theta)])] 
					(. ctx lineTo (+ cx x) (+ cy y)))
				(. ctx stroke))
		 	 (range 0 360 thetaStep))))

; data [0 306 180...]
(defn fill-hue-graph [cx cy rMin rMax thetaW pop popCap ctx canvas]
	(let [scale #(* % (/ (- rMax rMin) popCap))]
		(doall
			(map (fn [[deg popCnt]]
					(set! (. ctx -fillStyle) (logic/hsl->str [deg 1 0.5]))
			  		(. ctx beginPath)
			  		(let [[x y] (logic/polar->cart [rMin (logic/deg->rad deg)])]
			  			(. ctx moveTo (+ cx x) (+ cy y)))
		  			(. ctx arc cx cy rMin (logic/deg->rad deg) (logic/deg->rad (+ deg thetaW)))
		  			(let [[x y] (logic/polar->cart [(+ rMin (scale popCnt)) (logic/deg->rad (+ deg thetaW))])]
			  			(. ctx lineTo (+ cx x) (+ cy y)))
		  			(. ctx arc cx cy (+ rMin (scale popCnt)) (logic/deg->rad (+ deg thetaW)) (logic/deg->rad deg) true)
		  			(let [[x y] (logic/polar->cart [rMin (logic/deg->rad deg)])]
			  			(. ctx lineTo (+ cx x) (+ cy y)))
		  			(. ctx fill))
				 pop))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(rum/defc hue-graph [data dataCap scale thetaW]
	(canvasElem "hue-graph" 
		scale 
		scale
		(let [cx (/ scale 2) 
			  cy (/ scale 2) 
			  rStep (/ scale 10) 
			  rMax (/ scale 2) 
			  thetaSetp 60 
			  tW thetaW
			  rMin (/ scale 10)]
			(do-with-args
				clear
				(partial polar-graph-axis cx cy rStep rMax thetaSetp "#000000")
				(partial fill-hue-graph cx cy rMin rMax tW data dataCap)))))



(def curve (partial canvasElem "curve" points->curve 500 500))

(defn bates-curve [n x0 y0 scale]
	(curve {:points (-> (logic/bates->points n 50)
		                (logic/scale-points , scale)
		                (logic/translate-points , x0 y0)
		                (logic/cart->can , 500))}))

(defn wigner-curve [r scale]
	(curve {:points (-> (logic/func->points (partial logic/wigner-semicircle r)
							 	  			-5 5 100)
						(logic/scale-points , scale)
						(logic/translate-points , (* 5 scale) 0)
						(logic/cart->can , 500))}))

(defn truncated-normal-curve [a b u sd x0 y0 scale]
	(curve {:points (-> (logic/func->points (partial logic/truncated-normal a b u sd) a b 100)
						(logic/translate-points, x0 y0)
						(logic/scale-points , scale)
						(logic/cart->can , 500))}))

(defn truncated-normal->points [w h ctrl]
	(let [a -1 b 1 u 0 sd ctrl]
		 (-> (logic/func->points (partial logic/truncated-normal a b u sd) a b 100)
		 	 (logic/translate-points , (. js/Math abs a) 0)
			 (logic/scale-points , (/ w 2))
			 (logic/cart->can , h))))

(defn rainbow-bar [offset thetaW w h ctx canvas]
	(let [xStep (. js/Math round (/ w (/ 360 thetaW)))]
		(loop [rectX 0 relX (mod  offset w)]
			(if (< rectX w)
				(let [step (- xStep (mod relX xStep))]
					(do
					  (set! (. ctx -fillStyle) (logic/hsl->str [(* (* (quot relX xStep) xStep) (/ 360 w)) 1 0.5]))
					  (. ctx fillRect rectX 0 step h)
					  (recur (+ rectX step) (mod (+ relX step) w))))))))

(rum/defcs color-drag-bar <
	(rum/local false ::clicked?)
	(rum/local nil ::prevX)
	(rum/local 0 ::delta)
	[state w h thetaW deltaUpdate]
	[[:div {:on-mouse-down (fn [e] 
						     (reset! (::clicked? state) true)
						     (reset! (::prevX state) (. e -clientX)))
			:on-mouse-up (fn [_]
							(reset! (::clicked? state) false)
							(reset! (::prevX state) nil))
			:on-mouse-move (fn [e]
								(if @(::clicked? state)
									(do
										(swap! (::delta state)
											   (fn [dx]
											     (+ dx (- @(::prevX state) (. e -clientX)))))
										(reset! (::prevX state) (. e -clientX))
										(deltaUpdate @(::delta state)))))
			:on-mouse-out (fn [_]
							(reset! (::clicked? state) false)
							(reset! (::prevX state) nil))}
		(canvasElem "fd"
				    w
				    h
				    (partial rainbow-bar @(::delta state) thetaW w h))]])

(rum/defcs fitness-control <
	(rum/local 0 ::bar-delta)
	(rum/local 0 ::vert-ctrl)
	(rum/local 0.1 ::horiz-ctrl)
	[state w h thetaW updateFitness]
	[:div {:style {:display "flex" :flex-direction "column"}}
		[:div {:style {:display "flex" :flex-direction "row"}}
			[:div {:border "1px solid black"}]
			[:div {:style {:padding-bottom (str @(::vert-ctrl state) "px")}}
				(canvasElem "iid"
							w
							h
							(do-with-args
								clear
								(->> (truncated-normal->points w h @(::horiz-ctrl state))
									 (partial points->curve))))]
			[:input {:type "range"
					 :min "0"
					 :man "100"
					 :step "1"
					 :value @(::vert-ctrl state)
					 :style {:height "200px" :appearance "slider-vertical"}
					 :on-input (fn [e] 
					 				(reset! (::vert-ctrl state) (.. e -target -value))
					 				(updateFitness [@(::bar-delta state)
					 							 	@(::vert-ctrl state)
					 							 	@(::horiz-ctrl state)]))}]]
		(color-drag-bar w 
						40 
						thetaW 
						(fn [v]
							(reset! (::bar-delta state) v)
							(updateFitness [@(::bar-delta state)
					 						@(::vert-ctrl state)
					 						@(::horiz-ctrl state)])))
		[:input {:type "range"
				 :min "0.1"
				 :max "2"
				 :step "0.05"
				 :value @(::horiz-ctrl state)
				 :on-input (fn [e] 
					 		 (reset! (::horiz-ctrl state) (.. e -target -value))
					 		 (updateFitness [@(::bar-delta state)
					 		                 @(::vert-ctrl state)
					 						 @(::horiz-ctrl state)]))}]])

(rum/defcs curve-element < (rum/local 1 ::ctrl)
	[state w h]
	[(canvasElem (hash state) 
					w 
					h
					(do-with-args
						clear
						(partial box 0 0 w h)
						(->> (truncated-normal->points w h @(::ctrl state))
							 (partial points->curve))))
	 [:input {:type "range" 
	  		  :min "0.1" 
	  		  :max "2" 
	  		  :step "0.025" 
	  		  :value @(::ctrl state) 
	  		  :on-input (fn [e] (reset! (::ctrl state) (.. e -target -value)))}]])

(rum/defcs ctrl-curve <
	(rum/local 0.1 ::ctrl)
	[state w h ctrlMin ctrlMax step updateCtrl]
	[:div
		(canvasElem (hash state)
					w
					h
					(do-with-args
						clear
						(->> (truncated-normal->points w h @(::ctrl state))
							 (partial points->curve))))
		[:input {:type "range"
				 :min ctrlMin
				 :max ctrlMax
				 :step step
				 :value @(::ctrl state) 
				 :on-input (fn [e]
				 				(reset! (::ctrl state) (.. e -target -value))
				 				(updateCtrl @(::ctrl state)))}]])

(rum/defcs simulation-pop <
	(rum/local 0 ::time)
	(rum/local nil ::timerId)
	(rum/local true ::paused?)
	(rum/local (dummy-pop 30 1000) ::freq)
	[state scale mutate fitness thetaW]
	(let [start (fn [] 
					(reset! (::paused? state) false)
					(reset! (::timerId state) 
							(js/setInterval 
								(fn [] (reset! (::freq state) (logic/repl-dyn @(::freq state) mutate fitness (/ 360 thetaW))))
								500)))
		  stop  (fn []
		  			(reset! (::paused? state) true)
		  			(js/clearInterval @(::timerId state)))
		  reset (fn []
		  			(stop)
		  			(reset! (::freq state) (dummy-pop 30 1000)))]
		[:div 
			(if @(::paused? state)
				[:button {:on-click start} "Play"]
				[:button {:on-click stop} "Pause"])
			[:button {:on-click reset}"Reset"]
			(hue-graph @(::freq state) (apply + (vals @(::freq state))) scale thetaW)]))

(rum/defcs sim-with-controls <
	(rum/local 1 ::mutationVariance)
	(rum/local 0 ::fitnessVariance)
	(rum/local 0 ::fitnessAbsolute)
	[state]
	[:div {:style {:display "flex" :flex-direction "column" :height "100%"}}
		(simulation-pop 1000
						(logic/pdf->mutate-vector (partial logic/truncated-normal 0 1 0.5 @(::mutationVariance state))
												  (/ 360 30))
						(fn [x] (/ x 360))
						30)
		(ctrl-curve 500 1000 0.001 2 0.05 (fn [x] (println "ctrl:" x)))
		(fitness-control 500 1000 30 (fn [x] (println "fit:" x)))])




(rum/defc topLevel []
	(sim-with-controls))