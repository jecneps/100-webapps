(ns make100.evosim.core
	(:require [rum.core :as rum]
			  [js-combinatorics :as combo]
			  [make100.evosim.test :as test]
			  [make100.evosim.logic :as logic]))

;; {0 n 1 n1 2 n2...}
(defn dummy-pop [width]
	(reduce (fn [acc x]
				(assoc acc x (* 360 (rand))))
			{}
			(range 0 360 width)))

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
	(let [scale #(- (* % (/ rMax popCap)) rMin)]
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

(rum/defc hue-graph [data dataCap]
	(canvasElem "hue-graph" 
		500 
		500
		(let [cx 250 cy 250 rStep 25 rMax 250 thetaSetp 60 thetaW 10 rMin 25]
			(do-with-args
				clear
				(partial polar-graph-axis cx cy rStep rMax thetaSetp "#000000")
				(partial fill-hue-graph cx cy rMin rMax thetaW data dataCap)))))



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


(rum/defcs curve-element < (rum/local 1 ::ctrl)
	[state id w h]
	[(canvasElem id 
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

(rum/defcs simulation-pop <
	(rum/local 0 ::time)
	(rum/local nil ::timerId)
	(rum/local true ::paused?)
	(rum/local (dummy-pop 10) ::freq)
	[state mutate fitness]
	(let [start (fn [] 
					(reset! (::paused? state) false)
					(reset! (::timerId state) 
							(js/setInterval 
								(fn [] (reset! (::freq state) (logic/next-generation @(::freq state) mutate fitness 10)))
								500)))
		  stop  (fn []
		  			(reset! (::paused? state) true)
		  			(js/clearInterval @(::timerId state)))
		  reset (fn []
		  			(stop)
		  			(reset! (::freq state) (dummy-pop 10)))]
		[:div 
			(if @(::paused? state)
				[:button {:on-click start} "Play"]
				[:button {:on-click stop} "Pause"])
			[:button {:on-click reset}"Reset"]
			(hue-graph @(::freq state) 360)]))

(rum/defcs sim-with-controls <
	(rum/local 0 ::mutationVariance)
	(rum/local 0 ::fitnessVariance)
	(rum/local 0 ::fitnessAbsolute)
	[]
	())




(rum/defc topLevel []
	(simulation-pop identity #(/ % 360)))

;;TODO scaling didn't work, set's things backwards. Figure out how to put a population cap on drawing the graph.