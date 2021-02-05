(ns make100.evosim.core
	(:require [rum.core :as rum]
			  [js-combinatorics :as combo]
			  [make100.evosim.test :as test]
			  [make100.evosim.logic :as logic]))

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
(defn fill-hue-graph [cx cy rMin thetaW data ctx canvas]
	(let [binned (reduce (fn [acc x]
							(assoc acc 
								   (quot x thetaW) 
								   (conj (get acc (quot x thetaW) []) x))) 
						 {} 
						 data)
		  flip (fn [y] (- (. canvas -height) y))]
		(set! (. ctx -strokeStyle) "#ff6300")
		
		(doall
			(map (fn [[n pop]]
					(set! (. ctx -fillStyle) (logic/hsl->str [(* n thetaW) 1 0.5]))
			  		(. ctx beginPath)
			  		(let [[x y] (logic/polar->cart [rMin (logic/deg->rad (* n thetaW))])]
			  			(. ctx moveTo (+ cx x) (+ cy y)))
		  			(. ctx arc cx cy rMin (logic/deg->rad (* n thetaW)) (logic/deg->rad (* (inc n) thetaW)))
		  			(let [[x y] (logic/polar->cart [(+ rMin (count pop)) (logic/deg->rad (* (inc n) thetaW))])]
			  			(. ctx lineTo (+ cx x) (+ cy y)))
		  			(. ctx arc cx cy (+ rMin (count pop)) (logic/deg->rad (* (inc n) thetaW)) (logic/deg->rad (* n thetaW)) true)
		  			(let [[x y] (logic/polar->cart [rMin (logic/deg->rad (* n thetaW))])]
			  			(. ctx lineTo (+ cx x) (+ cy y)))
		  			(. ctx fill))
				 binned))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(rum/defc hue-graph [data]
	(canvasElem "hue-graph" 
				1000 
				1000
				(let [cx 500 cy 500 rStep 50 rMax 500 thetaSetp 60 thetaW 30 rMin 100]
					(do-with-args
						clear
						(partial polar-graph-axis cx cy rStep rMax thetaSetp "#000000")
						(partial fill-hue-graph cx cy rMin thetaW data)))))



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




(rum/defc topLevel []
	(hue-graph (repeatedly 3500 (fn [] (* 360 (rand))))))