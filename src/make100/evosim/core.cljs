(ns make100.evosim.core
	(:require [rum.core :as rum]
			  [js-combinatorics :as combo]
			  [make100.evosim.test :as test]
			  [make100.evosim.logic :as logic]))

(test/testAll)


(rum/defc canvasElem <
	{:did-mount (fn [state]
					(let [[id draw _ _ drawArgs] (:rum/args state)
						  canvas (. js/document getElementById id)
						  ctx (. canvas getContext "2d")]
						  (draw ctx drawArgs))
					state)}
	[id draw w h drawArgs]
	[:canvas {:id id :width w :height h}])


(defn box [ctx {}]
	(set! (. ctx -fillStyle) "#ff6300")
	(. ctx beginPath)
	(. ctx rect 100 100 50 50)
	(. ctx fill))

(defn bezier [ctx {}]
	(. ctx beginPath)
	(. ctx moveTo 20 20)
	(. ctx bezierCurveTo 20 100 200 100 200 20)
	(. ctx stroke))

(defn points->curve [ctx {points :points}]
	(let [[x y] (first points)]
		 (. ctx moveTo x y)
		 (doall (map (fn [[x y]]
		 				 (. ctx lineTo x y))
		 			 points))
		 (. ctx stroke)))



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

(defn cruveandaxis [w h ctrl])


(rum/defc topLevel []
	(truncated-normal-curve -3 3 0 0.2 3 0 50))