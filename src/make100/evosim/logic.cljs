(ns make100.evosim.logic
	(:require [normal-distribution :as nd]))


(defn func->points [f a b n]
	(let [step (/ (- b a) n)]
		(reduce (fn [acc i]
					(let [x (+ a (* i step))]
						(conj acc [x (f x)])))
			    []
			    (range (inc n)))))

(defn integrate [f a b n]
	(let [step (/ (- b a) n)]
		(reduce (fn [acc i] 
				    (let [x (+ a (* i step))]
				    	 (+ acc
				    	    (* (f (+ x (/ step 2)))
				    	   	   step))))
				0
				(range n))))


;;#########################################
;; PROBABILITY
;;#########################################
(defn neg [x]
	(if (< x 0)
		x
		(* -1 x)))

(defn sign [x]
	(cond 
		(> x 0) 1
		(= x 0) 0
		(> 0 x) -1))

(defn fact [n]
	(if (= n 0)
		1
		(reduce * (range 1 (inc n)))))

(defn choose [n k]
	(/ (fact n)
	   (* (fact k) (fact (- n k)))))

(defn bates [n x]
	(* (/ n 
		  (* 2 (fact (- n 1))))
	   (apply + (for [k (range (inc n))]
		   	   		(* (. js/Math pow -1 k)
		   	   		   (choose n k)
		   	   		   (. js/Math pow (- (* n x) k) (- n 1))
		   	   		   (sign (- (* n x) k)))))))

(defn wigner-semicircle [r x]
	(if (< (neg r) x r)
		(* (/ 2 (* (. js/Math -PI) (* r r)))
		   (. js/Math sqrt (- (* r r) (* x x))))
		0))


(defn sn-pdf [x]
	(. (new (.-default nd)) pdf x))

(defn sn-cdf [x]
	(. (new (.-default nd)) cdf x))

(defn truncated-normal [a b u sd x]
	(if (<= a x b)
		(* (/ 1 sd)
		   (/ (sn-pdf (/ (- x u) sd))
		   	  (- (sn-cdf (/ (- b u) sd)) 
		   	  	 (sn-cdf (/ (- a u) sd)))))
		0))



;;###############################################

(defn bates->points [n sampleRate]
	(func->points (partial bates n) 0 1 sampleRate))

(defn scale-points [points scale]
	(map (fn [[x y]]
			 [(* x scale) (* y scale)])
	     points))

(defn translate-points [points dx dy]
	(map (fn [[x y]]
			 [(+ x dx) (+ y dy)])
	     points))

(defn cart->can [points h]
	(map (fn [[x y]]
			 [x (- h y)])
	     points))
