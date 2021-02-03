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

; tested in repl, looks solid
(defn binary-search-inverse [f start end trg guess epsilon]
	(loop [lo start hi end mid guess]
		(if (< (. js/Math abs (- (f mid) trg))
			   epsilon)
			mid
			(if (< (f mid) trg)
				(recur mid hi (+ mid (/ (- hi mid) 2)))
				(recur lo mid (+ lo (/ (- mid lo) 2)))))))

;;#########################################
;; COLOR
;;#########################################

; h in [0-360]
; s in [0-1]
; l in [0-1]
; output rgb between 0-1
; https://en.wikipedia.org/wiki/HSL_and_HSV
(defn hsl->rgb [[h s l]]
	(let [c 		 (* (- 1 
						   (. js/Math abs (- (* 2 l) 
				  			                 1)))
			   		    s)	
		  hp 		 (/ h 60)
		  x          (* c
				  	    (- 1 
				  	   	   (. js/Math abs (- (mod hp 2) 
				  	   	   					 1))))
		  m (- l (/ c 2))
		  [r1 g1 b1] (cond
						(<= 0 hp 1) [c x 0]
						(<= 1 hp 2) [x c 0]
						(<= 2 hp 3) [0 c x]
						(<= 3 hp 4) [0 x c]
						(<= 4 hp 5) [x 0 c]
						(<= 5 hp 6) [c 0 x])]
		[(+ r1 m) (+ g1 m) (+ b1 m)]))

(defn hsl->rgbInt [v]
	(->> (hsl->rgb v)
		 (map #(* % 255))
		 (map (. js/Math -round))))





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

(defn sample-from [cdf start end]
	(binary-search-inverse cdf start end (rand) 0.5 0.001))

(defn pdf->cdf [pdf start]
	(fn [x] (partial (integrate pdf start x 100))))

;;#################################################

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



(defn sample-tr-norm [a b u sd]
	(-> (pdf->cdf (partial truncated-normal a b u sd) a)
		 (sample-from , a b)))

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

;;#######################################################
;; SIMULATION LOGIC
;;#######################################################

(defn replicateVal [x sf] 
	(as-> (sf) $
	      (* 360 $)
	      (- $ 180)
	      (- x $)
	      (mod $ 360)))
