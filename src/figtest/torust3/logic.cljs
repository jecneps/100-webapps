(ns figtest.torust3.logic
	(:require [cljs.core.match :refer-macros [match]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def LEN 3)

(defrecord LogicCell [icon winner?])

(defrecord ViewCell [icon winner? focus? dist])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;needs to be a vec for associative access methods
(defn newBoard [cell]
	(into [] (map #(into [] %) (partition LEN (repeat (* LEN LEN) cell)))))

(defn emptyBoard []
	(newBoard (LogicCell. :empty false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INDEX GENERATORS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn allInd []
	(for [r (range LEN)
		  c (range LEN)]
		  [r c]))

(defn rowInd []
	(for [r (range LEN)]
		(for [c (range LEN)]
			[r c])))

(defn columnInd []
	(for [c (range LEN)]
		(for [r (range LEN)]
			[r c])))

(defn diagInd []
	(vector (for [i (range LEN)]
				[i i])
			(for [i (range LEN)]
				[i (- LEN i 1)])))

(defn torusDiagInd []
	(let [base [[0 0] [1 0] [2 0]]]
		(concat (map (fn [[r c]] 
					 	(for [i (range LEN)]
							 [(mod (+ r i) LEN) (mod (+ c i) LEN)]))
			 		 base)
				(map (fn [[r c]]
						(for [i (range LEN)]
							[(mod (+ r i) LEN) (mod (- c i) LEN)]))
					 base))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TRANSFORMING FROM IND
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;indexCol are double nested [[r c] [r c] [r c] []]
(defn indColl->cells [board indexColl]
	(map #(get-in board %) indexColl))

(defn indColl->record [k board indexColl]
	(map #(k (get-in board %)) indexColl))

;indexes are triple nested [[[r c],[...],[]],,[[],[],[]],,[[],[],[]]]
(defn iconsFromIndexes [indexes board]
	(map (partial indColl->record :icon board) indexes))
	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VANILA ICON ACCESS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def rows (partial iconsFromIndexes (rowInd)))

(def columns (partial iconsFromIndexes (columnInd)))

(def diags (partial iconsFromIndexes (diagInd)))

(def torusDiags (partial iconsFromIndexes (torusDiagInd)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WINNING LOGIC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn inARow? [coll]
	(let [s (into #{} coll)] ;;lesson-learned: contains? is associative
		(and (= 1 (count s))
			 (not (contains? s :empty)))))



(defn inPlay? [board]
	(some #(= :empty %) (indColl->record :icon board (allInd))))
	

(defn draw? [board]
	(not (inPlay? board)))

;; returns nil if no winner, or the indices
;; of the winning psymbols
(defn boundedWinners? [board]
	(first (filter (fn [indexes] (inARow? (indColl->record :icon board indexes))) 
				   (concat (rowInd) 
						  	(columnInd) 
						  	(diagInd)))))

;;BAD copy and pasted :(
(defn torusWinners? [board]
	(first (filter (fn [indexes] (inARow? (indColl->record :icon board indexes))) 
				   (concat (rowInd) 
						  	(columnInd) 
						  	(torusDiagInd)))))

(def winMap {:bounded boundedWinners? :torus torusWinners?})

(defn gameStatus [board boardType]
	(if ((winMap boardType) board)
		:win
		(if (draw? board)
			:draw
			:in-play)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BOARD MANIPULATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn replaceCell [board index {icon :icon winner? :winner?}]
	(let [{i :icon w? :winner?} (get-in board index)]
		(match [(nil? icon) (nil? winner?)]
			[false false] (assoc-in board index (LogicCell. icon winner?))
			[false true]  (assoc-in board index (LogicCell. icon w?))
			[true false]  (assoc-in board index (LogicCell. i winner?))
			[true true]   board)))

(defn play [board index icon]
	(replaceCell board index {:icon icon}))

(defn annotateWinners [board winners]
	(reduce (fn [accBoard index] 
				(replaceCell accBoard index {:winner? true})) 
			board 
			winners))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONVERSION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn convertBoard [board f]
	(into [] (map (fn [row]
					(into [] (map (fn [inside]
									(f inside))
				  				row)))
		 			board)))

(defn convertIcons->cells [board]
	(annotateWinners (convertBoard board (fn [icon] (LogicCell. icon false)))))

(defn convertCells->icons [board]
	(convertBoard board #(:icon %)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STATE MANIPULATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;TODO
(defn takeTurn [boardType swapPlayers player boardAtom index]
	(println index)
	(let [newBoard (play @boardAtom index player)
		  winners? (winMap boardType)]
		(if-let [winners (winners? newBoard)]
			(reset! boardAtom (annotateWinners newBoard winners))
			(do 
				(reset! boardAtom newBoard)
				(swapPlayers)))))
			
					

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stuff for Torus TicTacToe with fading
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn distance [[x y] [a b]]
	(Math/round (Math/sqrt (+ (Math/pow (Math/abs (- x a)) 2) 
							  (Math/pow (Math/abs (- y b)) 2)))))

(defn global->local [[gridR gridC] board [fr fc]]
	(let [localR (mod gridR LEN) localC (mod gridC LEN)
		  localFR (mod fr LEN) localFC (mod fc LEN)]
		(let [lCell (get-in board [localR localC])]
 			(ViewCell.    (:icon lCell)
 							(:winner? lCell)
 							(= [localR localC] [localFR localFC])
 							(distance [gridR gridC] [fr fc])))))

 (defn totalBoard [board [fr fc] [x y]]
 	(for [r (range (* y LEN))]
 		(for [c (range (* x LEN))]
 			(global->local [r c] board [fr fc]))))






