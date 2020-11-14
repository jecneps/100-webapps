(ns make100.torust3.core
  (:require
  			[rum.core :as rum]
  			[make100.torust3.logic :as logic]
  			[make100.torust3.tests :as tests]
  			[cljs.core.match :refer-macros [match]]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(tests/testAll)

(def root (.getElementById js/document "app"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MAPS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def iconMap {:X "X" :O "O" :empty ""})

;(def viewMap {:torus torusBoardView :bounded boundedBoardView})

(def type->n {:torus [8 8] :bounded [1 1]})

(defn swapPlayers [icon]
	(case icon
		:X :O
		:O :X
		:empty :empty
		:else "SWAP ERROR"))

(defn scale [[srcStart srcEnd] [trgStart trgEnd] src]
	(let [offSet (/ (* (- src srcStart) 
					   (- trgEnd trgStart)) 
					(+ (- srcEnd src)
					   (- src srcStart)))]
		(+ trgStart offSet)))

(def opacityScale (partial scale [0 10] [0 100]))

(defn dist->opacity [d]
	(- 100 (opacityScale d)))

(defn dist->style [d]
	{:opacity (str (dist->opacity d) "%")})

;; anything that gives NaN when modded
(def emptyIndex [:n :n])

(defn local [[r c]]
	[(mod r logic/LEN) (mod c logic/LEN)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(rum/defc pageHeader []
	[:h1 "Come Play Tic-Tac-Toe!"])

(rum/defc restartButton []
	[:button 
		{:on-click (fn [] (rum/mount (selectMode) root))}
		"Restart"])

(rum/defc cell [{icon :icon winner? :winner? focus? :focus? d :dist}
				onClick onFocus offFocus gameOver?]
	(match [winner? gameOver? icon focus?]
		[true _ _ _]               [:button.Cell.Winner {:style (dist->style d)} (iconMap icon)]
		[false false :empty true]  [:button.Cell.Selected  {:on-click onClick
															   :on-mouse-enter onFocus
															   :on-mouse-leave offFocus
															   :style (dist->style d)} (iconMap icon)]
		[false false :empty false] [:button.Cell {:on-click onClick
															   :on-mouse-enter onFocus
															   :on-mouse-leave offFocus
															   :style (dist->style d)} (iconMap icon)]
		:else 				  	   [:button.Cell {:style (dist->style d)} (iconMap icon)]))



(rum/defc globalBoardView [globalBoard onClick onFocus offFocus gameOver?]
	[:div.Grid (map-indexed
					(fn [r row]
						[:div.Row (map-indexed (fn [c viewCell]
													;(println r c (local [r c]))
													(cell viewCell 
														  (partial onClick (local [r c])) 
														  (partial onFocus [r c]) 
														  offFocus 
														  gameOver?))
												row)])
					globalBoard)])



(rum/defcs game < (rum/local :X ::curPlayer)
				  (rum/local (logic/emptyBoard) ::gameBoard)
				  (rum/local emptyIndex ::focusIndex)
			      {:init (fn [{args :rum/args :as state}]
			    		     (assoc state ::curPlayer (atom (second args))))}
	[state boardType player]
	(let [onClick (partial 
						logic/takeTurn
						boardType 
						#(swap! (::curPlayer state) swapPlayers)
						@(::curPlayer state)
						(::gameBoard state))
		  onFocus (fn [index] (reset! (::focusIndex state) index))
		  offFocus #(reset! (::focusIndex state) emptyIndex)]
		[:div.Game
			[:h1 (match [(logic/gameStatus @(::gameBoard state) boardType) @(::curPlayer state)]
					[:win icon] (str "Player " (iconMap icon) " has won! Restart to play again")
					[:draw _] (str "It's a draw! Restart to play again")
					[:in-play icon] (str (iconMap icon) ", it's your turn"))]
			(globalBoardView (logic/totalBoard @(::gameBoard state) @(::focusIndex state) (type->n boardType))
							 onClick
							 onFocus
							 offFocus
							 (not (= :in-play (logic/gameStatus @(::gameBoard state) boardType))))
			(restartButton)
		]))



(rum/defc selectMode []
	[(pageHeader)
	[:div.ButtonContainer 
		[:button.GoodButton 
			{:on-click (fn [] (do (rum/mount (game :bounded :X) root)))}
			"Bounded Tic-Tac-Toe"]
		[:button.BadButton 
			{:on-click (fn [] (do (rum/mount (game :torus :X) root)))} 
			"Torus Tic-Tac-Toe"]]])




;;lesson-learned: oct 15,
;;@ is a reader macro for atoms, no ()
;; mixins like :init solve the.... init problem
;; every chunk of state you want rum to keep track of needs to be declared as
;; 'rum/local'
;; rum state is render func, comp, args, and state you supply. 
;; the keys are just qualified namespace keys
;;
;; :init passes you state and prop, but didn't figure out how "props" is encoded
;; so i took it from the state. Yeah, ignoring it, can't figure. Possibly
;; a weird js->clojure thing

