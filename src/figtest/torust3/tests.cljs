(ns figtest.torust3.tests
  (:require [cljs.test :refer-macros [deftest is testing run-tests]]
  			[figtest.torust3.logic :as logic]))

(def testboard1 (logic/convertIcons->cells [[:X :empty :O]
									 		[:O :X :O]
									 		[:empty :empty :X]]))
(def testboard2 (logic/convertIcons->cells [[:X :empty :O]
									 		[:O :X :O]
									 		[:empty :empty :empty]]))
(def testboard3 (logic/convertIcons->cells [[:X :empty :O]
									 		[:O :X :O]
									 		[:empty :empty :empty]]))
(def testboard4 (logic/convertIcons->cells [[:O :empty :O]
									 		[:X :O :empty]
									 		[:O :empty :X]]))

(def testboard5 (logic/convertIcons->cells [[:X :O :X]
									 		[:X :O :O]
									 		[:O :X :O]]))

(deftest testAllInd 
	(is (= [[0 0] [0 1] [0 2] [1 0] [1 1] [1 2] [2 0] [2 1] [2 2]]
			(logic/allInd))))

(deftest testRows
	(is (= logic/LEN (count (logic/rows (logic/emptyBoard)))))
	(is (let [cnts (into #{} (map count (logic/rows (logic/emptyBoard))))]
			(and (= 1 (count cnts))
				 (contains? cnts logic/LEN))))
	(is (= #{[:X :empty :O]
			 [:O :X :O]
			 [:empty :empty :empty]}
			 (into #{} (logic/rows testboard2)))))

(deftest testColumns
	(is (= logic/LEN (count (logic/columns (logic/emptyBoard)))))
	(is (let [cnts (into #{} (map count (logic/columns (logic/emptyBoard))))]
			(and (= 1 (count cnts))
				 (contains? cnts logic/LEN))))
	(is (= #{[:X :O :empty]
			 [:empty :X :empty]
			 [:O :O :X]}
			 (into #{} (logic/columns testboard1)))))

(deftest testDiags
	(is (= 2 (count (logic/diags (logic/emptyBoard)))))
	(is (let [cnts (into #{} (map count (logic/diags (logic/emptyBoard))))]
			(and (= 1 (count cnts))
				 (contains? cnts logic/LEN))))
	(is (= #{[:O :O :O] [:O :O :X]} 
			(into #{} (logic/diags testboard4)))))

(deftest testInARow? 
	(is (false? (logic/inARow? [:empty :empty :empty])))
	(is (true? (logic/inARow? [:X :X :X])))
	(is (true? (logic/inARow? [:O :O :O])))
	(is (false? (logic/inARow? [:X :X :O])))
	(is (false? (logic/inARow? [:X :O :empty]))))

(deftest testWinner?
	(is (= [[0 0] [1 1] [2 2]] (logic/boundedWinners? testboard1)))
	(is (nil? (logic/boundedWinners? testboard2)))
	(is (= [[0 2] [1 1] [2 0]] (logic/boundedWinners? testboard4))))

(deftest testPlay
	(is (= testboard1 (logic/play testboard3 [2 2] :X)))
	(is (not= testboard2 (logic/play testboard1 [2 3] :O)))
	(is (= testboard2 (logic/play testboard2 [0 0] :X))))

(deftest testDraw
	(is (false? (logic/draw? testboard1)))
	(is (false? (logic/draw? testboard2)))
	(is (false? (logic/draw? testboard3)))
	(is (false? (logic/draw? testboard4)))
	(is (true? (logic/draw? testboard5))))

(deftest testGameStatus
	(is (= :win (logic/gameStatus testboard4 :bounded)))
	(is (= :draw (logic/gameStatus testboard5 :bounded)))
	(is (= :in-play (logic/gameStatus testboard2 :bounded))))

(defn testAll []
	(run-tests))