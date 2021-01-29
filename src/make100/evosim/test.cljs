(ns make100.evosim.test
	(:require [cljs.test :refer-macros [deftest is testing run-tests]]
			  [make100.evosim.logic :as logic]))

(defn almost-equal [a b epsilon]
	(< (. js/Math abs (- a b)) epsilon))


(deftest testFunc->Points
	(let [cases [{:f identity
		          :a 0
		          :b 4
		          :n 4
		          :want (map #(vector % %) (range 0 5))}
		         {:f (fn [_] 5)
		          :a 0
		          :b 4
		          :n 4
		          :want (map #(vector % 5) (range 0 5))}
		         {:f identity
		          :a 0
		          :b 1
		          :n 4
		          :want [[0 0] [0.25 0.25] [0.5 0.5] [0.75 0.75] [1 1]]}]]
		(doall
			(map (fn [{f :f a :a b :b n :n want :want}]
					(is (= want (logic/func->points f a b n))))
				 cases))))

(deftest testIntegrate
	(let [epsilon 0.01
		  cases [{:f identity
				  :a 0
				  :b 10
				  :n 30
				  :want 50}
				 {:f (fn [_] 10)
				  :a 0
				  :b 10
				  :n 30
				  :want 100}
				 {:f (fn [x] (* x x))
				  :a 0
				  :b 3
				  :n 30
				  :want 9}
				 {:f (fn [x] (* x x))
				  :a 0
				  :b 10
				  :n 100
				  :want (/ 1000 3)}]]
		(doall
			(map (fn [{f :f a :a b :b n :n want :want}]
					(is (= true (almost-equal want (logic/integrate f a b n) epsilon))))
				 cases))))

(deftest testChoose
	(let [cases [{:n 10
		          :k 4
		          :want 210}
		         {:n 10
		          :k 10
		          :want 1}
		         {:n 10
		          :k 0
		          :want 1}]]
		 (doall
		 	(map (fn [{n :n k :k want :want}]
		 			(is (= want (logic/choose n k))))
		 		 cases))))

(deftest testBatesSumsToOne
	(let [epsilon 0.001
		  cases [{:n 1
			      :step 10
			      :want 1}
			     {:n 2
			      :step 10
			      :want 1}
			     {:n 3
			      :step 20
			      :want 1}]]
		(doall
			(map (fn [{n :n step :step want :want}]
					(is (= true (almost-equal want (logic/integrate (partial logic/bates n) 0 1 step) epsilon))))
				 cases))))

(deftest testTruncatedNormalSumsToOne
	(let [epsilon 0.01
		  cases [{:a -1
		 		  :b 1
		 		  :u 0
		 		  :sd 1
		 		  :step 30
		 		  :want 1}
		 		 {:a -1
		 		  :b 1
		 		  :u 0
		 		  :sd 1.5
		 		  :step 40
		 		  :want 1}]]
		(doall
			(map (fn [{a :a b :b n :n sd :sd want :want step :step}]
						(is (= true (almost-equal want (logic/integrate (partial logic/truncated-normal a b n sd) a b step) epsilon))))
				 cases))))

(defn testAll []
	(run-tests))