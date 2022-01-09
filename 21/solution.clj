(require '[clojure.string :as s])

(defn advance-position [start n]
  (-> start
      (- 1)
      (+ n)
      (mod 10)
      (+ 1)))

(def dice-seq (cycle (range 1 101)))

(defn simulate-game [start-1 start-2]
  (loop [pos-1 start-1
         pos-2 start-2
         score-1 0
         score-2 0
         dice dice-seq
         dice-rolls 0
         turn :first]
    (cond
      (>= score-1 1000) (* score-2 dice-rolls)
      (>= score-2 1000) (* score-1 dice-rolls)
      :else 
      (let [move-by (apply + (take 3 dice))]
        (if (= turn :first)
          (recur (advance-position pos-1 move-by)
                 pos-2
                 (+ score-1 (advance-position pos-1 move-by))
                 score-2
                 (drop 3 dice)
                 (+ 3 dice-rolls)
                 :second)
          (recur pos-1
                 (advance-position pos-2 move-by)
                 score-1
                 (+ score-2 (advance-position pos-2 move-by))
                 (drop 3 dice)
                 (+ 3 dice-rolls)
                 :first))))))

(def start-1 4)
(def start-2 1)

(prn (simulate-game start-1 start-2))
