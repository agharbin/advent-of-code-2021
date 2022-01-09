(require '[clojure.string :as s])
(require '[clojure.pprint :as pp])

(def distribution {3 1, 4 3, 5 6, 6 7, 7 6, 8 3, 9 1})

(defn advance-position [start n]
  (-> start
      (- 1)
      (+ n)
      (mod 10)
      (+ 1)))

(def score-target 21)
(def possible-turns [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20])
(def possible-positions [1 2 3 4 5 6 7 8 9 10])
(def possible-scores [0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21])
(def initial-turn 1)
(def initial-score 0)

(defn simulate-game [start-1 start-2]
  (let [universes (apply vector (for [turns (range (inc (count possible-turns)))]
                    (apply vector (for [positions-1 (range (inc (count possible-positions)))]
                      (apply vector (for [score-1 (range (inc (count possible-scores)))]
                        (apply vector (for [positions-2 (range (inc (count possible-positions)))]
                          (apply vector (for [score-2 (range (inc (count possible-scores)))] 0))))))))))
        pos-cells (for [turn possible-turns
                        pos-1 possible-positions
                        score-1 possible-scores
                        pos-2 possible-positions
                        score-2 possible-scores
                        outcome (seq distribution)]
                    [turn pos-1 score-1 pos-2 score-2 outcome])
        initial-values (-> universes
                           (assoc-in [initial-turn start-1 initial-score start-2 initial-score] 1))]
    (loop [cells pos-cells universes initial-values]
      (if (seq cells)
        (let [[t p1 s1 p2 s2 [roll freq]] (first cells)
              universes-at-starting-point (get-in universes [t p1 s1 p2 s2])]
          (if (or (>= s1 score-target) (>= s2 score-target) (= 0 universes-at-starting-point))
            (recur (rest cells) universes) ; Skip any terminated games
            (if (= 1 (rem t 2)) ; P1's turn
              (let [landing-spot (advance-position p1 roll)
                    new-score (min (+ s1 landing-spot) 21)
                    new-key [(inc t) landing-spot new-score p2 s2]
                    old-value (get-in universes new-key)
                    new-value (+ old-value (* universes-at-starting-point freq))
                    new-universes (assoc-in universes new-key new-value)]
                (recur (rest cells) new-universes))
              (let [landing-spot (advance-position p2 roll)
                    new-score (min (+ s2 landing-spot) 21)
                    new-key [(inc t) p1 s1 landing-spot new-score]
                    old-value (get-in universes new-key)
                    new-value (+ old-value (* universes-at-starting-point freq))
                    new-universes (assoc-in universes new-key new-value)]
                (recur (rest cells) new-universes)))))
        (->> (for [t possible-turns p1 possible-positions p2 possible-positions s possible-scores] 
               (get-in universes [t p1 s p2 21]))
             (apply +))))))

(def start-1 4)
(def start-2 1)

(pp/pprint (simulate-game start-1 start-2))
