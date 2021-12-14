(require '[clojure.string :as s])
(require '[clojure.pprint :as pp])
(require '[clojure.set :as cset])

(defn parse-int [s] (Integer/parseInt s))

(defn position [row col] (+ col (* 5 row)))

(def winning-positions
  [#{0 1 2 3 4}
   #{5 6 7 8 9}
   #{10 11 12 13 14}
   #{15 16 17 18 19}
   #{20 21 22 23 24}
   #{0 5 10 15 20}
   #{1 6 11 16 21}
   #{2 7 12 17 22}
   #{3 8 13 18 23}
   #{4 9 14 19 24}])

(defn mark-seen [board n]
  (assoc-in board [n :seen] true))

(defn marked-positions [board]
  (->> board
       (filter (fn [[k v]] (v :seen)))
       (map second)
       (map :pos)
       set))

(defn is-winning-position? [board position]
  (cset/subset? position (marked-positions board)))

(defn is-winning-board? [board]
  (some true? (map #(is-winning-position? board %) winning-positions)))

(defn to-map-representation [numbers]
  (let [num-vec (apply vector numbers)
        map-vals (mapcat #(list (num-vec %) {:pos % :seen false}) (range 25))]
    (apply assoc {} map-vals)))

(defn compute-output [board n]
  (->> board
       (filter (fn [[k v]] (= false (v :seen))))
       (map first)
       (apply +)
       (* n)))

(defn solve [input]
  (let [number-strings (-> input first (s/split #","))
        numbers (map parse-int number-strings)
        boards (->> input
                    (drop 2)
                    (filter (complement s/blank?))
                    (partition 5)
                    (map #(s/join " " %))
                    (map s/trim)
                    (map #(s/split % #"\s+")) 
                    (map (partial map parse-int))
                    (map to-map-representation))]
    (loop [b boards
           ns numbers]
      (let [new-boards (map #(mark-seen % (first ns)) b)
            winning-boards (filter is-winning-board? new-boards)
            not-winning-boards (filter (complement is-winning-board?) new-boards)]
        ; loop until there is only a single board left in consideration and it has won
        (if (not (and (= 1 (count new-boards)) (= 1 (count winning-boards))))
             (recur not-winning-boards (rest ns))
             (compute-output (first winning-boards) (first ns)))))))

(defn solve-file [input]
  (-> input
      slurp
      (s/split #"\n")
      solve
      pp/pprint))

(solve-file (first *command-line-args*))
