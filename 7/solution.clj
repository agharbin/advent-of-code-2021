(require '[clojure.string :as s])

(defn parse-int [x] (Integer/parseInt x))

(defn abs [x] (if (>= x 0) x (* -1 x)))

(defn triangle-number [n] (/ (* n (+ n 1)) 2))

(defn total-dist [crabs position]
  (->> crabs
      (map #(- % position))
      (map abs)
      (map triangle-number)
      (apply +)))

(defn compute-min-dist [crabs]
  (let [left (apply min crabs)
        right (apply max crabs)
        positions-to-check (range left (inc right))]
    (->> positions-to-check
         (map (partial total-dist crabs))
         (apply min))))

(defn solve [input]
  (->> (s/split (first input) #",")
       (map parse-int)
       compute-min-dist))

(defn solve-file [input]
  (-> input
      slurp
      (s/split #"\n")
      solve
      prn))

(solve-file (first *command-line-args*))
