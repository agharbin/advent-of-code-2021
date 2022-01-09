(require '[clojure.string :as s])

(defn parse-int [x] (Integer/parseInt x))

(defn to-ints [[on-off x1 x2 y1 y2 z1 z2]]
  [(keyword on-off) (parse-int x1) (parse-int x2) (parse-int y1) (parse-int y2) (parse-int z1) (parse-int z2)])

(defn parse-line [line]
  (->> line
       (re-matches #"(on|off) x=(-?\d+)..(-?\d+),y=(-?\d+)..(-?\d+),z=(-?\d+)..(-?\d+)")
       (drop 1) 
       (to-ints)))

(defn parse-input [input]
  (->> input
       (s/split-lines)
       (map parse-line)))

(def initial-space
  (into {}
    (for [x (range -50 (inc 50)) y (range -50 (inc 50)) z (range -50 (inc 50))]
      [[x y z] :off])))

(defn get-next-state [initial-state [command x1 x2 y1 y2 z1 z2]]
  (if (or (> x1 50) (< x2 -50) (> y1 50) (< y2 -50) (> z1 50) (< z2 -50))
    initial-state
    (let [points-to-update (for [x (range x1 (inc x2)) y (range y1 (inc y2)) z (range z1 (inc z2))] [x y z])]
      (loop [points points-to-update prior-state initial-state]
        (if (seq points)
          (if (contains? prior-state (first points))
            (recur (rest points) (assoc prior-state (first points) command))
            (recur (rest points) prior-state))
          prior-state)))))

(defn count-cubes [state]
  (->> state
       (map second)
       frequencies))

(defn solve [input]
  (count-cubes (reduce get-next-state initial-space input)))

(defn solve-file [input]
  (-> input
      slurp
      parse-input
      solve
      prn))

(solve-file (first *command-line-args*))
