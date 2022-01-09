(require '[clojure.string :as s])
(require '[clojure.data.priority-map :refer [priority-map]])

(defn parse-int [x] (Integer/parseInt x))

(def max-int 9999999)

(defn get-neighbors [[x y]]
  (list [(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]))

(defn edge-value [m [x y]]
  (let [orig-rows (count m)
        orig-cols (count (first m))
        submap-x (int (/ x orig-rows))
        submap-y (int (/ y orig-cols))
        increment (+ submap-x submap-y)
        uncapped-value (+ (get-in m [(mod y orig-rows) (mod x orig-cols)]) increment)]
    (if (<= uncapped-value 9)
      uncapped-value
      (- uncapped-value 9))))

(defn dijkstra [grid]
  (let [rows (* 5 (count grid))
        cols (* 5 (count (first grid)))
        all-points (into #{} (for [x (range cols) y (range rows)] [x y]))
        start [0 0]
        goal [(dec cols) (dec rows)]]
    (loop [visited #{}
           queue (priority-map [0 0] 0)]
      (if (= goal (first (peek queue)))
        (peek queue)
        (let [[location dist] (peek queue)
              new-queue (->> (get-neighbors location)
                             (filter all-points)
                             (filter #(not (visited %)))
                             (map #(vector % (+ dist (edge-value grid %))))
                             (filter (fn [[k v]] (< v (get queue k max-int))))
                             (into (pop queue)))]
          (recur (conj visited location) new-queue))))))
              
(defn parse-input [input]
  (->> input
       s/split-lines
       (map seq)
       (map #(map str %))
       (map #(map parse-int %))
       (map #(apply vector %))
       (apply vector)))

(defn solve-file [input]
  (-> input
      slurp
      parse-input
      dijkstra
      prn))

(solve-file (first *command-line-args*))
