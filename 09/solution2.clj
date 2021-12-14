(require '[clojure.string :as s])

(defn parse-int [x] (Integer/parseInt x))

(def neighbor-offsets #{[1 0] [-1 0] [0 1] [0 -1]})

(defn neighbor-candidates [v]
  (for [offset neighbor-offsets] (map + v offset)))

(defn is-within-map [map-width map-height [x y]]
  (and (>= x 0)
       (<  x map-width)
       (>= y 0)
       (<  y map-height)))

(defn valid-neighbors [map-width map-height point]
  (filter #(is-within-map map-width map-height %) (neighbor-candidates point)))

(defn value-at-position [[x y] m]
  (-> m (nth y) (nth x)))

(defn parse-line [line]
  (->> line
       seq
       (map str)
       (map parse-int)))

(defn is-minimum [point height width m]
  (let [this-height (value-at-position point m)
        neighbor-heights (->> point
                             (valid-neighbors width height)
                             (map #(value-at-position % m)))
        higher-neighbors (filter #(< this-height %) neighbor-heights)]
    (if (= (count higher-neighbors) (count neighbor-heights))
      point
      nil)))

(defn get-mins [m]
  (let [height (count m)
        width  (count (first m))
        all-points (for [y (range height) x (range width)] [x y])]
    (filter #(is-minimum % height width m) all-points)))

(defn basin-size [p m]
  (loop [queue [p]
         points-in-basin (set [p])]
    (if (seq queue)
      (let [nxt (first queue)
            height (count m)
            width (count (first m))
            neighbors (valid-neighbors width height nxt)
            neighbors-in-basin (filter #(not= 9 (value-at-position % m)) neighbors)
            neighbors-not-already-added (filter (complement #(contains? points-in-basin %)) neighbors-in-basin)]
        (recur (into (rest queue) neighbors-not-already-added) (into points-in-basin neighbors-not-already-added)))
      (count points-in-basin))))

(defn solve [input]
  (let [m (->> input (map parse-line))]
    (->> m
        get-mins
        (map #(apply list %))
        (map #(basin-size % m))
        sort
        reverse
        (take 3)
        (apply *))))

(defn solve-file [input]
  (-> input
      slurp
      (s/split #"\n")
      solve
      prn))

(solve-file (first *command-line-args*))
