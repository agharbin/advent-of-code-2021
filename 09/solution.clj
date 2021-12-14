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
      this-height
      nil)))

(defn get-mins [m]
  (let [height (count m)
        width  (count (first m))
        all-points (for [y (range height) x (range width)] [x y])]
    (map #(is-minimum % height width m) all-points)))

(defn solve [input]
  (->> input
       (map parse-line)
       get-mins
       (filter some?)
       (map inc)
       (apply +)))

(defn solve-file [input]
  (-> input
      slurp
      (s/split #"\n")
      solve
      prn))

(solve-file (first *command-line-args*))
