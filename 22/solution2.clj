(require '[clojure.string :as s])

(defn parse-int [x] (Integer/parseInt x))

(defn to-ints [[on-off x1 x2 y1 y2 z1 z2]]
  [(keyword on-off)
   [[(parse-int x1) (parse-int x2)] [(parse-int y1) (parse-int y2)] [(parse-int z1) (parse-int z2)]]])

(defn parse-line [line]
  (->> line
       (re-matches #"(on|off) x=(-?\d+)..(-?\d+),y=(-?\d+)..(-?\d+),z=(-?\d+)..(-?\d+)")
       (drop 1) 
       (to-ints)))

(defn parse-input [input]
  (->> input
       (s/split-lines)
       (map parse-line)))

(defn cube-size [[[min-x max-x] [min-y max-y] [min-z max-z]]]
  (* (inc (- max-x min-x)) (inc (- max-y min-y)) (inc (- max-z min-z))))

(defn axis-intersection [[min-1 max-1] [min-2 max-2]]
  (let [bounds (->> (sort [min-1 max-1 min-2 max-2])
                    (drop 1)
                    (drop-last 1))]
    (vector (first bounds) (last bounds))))

(defn cube-intersection [[x1 y1 z1] [x2 y2 z2]]
  (vector (axis-intersection x1 x2) (axis-intersection y1 y2) (axis-intersection z1 z2)))

(defn intersects? [[[min-x-1 max-x-1] [min-y-1 max-y-1] [min-z-1 max-z-1]]
                   [[min-x-2 max-x-2] [min-y-2 max-y-2] [min-z-2 max-z-2]]]
  (cond
    (or (< max-x-2 min-x-1) (< max-x-1 min-x-2)) false
    (or (< max-y-2 min-y-1) (< max-y-1 min-y-2)) false
    (or (< max-z-2 min-z-1) (< max-z-1 min-z-2)) false
    :else true))

(defn solve [input]
  (loop [instructions input
         positive-cubes []
         negative-cubes []]
    (if (seq instructions)
      (let [[command cube] (first instructions)
            new-positive-cubes (into positive-cubes (->> negative-cubes
                                                         (filter (partial intersects? cube))
                                                         (map (partial cube-intersection cube))))
            new-negative-cubes (into negative-cubes (->> positive-cubes
                                                         (filter (partial intersects? cube))
                                                         (map (partial cube-intersection cube))))]
        (if (= command :on)
          (recur (rest instructions) (conj new-positive-cubes cube) new-negative-cubes)
          (recur (rest instructions) new-positive-cubes new-negative-cubes)))
      (- (apply + (map cube-size positive-cubes)) (apply + (map cube-size negative-cubes))))))

(defn solve-file [input]
  (-> input
      slurp
      parse-input
      solve
      prn))

(solve-file (first *command-line-args*))
