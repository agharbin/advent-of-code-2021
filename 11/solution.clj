(require '[clojure.string :as s])
(require '[clojure.pprint :as pp])

(defn parse-int [x] (Integer/parseInt x))

(defn make-initial-state [grid]
  {:grid grid
   :flashes 0})

(def neighbor-offsets #{[1 0] [-1 0] [0 1] [0 -1]
                        [1 1] [-1 -1] [-1 1] [1 -1]})

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

(defn all-positions [map-width map-height]
  (for [x (range map-width) y (range map-height)] [x y]))

(defn should-flash [point m]
  (> (value-at-position point m) 9))

(defn increment-all [grid]
  (map #(map inc %) grid))

(defn increment-neighbors [grid [x y :as point]]
  (let [map-height (count grid)
        map-width  (count (first grid))
        vector-grid (apply vector (map #(apply vector %) grid))
        neighbors (valid-neighbors map-width map-height point)]
    (reduce
      (fn [g [x y :as pt]] (assoc-in g [y x] (inc (value-at-position pt g))))
      vector-grid
      neighbors)))

(defn zero-points [grid xs]
  (reduce
    (fn [g [x y :as pt]] (assoc-in g [y x] 0))
    grid
    xs))

(defn next-state [{:keys [grid flashes]}]
  (let [new-grid (increment-all grid)
        map-height (count grid)
        map-width (count (first grid))
        positions (all-positions map-width map-height)]
    (loop [g new-grid
           have-flashed #{}]
      (let [should-flash (set (filter #(and (should-flash % g) (not (contains? have-flashed %)))
                                      positions))]
        (if (seq should-flash)
          (recur (increment-neighbors g (first should-flash))
                 (into have-flashed (take 1 should-flash)))
          {:grid (zero-points g have-flashed) :flashes (+ (count have-flashed) flashes) })))))

(defn parse-line [line]
  (->> line
       seq
       (map str)
       (map parse-int)))

(defn solve [input]
  (->> input
       (map parse-line)
       make-initial-state
       (iterate next-state)
       (drop 100)
       first
       pp/pprint))

(defn solve-file [input]
  (-> input
      slurp
      (s/split #"\n")
      solve
      prn))

(solve-file (first *command-line-args*))
