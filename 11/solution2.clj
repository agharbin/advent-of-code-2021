(require '[clojure.string :as s])
(require '[clojure.pprint :as pp])

(defn parse-int [x] (Integer/parseInt x))

(defn make-initial-state [grid]
  {:grid grid
   :flashes 0
   :step 0})

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

(defn get-height [grid] (count grid))

(defn get-width [grid] (count (first grid)))

(defn increment-neighbors [grid [x y :as point]]
  (let [vector-grid (apply vector (map #(apply vector %) grid))
        neighbors (valid-neighbors (get-width grid) (get-height grid) point)]
    (reduce
      (fn [g [x y :as pt]] (assoc-in g [y x] (inc (value-at-position pt g))))
      vector-grid
      neighbors)))

(defn set-positions-to-zero [grid xs]
  (reduce
    (fn [g [x y :as pt]] (assoc-in g [y x] 0))
    grid
    xs))

(defn is-grid-empty? [{:keys [grid]}]
  (->> grid
       (apply concat)
       (filter (complement zero?))
       empty?))

(defn next-state [{:keys [grid flashes step]}]
  (let [new-grid (increment-all grid)]
    (loop [g new-grid
           have-flashed #{}]
      (let [should-flash (->> (all-positions (get-width grid) (get-height grid))
                              (filter #(and (should-flash % g) (not (have-flashed %))))
                              set)]
        (if (seq should-flash)
          (recur (increment-neighbors g (first should-flash))
                 (into have-flashed (take 1 should-flash)))
          {:grid (set-positions-to-zero g have-flashed)
           :flashes (+ (count have-flashed) flashes)
           :step (inc step)})))))

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
       (drop-while (complement is-grid-empty?))
       first
       pp/pprint))

(defn solve-file [input]
  (-> input
      slurp
      (s/split #"\n")
      solve
      prn))

(solve-file (first *command-line-args*))
