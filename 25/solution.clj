(require '[clojure.string :as s])

(defn parse-int [x] (Integer/parseInt x))

(defn render-state [grid]
  (->> grid
       (map #(apply str %))
       (s/join "\n")))

(defn parse-input [input]
  (->> input
       (s/split-lines)
       (map #(apply vector %))
       (apply vector)))

(defn space-to-right [[row col] map-height map-width]
  [row (mod (inc col) map-width)])

(defn space-below [[row col] map-height map-width]
  [(mod (inc row) map-height) col])

(defn empty-space? [grid [row col]]
  (= \. (get-in grid [row col])))

(defn positions-of-type [grid cucumber-type]
  (let [rows (count grid)
        cols (count (first grid))]
    (for [r (range rows) c (range cols) :when (= cucumber-type (get-in grid [r c]))] [r c])))

(defn eastward-move [map-height map-width grid]
  (loop [xs (positions-of-type grid \>)
         g  grid]
    (if (seq xs)
      (let [cucumber (first xs)] 
        (if (empty-space? grid (space-to-right cucumber map-height map-width))
            (recur (rest xs) (-> g (assoc-in cucumber \.) (assoc-in (space-to-right cucumber map-height map-width) \>)))
            (recur (rest xs) g)))
      g)))

(defn southward-move [map-height map-width grid]
  (loop [xs (positions-of-type grid \v)
         g  grid]
    (if (seq xs)
      (let [cucumber (first xs)] 
        (if (empty-space? grid (space-below cucumber map-height map-width))
            (recur (rest xs) (-> g (assoc-in cucumber \.) (assoc-in (space-below cucumber map-height map-width) \v)))
            (recur (rest xs) g)))
      g)))

(defn next-state [grid]
  (let [map-height (count grid)
        map-width (count (first grid))]
    (->> grid 
         (eastward-move map-height map-width)
         (southward-move map-height map-width))))

(defn solve
  ([input]
   (solve input 0))

  ([input i]
   (let [next-grid (next-state input)]
     (if (= input next-grid)
       (inc i)
       (recur next-grid (inc i))))))

(defn solve-file [input]
  (->> input
       slurp
       parse-input
       solve
       print))

(solve-file (first *command-line-args*))
