(require '[clojure.string :as s])
(require '[clojure.pprint :as pp])

(defn parse-int [x] (Integer/parseInt x))

(defn parse-input [input]
  (let [[points-str folds-str] (s/split input #"\n\n")
        points (->> (s/split points-str #"\n")
                    (map #(s/split % #","))
                    (map #(map parse-int %))
                    (map #(apply vector %)))
        folds (->> (s/split folds-str #"\n")
                   (map #(re-matches #"fold along ([xy])=(\d+)" %))
                   (map #(drop 1 %))
                   (map #(vector (first %) (parse-int (second %)))))]
    [(set points) folds]))

(def x-axis 0)
(def y-axis 1)
(def label-to-axis {"y" y-axis, "x" x-axis})

(defn reflect [axis position [x y]]
  (case axis
    0 [(- position (- x position)) y]
    1 [x (- position (- y position))]))

(defn fold-at [points fold-instruction]
  (let [axis (label-to-axis (first fold-instruction))
        position (second fold-instruction)
        filter-fn (fn [p] (> (nth p axis) position))
        points-above-fold (filter filter-fn points)
        points-below-fold (filter (complement filter-fn) points)]
    (-> #{}
        (into points-below-fold)
        (into (map (partial reflect axis position) points-above-fold)))))

(defn plot [points]
  ; For printing purposes, we need to print one 'row' at a time, which
  ; corresponds to points along the y-axis. This can be handled by reversing (x,y) -> (y,x)
  (let [reversed-points (into #{} (map (fn [[x y]] [y x]) points))
        x-extent (inc (apply max (map first reversed-points)))
        y-extent (inc (apply max (map second reversed-points)))]
    (->> (for [x (range x-extent) y (range y-extent)] [x y])
         (map #(if (reversed-points %) \# \space))
         (partition y-extent)
         (map #(apply str %))
         (s/join "\n")
         print)))

(defn solve [points folds]
    (reduce fold-at points folds))

(defn solve-file [input]
  (->> input
       slurp
       parse-input
       (apply solve)
       plot))

(solve-file (first *command-line-args*))
(print "\n")
