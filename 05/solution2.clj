(require '[clojure.string :as s])

(defn parse-int [x] (Integer/parseInt x))

(defn diagonal-points [[x1 y1 x2 y2]]
  (let [x-step (if (< x1 x2) 1 -1)
        y-step (if (< y1 y2) 1 -1)
        steps  (Math/abs (- x2 x1))]
    (->> steps
         inc
         range
         (map #(list (+ x1 (* % x-step)) (+ y1 (* % y-step)))))))

(defn points-on-line [[x1 y1 x2 y2 :as line]]
  (cond
    (= x1 x2) (->>
                (range (min y1 y2) (inc (max y1 y2))) ; y-values of vertical line
                (interleave (repeat x2))
                (partition 2))
    (= y1 y2) (->>
                (range (min x1 x2) (inc (max x1 x2))) ; x-values of horizontal line
                (#(interleave % (repeat y1)))
                (partition 2))
    :else (diagonal-points line)))

(defn parse-line [line]
  (->> line
       (re-matches #"(\d+),(\d+) -> (\d+),(\d+)")
       (drop 1)
       (map parse-int)))

(defn solve [input]
  (->> input
       (map parse-line)
       (mapcat points-on-line)
       (frequencies)
       (filter (fn [[k v]] (>= v 2)))
       (map first)
       count))

(defn solve-file [input]
  (-> input
      slurp
      (s/split #"\n")
      solve
      prn))

(solve-file (first *command-line-args*))
