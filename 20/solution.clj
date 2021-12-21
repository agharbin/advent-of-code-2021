(require '[clojure.string :as s])
(require '[clojure.pprint :as pp])

(defn parse-input [input]
  (let [enhancement (->> input (s/split-lines) first)
        start-state (->> input (s/split-lines) (drop 2) (map #(apply vector %)) (apply vector))]
    [enhancement start-state]))

(defn neighbors [[r c]]
  [[(dec r) (dec c)]
   [(dec r) c]
   [(dec r) (inc c)]
   [r       (dec c)]
   [r       c]
   [r       (inc c)]
   [(inc r) (dec c)]
   [(inc r) c]
   [(inc r) (inc c)]])

(defn binary-equivalent [points state]
  (let [binary-string (->> points
                           (map #(if (contains? state %) \1 \0))
                           (apply str))]
    (Integer/parseInt binary-string 2)))

(defn get-light-coords [points]
  (let [rows (count points)
        cols (count (first points))]
    (into #{}
      (filter some?
        (for [r (range rows) c (range cols)]
          (when (= \# (get-in points [r c]))
            [r c]))))))

(defn point-in-next-state [p last-state enhancement]
  (let [neighbors (->> p neighbors)
        binary-equivalent (binary-equivalent neighbors last-state)]
    (= \# (get enhancement binary-equivalent))))

(def mn -100)
(def mx 200)

(defn area [minval maxval] (for [r (range minval maxval) c (range minval maxval)] [r c]))

(defn next-state [enhancement last-state]
  (->> (area mn mx)
       (filter #(point-in-next-state % last-state enhancement))
       (into #{})))

(defn points-in-bounds [points]
  (->> points
       (filter #(and (< (inc mn) (first %) (dec mx)) (< (inc mn) (second %) (dec mx))))
       (into #{})))

(defn advance-twice [enhancements state]
  (points-in-bounds (next-state enhancements (next-state enhancements state))))

(defn solve [[enhancements start-state]]
  (let [light-coords (get-light-coords start-state)]
    (->> light-coords
         (iterate (partial advance-twice enhancements))
         (drop 25)
         first
         count)))

(defn solve-file [input]
  (-> input
      slurp
      parse-input
      solve
      prn))

(solve-file (first *command-line-args*))
