(require '[clojure.string :as s])

(defn parse-int [x] (Integer/parseInt x))

(defn point-in-target [[x-left x-right y-bottom y-top] [x y]]
  (and (<= x-left x x-right) (<= y-bottom y y-top)))

(defn point-not-below-target [[_ _ y-bottom _] [_ y]]
  (>= y y-bottom))

(defn apply-gravity-and-drag [[velocity-x velocity-y]]
  [(max 0 (dec velocity-x)) (dec velocity-y)])

(defn position-sequence [[[x y] [velocity-x velocity-y :as velocity]]]
  [[(+ x velocity-x) (+ y velocity-y)] (apply-gravity-and-drag velocity)])

(defn hits-target [target velocity]
  (let [points-within-target (->> (iterate position-sequence [[0 0] velocity])
                                  (take-while #(point-not-below-target target (first %)))
                                  (filter #(point-in-target target (first %))))]
    (not (empty? points-within-target))))

(defn solve [[x-left x-right y-bottom y-top :as target]]
  (let [x-candidates (range 0 (inc x-right))
        y-candidates (range (dec y-bottom) (inc (* -1 y-bottom)))
        candidate-velocities (for [x x-candidates y y-candidates] [x y])]
    (->> candidate-velocities
         (filter #(hits-target target %))
         count)))

(defn parse-line [input]
  (->> input
       s/trim
       (re-matches #"target area: x=(\d+)..(\d+), y=(-?\d+)..(-?\d+)")
       (drop 1)
       (map parse-int)))

(defn solve-file [input]
  (-> input
      slurp
      parse-line
      solve
      prn))

(solve-file (first *command-line-args*))
