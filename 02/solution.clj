(require '[clojure.string :as s])

(defn parse-line [input]
  (let [[direction magnitude] (s/split input #" ")]
    (list (keyword direction) (Integer/parseInt magnitude))))

(defn compute-result [{:keys [distance depth]}]
  (* distance depth))

(def initial-state {:aim 0 :distance 0 :depth 0})

(defn next-state [{last-aim :aim last-distance :distance last-depth :depth :as last-state}
                  [instruction magnitude]]
  (case instruction
    :forward (assoc last-state
                    :distance (+ last-distance magnitude)
                    :depth (+ last-depth (* last-aim magnitude)))
    :down (assoc last-state :aim (+ last-aim magnitude))
    :up (assoc last-state :aim (- last-aim magnitude))))

(defn solve [input]
  (->> input
      (map parse-line)
      (reduce next-state initial-state)
      compute-result))

(defn solve-file [input]
  (-> input
      slurp
      (s/split #"\n")
      solve
      prn))

(solve-file (first *command-line-args*))
