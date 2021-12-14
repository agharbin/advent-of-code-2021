(require '[clojure.string :as s])

(defn parse-int [x] (Integer/parseInt x))

(defn solve [input]
  (->> input
       (map parse-int)
       (partition 3 1)
       (map (partial apply +))
       (partition 2 1)
       (map (partial apply <))
       (filter true?)
       count))

(defn solve-file [input]
  (-> input
      slurp
      (s/split #"\n")
      solve
      prn))

(solve-file (first *command-line-args*))
