(require '[clojure.string :as s])

(defn parse-input [lines]
  (let [[template rules-text] (s/split lines #"\n\n")
        rules (->> (s/split rules-text #"\n")
                   (map (partial re-matches #"([A-Z]{2}) -> ([A-Z])"))
                   (map (partial drop 1))
                   (map (partial apply vector))
                   (map (fn [[k v]] [k [(str (first k) v) (str v (second k))]]))
                   (into {}))]
    [rules template]))

(defn generate-solutions [[rules template]]
  (defn step [fqs]
    (->> fqs
         (map (fn [[k v]] [[(first (rules k)) v] [(second (rules k)) v]]))
         (map (partial into {}))
         (apply merge-with +)))
  (let [initial-pairs (->> (partition 2 1 template) (map (partial apply str)))
        initial-fqs (frequencies initial-pairs)]
    (iterate step initial-fqs)))

(defn compute-result [last-char solution]
  (let [final-freqs (update (->> solution
                                 (map (fn [[k v]] {(first k) v}))
                                 (apply merge-with +))
                            last-char
                            inc)
        max-value (apply max (map second final-freqs))
        min-value (apply min (map second final-freqs))]
    (- max-value min-value)))

(defn solve-file [input]
  (let [[rules template :as parsed-input] (->> input slurp parse-input)
        last-char (last template)]
    (->> parsed-input
         generate-solutions
         (drop 40)
         first
         (compute-result last-char)
         prn)))

(solve-file (first *command-line-args*))
