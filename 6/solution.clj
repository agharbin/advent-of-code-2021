(require '[clojure.string :as s])

(defn parse-int [x] (Integer/parseInt x))

(defn simulate-round [fish]
  (let [num-birthing (get fish 0 0)
        except-birthing (dissoc fish 0)
        next-fish (->> except-birthing
                      keys
                      (mapcat #(list (dec %) (except-birthing %)))
                      (apply assoc {}))]
    (-> next-fish
        (assoc 8 num-birthing)
        (assoc 6 (+ num-birthing (get next-fish 6 0))))))

(defn solve [input]
  (->> (s/split (first input) #",")
       (map parse-int)
       (frequencies)
       (iterate simulate-round)
       (drop 256)
       first
       (map second)
       (apply +)))

(defn solve-file [input]
  (-> input
      slurp
      (s/split #"\n")
      solve
      prn))

(solve-file (first *command-line-args*))
