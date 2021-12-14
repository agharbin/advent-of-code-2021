(require '[clojure.string :as s])

(defn compute-product [binary-string]
  (let [complement-string (s/escape binary-string {\0 \1 \1 \0})]
    (* (Integer/parseInt binary-string 2) (Integer/parseInt complement-string 2))))

(defn solve [input]
  (->> input
       (apply map vector)
       (map frequencies)
       (map #(if (> (% \0) (% \1)) \0 \1))
       (apply str)
       compute-product))

(defn solve-file [input]
  (-> input
      slurp
      (s/split #"\n")
      solve
      prn))

(solve-file (first *command-line-args*))
