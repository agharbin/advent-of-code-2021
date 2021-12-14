(require '[clojure.string :as s])

(defn parse-int [x] (Integer/parseInt x))

(defn split-on-whitespace [line] (s/split line #"\s+"))

(defn parse-line [line]
  (->> (s/split line #"\|")
       second
       s/trim
       split-on-whitespace
       (map count)))

(defn compute-result [frqs]
  (+ (frqs 2) (frqs 3) (frqs 4) (frqs 7)))

(defn solve [input]
  (->> input
       (map parse-line)
       (apply concat)
       frequencies
       compute-result))

(defn solve-file [input]
  (-> input
      slurp
      (s/split #"\n")
      solve
      prn))

(solve-file (first *command-line-args*))
