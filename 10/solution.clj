(require '[clojure.string :as s])

(defn parse-int [x] (Integer/parseInt x))

(defn solve-case [line]
  (loop [queue line
         stack '()]
    (if (seq queue)
      (case (first queue)
        \( (recur (rest queue) (conj stack (first queue)))
        \[ (recur (rest queue) (conj stack (first queue)))
        \{ (recur (rest queue) (conj stack (first queue)))
        \< (recur (rest queue) (conj stack (first queue)))
        \) (if (= \( (first stack))
             (recur (rest queue) (rest stack))
             3)
        \] (if (= \[ (first stack))
             (recur (rest queue) (rest stack))
             57)
        \} (if (= \{ (first stack))
             (recur (rest queue) (rest stack))
             1197)
        \> (if (= \< (first stack))
             (recur (rest queue) (rest stack))
             25137))
      nil)))

(defn solve [input]
  (->> input
       (map solve-case)
       (filter some?)
       (apply +)))

(defn solve-file [input]
  (-> input
      slurp
      (s/split #"\n")
      solve
      prn))

(solve-file (first *command-line-args*))
