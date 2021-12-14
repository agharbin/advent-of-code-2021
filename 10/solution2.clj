(require '[clojure.string :as s])

(defn parse-int [x] (Integer/parseInt x))

(def value-of { \) 1, \] 2, \} 3, \> 4 })

(defn score [s]
  (reduce (fn [value c] (+ (* 5 value) (value-of c))) 0 s))

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
             nil)
        \] (if (= \[ (first stack))
             (recur (rest queue) (rest stack))
             nil)
        \} (if (= \{ (first stack))
             (recur (rest queue) (rest stack))
             nil)
        \> (if (= \< (first stack))
             (recur (rest queue) (rest stack))
             nil))
      stack)))

(defn median-of-sorted [xs]
  (let [num-to-drop (int (/ (count xs) 2))]
    (first (drop num-to-drop xs))))

(defn solve [input]
  (->> input
       (map solve-case)
       (filter some?)
       (map #(apply str %))
       (map #(s/escape % {\[ \], \( \), \{ \}, \< \>}))
       (map score)
       sort
       median-of-sorted))

(defn solve-file [input]
  (-> input
      slurp
      (s/split #"\n")
      solve
      prn))

(solve-file (first *command-line-args*))
