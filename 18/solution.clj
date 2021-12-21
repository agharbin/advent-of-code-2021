(require '[clojure.string :as s])
(require '[clojure.pprint :as pp])

(defn parse-int [x] (Integer/parseInt x))

(defn render-tree [t]
  (->> t
       (map #(if (int? %) (str " " % " ") %))
       (apply str)
       read-string))

(defn flatten-tree [t]
  (flatten (map #(if (vector? %) ["[" (first %) (second %) "]"] %) t)))

;;; Exploding Logic ;;;

(defn mark-explodes
  ([t] (mark-explodes 0 t))
  ([level t] 
   (if (not (vector? t))
     [[t, false]]
     (if (>= level 4)
       [[t, true]]
       (concat [["[", false]] (mapcat (partial mark-explodes (inc level)) t) [["]" false]])))))

(defn should-explode [t]
  (let [marked-tree (mark-explodes t)]
    (->> marked-tree (filter #(true? (second %))) count pos?)))

(defn add-to-front [t n]
  (loop [to-go t
         result []
         have-added false]
    (if (seq to-go)
      (if (int? (first to-go))
        (if have-added
          (recur (rest to-go) (conj result (first to-go)) have-added)
          (recur (rest to-go) (conj result (+ n (first to-go))) true))
        (recur (rest to-go) (conj result (first to-go)) have-added))
      result)))

(defn add-to-back [t n]
  (reverse (add-to-front (reverse t) n)))

(defn compute-explode [t]
  (let [marked-tree (mark-explodes t)
        prefix (->> marked-tree (take-while #(false? (second %))) (map first) flatten-tree)
        [l-value r-value] (->> marked-tree (filter #(true? (second %))) first first)
        postfix (->> marked-tree (drop-while #(false? (second %))) (drop 1) (map first) flatten-tree)]
    (render-tree (concat (add-to-back prefix l-value) [0] (add-to-front postfix r-value)))))

;;; Splitting Logic ;;;

(defn mark-splits [t] 
  (if (not (vector? t))
    (if (>= t 10)
      [[t true]]
      [[t false]])
      (concat [["[", false]] (mapcat mark-splits t) [["]" false]] )))

(defn should-split [t]
  (if (not (vector? t))
    (if (>= t 10)
      true
      false)
    (some true? (map should-split t))))

(defn get-split-pair [n]
  [ "[" (int (Math/floor (/ n 2))) (int (Math/ceil (/ n 2))) "]" ])

(defn compute-split [t]
  (let [marked-tree (mark-splits t)
        prefix (->> marked-tree (take-while #(false? (second %))) (map first) flatten-tree)
        val-to-split (->> marked-tree (filter #(true? (second %))) first first)
        postfix (->> marked-tree (drop-while #(false? (second %))) (drop 1) (map first) flatten-tree)]
    (render-tree (concat prefix (get-split-pair val-to-split) postfix))))

;;; Combined Logic ;;;

(defn needs-reduction [t]
  (or (should-split t) (should-explode t)))

(defn reduce-tree [t]
  (cond
    (should-explode t) (compute-explode t)
    (should-split t) (compute-split t)
    :else (prn "ERROR: called reduce on a non-reducible tree")))

(defn add-lists [t1 t2]
  (->> [t1 t2]
       (iterate reduce-tree)
       (drop-while needs-reduction)
       first))

(defn compute-magnitude [t]
  (if (vector? t)
    (+ (* 3 (compute-magnitude (first t)))
       (* 2 (compute-magnitude (second t))))
    t))

(defn parse-input [input]
  (->> input
       s/split-lines
       (map read-string)))

(defn solve [input]
  (reduce add-lists input))

(defn solve-file [input]
  (-> input
      slurp
      parse-input
      solve
      compute-magnitude
      prn))

(solve-file (first *command-line-args*))
