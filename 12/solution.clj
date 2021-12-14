(require '[clojure.string :as s])

(defn parse-line [line]
  (s/split line #"-"))

(defn lowercase? [s]
  (= (s/lower-case s) s))

(defn uppercase? [s]
  (= (s/upper-case s) s))

(defn connections [node edges]
  (->> edges
       (filter #(= node (first %)))
       (map second)))

(defn generate-connecting-paths [edges p]
  (->> edges
       (connections (last p))
       (map #(conj p %))))

(defn meets-duplicate-smallcave-criteria? [duplicate-smallcaves]
  (or 
     (empty? duplicate-smallcaves)
     (let [num-caves (count duplicate-smallcaves)
          [cave times-appearing] (first duplicate-smallcaves)]
       (and (= num-caves 1) (not= "start" cave) (= 2 times-appearing)))))

(defn lowercase-appearing-multiple-times [[k v]]
  (and (lowercase? k) (> v 1)))

(defn valid-path [p]
  (->> p
       frequencies
       (filter lowercase-appearing-multiple-times)
       meets-duplicate-smallcave-criteria?))

(defn valid-next-paths [p edges finished-paths]
  (->> p
       (generate-connecting-paths edges)
       (filter valid-path)
       (filter #(not (finished-paths %)))))

(defn compute-paths [edges]
  (loop [partial-paths [["start"]]
         finished-paths #{}]
    (if (seq partial-paths)
      (let [p (first partial-paths)]
        (if (= (last p) "end")
          (recur (rest partial-paths) (conj finished-paths p))
          (recur (into (rest partial-paths) (valid-next-paths p edges finished-paths)) finished-paths)))
      finished-paths)))

(defn create-undirected-edge-set [pairs]
  (->> pairs
       (mapcat (fn [[x y]] [[x y] [y x]]))
       (into #{})))

(defn solve [input]
  (->> input
       (map parse-line)
       create-undirected-edge-set
       compute-paths
       count))

(defn solve-file [input]
  (-> input
      slurp
      (s/split #"\n")
      solve
      prn))

(solve-file (first *command-line-args*))
