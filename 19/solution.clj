(require '[clojure.string :as s])
(require '[clojure.pprint :as pp])
(require '[clojure.set :as cset])
(require '[clojure.math.combinatorics :as combo])

(defn parse-int [x] (Integer/parseInt x))

(defn parse-line [line]
  (->> line
       (drop 1)
       (map #(s/split % #","))
       (map #(map parse-int %))))

(defn parse-input [input]
  (->> (s/split input #"\n\n")
       (map #(s/split % #"\n"))
       (map parse-line)))

(defn vec-op [op v1 v2]
  (apply vector (map op v1 v2)))

(defn all-orientations [[x y z]]
  (let [rotations [[x  y  z] [x  z  y] [y  x  z] [y  z  x] [z  x  y] [z  y  x]]
        reflections [[1 1 1] [-1 1 1] [1 -1 1] [1 1 -1] [-1 -1 1] [1 -1 -1] [-1 1 -1] [-1 -1 -1]]]
    (for [rot rotations refl reflections] (vec-op * rot refl))))

(defn find-all-mappings [source-scanner target-scanner]
  (let [source-rotations (->> source-scanner
                              (map all-orientations)
                              (apply mapv vector)
                              (map #(into #{} %)))
        target (->> target-scanner
               (map #(apply vector %))
               (into #{}))]
    (for [rotation source-rotations
          point-to-map-to target
          point-to-map-from rotation]
      (let [dist (vec-op - point-to-map-to point-to-map-from)
            transposed (into #{} (map #(vec-op + dist %) rotation))
            intersection (cset/intersection transposed target)]
        (if (>= (count intersection) 12)
          transposed
          nil)))))

(defn get-mapping [source-scanner target-scanner]
  (first (filter some? (find-all-mappings source-scanner target-scanner))))

(defn solve [input]
  (let [
        map-0  (->> (nth input 0) (map #(apply vector %)) (into #{}))
        map-24 (get-mapping (nth input 24) map-0)
        map-16 (get-mapping (nth input 16) map-24)
        map-18 (get-mapping (nth input 18) map-24)
        map-5  (get-mapping (nth input 5) map-16)
        map-2  (get-mapping (nth input 2) map-18)
        map-13 (get-mapping (nth input 13) map-18)
        map-21 (get-mapping (nth input 21) map-2)
        map-15 (get-mapping (nth input 15) map-2)
        map-3  (get-mapping (nth input 3) map-13)
        map-17 (get-mapping (nth input 17) map-21)
        map-4  (get-mapping (nth input 4) map-21)
        map-11 (get-mapping (nth input 11) map-21)
        map-12 (get-mapping (nth input 12) map-15)
        map-23 (get-mapping (nth input 23) map-17)
        map-10 (get-mapping (nth input 10) map-17)
        map-14 (get-mapping (nth input 14) map-17)
        map-6  (get-mapping (nth input 6) map-11)
        map-7  (get-mapping (nth input 7) map-11)
        map-8  (get-mapping (nth input 8) map-12)
        map-9  (get-mapping (nth input 9) map-12)
        map-1  (get-mapping (nth input 1) map-23)
        map-22 (get-mapping (nth input 22) map-14)
        map-20 (get-mapping (nth input 20) map-7)
        map-19 (get-mapping (nth input 19) map-1)
        ]
    (->> (cset/union
            map-0 
            map-24
            map-16
            map-18
            map-5 
            map-2 
            map-13
            map-21
            map-15
            map-3 
            map-17
            map-4 
            map-11
            map-12
            map-23
            map-10
            map-14
            map-6 
            map-7 
            map-8 
            map-9 
            map-1 
            map-22
            map-20
            map-19)
         count
    )
  )
)

(defn solve-file [input]
  (-> input
      slurp
      parse-input
      solve
      pp/pprint
      ))

(solve-file (first *command-line-args*))
