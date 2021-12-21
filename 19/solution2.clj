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
          [transposed dist]
          nil)))))

(defn get-mapping [source-scanner target-scanner]
  (first (filter some? (find-all-mappings source-scanner target-scanner))))

(defn get-distances [input]
  (let [map-0 (->> (nth input 0) (map #(apply vector %)) (into #{}))
       [map-24 dist-24] (get-mapping (nth input 24) map-0)
       [map-16 dist-16] (get-mapping (nth input 16) map-24)
       [map-18 dist-18] (get-mapping (nth input 18) map-24)
       [map-5 dist-5]   (get-mapping (nth input 5) map-16)
       [map-2 dist-2]   (get-mapping (nth input 2) map-18)
       [map-13 dist-13] (get-mapping (nth input 13) map-18)
       [map-21 dist-21] (get-mapping (nth input 21) map-2)
       [map-15 dist-15] (get-mapping (nth input 15) map-2)
       [map-3 dist-3]   (get-mapping (nth input 3) map-13)
       [map-17 dist-17] (get-mapping (nth input 17) map-21)
       [map-4 dist-4]   (get-mapping (nth input 4) map-21)
       [map-11 dist-11] (get-mapping (nth input 11) map-21)
       [map-12 dist-12] (get-mapping (nth input 12) map-15)
       [map-23 dist-23] (get-mapping (nth input 23) map-17)
       [map-10 dist-10] (get-mapping (nth input 10) map-17)
       [map-14 dist-14] (get-mapping (nth input 14) map-17)
       [map-6 dist-6]   (get-mapping (nth input 6) map-11)
       [map-7 dist-7]   (get-mapping (nth input 7) map-11)
       [map-8 dist-8]   (get-mapping (nth input 8) map-12)
       [map-9 dist-9]   (get-mapping (nth input 9) map-12)
       [map-1 dist-1]   (get-mapping (nth input 1) map-23)
       [map-22 dist-22] (get-mapping (nth input 22) map-14)
       [map-20 dist-20] (get-mapping (nth input 20) map-7)
       [map-19 dist-19]  (get-mapping (nth input 19) map-1)]
   [[0 0 0] 
    dist-24
    dist-16
    dist-18
    dist-5 
    dist-2 
    dist-13
    dist-21
    dist-15
    dist-3 
    dist-17
    dist-4 
    dist-11
    dist-12
    dist-23
    dist-10
    dist-14
    dist-6 
    dist-7 
    dist-8 
    dist-9 
    dist-1 
    dist-22
    dist-20
    dist-19]))

(defn solve [distances]
  (apply max 
    (for [x distances y distances :when (not= x y)]
      (->> (vec-op - x y)
           (map #(if (< % 0) (* -1 %) %))
           (apply +)))))

(defn solve-file [input]
  (-> input
      slurp
      parse-input
      get-distances
      solve
      pp/pprint))

(solve-file (first *command-line-args*))
