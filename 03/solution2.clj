(require '[clojure.string :as s])

(defn compute-product [oxygen-rating c02-rating]
    (* (Integer/parseInt oxygen-rating 2) (Integer/parseInt c02-rating 2)))

(defn find-oxygen-rating [position input]
  (if (= 1 (count input))
    (first input)
    (let [f (frequencies (map #(nth % position) input))
          most-common (if (> (f \0) (f \1)) \0 \1)
          filtered (filter #(= most-common (nth % position)) input)]
      (recur (inc position) filtered))))

(defn find-c02-rating [position input]
  (if (= 1 (count input))
    (first input)
    (let [f (frequencies (map #(nth % position) input))
          least-common (if (< (f \1) (f \0)) \1 \0)
          filtered (filter #(= least-common (nth % position)) input)]
      (recur (inc position) filtered))))

(defn solve [input]
  (let [oxygen-rating (find-oxygen-rating 0 input)
        c02-rating (find-c02-rating 0 input)]
    (compute-product oxygen-rating c02-rating)))

(defn solve-file [input]
  (-> input
      slurp
      (s/split #"\n")
      solve
      prn))

(solve-file (first *command-line-args*))
