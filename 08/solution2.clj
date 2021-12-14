(require '[clojure.string :as s])
(require '[clojure.set :as cset])

(defn parse-int [x] (Integer/parseInt x))

(defn parse-line [line]
  (as-> line x
      (s/split x #"\|")
      (map s/trim x)
      (map #(s/split % #"\s+") x)))

(defn find-assignments [patterns]
  (let [one-pattern   (apply set (filter #(= 2 (count %)) patterns))
        four-pattern  (apply set (filter #(= 4 (count %)) patterns))
        seven-pattern (apply set (filter #(= 3 (count %)) patterns))
        eight-pattern (apply set (filter #(= 7 (count %)) patterns))
        six-pattern   (->> patterns
                           (map set)
                           (filter #(= 6 (count %)))
                           (filter #((complement cset/subset?) one-pattern %))
                           first)
        nine-pattern  (->> patterns
                           (map set)
                           (filter #(= 6 (count %)))
                           (filter #(cset/subset? one-pattern %))
                           (filter #(cset/subset? four-pattern %))
                           first)
        zero-pattern  (->> patterns
                           (map set)
                           (filter #(= 6 (count %)))
                           (filter #(not= six-pattern %))
                           (filter #(not= nine-pattern %))
                           first)
        e-segment     (cset/difference eight-pattern nine-pattern)
        three-pattern (->> patterns
                           (map set)
                           (filter #(= 5 (count %)))
                           (filter #(cset/subset? one-pattern %))
                           first)
        two-pattern   (->> patterns
                           (map set)
                           (filter #(= 5 (count %)))
                           (filter #((complement cset/subset?) one-pattern %))
                           (filter #(cset/subset? e-segment %))
                           first)
        five-pattern  (->> patterns
                           (map set)
                           (filter #(= 5 (count %)))
                           (filter #((complement cset/subset?) one-pattern %))
                           (filter #((complement cset/subset?) e-segment %))
                           first)]
    {one-pattern   \1,
     two-pattern   \2,
     three-pattern \3,
     four-pattern  \4,
     five-pattern  \5,
     six-pattern   \6,
     seven-pattern \7,
     eight-pattern \8,
     nine-pattern  \9,
     zero-pattern  \0}))

(defn solve-case [[patterns output]]
  (let [assignments (find-assignments patterns)
        output-patterns (map set output)]
    (->> output-patterns
         (map assignments)
         (apply str))))

(defn solve [input]
  (->> input
       (map parse-line)
       (map solve-case)
       (map parse-int)
       (apply +)))

(defn solve-file [input]
  (-> input
      slurp
      (s/split #"\n")
      solve
      prn))

(solve-file (first *command-line-args*))
