(require '[clojure.string :as s])

(def to-binary
  {\0 "0000" \1 "0001" \2 "0010" \3 "0011" \4 "0100" \5 "0101" \6 "0110" \7 "0111"
   \8 "1000" \9 "1001" \A "1010" \B "1011" \C "1100" \D "1101" \E "1110" \F "1111"})

(defn parse-binary [input]
  (Long/parseLong input 2))

(defn parse-literal [unconsumed-bits]
  (loop [i unconsumed-bits
         bits []]
    (let [first-five-bits (take 5 i)
          remaining-input (drop 5 i)]
      (if (= \1 (first first-five-bits))
        (recur (->> remaining-input (apply str))
               (concat bits (->> first-five-bits (drop 1) (apply str)))) ; not done
        [(->>
          (concat bits (->> first-five-bits (drop 1)))
          (apply str)
          parse-binary)
         (->> remaining-input (apply str))]))))

(defn apply-op [opcode input]
  (case opcode
    0 (apply + input)
    1 (apply * input)
    2 (apply min input)
    3 (apply max input)
    5 (if (> (first input) (second input)) 1 0)
    6 (if (< (first input) (second input)) 1 0)
    7 (if (= (first input) (second input)) 1 0)))

(declare parse-packet)

(defn parse-n-bits [type-id unconsumed-bits]
  (let [num-bits (->> unconsumed-bits (take 15) (apply str) parse-binary)]
    (loop [input (->> unconsumed-bits (drop 15) (take num-bits) (apply str))
           result []]
      (let [output (parse-packet input)]
        (if output
          (recur (second output) (conj result (first output)))
          [(apply-op type-id result) (->> unconsumed-bits (drop 15) (drop num-bits) (apply str))])))))

(defn parse-n-packets [type-id unconsumed-bits]
  (let [num-iterations (->> unconsumed-bits (take 11) (apply str) parse-binary)]
    (loop [input (->> unconsumed-bits (drop 11) (apply str))
           result []
           iterations num-iterations]
      (if (= 0 iterations)
        [(apply-op type-id result) input]
        (let [output (parse-packet input)]
          (recur (second output) (conj result (first output)) (dec iterations)))))))

(defn parse-packet [unconsumed-bits]
  (if (> (count unconsumed-bits) 6)
    (let [version (->> unconsumed-bits (take 3) (apply str) parse-binary)
          type-id (->> unconsumed-bits (drop 3) (take 3) (apply str) parse-binary)
          body-str (->> unconsumed-bits (drop 6) (apply str))
          body-val (if (= type-id 4)
                     (parse-literal body-str)
                     (if (= \0 (first body-str))
                         (parse-n-bits type-id (->> body-str (drop 1) (apply str)))
                         (parse-n-packets type-id (->> body-str (drop 1) (apply str)))))]
      body-val)))

(defn solve [input]
  (->> input
       parse-packet))

(defn parse-input [input]
  (->> input
       (map to-binary)
       (apply str)))

(defn solve-file [input]
  (-> input
      slurp
      parse-input
      solve
      prn))

(solve-file (first *command-line-args*))
