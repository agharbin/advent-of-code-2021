(require '[clojure.string :as s])
(require '[clojure.pprint :as pp])

(def initial-registers {:w 0 :x 0 :y 0 :z 0})

(defn parse-int [x] (Integer/parseInt x))

(defn keyword-or-literal [x]
  (if (contains? #{"w" "x" "y" "z"} x)
    (keyword x)
    (parse-int x)))

(defn parse-instruction [instruction]
  (let [[ins arg1 arg2] instruction]
    (if (= ins "inp")
      [(keyword ins) (keyword arg1)]
      [(keyword ins) (keyword arg1) (keyword-or-literal arg2)])))

(defn parse-line [line]
  (-> line
      (s/split #"\s+")
      parse-instruction))

(defn parse-input [input]
  (->> input
       (s/split-lines)
       (map parse-line)))

(defn evaluate [{{w :w x :x y :y z :z :as registers} :registers input :input} [instruction arg1 arg2]]
  (let [arg1-val (registers arg1)
        arg2-val (if (keyword? arg2) (registers arg2) arg2)]
    (case instruction
      :inp {:registers (assoc registers arg1 (first input)) :input (rest input)}
      :add {:registers (assoc registers arg1 (+ arg1-val arg2-val)) :input input}
      :mul {:registers (assoc registers arg1 (* arg1-val arg2-val)) :input input}
      :div {:registers (assoc registers arg1 (quot arg1-val arg2-val)) :input input}
      :mod {:registers (assoc registers arg1 (mod arg1-val arg2-val)) :input input}
      :eql {:registers (assoc registers arg1 (if (= arg1-val arg2-val) 1 0)) :input input})))

(defn run-program [inputs instruction-set]
  (reduce evaluate {:registers initial-registers :input inputs} instruction-set))

(defn show-z-val [input]
  (let [z (get-in input [:registers :z])]
    ((juxt
       #(-> % (mod 26))
       #(-> % (quot 26) (mod 26))
       #(-> % (quot 26) (quot 26) (mod 26))
       #(-> % (quot 26) (quot 26) (quot 26) (mod 26))
       #(-> % (quot 26) (quot 26) (quot 26) (quot 26) (mod 26))
       #(-> % (quot 26) (quot 26) (quot 26) (quot 26) (quot 26) (mod 26))
       #(-> % (quot 26) (quot 26) (quot 26) (quot 26) (quot 26) (quot 26) (mod 26)))
     z)))

(defn find-zero [input p1 p2 program]
  (let [combinations (for [x (range 1 10) y (range 1 10)] [x y])]
    (loop [xs combinations]
      (when (seq xs)
        (let [[x y] (first xs)
              test-input (-> input (assoc p1 x) (assoc p2 y))
              computed-result (reduce evaluate {:registers initial-registers :input test-input} program)]
          (when (= 0 (get-in computed-result [:registers :z]))
            (prn x y)))
        (recur (rest xs))))))

(defn solve-file [input]
  (->> input
       slurp
       parse-input
       ;(find-zero [1 1 7 2 1 1 5 1 1 1 8 1 7 5] 0 13)
       (run-program [1 1 7 2 1 1 5 1 1 1 8 1 7 5])
       show-z-val
       pp/pprint))

(solve-file (first *command-line-args*))
