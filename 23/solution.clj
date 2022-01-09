(require '[clojure.string :as s])
(require '[clojure.set :as cset])
(require '[clojure.pprint :as pp])
(require '[clojure.data.priority-map :refer [priority-map]])

(def sample-state
  {{:room :H :position 0} nil
   {:room :H :position 1} nil
   {:room :H :position 3} nil
   {:room :H :position 5} nil
   {:room :H :position 7} nil
   {:room :H :position 9} nil
   {:room :H :position 10} nil
   {:room :A :position 1} :B
   {:room :A :position 2} :A
   {:room :B :position 1} :C
   {:room :B :position 2} :D
   {:room :C :position 1} :B
   {:room :C :position 2} :C
   {:room :D :position 1} :D
   {:room :D :position 2} :A})

(def winning-state
  {{:room :H :position 0} nil
   {:room :H :position 1} nil
   {:room :H :position 3} nil
   {:room :H :position 5} nil
   {:room :H :position 7} nil
   {:room :H :position 9} nil
   {:room :H :position 10} nil
   {:room :A :position 1} :A
   {:room :A :position 2} :A
   {:room :B :position 1} :B
   {:room :B :position 2} :B
   {:room :C :position 1} :C
   {:room :C :position 2} :C
   {:room :D :position 1} :D
   {:room :D :position 2} :D})

(def room-exit-positions { :A 2 :B 4 :C 6 :D 8 })
(def cost-per-move { :A 1 :B 10 :C 100 :D 1000 })
(def hallway-positions
  [{:room :H :position 0}
   {:room :H :position 1}
   {:room :H :position 3}
   {:room :H :position 5}
   {:room :H :position 7}
   {:room :H :position 9}
   {:room :H :position 10}])

(defn abs [x] (if (< x 0) (* -1 x) x))

(defn get-occupied-hallway-positions [state]
  (->> state
      (filter (fn [[{room :room pos :position} piece]] (and (= room :H) (not= piece nil))))
      (map #(:position (first %)))))

(defn is-path-clear? [state {room-1 :room pos-1 :position :as place-1} {room-2 :room pos-2 :position :as place-2}]
    (let [hallway-position-1 (if (= :H room-1) pos-1 (room-exit-positions room-1))
          hallway-position-2 (if (= :H room-2) pos-2 (room-exit-positions room-2))
          min-hallway-position (min hallway-position-1 hallway-position-2)
          max-hallway-position (max hallway-position-1 hallway-position-2)
          occupied-hallway-positions (get-occupied-hallway-positions state)
          pieces-in-the-way? (->> occupied-hallway-positions
                                  (filter #(< min-hallway-position % max-hallway-position))
                                  count
                                  (< 0))
          blocked-at-entrance-1? (and (not= :H room-1) (= 2 pos-1) (some? (state {:room room-1 :position 1})))
          blocked-at-entrance-2? (and (not= :H room-2) (= 2 pos-2) (some? (state {:room room-2 :position 1})))]
      (and (not pieces-in-the-way?) (not blocked-at-entrance-1?) (not blocked-at-entrance-2?))))

(defn get-dist [{room-1 :room pos-1 :position :as place-1} {room-2 :room pos-2 :position :as place-2}]
  (+
   (if (not= :H room-1) pos-1 0)
   (if (not= :H room-2) pos-2 0)
   (abs (- (if (= :H room-1) pos-1 (room-exit-positions room-1))
           (if (= :H room-2) pos-2 (room-exit-positions room-2))))))

(defn get-cost [piece source-place dest-place]
  (* (cost-per-move piece)
     (get-dist source-place dest-place)))

(defn reachable-states-and-costs [state {room :room pos :position :as current-place} piece]
  (cond
    (and (is-path-clear? state current-place {:room piece :position 2}) (nil? (state {:room piece :position 2})))
      [[(-> state (assoc current-place nil) (assoc {:room piece :position 2} piece)) (get-cost piece current-place {:room piece :position 2})]]
    (and (is-path-clear? state current-place {:room piece :position 1}) (nil? (state {:room piece :position 1})) (= piece (state {:room piece :position 2})))
      [[(-> state (assoc current-place nil) (assoc {:room piece :position 1} piece)) (get-cost piece current-place {:room piece :position 1})]]
    (= room :H)
      []
    :else
      (->> hallway-positions
           (filter #(nil? (state %)))
           (filter #(is-path-clear? state % current-place))
           (map #(vector (-> state (assoc % piece) (assoc current-place nil)) (get-cost piece current-place %)))
           (apply vector))))

(defn solve [start-state] 
  (loop [states-and-dists (priority-map start-state 0)
         checked-states #{}
         counter 0]
    (let [[state cost-so-far] (peek states-and-dists)]
      (cond 
        (= winning-state state) cost-so-far
        (contains? checked-states state) (recur (pop states-and-dists) checked-states (inc counter))
        :else
          (let [current-positions-with-pieces (filter (fn [[k v]] (some? v)) state)
                possible-next-states-and-costs (mapcat #(reachable-states-and-costs state (first %) (second %)) current-positions-with-pieces)
                possible-next-states-and-total-costs (map (fn [[s c]] [s (+ c cost-so-far)]) possible-next-states-and-costs)
                better-next-states (filter (fn [[s c]] (or (not (contains? states-and-dists s)) (< c (states-and-dists s)))) possible-next-states-and-total-costs)
                next-queue (into (pop states-and-dists) better-next-states)]
            (recur next-queue (conj checked-states state) (inc counter)))))))

(prn (solve sample-state))
