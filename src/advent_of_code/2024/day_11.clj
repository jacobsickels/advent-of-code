(ns advent-of-code.2024.day-11)

(def test-data {125 1 17 1})
(def data {2 1 54 1 992917 1 5270417 1 2514 1 28561 1 0 1 990 1})

(defn replace-stone [stone]
  (cond
    (zero? stone)
    [1]

    (even? (count (str stone)))
    (let [stone-str (str stone)]
      (->> (partition-all (/ (count stone-str) 2) stone-str)
           (map #(apply str %))
           (map #(BigInteger. %))))

    :else
    [(* stone 2024)]))

(defn get-next-stones-map [stones]
  (->> stones
       (map (fn [[k v]]
              (let [replaced (replace-stone k)]
                (map (fn [s] {s v}) replaced))))
       flatten
       (apply merge-with +)))

(defn get-stone-count [input blinks]
  (loop [stone-map input
         cnt 0]
    (if (= blinks cnt)
      (reduce + (vals stone-map))
      (let [next-stone-map (get-next-stones-map stone-map)]
        (recur next-stone-map (inc cnt))))))

(defn part-1 [input]
  (get-stone-count input 25))

(defn part-2 [input]
  (get-stone-count input 75))