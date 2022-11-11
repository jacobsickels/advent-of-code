(ns advent-of-code.2020.day-23)

(def test-data [3 8 9 1 2 5 4 6 7])
(def data [3 9 8 2 5 4 7 1 6])

(defn make-loop [col]
  (second (reduce
            (fn [[l acc] item]
              (if (nil? l)
                [item acc]
                [item (assoc acc l item)]))
            [nil {(last col) (first col)}]
            col)))

(defn- get-next-three [col point]
  (loop [count 3
         found point
         acc []]
    (if (zero? count)
      acc
      (recur (dec count) (get col found) (conj acc (get col found))))))

(defn get-next-destination [col three-to-check point]
  (loop [next-point (if (zero? (dec point)) (count col) (dec point))]
    (if (contains? (set three-to-check) next-point)
      (recur (if (zero? (dec next-point)) (count col) (dec next-point)))
      next-point)))

(defn do-move [col point]
  (let [three-away (get-next-three col point)
        destination (get-next-destination col three-away point)]
    [(get col (last three-away))
     (assoc col point (get col (last three-away))
                destination (get col point)
                (last three-away) (get col destination))]))

(defn get-cups-after-one [col]
  (loop [found []
         point 1]
    (if (= (get col point) 1)
      found
      (recur (conj found (get col point)) (get col point)))))

(defn part-1 [col]
  (loop [data (make-loop col)
         move (first col)
         move-count 0]
    (if (> move-count 99)
      (get-cups-after-one data)
      (let [[next-move new-data] (do-move data move)]
        (recur new-data
               next-move
               (inc move-count))))))

(def part-2-col (concat data (range (inc (apply max data)) (inc 1000000))))
(def part-2-test-col (concat test-data (range (inc (apply max test-data)) (inc 1000000))))

(defn part-2 [col]
  (loop [data (make-loop col)
         move (first col)
         move-count 0]
    (if (>= move-count 10000000)
      (reduce * [(get data 1) (get data (get data 1))])
      (let [[next-move new-data] (do-move data move)]
        (recur new-data
               next-move
               (inc move-count))))))