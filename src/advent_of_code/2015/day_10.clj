(ns advent-of-code.2015.day-10)

(defn get-next-iteration [col]
  (->> (partition-by identity col)
       (mapcat #(vector (count %) (first %)))))

(defn part-1 []
  (loop [ittr [1 3 2 1 1 3 1 1 1 2]
         cnt 0]
    (if (= cnt 40)
      (count ittr)
      (recur (get-next-iteration ittr) (inc cnt)))))

(defn part-2 []
  (loop [ittr [1 3 2 1 1 3 1 1 1 2]
         cnt 0]
    (if (= cnt 50)
      (count ittr)
      (recur (get-next-iteration ittr) (inc cnt)))))