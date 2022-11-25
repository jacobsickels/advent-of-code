(ns advent-of-code.2015.day-10)

(defn get-next-iteration [col]
  (->> (partition-by identity col)
       (mapcat #(vector (count %) (first %)))))

(defn loop-to-iteration [max-cnt]
  (loop [ittr [1 3 2 1 1 3 1 1 1 2]
         cnt 0]
    (if (= cnt max-cnt)
      (count ittr)
      (recur (get-next-iteration ittr) (inc cnt)))))

(defn part-1 [] (loop-to-iteration 40))
(defn part-2 [] (loop-to-iteration 50))