(ns advent-of-code.2018.day-08
  (:require [advent-of-code.shared.read-file :as read]))

(def test-data [2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2])

(defn find-children [col number-children]
  (when-not (or (empty? col) (zero? number-children))
    (let [partition-number (Math/floorDiv (count col) number-children)
          partitioned (partition-all partition-number col)]
      (if (not= partition-number (count (last partitioned)))
        (let [t (drop-last 2 partitioned)
              to-merge (take-last 2 partitioned)]
          (conj t (apply concat to-merge)))
        partitioned))))

(defn make-structure [col]
  (let [[num-children num-metadata] (take 2 col)
        children-col (drop-last num-metadata (drop 2 col))
        found-children (when-not (zero? num-children)
                         (find-children children-col num-children))]
    {:meta-data (take-last num-metadata col)
     :children  (map make-structure found-children)}))

(defn make-all-meta [col]
  (let [[num-children num-metadata] (take 2 col)
        children-col (drop-last num-metadata (drop 2 col))
        found-children (find-children children-col num-children)
        _ (println col)
        _ (println found-children)]
    (+
      (reduce + (map make-all-meta found-children))
      (reduce + (take-last num-metadata col)))))

(defn part-1 []
  (let [data (read/load-edn "2018/day_08.edn")
        meta-data-sum (make-all-meta data)]
    meta-data-sum))