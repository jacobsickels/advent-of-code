(ns advent-of-code.2025.day-06
  (:require [advent-of-code.shared.read-file :as read]
            [clojure.string :as str]))

(def data (->> (read/read-file "resources/2025/day_06.txt")
               (map #(str/split % #" "))
               (map #(remove empty? %))))

(def columns (let [rows (butlast data)
                   indexes (range 0 (count (first rows)))]
               (map #(map (fn [row] (Integer/parseInt (nth row %))) rows) indexes)))

(def ops (map #(if (= % "+") + *) (last data)))

(defn part-1 []
  (->> (map (fn [c o] (apply o c)) columns ops)
       (reduce +)))

(defn read-in-cephalopod []
  (let [data (->> (read/read-file "resources/2025/day_06.txt")
                  (butlast))
        max-count (count (last (sort-by count data)))
        indexes (range 0 max-count)]
    (->> (map #(map (fn [row] (nth row % \space)) data) indexes)
         (map #(str/trim (apply str %)))
         (partition-by empty?)
         (remove #(= 1 (count %)))
         (map #(map (fn [n] (Integer/parseInt n)) %)))))

(defn part-2 []
  (->> (map (fn [c o] (apply o c)) (read-in-cephalopod) ops)
       (reduce +)))
