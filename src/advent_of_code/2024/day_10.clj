(ns advent-of-code.2024.day-10
  (:require [advent-of-code.shared.point :as points]
            [advent-of-code.shared.read-file :as read]
            [clojure.string :as str]))

(def data (->> (read/read-file "resources/2024/day_10.txt")))

(def points (->> (map #(str/split % #"") data)
                 (map-indexed (fn [y line]
                                (map-indexed (fn [x space] [(Integer/parseInt space) x y]) line)))
                 (apply concat)))

(def starts (->> (filter #(zero? (first %)) points)))

(defn get-next-points [[v x y]]
  (let [points-around (set (points/cardinal-points-around [x y]))
        data-points-around (filter (fn [[v x y]] (contains? points-around [x y])) points)]
    (filter #(= (inc v) (first %)) data-points-around)))

(defn get-next-trails [trail]
  (let [curr (last trail)
        next-points (get-next-points curr)]
    (map #(conj trail %) next-points)))

(defn follow-paths [[v x y]]
  (loop [acc [[[v x y]]]]
    (let [trail-ends (map last acc)
          trail-end-values (set (map first trail-ends))]
      (if (and (= 1 (count trail-end-values)) (= 9 (first trail-end-values)))
        acc
        (recur (mapcat get-next-trails acc))))))

(defn part-1 []
  (->> (mapcat follow-paths starts)
       (sort-by first)
       (partition-by first)
       (map #(set (map last %)))
       (map count)
       (reduce +)))

(defn part-2 []
  (->> (mapcat follow-paths starts)
       (sort-by first)
       (partition-by first)
       (map set)
       (map count)
       (reduce +)))
