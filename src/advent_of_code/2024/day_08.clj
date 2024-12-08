(ns advent-of-code.2024.day-08
  (:require [advent-of-code.shared.read-file :as read]
            [clojure.set :as set]
            [clojure.string :as str]))

(def data (->> (read/read-file "resources/2024/day_08.txt")))

(def points (->> (map #(str/split % #"") data)
                 (map-indexed (fn [y line]
                                (map-indexed (fn [x space] [space x y]) line)))
                 (apply concat)))

(def antennas (->> (remove #(= "." (first %)) points)
                   (group-by first)))

(defn get-antinode [a1 a2]
  (let [[_ a1x a1y] a1
        [_ a2x a2y] a2
        diff-x (- a1x a2x)
        diff-y (- a1y a2y)]
    [(+ a1x diff-x) (+ a1y diff-y)]))


(defn get-antinodes [antenna others]
  (map #(get-antinode antenna %) others))

(defn get-all-antinodes [antennas]
  (->> (mapcat #(get-antinodes % (remove (fn [a] (= a %)) antennas)) antennas)
       set))

(def valid-points (set (map (fn [[p x y]] [x y]) points)))
(def max-height (second (last (sort-by second valid-points))))
(def max-width (first (last (sort-by first valid-points))))
(defn is-point-valid? [[x y]]
  (let [min-height 0
        min-width 0]
    (and (<= min-height y max-height) (<= min-width x max-width))))

(defn part-1 []
  (->> (reduce-kv (fn [acc k v] (assoc acc k (get-all-antinodes v))) {} antennas)
       vals
       (apply set/union)
       (filter is-point-valid?)
       count))


(defn get-all-antinodes-in-line [a1 a2]
  (let [[_ a1x a1y] a1
        [_ a2x a2y] a2
        diff-x (- a1x a2x)
        diff-y (- a1y a2y)]
    (loop [acc [[a1x a1y]]
           [c1x c1y] [a1x a1y]]
      (let [antinode [(+ c1x diff-x) (+ c1y diff-y)]]
        (if (is-point-valid? antinode)
          (recur (conj acc antinode) antinode)
          acc)))))

(defn get-line-antinodes [antenna others]
  (mapcat #(get-all-antinodes-in-line antenna %) others))

(defn get-all-line-antinodes [antennas]
  (->> (mapcat #(get-line-antinodes % (remove (fn [a] (= a %)) antennas)) antennas)))

(defn part-2 []
  (->> (reduce-kv (fn [acc k v] (assoc acc k (get-all-line-antinodes v))) {} antennas)
       vals
       (apply concat)
       set
       count))