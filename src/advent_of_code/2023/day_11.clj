(ns advent-of-code.2023.day-11
  (:require [advent-of-code.shared.point :as point]
            [advent-of-code.shared.read-file :as read]
            [clojure.math.combinatorics :as combo]))

(def data (read/read-file "resources/2023/day_11.txt"))

(def initial-galaxies (->> (read/read-file "resources/2023/day_11.txt")
                           (map-indexed (fn [row line]
                                          (map-indexed (fn [column character] (if (= \# character) [column row] nil))
                                                       line)))
                           (apply concat)
                           (remove nil?)))

(defn find-empty-columns []
  (->> (map (fn [n] (map #(nth % n) data))
            (range 0 (count data)))
       (map frequencies)
       (map count)
       (map-indexed (fn [n char-count] (if (= 1 char-count) n nil)))
       (remove nil?)))


(defn find-empty-rows []
  (->> (map frequencies data)
       (map count)
       (map-indexed (fn [n char-count] (if (= 1 char-count) n nil)))
       (remove nil?)))

(defn translate-point-column [[x y] empty-columns diff]
  (let [dx (->> (map #(> x %) empty-columns)
                (filter true?)
                count)]
    [(+ (* dx (dec diff)) x) y]))

(defn translate-point-row [[x y] empty-rows diff]
  (let [dy (->> (map #(> y %) empty-rows)
                (filter true?)
                count)]
    [x (+ (* dy (dec diff)) y)]))

(defn translate-point [[x y] empty-rows empty-columns diff]
  (-> (translate-point-column [x y] empty-columns diff)
      (translate-point-row empty-rows diff)))

(defn expand-galaxies []
  (let [empty-rows (find-empty-rows)
        empty-columns (find-empty-columns)]
    (->> (map (fn [point] (translate-point point empty-rows empty-columns 2)) initial-galaxies))))

(defn galaxy-distances [expand-galaxies-fn]
  (let [galaxies (expand-galaxies-fn)
        galaxy-pairs (->> (combo/cartesian-product (range 0 (count galaxies)) (range 0 (count galaxies)))
                          (remove (fn [[x y]] (= x y)))
                          (map set)
                          set)]
    (->> (map (fn [pair]
                [pair (point/manhattan-distance (nth galaxies (first pair)) (nth galaxies (second pair)))])
              galaxy-pairs)
         (map second)
         (reduce +))))

(defn part-1 []
  (galaxy-distances expand-galaxies))

(defn expand-galaxies-million []
  (let [empty-rows (find-empty-rows)
        empty-columns (find-empty-columns)]
    (->> (map (fn [point] (translate-point point empty-rows empty-columns 1000000)) initial-galaxies))))

(defn part-2 []
  (galaxy-distances expand-galaxies-million))