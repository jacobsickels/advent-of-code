(ns advent-of-code.2023.day-21
  (:require [advent-of-code.shared.point :as points]
            [advent-of-code.shared.read-file :as read]
            [clojure.math.combinatorics :as combo]
            [clojure.set :as set]
            [clojure.string :as str]))

(def data (read/read-file "resources/2023/day_21.txt"))

(def starting-point (let [[y col] (->> (map-indexed (fn [i col] (if (str/includes? col "S") [i col] [i nil])) data)
                                       (filter #(not (nil? (second %))))
                                       first)]
                      [(str/index-of col "S") y]))

(def finish-point (let [[y col] (->> (map-indexed (fn [i col] (if (str/includes? col "S") [i col] [i nil])) data)
                                     (filter #(not (nil? (second %))))
                                     first)]
                    [(str/index-of col "F") y]))

(def walls
  (->> data
       (map-indexed (fn [row line]
                      (map-indexed (fn [column character] (if (= \# character) [column row] nil))
                                   line)))
       (apply concat)
       (remove nil?)
       set))

(defn part-1 []
  (loop [points #{starting-point}
         steps 0]
    (if (= steps 64)
      points
      (let [next-steps (set/difference (set (mapcat points/cardinal-points-around points)) walls)]
        (recur next-steps (inc steps))))))

(declare memoized-is-point-wall?)

(defn is-point-wall? [[x y _]]
  (let [width (count (first data))
        height (count data)]
    (= \# (get-in data (reverse [(mod x width) (mod y height)])))))

(def memoized-is-point-wall? (memoize is-point-wall?))

(defn collapse-points [points]
  (let [width (count (first data))
        height (count data)]
    (->> (map (fn [[x y c]] [(mod x width) (mod y height) c]) points)
         (sort-by (fn [[x y c]] [x y]))
         (partition-by (fn [[x y c]] [x y]))
         (map (fn [col] (conj (vec (take 2 (first col))) (->> (map last col)
                                                              (reduce +))))))))


(defn remove-duplicates [points]
  (loop [p points
         visited #{}
         result []]
    (if (empty? p)
      result
      (if (contains? visited (take 2 (first p)))
        (recur (rest p) visited result)
        (recur (rest p) (conj visited (take 2 (first p))) (conj result (first p)))))))


(defn part-2 []
  (loop [points [(conj starting-point 1)]
         steps 0]
    ;(println points)
    (if (= steps 8)
      points
      (let [next-steps (->> points
                            (mapcat (fn [[x y amt]] (map (fn [point] (conj point amt)) (points/cardinal-points-around [x y]))))
                            (filter (fn [[x y amt]] (not (is-point-wall? [x y]))))
                            remove-duplicates
                            collapse-points)]
        (recur next-steps (inc steps))))))
