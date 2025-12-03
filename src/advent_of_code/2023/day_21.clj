(ns advent-of-code.2023.day-21
  (:require [advent-of-code.shared.point :as points]
            [advent-of-code.shared.read-file :as read]
            [clojure.math.combinatorics :as combo]
            [clojure.set :as set]
            [clojure.string :as str]))

(def data (read/read-file "resources/2023/day_21.txt"))

(def width (count (first data)))
(def height (count data))
(def starting-point (let [[y col] (->> (map-indexed (fn [i col] (if (str/includes? col "S") [i col] [i nil])) data)
                                       (filter #(not (nil? (second %))))
                                       first)]
                      [(str/index-of col "S") y]))

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
      (count points)
      (let [next-steps (set/difference (set (mapcat points/cardinal-points-around points)) walls)]
        (recur next-steps (inc steps))))))


(defn is-wall? [[x y]]
  (or (contains? walls [x y]) (contains? walls [(mod x width) (mod y height)])))

(defn points-around-keep [[[x y] n]]
  (->> (points/cardinal-points-around [x y])
       (filter #(not (is-wall? %)))
       (map #(conj [%] n))
       (into {})))

;(defn collapse-counts [points]
;  (->> (group-by first points)
;       (map (fn [[k v]] [k (count v)]))))

(defn translate-outside-point [[[x y] n]]
  [[(mod x width) (mod y height)] n])

(defn get-next-steps [points-map]
  (reduce (fn [acc [k v]] (assoc acc k (reduce + (map last v))))
          {}
          (->> (map points-around-keep points-map)
               (apply merge)
               #_(set)
               (map translate-outside-point)
               (group-by first))))

(defn get-next-steps-2 [points-map]
  (->> (map points-around-keep points-map)
       (apply merge)
       #_(set)
       (map translate-outside-point)
       #_(group-by first)))

(defn part-2 []
  (loop [points {starting-point 1}
         steps 0]
    (println points)
    (if (= steps 6)
      (reduce + (vals points))
      (let [next-steps (get-next-steps points)]
        (recur next-steps (inc steps))))))