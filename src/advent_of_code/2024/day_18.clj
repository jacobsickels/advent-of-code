(ns advent-of-code.2024.day-18
  (:require [advent-of-code.shared.point :as points]
            [advent-of-code.shared.read-file :as read]
            [advent-of-code.shared.utils :as utils]
            [clojure.set :as set]))

(def falling-bytes (->> (read/read-file "resources/2024/day_18.txt")
                       (map #(re-seq #"\d+" %))
                       (map utils/parse-int-col)))

(defn is-invalid-point? [[x y] width height invalid-points]
  (or (neg? x)
      (neg? y)
      (>= x (inc width))
      (>= y (inc height))
      (contains? invalid-points [x y])))

(defn walkable-points
  [[x y] width height invalid-points]
  (let [points-around (->> (points/cardinal-points-around [x y])
                           (remove #(is-invalid-point? % width height invalid-points)))]
    points-around))

(defn walk [destination width height invalid-points]
  (loop [points [[0 0]]
         visited #{[0 0]}
         iterations 0]
    (if (empty? points)
      [iterations false]
      (if (contains? (set points) destination)
        [iterations true]
        (let [next-points (set (apply concat (map #(walkable-points % width height invalid-points) points)))]
          (recur (set/difference next-points visited)
                 (set/union visited next-points)
                 (inc iterations)))))))

(defn part-1 []
  (let [width 70
        height 70
        invalid-points (set (take 1024 falling-bytes))]
    (first (walk [70, 70] width height invalid-points))))

(defn part-2 []
  (let [width 70 height 70]
    (loop [cnt-take 1024]
      (let [[_ exited?] (walk [70 70] width height (set (take cnt-take falling-bytes)))]
        (if exited?
          (recur (inc cnt-take))
          (last (take cnt-take falling-bytes)))))))