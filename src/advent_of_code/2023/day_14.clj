(ns advent-of-code.2023.day-14
  (:require [advent-of-code.shared.read-file :as read]
            [advent-of-code.shared.utils :as utils]
            [clojure.math.combinatorics :as combo]
            [clojure.set :as set]))

(def data (->> (read/read-file "resources/2023/day_14.txt")))

(defn find-rocks [rock-type]
  (->> (read/read-file "resources/2023/day_14.txt")
       (map-indexed (fn [row line]
                      (map-indexed (fn [column character] (if (= rock-type character) [column row] nil))
                                   line)))
       (apply concat)
       (remove nil?)))

(defn border-points [width height]
  (->> (combo/cartesian-product (range -1 (inc width)) (range -1 (inc height)))
       (filter (fn [[x y]] (or (neg? x) (neg? y) (= x  width) (= y height))))
       (map vec)
       set))

(def round-rocks (find-rocks \O))
(def square-rocks (set (find-rocks \#)))
(def border (border-points (count (first data)) (count data)))

(def unmovable (set/union square-rocks border))

(defn tilt-rock [next-rock-fn starting-rock-point rested-round-rocks]
  (loop [[x y] starting-rock-point]
    (let [next-rock-point (next-rock-fn [x y])]
      (if (contains? (set/union rested-round-rocks unmovable) next-rock-point)
        [x y]
        (recur next-rock-point)))))

(defn tilt-rock-north [starting-rock-point rested-round-rocks]
  (tilt-rock (fn [[x y]] [x (dec y)]) starting-rock-point rested-round-rocks))

(defn tilt-rock-west [starting-rock-point rested-round-rocks]
  (tilt-rock (fn [[x y]] [(dec x) y]) starting-rock-point rested-round-rocks))

(defn tilt-rock-south [starting-rock-point rested-round-rocks]
  (tilt-rock (fn [[x y]] [x (inc y)]) starting-rock-point rested-round-rocks))

(defn tilt-rock-east [starting-rock-point rested-round-rocks]
  (tilt-rock (fn [[x y]] [(inc x) y]) starting-rock-point rested-round-rocks))

(defn part-1 []
  (->> (reduce (fn [acc rock] (conj acc (tilt-rock-north rock acc))) #{} round-rocks)
       (map (fn [[x y]] (- (count data) y)))
       (reduce +)))


(defn tilt-rocks-direction [direction-fn rounded]
  (reduce (fn [acc rock] (conj acc (direction-fn rock acc))) #{} rounded))


(defn cycle-rocks [rounded]
  (let [rounded-sorted-for-north (->> (sort-by second rounded)
                                      (partition-by second)
                                      (mapcat #(sort-by first %)))
        tilted-north (tilt-rocks-direction tilt-rock-north rounded-sorted-for-north)
        round-sorted-for-west (->> (sort-by second tilted-north)
                                   (partition-by second)
                                   (mapcat #(sort-by first %)))
        tilted-west (tilt-rocks-direction tilt-rock-west round-sorted-for-west)
        round-sorted-for-south (->> (sort-by second > tilted-west)
                                    (partition-by second)
                                    (mapcat #(sort-by first %)))
        tilted-south (tilt-rocks-direction tilt-rock-south round-sorted-for-south)
        round-sorted-for-east (->> (sort-by second tilted-south)
                                   (partition-by second)
                                   (mapcat #(sort-by first > %)))
        tilted-east (tilt-rocks-direction tilt-rock-east round-sorted-for-east)]
    tilted-east))

(defn get-remaining-cycles [start increment]
  (dec (rem (- 1000000000 start) increment)))

(defn part-2 [rounded]
  (loop [rounded-rocks rounded
         results []
         iterations 0]
    (println "iterations" iterations results)
    (let [next-rocks (cycle-rocks rounded-rocks)
          next-rock-board-number (hash next-rocks)
          hash-results (mapv first results)]
      (cond
        (contains? (set hash-results) next-rock-board-number)
        (let [start (first (utils/index-of next-rock-board-number hash-results))
              cycles-left (get-remaining-cycles start (- iterations start))]
          (nth results (+ start cycles-left)))

        :else
        (recur next-rocks (conj results [next-rock-board-number (->> next-rocks
                                                                     (map (fn [[x y]] (- (count data) y)))
                                                                     (reduce +))])
               (inc iterations))))))





