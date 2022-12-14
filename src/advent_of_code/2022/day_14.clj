(ns advent-of-code.2022.day-14
  (:require [advent-of-code.shared.utils :as utils]
            [advent-of-code.shared.read-file :as read]
            [clojure.set :as set]))

(defn line-to-points [line]
  (->> (re-seq #"\d+" line)
       utils/parse-int-col
       (partition 2)
       (partition 2 1)
       (mapcat (fn [[p1 p2]] (utils/points-between p1 p2)))
       set))

(defn get-rocks []
  (->> (read/read-file "resources/2022/day_14.txt")
       (mapcat line-to-points)
       set))

(defn is-obstacle-below? [obstacles [x y]]
  (not-empty (set/intersection obstacles (set [[x (inc y)]]))))

(defn is-obstacle-left? [obstacles [x y]]
  (not-empty (set/intersection obstacles (set [[(dec x) (inc y)]]))))

(defn is-obstacle-right? [obstacles [x y]]
  (not-empty (set/intersection obstacles (set [[(inc x) (inc y)]]))))

(defn drop-sand [rocks rested-sand max-y]
  (let [obstacles (set/union rocks rested-sand)]
    (loop [[sandX sandY] [500 0]]
      (cond
        (> sandY max-y)
        nil

        (not (is-obstacle-below? obstacles [sandX sandY]))
        (recur [sandX (inc sandY)])

        (not (is-obstacle-left? obstacles [sandX sandY]))
        (recur [(dec sandX) (inc sandY)])

        (not (is-obstacle-right? obstacles [sandX sandY]))
        (recur [(inc sandX) (inc sandY)])

        :else [sandX sandY]))))

(defn part-1 []
  (let [rocks (get-rocks)
        abyss (->> (sort-by second rocks)
                   last
                   last)]
    (loop [rested-sand #{}]
      (if-let [sand-position (drop-sand rocks rested-sand abyss)]
        (recur (conj rested-sand sand-position))
        (count rested-sand)))))

(defn drop-sand-floor [rocks rested-sand floor]
  (let [obstacles (set/union rocks rested-sand)]
    (loop [[sandX sandY] [500 0]]
      (cond
        (= sandY (dec floor))
        [sandX sandY]

        (not (is-obstacle-below? obstacles [sandX sandY]))
        (recur [sandX (inc sandY)])

        (not (is-obstacle-left? obstacles [sandX sandY]))
        (recur [(dec sandX) (inc sandY)])

        (not (is-obstacle-right? obstacles [sandX sandY]))
        (recur [(inc sandX) (inc sandY)])

        :else [sandX sandY]))))

(defn part-2 []
  (let [rocks (get-rocks)
        abyss (->> (sort-by second rocks)
                   last
                   last)]
    (loop [rested-sand #{}]
      (let [sand-position (drop-sand-floor rocks rested-sand (+ 2 abyss))]
        (if (= sand-position [500 0])
          (inc (count rested-sand))
          (recur (conj rested-sand sand-position)))))))






