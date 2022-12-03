(ns advent-of-code.2022.day-03
  (:require [advent-of-code.shared.read-file :as read]
            [clojure.set :as set]))

(def data (read/read-file "resources/2022/day_03.txt"))

(defn get-char-amount [c]
  (if (Character/isUpperCase c)
    (- (int c) 38)
    (- (int c) 96)))

(defn part-1 []
  (->> (map #(partition (/ (count %) 2) %) data)
       (map #(map set %))
       (map #(apply set/intersection %))
       (map #(get-char-amount (first %)))
       (reduce +)))

(defn part-2 []
  (->> (map set data)
       (partition 3)
       (map #(apply set/intersection %))
       (map #(get-char-amount (first %)))
       (reduce +)))