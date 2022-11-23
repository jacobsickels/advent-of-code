(ns advent-of-code.2015.day-09
  (:require [advent-of-code.shared.read-file :as read]
            [clojure.string :as str]
            [clojure.math.combinatorics :as combo]
            [clojure.set :as set]))

(defn parse-line [s]
  (let [[f r] (str/split s #" to ")
        [s n] (str/split r #" = ")]
    (vector (set [f s]) (Integer/parseInt n))))

(def parsed (->> (read/read-file "resources/2015/day_09.txt")
                 (map parse-line)
                 (into {})))

(defn get-unique-cities [col]
  (apply set/union (map first col)))

(defn traverse-cities [cities col]
  (loop [current (first cities)
         others (rest cities)
         acc 0]
    (if (empty? others)
      acc
      (recur (first others)
             (rest others)
             (+ acc (get col (set [current (first others)])))))))

(defn part-1 [col]
  (loop [city-checks (combo/permutations (get-unique-cities col))
         acc {}]
    (if (empty? city-checks)
      (second (apply min-key val acc))
      (recur (rest city-checks)
             (assoc acc (first city-checks) (traverse-cities (first city-checks) col))))))

(defn part-2 [col]
  (loop [city-checks (combo/permutations (get-unique-cities col))
         acc {}]
    (if (empty? city-checks)
      (second (apply max-key val acc))
      (recur (rest city-checks)
             (assoc acc (first city-checks) (traverse-cities (first city-checks) col))))))