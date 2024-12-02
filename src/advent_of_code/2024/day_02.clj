(ns advent-of-code.2024.day-02
  (:require [advent-of-code.shared.read-file :as read]
            [advent-of-code.shared.utils :as utils]
            [clojure.string :as str]))

(defn parse [input]
  (map #(utils/parse-int-col (str/split % #" ")) input))


(def data (parse (vec (read/read-file "resources/2024/day_02.txt"))))

(def test-data (parse ["7 6 4 2 1"
                       "1 2 7 8 9"
                       "9 7 6 2 1"
                       "1 3 2 4 5"
                       "8 6 4 4 1"
                       "1 3 6 7 9"]))

(defn differences [col]
  (loop [data col
         acc []]
    (if (or (empty? data) (nil? (second data)))
      acc
      (recur (rest data)
             (conj acc (- (first data) (second data)))))))

(defn decreasing-safe? [col]
  (let [diffs (differences col)]
    (every? #(and (<= 1 %) (>= 3 %)) diffs)))

(defn increasing-safe? [col]
  (let [diffs (differences col)]
    (every? #(and (>= -1 %) (<= -3 %)) diffs)))

(defn is-safe? [col]
  (let [diff (- (first col) (second col))
        is-increasing? (neg? diff)]
    (if is-increasing?
      (increasing-safe? col)
      (decreasing-safe? col))))

(defn part-1 [col]
  (->> (map is-safe? col)
       frequencies))

(defn is-safe-2? [col]
  (let [indexes (range 0 (count col))]
    (->> (map #(utils/dissoc-idx col %) indexes)
         (some #(is-safe? (vec %)))
         nil?
         not)))

(defn part-2 [col]
  (->> (map vec col)
       (map is-safe-2?)
       frequencies))
