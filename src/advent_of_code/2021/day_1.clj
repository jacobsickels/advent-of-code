(ns advent-of-code.2021.day-1
  (:require [advent-of-code-2021.core :as core]
            [advent-of-code.shared.read-file :as read]))

(def data (read/load-edn "day_01.edn"))

(defn part-1 [input]
  (loop [increases 0
         d input]
    (if (= (count d) 1) 
      increases
      (if (> (second d) (first d))
        (recur (inc increases) (rest d))
        (recur increases (rest d))))))

(defn part-2 []
  (part-1 (map #(apply + %) (partition 3 1 data))))