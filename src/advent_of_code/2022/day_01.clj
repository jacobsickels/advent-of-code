(ns advent-of-code.2022.day-01
  (:require [advent-of-code.shared.read-file :as read]
            [advent-of-code.shared.utils :as utils]))

(def data (->> (read/read-file "resources/2022/day_01.txt")
               (partition-by empty?)
               (remove #(= (first %) ""))
               (map utils/parse-int-col)))

(defn part-1 []
  (apply max (map #(reduce + %) data)))

(defn part-2 []
  (reduce + (take-last 3 (sort (map #(reduce + %) data)))))