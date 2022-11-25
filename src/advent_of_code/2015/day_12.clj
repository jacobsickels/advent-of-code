(ns advent-of-code.2015.day-12
  (:require [clojure.data.json :as json]
            [advent-of-code.shared.read-file :as read]))

(defn get-sum [col]
  (loop [c col
         sum 0]
    (if (empty? c)
      sum
      (cond
        (coll? (first c))
        (recur (rest c) (+ sum (get-sum (first c))))

        (number? (first c))
        (recur (rest c) (+ sum (first c)))

        :else
        (recur (rest c) sum)))))

(defn part-1 []
  (get-sum (json/read-str (first (read/read-file "resources/2015/day_12.txt")))))

(defn get-sum-no-red [col]
  (loop [c col
         sum 0]
    (if (empty? c)
      sum
      (cond
        (map? (first c))
        (if (contains? (set (vals (first c))) "red")
          (recur (rest c) (+ sum 0))
          (recur (rest c) (+ sum (get-sum-no-red (first c)))))

        (coll? (first c))
        (recur (rest c) (+ sum (get-sum-no-red (first c))))

        (number? (first c))
        (recur (rest c) (+ sum (first c)))

        :else
        (recur (rest c) sum)))))


(defn part-2 []
  (get-sum-no-red (json/read-str (first (read/read-file "resources/2015/day_12.txt")))))
