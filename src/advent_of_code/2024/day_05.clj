(ns advent-of-code.2024.day-05
  (:require [advent-of-code.shared.read-file :as read]
            [advent-of-code.shared.utils :as utils]
            [clojure.math.combinatorics :as combo]
            [clojure.set :as set]
            [clojure.string :as str]))

(def data (->> (read/read-file "resources/2024/day_05.txt")))

(def rules (->> (partition-by empty? data)
                first
                (map #(str/split % #"\|"))
                (map utils/parse-int-col)))

(def updates (->> (partition-by empty? data)
                  last
                  (map #(str/split % #","))
                  (map utils/parse-int-col)))

(defn follows-rule? [update rule]
  (let [rule-values (filter #(contains? (set rule) %) update)]
    (if (= 2 (count rule-values))
      (= rule rule-values)
      ;; If this count is 1 this doesn't have the values to follow the rule
      true)))

(defn is-page-valid? [page update rules]
  (let [relavent-rules (filter #(or (= (first %) page) (= (second %) page)) rules)]
    (every? #(follows-rule? update (vec %)) relavent-rules)))

(defn is-update-valid? [update rules]
  (every? #(is-page-valid? % update rules) update))

(defn get-middle [update]
  (nth update (quot (count update) 2)))

(defn part-1 []
  (->> (filter #(is-update-valid? % rules) updates)
       (map get-middle)
       (reduce +)))


(defn is-valid-2-pages? [[p1 p2]]
  (every? #(follows-rule? [p1 p2] %) rules))

(defn do-swap [col]
  (concat (reverse (take 2 col)) (drop 2 col)))

(defn do-swaps [bad-update]
  (loop [col bad-update
         swaps 0
         acc []]
    (if (empty? col)
      [acc swaps]
      (if (is-valid-2-pages? [(first col) (second col)])
        (recur (rest col) swaps (conj acc (first col)))
        (recur (do-swap col) (inc swaps) acc)))))

(defn fix-bad-update [bad-update]
  (loop [col bad-update]
    (let [[swapped swaps] (do-swaps col)]
      (if (zero? swaps)
        swapped
        (recur swapped)))))

(defn part-2 []
  (->> (remove #(is-update-valid? % rules) updates)
       (map fix-bad-update)
       (map get-middle)
       (reduce +)))
