(ns advent-of-code.2015.day-17
  (:require [clojure.math.combinatorics :as combo]))

(def containers [50 44 11 49 42 46 18 32 26 40 21 7 18 43 10 47 36 24 22 40])

;; https://github.com/rxedu/adventofcode-2015-clojure/blob/master/src/adventofcode/day_17.clj

(defn combinations-to-size
  [containers size]
  (filter #(= size (reduce + (map second %)))
          (combo/subsets (map-indexed #(vector %1 %2) containers))))


(defn part-1 []
  (count (combinations-to-size containers 150)))

(defn part-2 []
  (let [combinations (combinations-to-size containers 150)]
    (->> (map count combinations)
         frequencies
         (sort-by first)
         first
         last)))